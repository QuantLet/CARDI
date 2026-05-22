# Process_FirmReturns Documentation

## Overview

`Code/Firm_Test/Process_FirmReturns` contains a modular empirical-finance workflow for constructing firm-level stock returns, estimating pure returns, aggregating HC/MC/LC pool returns, computing relative volatility, and testing whether lagged CARDI or related indicators predict future HC relative volatility.

The workflow is orchestrated by `main.R` and configured through `config.R`. Each module is designed to be reusable and to avoid modifying upstream reference code.

## Directory Role

The module reads existing raw and processed project inputs and writes outputs to:

```text
Output/FirmReturns/
Output/FirmReturns/Predictability/
```

The code supports cached outputs: if an output exists and `overwrite = FALSE`, the workflow loads it rather than regenerating it.

## Main Pipeline

1. Load configuration and source helper modules.
2. Load risk-free rates, CARDI, macro controls, indicators, events, industry codes, financial controls, and carbon-emissions data.
3. Build firm-level daily returns:
   - `Return = log(price_t) - log(price_{t-1})`
   - `ExReturn = Return - IndexRiskFreeRate`
4. Construct previous-year firm controls:
   - `lag_logAsset`
   - `lag_ROE`
   - `lag_BM`
   - `lag_CapExRatio`
   - `lag_logEmissions`
5. Estimate pure returns as residuals from:

   ```text
   ExReturn_{i,t}
     ~ lag_logAsset_{i,t}
     + lag_ROE_{i,t}
     + lag_BM_{i,t}
     + lag_CapExRatio_{i,t}
     + industry fixed effects
     + error_{i,t}
   ```

   `lag_logEmissions` is retained in output datasets but is intentionally not included in the pure-return regression.

6. Build firm-level monthly returns from month-end prices.
7. Compute monthly firm volatility from daily observations within each firm-month:
   - `vola_return`
   - `vola_ExReturn`
   - `vola_PurReturn`
8. Aggregate HC/MC/LC pool-level daily and monthly returns.
9. Build HC-vs-LC relative volatility datasets:
   - `HC_Firm_Monthly.rds`
   - `HC_Monthly.rds`
10. Run selected predictability regressions.

## Key Output Files

```text
Output/FirmReturns/Firm_return_daily.rds
Output/FirmReturns/Firm_return_month.rds
Output/FirmReturns/HC_daily.rds
Output/FirmReturns/MC_daily.rds
Output/FirmReturns/LC_daily.rds
Output/FirmReturns/HC_monthly.rds
Output/FirmReturns/MC_monthly.rds
Output/FirmReturns/LC_monthly.rds
Output/FirmReturns/HC_Firm_Monthly.rds
Output/FirmReturns/HC_Monthly.rds
Output/FirmReturns/workflow_validation_summary.csv
```

Predictability outputs are written under:

```text
Output/FirmReturns/Predictability/
```

Examples:

```text
predictability_summary_HC_Firm_Monthly.csv
CARDI_5P_M_HC_Firm_Monthly_predictability.csv
CARDI_5P_LogDiff_M_HC_Firm_Monthly_predictability.csv
regression_HC_Firm_Monthly_Rela_vola_Return.csv
regression_HC_Firm_Monthly_Rela_vola_ExReturn.csv
regression_HC_Firm_Monthly_Rela_vola_PurReturn.csv
```

## Configuration

`config.R` returns one list from `firm_returns_config()`. Important controls include:

```r
overwrite = FALSE

modules = list(
  process_firm_returns = TRUE,
  estimate_pure_returns = TRUE,
  build_pool_returns = TRUE,
  build_relative_volatility = TRUE,
  run_predictability_tests = TRUE
)
```

Predictability output can be narrowed without editing model code:

```r
predictability = list(
  scope = "firm",                 # "firm", "pool", or "both"
  predictor_family = "cardi",     # "cardi", "indicators", or "all"
  predictor_vars = c("CARDI_5P_M", "CARDI_5P_LogDiff_M"),
  dependent_vars = NULL,
  specifications = NULL,
  write_stata_tables = TRUE,
  write_predictor_summaries = TRUE,
  write_dataset_summaries = TRUE
)
```

Equivalent command-line overrides are supported:

```bash
Rscript main.R --pred-scope=firm --pred-family=cardi
Rscript main.R --pred-vars=CARDI_5P_M,CARDI_5P_LogDiff_M
Rscript main.R --pred-deps=Rela_vola_Return,Rela_vola_PurReturn
Rscript main.R --pred-specs=NoControls,ControlsFirmFE,ControlsFirmFEYearTrend,ControlsFirmFEYearFE
```

## Script Responsibilities

### `main.R`

Top-level orchestrator. It sources all modules, reads configuration, parses command-line overrides, runs enabled stages, and writes validation summaries.

### `config.R`

Defines all paths, column mappings, module toggles, empirical parameters, and output filters. This is the main place to change workflow behavior.

### `functions_utils.R`

Shared utility layer:

- package checks
- date parsing
- stock-code normalization
- winsorization
- finite-complete-row checks
- wide-to-long pivoting
- safe I/O helpers
- validation-log helpers

### `functions_load_data.R`

Read-only data loaders:

- stock price and market-value panels
- daily and monthly Fama risk-free rates
- monthly CARDI
- monthly macro controls
- monthly alternative indicators
- important carbon events
- industry codes
- annual firm financial data
- carbon-emissions ranks

### `functions_returns.R`

Constructs firm-level daily and monthly returns:

- long firm panel from HC/MC/LC source pools
- daily log returns
- daily excess returns
- month-end prices
- monthly log returns
- monthly excess returns

### `functions_controls.R`

Builds reusable annual firm controls from `Data_FI_WIND.rds`, market capitalization, and carbon-emissions data. Controls are winsorized and merged as previous-year values.

### `functions_pure_return.R`

Estimates pure returns as residuals from pooled OLS regressions of firm excess returns on previous-year controls and industry fixed effects.

### `functions_pool_returns.R`

Aggregates firm-level data into HC/MC/LC pool-level datasets:

- daily value-weighted pool returns
- monthly volatility from daily pool returns
- value-weighted average firm-level monthly volatility
- monthly enrichment with CARDI, macro, event, and indicator data

### `functions_relative_volatility.R`

Constructs HC-vs-LC relative volatility measures for firm-level and pool-level tests:

```text
Rela_vola_Return    = HC volatility / LC benchmark volatility
Rela_vola_ExReturn  = HC excess-return volatility / LC benchmark volatility
Rela_vola_PurReturn = HC pure-return volatility / LC benchmark volatility
```

### `functions_predictability_CARDI.R`

Runs predictability regressions for selected predictors and outcomes. It also writes:

- machine-readable summary CSVs
- predictor-specific CSVs
- Stata-style regression tables

Firm-level regressions use clustered standard errors by `StockID`. Pool-level regressions use Newey-West standard errors.

## Predictability Regression Design

For HC firm-level regressions:

```text
Rela_vola_*_{i,t}
  ~ Predictor_{t-1}
  + macro controls_t
  + event controls_t
  + firm controls_{i,t}
  + optional YearTrend_t
  + firm fixed effects
  + year fixed effects
  + error_{i,t}
```

The current firm-level specifications are:

```text
NoControls
ControlsFirmFE
ControlsFirmFEYearTrend
ControlsFirmFEYearFE
```

For HC pool-level regressions:

```text
Rela_vola_*_{t}
  ~ Predictor_{t-1}
  + macro controls_t
  + event controls_t
  + year fixed effects
  + error_t
```

## Event Controls

Event variables are standardized to:

```text
Event_Covid_M
Event_China_M
Event_International_M
```

The predictability module coalesces older cached `.x`/`.y` event columns back to these canonical names.

## Validation

`workflow_validation_summary.csv` records row counts, firm counts, month counts, and stage-specific notes. It is intended as a lightweight audit trail for each workflow run.

## Development Notes

- Raw data are read-only.
- Existing reference code outside this module is not modified.
- The module uses base R and `readxl`/`sandwich` where needed.
- Expensive stages should be controlled with `overwrite` and module toggles.
