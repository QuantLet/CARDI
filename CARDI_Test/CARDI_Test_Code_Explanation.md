# CARDI_Test Modular Workflow: Detailed Code Explanation

## 1. Purpose of the modular workflow

The `CARDI_Test` workflow reorganizes the existing CARDI portfolio, factor
regression, and predictability-regression logic into a cleaner modular system.
The goal is not to change the research methodology. The goal is to make the
workflow easier to run, easier to inspect, and easier to extend across monthly
and weekly frequencies.

The original reference scripts are treated as read-only sources of logic:

- `Code/Data_Process/Process_FamaFactor.R`
- `Code/Data_Process/Process_MacroVariables.R`
- `Code/Data_Process/Process_CARDI_Frequency.R`
- `Code/R/CARDI_Portfolio_2026-05-06.R`
- `Code/R/CARDI_Portfolio_Factor_Regression.R`
- `Code/Stata/CARDI_Task4_Predictability_Regressions.do`

The modular workflow follows their logic but separates the work into focused
function files:

- workflow control;
- configuration;
- loading existing data;
- generating missing data only when needed;
- portfolio construction;
- factor regression;
- CARDI predictability regression;
- frequency/date handling.

The new workflow is designed so that one setting controls whether the analysis
runs at monthly or weekly frequency.

## 2. Main design principle

The workflow follows a conservative data rule:

1. If the required processed data already exists, load it.
2. If a required intermediate output already exists in the new test output
   folder, load it.
3. Only construct missing intermediate outputs when needed.
4. Do not overwrite existing project outputs.
5. Do not edit the original reference code.

This is important because the original processing scripts may write to existing
project output folders. The modular workflow avoids accidental changes to those
files by writing its own outputs into the `CARDI_Test` output folder.

## 3. Frequency control

The workflow supports two frequencies:

- `monthly`
- `weekly`

The frequency is selected in the controller script through:

```r
CARDI_TEST_FREQUENCY <- Sys.getenv("CARDI_TEST_FREQUENCY", unset = "monthly")
```

This means the default frequency is monthly. To run the weekly version, set the
environment variable before running the script:

```bash
CARDI_TEST_FREQUENCY=weekly Rscript Code/CARDI_Test/main.R
```

To run the monthly version:

```bash
CARDI_TEST_FREQUENCY=monthly Rscript Code/CARDI_Test/main.R
```

The frequency choice affects:

- which Fama factor file is loaded;
- which CARDI frequency file is loaded;
- which macro-variable file is loaded;
- how periods are constructed;
- the rolling VaR window;
- where outputs are saved;
- the suffixes used for CARDI and macro variables.

Monthly variables use suffix `_M`. Weekly variables use suffix `_W`.

## 4. Overall execution order

The main script runs the workflow in this order:

1. Load configuration.
2. Source all function files.
3. Create the test output directory.
4. Check that processed frequency-level inputs exist.
5. Build or load portfolio risk premium data.
6. Run the factor regression and construct pure LC premium.
7. Build VaR and AR(1)-adjusted premium measures.
8. Run CARDI predictability regressions.
9. Save summary tables and dependent-variable-specific regression tables.

The workflow controller is intentionally short. Most of the logic is delegated
to function files.

## 5. `main.R`

`main.R` is the entry point of the modular system.

Its responsibilities are:

- define the target frequency;
- locate the script folder;
- source all function files;
- build the configuration object;
- create the output directory;
- call the portfolio, factor-regression, and predictability-regression modules.

The core execution flow is:

```r
config <- cardi_test_config(CARDI_TEST_FREQUENCY)
ensure_dir(config$output_dir)

ensure_processed_frequency_inputs(config)

portfolio_premiums <- build_portfolio_premiums(config, config$frequency)
enriched <- run_factor_regression(config, portfolio_premiums)
predictability <- run_predictability_outputs(config, enriched)
```

This structure makes the workflow easy to audit because each major step has a
single function call.

## 6. `config.R`

`config.R` creates the configuration object used by every module.

The function:

```r
cardi_test_config(frequency)
```

returns a list containing:

- the project root;
- the test workflow folder;
- the output folder;
- the selected frequency;
- the frequency suffix;
- reference input paths;
- processed data paths;
- output file paths;
- rolling-window parameters;
- Newey-West lag length;
- flags controlling recomputation.

Important configuration fields include:

```r
frequency
frequency_suffix
input_dir
carbon_rank_file
fama_files
cardi_files
macro_files
portfolio_premium_file
enriched_file
regression_summary_file
var_window
nw_lag
force_recompute_portfolio
force_recompute_regression
allow_reference_generation
```

The monthly setting uses:

```r
var_window = 24
```

The weekly setting uses:

```r
var_window = 52
```

This keeps the VaR window economically comparable across frequencies: about
two years for monthly data and about one year for weekly data.

## 7. `functions_frequency.R`

This file contains shared date and frequency utilities.

Main functions:

```r
parse_date()
normalize_frequency()
frequency_suffix()
period_id()
period_start_date()
normalize_period_key()
safe_mean()
as_numeric_columns()
check_required_columns()
first_existing_path()
clean_stock_id()
finite_complete()
```

### 7.1 Date parsing

`parse_date()` handles different possible date formats, including:

- `Date`;
- `POSIXct`;
- Excel numeric dates;
- character dates such as `YYYY-MM-DD`;
- character dates such as `YYYY/MM/DD`;
- compact dates such as `YYYYMMDD`;
- monthly identifiers such as `YYYY-MM`.

This is necessary because the raw and processed project files use different
date formats.

### 7.2 Period construction

`period_id()` converts daily dates into the selected frequency:

- monthly: `YYYY-MM`;
- weekly: ISO-style `YYYY-WW`.

For example:

```r
period_id(as.Date("2020-03-15"), "monthly")
```

returns:

```r
"2020-03"
```

The same function with weekly frequency returns a week identifier.

### 7.3 Period key normalization

`normalize_period_key()` creates a common merge key across datasets. It checks
for existing identifiers in this order:

1. `Period`
2. `FrequencyID`
3. `Month`
4. `Week`
5. `Date`

This avoids hard-coding one date-column name across all input files.

### 7.4 Complete finite rows

`finite_complete()` checks whether a set of columns has complete finite numeric
values. It is used before regressions so that the regression sample does not
contain missing or infinite values.

## 8. `functions_load_data.R`

This file loads existing processed data and daily stock-panel inputs.

Main functions:

```r
require_package()
read_excel_as_df()
read_numeric_panel()
load_group_panels()
combine_stock_panels()
load_fama_factors()
load_cardi_frequency()
load_macro_frequency()
load_reference_monthly_premiums()
```

### 8.1 Loading stock panels

`read_numeric_panel()` reads a panel file where the first column is a date and
the remaining columns are stock IDs. It:

- parses the date column;
- converts stock columns to numeric;
- removes rows with missing dates;
- sorts by date;
- cleans stock ID names.

`load_group_panels()` loads the price and market-cap panels for one carbon
group:

- `HighCarbonIntens`
- `MedCarbonIntens`
- `LowCarbonIntens`

It follows the same folder and filename convention as the reference portfolio
script.

### 8.2 Combining stock panels

`combine_stock_panels()` merges high-, medium-, and low-carbon stock panels by
date. Duplicate stock columns are removed after merging. This creates one
universe-level price panel and one universe-level market-cap panel.

### 8.3 Loading Fama factor data

`load_fama_factors()` loads either:

- `FamaFactors_Monthly.xlsx`, or
- `FamaFactors_Weekly.xlsx`.

It requires:

- `Date`;
- `MarketPremium`;
- `SMB2`;
- `HML2`;
- `RMW2`;
- `CMA2`.

If available, it also keeps:

- `IndexRiskFreeRate`.

The factor file is then assigned a common `Period` key so it can be merged with
portfolio, CARDI, and macro data.

### 8.4 Loading CARDI data

`load_cardi_frequency()` loads the frequency-specific CARDI file:

- monthly: `Month_CARDI.xlsx`;
- weekly: `Week_CARDI.xlsx`.

For monthly analysis, it requires:

- `CARDI_5P_M`;
- `CARDI_1P_M`;
- `CARDI_10P_M`;
- `CARDI_5P_LogDiff_M`;
- `CARDI_1P_LogDiff_M`;
- `CARDI_10P_LogDiff_M`.

For weekly analysis, it requires:

- `CARDI_5P_W`;
- `CARDI_1P_W`;
- `CARDI_10P_W`;
- `CARDI_5P_LogDiff_W`;
- `CARDI_1P_LogDiff_W`;
- `CARDI_10P_LogDiff_W`.

These variables represent level and change information from CARDI at the chosen
frequency.

### 8.5 Loading macro data

`load_macro_frequency()` loads either:

- `Month_Macro.xlsx`, or
- `Week_Macro.xlsx`.

The required controls are selected according to the frequency suffix. Monthly
controls include:

- `CarbonVol_M_Shenzhen`;
- `CarbonVol_M_Guangdong`;
- `CarbonVol_M_Hubei`;
- `RealEstate_Premium_M`;
- `Slope_M`;
- `TED_M`;
- `TY3M_Change_M`;
- `MarketVol_M`;
- `Event_dummy_M`;
- `Event_Covid_M`;
- `Event_China_M`;
- `Event_International_M`.

Weekly controls use the same names with `_W` suffix.

## 9. `functions_generate_data.R`

This file controls missing-data behavior and safe writing.

Main functions:

```r
ensure_dir()
ensure_processed_frequency_inputs()
save_new_dataset()
write_new_csv()
save_new_rds()
```

### 9.1 Processed input checks

`ensure_processed_frequency_inputs()` checks whether the required processed
frequency-level input files exist:

- Fama factor file;
- CARDI frequency file;
- macro frequency file.

If all exist, the workflow continues.

If any are missing, the function stops with a clear message. It does not run
the original reference processing scripts by default, because those scripts may
write to existing project output folders.

### 9.2 Overwrite protection

The safe writer functions refuse to overwrite existing files. This behavior
protects earlier test runs and prevents accidental replacement of existing
results.

The workflow therefore follows this rule:

- if an output exists, load it;
- if it does not exist, create it;
- never silently overwrite it.

## 10. `functions_portfolio.R`

This file constructs HC, MC, LC, and LC-HC portfolio returns and risk premiums.

Main functions:

```r
make_period_stock_panels()
double_sort_ids()
weighted_group_return()
make_dynamic_double_sort_returns()
construct_portfolio_returns()
build_portfolio_premiums()
```

### 10.1 Period stock panels

`make_period_stock_panels()` converts daily stock price and market-cap panels
into period-level stock returns and period-end market caps.

For each stock and each period:

- the period return is calculated from the last available price and first
  available price within the period;
- the period-end market cap is the last available market cap in the period.

This follows the portfolio construction logic in the reference R script:

1. calculate period returns using within-period prices;
2. use lagged period-end market capitalization for value weighting.

### 10.2 Dynamic size-by-carbon double sorting

`make_dynamic_double_sort_returns()` implements the dynamic double-sort
portfolio construction.

At each period `t`, it uses information from period `t-1`:

1. Identify stocks with valid lagged market cap and current returns.
2. Split stocks by lagged market cap at the median:
   - Big;
   - Small.
3. Within each size group, sort stocks by carbon intensity from
   `Carbon_Rank.rds`.
4. Use carbon intensity cutoffs:
   - below 30%: Low carbon;
   - 30% to 70%: Medium carbon;
   - above 70%: High carbon.
5. Construct six groups:
   - Big Low;
   - Small Low;
   - Big Medium;
   - Small Medium;
   - Big High;
   - Small High.
6. Calculate value-weighted returns within each group using lagged market cap.

This preserves the reference idea that portfolio membership is based on
information available before the return period.

### 10.3 HC, MC, LC returns

After the six double-sort returns are calculated, the code constructs:

```r
HC_Return = 0.5 * (Big_High + Small_High)
MC_Return = 0.5 * (Big_Medium + Small_Medium)
LC_Return = 0.5 * (Big_Low + Small_Low)
```

The long-short low-carbon minus high-carbon return is:

```r
LC_HC_Return = LC_Return - HC_Return
```

This is consistent with the user-specified rule that the strategy of interest
is long low-carbon and short high-carbon when the CARDI signal supports
executing the low-carbon strategy.

### 10.4 Portfolio premiums

`build_portfolio_premiums()` merges portfolio returns with the frequency-level
Fama factor file and uses:

```r
IndexRiskFreeRate
```

to calculate risk premiums:

```r
HC_Premium = HC_Return - IndexRiskFreeRate
MC_Premium = MC_Return - IndexRiskFreeRate
LC_Premium = LC_Return - IndexRiskFreeRate
LC_HC_Premium = LC_Premium - HC_Premium
```

Because the risk-free rate cancels out in the long-short portfolio:

```r
LC_HC_Premium = LC_Return - HC_Return
```

The monthly branch can reuse the existing monthly reference premium file if it
already exists. The weekly branch constructs weekly portfolio premiums from the
daily panels and weekly factor data.

## 11. `functions_factor_regression.R`

This file estimates the risk-factor-adjusted LC-HC premium and constructs
additional premium measures.

Main functions:

```r
rolling_lower_tail_quantile()
add_ar1_residual()
run_factor_regression()
```

### 11.1 Merge analysis dataset

`run_factor_regression()` merges four datasets by the common `Period` key:

1. portfolio premiums;
2. Fama factor data;
3. CARDI frequency data;
4. macro/event controls.

The merged dataset is saved before factor-regression variables are added. This
creates an auditable intermediate file.

### 11.2 Five-factor regression

The main factor regression is:

```r
LC_HC_Premium ~ MarketPremium + SMB2 + HML2 + RMW2 + CMA2
```

R includes the intercept automatically. The intercept is the regression alpha.
The code does not add a separate `alpha` variable.

### 11.3 Pure LC premium

The fitted value from the regression is saved as:

```r
fitted_LC_HC_Premium
```

The residual is saved as:

```r
pure_LC_premium
```

The interpretation is:

```text
pure_LC_premium = part of the LC-HC premium unexplained by standard risk factors
```

This residual is the main risk-adjusted outcome used in the CARDI
predictability tests.

### 11.4 Rolling VaR measures

The code constructs rolling lower-tail quantiles of the pure LC premium:

```r
pure_LC_premium_VaR_10
pure_LC_premium_VaR_5
pure_LC_premium_VaR_1
```

The definitions are:

- VaR 10% = rolling 10th percentile;
- VaR 5% = rolling 5th percentile;
- VaR 1% = rolling 1st percentile.

The rolling window is frequency-specific:

- monthly: 24 observations;
- weekly: 52 observations.

### 11.5 AR(1)-adjusted premium

The AR(1) adjustment estimates:

```r
pure_LC_premium_t = alpha + beta * pure_LC_premium_{t-1} + error_t
```

The residual from this regression is:

```r
AR1_Premium
```

This removes the predictable own-lag component of the pure LC premium. The code
does not constrain the lag coefficient to one; it estimates beta from the data.

The code also constructs VaR measures for `AR1_Premium`:

```r
AR1_Premium_VaR_10
AR1_Premium_VaR_5
AR1_Premium_VaR_1
```

## 12. `functions_predictability.R`

This file converts the Stata Task 4.5 and 4.6 predictability-regression logic
into R.

Main functions:

```r
nw_test_for_model()
predictability_variable_lists()
short_name()
fit_predictability_grid()
star_for_p()
fmt_coef()
fmt_se_text()
write_dependent_variable_table()
run_predictability_outputs()
```

### 12.1 Dependent variables

The main dependent variables are:

```r
HC_Premium
MC_Premium
LC_Premium
LC_HC_Premium
pure_LC_premium
AR1_Premium
```

For each dependent variable, the code constructs a one-period-ahead value:

```r
future_HC_Premium
future_MC_Premium
future_LC_Premium
future_LC_HC_Premium
future_pure_LC_premium
future_AR1_Premium
```

This matches the hypothesis that current CARDI predicts future portfolio
premium outcomes.

### 12.2 CARDI predictors

The CARDI predictors depend on frequency.

Monthly:

```r
CARDI_5P_M
CARDI_1P_M
CARDI_10P_M
CARDI_5P_LogDiff_M
CARDI_1P_LogDiff_M
CARDI_10P_LogDiff_M
```

Weekly:

```r
CARDI_5P_W
CARDI_1P_W
CARDI_10P_W
CARDI_5P_LogDiff_W
CARDI_1P_LogDiff_W
CARDI_10P_LogDiff_W
```

These variables test both CARDI levels and CARDI changes.

### 12.3 Regression specifications

For every dependent variable and CARDI predictor, the code estimates four
specifications:

#### Specification 1: Baseline

```r
future_dependent_variable ~ CARDI
```

#### Specification 2: Macro controls

```r
future_dependent_variable ~ CARDI + macro controls
```

Macro controls include:

- carbon market volatility for Shenzhen;
- carbon market volatility for Guangdong;
- carbon market volatility for Hubei;
- real estate premium;
- yield-curve slope;
- TED spread;
- change in 3-month Treasury yield;
- market volatility.

#### Specification 3: Macro controls plus general event dummy

```r
future_dependent_variable ~ CARDI + macro controls + Event_dummy
```

#### Specification 4: Macro controls plus event categories

```r
future_dependent_variable ~ CARDI
                           + macro controls
                           + Event_Covid
                           + Event_China
                           + Event_International
```

If a control variable is unavailable, the code includes only controls that
exist in the merged dataset.

### 12.4 Newey-West correction

The code reports Newey-West standard errors with 12 lags:

```r
sandwich::NeweyWest(fit, lag = 12, prewhite = FALSE, adjust = TRUE)
```

The Newey-West correction is used for the displayed standard errors, t-stats,
p-values, and significance stars in the formatted regression tables.

The code also records ordinary t-statistics and p-values in the summary CSV.

### 12.5 Regression summary output

The summary table includes:

- regression name;
- dependent variable;
- CARDI predictor;
- specification name;
- CARDI coefficient;
- ordinary t-statistic;
- ordinary p-value;
- Newey-West t-statistic;
- Newey-West p-value;
- R-squared;
- adjusted R-squared;
- number of observations;
- included controls;
- indicator for positive CARDI coefficient;
- significance flags at 10%, 5%, and 1%.

The hypothesis flag checks whether:

```r
CARDI coefficient > 0
```

and whether the coefficient is statistically significant, especially under
Newey-West standard errors.

### 12.6 Dependent-variable-specific tables

The code writes one formatted regression table per future dependent variable:

- `regression_future_HC_Premium.csv`
- `regression_future_MC_Premium.csv`
- `regression_future_LC_Premium.csv`
- `regression_future_LC_HC_Premium.csv`
- `regression_future_pure_LC_premium.csv`
- `regression_future_AR1_Premium.csv`

Each table follows the Stata-style format:

- columns are model specifications, numbered `(1)`, `(2)`, `(3)`, etc.;
- the second header row shows the dependent variable name;
- the first column is `VARIABLES`;
- coefficient rows show coefficients with stars;
- the row below each coefficient shows Newey-West standard errors in
  parentheses;
- summary rows report observations, R-squared, adjusted R-squared, and whether
  controls are included;
- notes explain the Newey-West errors and significance-star convention.

Parentheses are written as text so spreadsheet software does not interpret the
standard-error rows as negative numbers.

## 13. Output structure

The workflow saves new outputs under a frequency-specific test folder.

Monthly outputs include:

```text
output/monthly/portfolio_premiums_monthly.csv
output/monthly/portfolio_premiums_monthly.rds
output/monthly/merged_analysis_dataset_monthly.csv
output/monthly/enriched_premium_dataset_monthly.csv
output/monthly/enriched_premium_dataset_monthly.rds
output/monthly/factor_models_monthly.rds
output/monthly/predictability_summary_monthly.csv
output/monthly/predictability_models_monthly.rds
output/monthly/regression_future_HC_Premium.csv
output/monthly/regression_future_MC_Premium.csv
output/monthly/regression_future_LC_Premium.csv
output/monthly/regression_future_LC_HC_Premium.csv
output/monthly/regression_future_pure_LC_premium.csv
output/monthly/regression_future_AR1_Premium.csv
```

Weekly outputs use the same naming pattern with `weekly`.

## 14. Relationship to the original reference scripts

The modular workflow preserves the key behavior of the original code:

- dynamic HC/MC/LC portfolio construction;
- value-weighting by lagged market cap;
- 30% and 70% carbon-intensity cutoffs;
- HC, MC, LC, and LC-HC portfolio premium construction;
- five-factor regression for risk adjustment;
- pure LC premium as a regression residual;
- VaR measures based on rolling lower-tail quantiles;
- AR(1)-adjusted premium using estimated lag coefficient;
- CARDI predictability regressions;
- Newey-West standard errors;
- dependent-variable-specific regression result tables.

The main difference is organization. Instead of one long R script and one
separate Stata reporting script, the new workflow separates reusable logic into
small R function files.

## 15. Interpretation of the key variables

### HC, MC, and LC premiums

These are excess returns of the high-, medium-, and low-carbon portfolios:

```r
PortfolioPremium = PortfolioReturn - IndexRiskFreeRate
```

### LC-HC premium

This is the low-carbon minus high-carbon long-short premium:

```r
LC_HC_Premium = LC_Premium - HC_Premium
```

Because both legs subtract the same risk-free rate, this equals:

```r
LC_Return - HC_Return
```

### Pure LC premium

This is the residual from the five-factor regression:

```r
LC_HC_Premium ~ MarketPremium + SMB2 + HML2 + RMW2 + CMA2
```

It measures the LC-HC premium not explained by standard risk factors.

### AR1 premium

This is the residual from the AR(1) regression of pure LC premium on its own
lag. It removes the own-lag predictable component of pure LC premium.

### CARDI predictors

CARDI level variables measure the average CARDI level during the period.
CARDI log-difference variables measure the increase or decrease in CARDI
between consecutive periods.

The main hypothesis is:

```text
Higher CARDI predicts higher future pure LC premium.
```

The regression summary therefore checks whether the CARDI coefficient is
positive and statistically significant.

## 16. Practical run notes

To run monthly:

```bash
CARDI_TEST_FREQUENCY=monthly Rscript Code/CARDI_Test/main.R
```

To run weekly:

```bash
CARDI_TEST_FREQUENCY=weekly Rscript Code/CARDI_Test/main.R
```

If `Rscript` is not available on the shell path, use the full local Rscript
path:

```bash
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/CARDI_Test/main.R
```

For weekly:

```bash
CARDI_TEST_FREQUENCY=weekly /Library/Frameworks/R.framework/Resources/bin/Rscript Code/CARDI_Test/main.R
```

The first weekly run can be slower because weekly portfolio premiums are built
from daily stock price and market-cap panels. After the weekly output files
exist, later runs load the saved test outputs instead of reconstructing them.

## 17. Important safeguards

The workflow includes several safeguards:

1. It checks required columns before analysis.
2. It parses dates before merging.
3. It creates a common period key before merging.
4. It removes missing and infinite observations before regressions.
5. It refuses to overwrite existing test output files.
6. It does not overwrite raw files.
7. It does not modify the original reference scripts.

These safeguards make the modular workflow useful for testing and extension
without disrupting the existing project codebase.
