# CARDI_Test modular workflow

This folder assembles the existing CARDI portfolio, factor-regression, and
predictability-regression workflow without editing the original reference
scripts.

## Files

- `main.R`: one-entry controller for the full workflow.
- `config.R`: paths, frequency, output names, rolling-window settings, and
  overwrite-protection settings.
- `functions_frequency.R`: date parsing, period keys, and shared validation
  helpers.
- `functions_load_data.R`: loaders for existing processed Fama factor, CARDI,
  macro, portfolio, and stock-panel data.
- `functions_generate_data.R`: guards around missing processed inputs and safe
  output writers.
- `functions_portfolio.R`: HC/MC/LC dynamic size-by-carbon double-sort
  portfolio returns and portfolio risk premiums.
- `functions_factor_regression.R`: factor regression, pure LC premium,
  rolling VaR, and AR(1)-adjusted premium construction.
- `functions_predictability.R`: R version of the Task 4.5/4.6 predictability
  regression grid and dependent-variable-specific output tables.

## Run

Set `CARDI_TEST_FREQUENCY` in the shell:

```bash
CARDI_TEST_FREQUENCY=monthly Rscript Code/CARDI_Test/main.R
```

or:

```bash
CARDI_TEST_FREQUENCY=weekly Rscript Code/CARDI_Test/main.R
```

If the environment variable is omitted, `main.R` defaults to monthly:

```bash
Rscript Code/CARDI_Test/main.R
```

All outputs are written under `Code/CARDI_Test/output/<frequency>/`. Existing
project outputs are read as inputs only and are not overwritten.
