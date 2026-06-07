# =============================================================================
# File    : main.R
# Purpose : Run Table8 CARDI portfolio performance outputs using local data.
# =============================================================================

SCRIPT_DIR <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Table8_Figure4"
setwd(SCRIPT_DIR)

source(file.path(SCRIPT_DIR, "config.R"))
source(file.path(SCRIPT_DIR, "functions_portfolio.R"))

config <- table8_portfolio_config()
setwd(config$table8_dir)

# Strategy-performance output choices:
# - "Baseline LC-HC" means Long LC, Short HC.
# - "Baseline HC-LC" means Long HC, Short LC.
# - "daily_mean_percentile_momentum_positive | lag=1" means
#   Long LC, Short HC based on the selected CARDI strategy.
config$strategy_performance$selected_strategies <- c(
  "Baseline LC-HC",
  "Baseline HC-LC",
  "daily_mean_percentile_momentum_positive | lag=1"
)

run_table8_portfolio_main()
