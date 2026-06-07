# =============================================================================
# File    : other_strategy_results.R
# Purpose : Export full auxiliary strategy results separately from Table8 main.
# =============================================================================

SCRIPT_DIR <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Table8"
setwd(SCRIPT_DIR)

source(file.path(SCRIPT_DIR, "config.R"))
source(file.path(SCRIPT_DIR, "functions_portfolio.R"))

config <- table8_portfolio_config()
setwd(config$table8_dir)

run_table8_extra_strategy_outputs()
