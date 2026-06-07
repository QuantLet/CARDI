# =============================================================================
# File    : config.R
# Purpose : Local configuration for the Table8 CARDI portfolio workflow.
# =============================================================================

table8_portfolio_config <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else ""
  script_dir <- if (nzchar(script_file) && file.exists(script_file)) {
    dirname(normalizePath(script_file, mustWork = TRUE))
  } else {
    getwd()
  }

  find_table8_root <- function(start_dir) {
    current <- normalizePath(start_dir, mustWork = TRUE)
    repeat {
      if (basename(current) == "Table8" &&
          dir.exists(file.path(current, "Data"))) return(current)
      parent <- dirname(current)
      if (identical(parent, current)) {
        stop("Could not locate Table8 root from: ", start_dir)
      }
      current <- parent
    }
  }

  root <- find_table8_root(script_dir)
  output_dir <- file.path(root, "Output", "Portfolio")

  list(
    table8_dir = root,
    project_root = root,
    module_dir = root,
    date_start_source = "20140704",
    date_end_source = "20250127",
    signal = "CARDI_5P",
    threshold_probs = c(median = 0.50, q25 = 0.25, q75 = 0.75),
    oos_execution_signal = list(
      primary_signal = "CARDI_5P",
      min_active_share = 0.30,
      selected_indicator = "momentum_positive",
      selected_parameter = "lag=2",
      selected_daily_indicator = "daily_mean_percentile_momentum_positive",
      selected_daily_parameter = "lag=1",
      threshold_direction = "above",
      selected_threshold_quantile = 0.45,
      threshold_grid = seq(0.05, 0.95, by = 0.05),
      zscore_grid = seq(-1.00, 1.00, by = 0.25),
      momentum_lags = c(1, 2, 3, 6, 9, 12),
      level_momentum_quantiles = seq(0.30, 0.80, by = 0.10),
      level_momentum_lags = c(1, 2, 3, 6, 9, 12),
      ma_short_windows = c(2, 3, 6, 9),
      ma_long_windows = c(6, 12, 24, 36),
      acceleration_quantiles = seq(0.30, 0.80, by = 0.10),
      acceleration_lags = c(1, 2, 3, 6),
      daily_momentum_lags = c(5, 10, 21, 42, 63, 126),
      daily_ma_short_windows = c(5, 10, 21),
      daily_ma_long_windows = c(42, 63, 126),
      daily_z_windows = c(21, 42, 63, 126),
      daily_z_cutoffs = c(-0.50, 0, 0.50, 1.00)
    ),
    nw_lag_months = 12,
    input_dir = file.path(root, "Data", "Processed", "Input"),
    cardi_file = file.path(root, "Data", "Processed", "FRM_Carbon_risk.csv"),
    carbon_rank_file = file.path(root, "Data", "Processed", "Carbon_Rank.rds"),
    output_dir = output_dir,
    figure_dir = file.path(output_dir, "figures"),
    fama_monthly_file = file.path(root, "Data", "Processed",
                                  "FamaFactors", "FamaFactors_Monthly.xlsx"),
    portfolio_premium_csv = file.path(output_dir,
                                      "cardi_portfolio_monthly_risk_premiums.csv"),
    portfolio_premium_rds = file.path(output_dir,
                                      "cardi_portfolio_monthly_risk_premiums.rds"),
    portfolio_group_schema = "HC_MC_LC_dynamic_double_sort_v1",
    strategy_results_rds = file.path(output_dir,
                                     "cardi_strategy_results_for_reporting.rds"),
    strategy_performance = list(
      selected_strategies = c(
        "Baseline LC-HC",
        "Baseline HC-LC",
        "daily_mean_percentile_momentum_positive | lag=1"
      )
    )
  )
}
