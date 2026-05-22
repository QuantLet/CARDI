# CARDI_Test modular workflow configuration.
#
# Change CARDI_TEST_FREQUENCY to "monthly" or "weekly" to run the same
# assembled workflow at a different frequency.

cardi_test_config <- function(frequency = c("monthly", "weekly")) {
  frequency <- match.arg(tolower(frequency), c("monthly", "weekly"))

  script_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", script_args, value = TRUE)
  script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else ""
  script_dir <- if (nzchar(script_file) && !identical(script_file, "-") &&
                    file.exists(script_file)) {
    dirname(normalizePath(script_file, mustWork = TRUE))
  } else {
    getwd()
  }

  find_project_root <- function(start_dir) {
    current <- normalizePath(start_dir, mustWork = TRUE)
    repeat {
      if (dir.exists(file.path(current, "Data", "Processed")) &&
          dir.exists(file.path(current, "Code", "R"))) {
        return(current)
      }
      parent <- dirname(current)
      if (identical(parent, current)) {
        stop("Could not locate project root from: ", start_dir)
      }
      current <- parent
    }
  }

  project_root <- find_project_root(script_dir)
  test_dir <- file.path(project_root, "Code", "CARDI_Test")
  output_dir <- file.path(test_dir, "output", frequency)

  list(
    project_root = project_root,
    test_dir = test_dir,
    output_dir = output_dir,
    frequency = frequency,
    frequency_suffix = if (identical(frequency, "monthly")) "M" else "W",
    date_start_source = "20140704",
    date_end_source = "20250127",
    input_dir = file.path(project_root, "Data", "Processed", "Input"),
    carbon_rank_file = file.path(project_root, "Output", "Carbon_Rank.rds"),
    reference_monthly_premium_file = file.path(
      project_root,
      "Output", "Portfolio", "CARDI_Portfolio_2026-05-06",
      "cardi_portfolio_monthly_risk_premiums.csv"
    ),
    fama_files = list(
      monthly = file.path(project_root, "Data", "Processed", "FamaFactors",
                          "FamaFactors_Monthly.xlsx"),
      weekly = file.path(project_root, "Data", "Processed", "FamaFactors",
                         "FamaFactors_Weekly.xlsx")
    ),
    cardi_files = list(
      monthly = c(
        file.path(project_root, "Data", "Processed", "CARDI",
                  "Month_CARDI.xlsx"),
        file.path(project_root, "Data", "CARDI", "Month_CARDI.xlsx")
      ),
      weekly = c(
        file.path(project_root, "Data", "Processed", "CARDI",
                  "Week_CARDI.xlsx"),
        file.path(project_root, "Data", "CARDI", "Week_CARDI.xlsx")
      )
    ),
    macro_files = list(
      monthly = file.path(project_root, "Data", "Processed", "Macro",
                          "Month_Macro.xlsx"),
      weekly = file.path(project_root, "Data", "Processed", "Macro",
                         "Week_Macro.xlsx")
    ),
    new_indicator_files = list(
      monthly = file.path(project_root, "Output", "NewIndicators", "Monthly",
                          "All_Indicators_Monthly.csv"),
      weekly = file.path(project_root, "Output", "NewIndicators", "Monthly",
                         "All_Indicators_Weekly.csv")
    ),
    portfolio_premium_file = file.path(
      output_dir, paste0("portfolio_premiums_", frequency, ".csv")
    ),
    portfolio_premium_rds = file.path(
      output_dir, paste0("portfolio_premiums_", frequency, ".rds")
    ),
    merged_analysis_file = file.path(
      output_dir, paste0("merged_analysis_dataset_", frequency, ".csv")
    ),
    enriched_file = file.path(
      output_dir, paste0("enriched_premium_dataset_", frequency, ".csv")
    ),
    enriched_rds = file.path(
      output_dir, paste0("enriched_premium_dataset_", frequency, ".rds")
    ),
    regression_summary_file = file.path(
      output_dir, paste0("predictability_summary_", frequency, ".csv")
    ),
    predictability_rds = file.path(
      output_dir, paste0("predictability_models_", frequency, ".rds")
    ),
    model_rds = file.path(output_dir, paste0("factor_models_", frequency,
                                             ".rds")),
    run_new_indicators_predictability = TRUE,
    new_indicator_analysis_file = file.path(
      output_dir,
      paste0("new_indicator_analysis_dataset_", frequency, ".csv")
    ),
    new_indicator_summary_file = file.path(
      output_dir,
      paste0("new_indicator_predictability_summary_", frequency, ".csv")
    ),
    new_indicator_comparison_file = file.path(
      output_dir,
      paste0("new_indicator_predictability_comparison_", frequency, ".csv")
    ),
    new_indicator_predictability_rds = file.path(
      output_dir,
      paste0("new_indicator_predictability_models_", frequency, ".rds")
    ),
    var_window = if (identical(frequency, "monthly")) 24L else 52L,
    nw_lag = 12L,
    force_recompute_portfolio = FALSE,
    force_recompute_regression = FALSE,
    allow_reference_generation = FALSE
  )
}
