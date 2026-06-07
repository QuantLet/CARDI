stage_dir <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Table4_6/04_new_indicator_predictability"
TABLE4_STAGE_DIR <- stage_dir
source(file.path(stage_dir, "config.R"))
source(file.path(stage_dir, "functions.R"))

frequency <- normalize_frequency(Sys.getenv("CARDI_TEST_FREQUENCY",
                                            unset = "monthly"))
table4_dir <- new_indicator_table4_dir()
output_dir <- file.path(table4_dir, "Output", frequency)
new_indicator_dir <- file.path(table4_dir, "Data", "NewIndicators")

# Selection parameters:
# - selected_future_dependent_variables choices:
#   "future_HC", "future_MC", "future_LC", "future_LC_HC",
#   "future_pure_LC", "future_AR1".
# - selected_specifications choices:
#   "Baseline", "Macro", "MacroEventDummy", "MacroEventCategories".
# - selected_new_indicator_predictors choices:
#   character(0) keeps all *_HL_Ratio variables in the NewIndicator file.
#   Otherwise, provide exact *_HL_Ratio column names.
config <- list(
  table4_dir = table4_dir,
  output_dir = output_dir,
  frequency = frequency,
  frequency_suffix = if (identical(frequency, "monthly")) "M" else "W",
  enriched_file = file.path(
    output_dir, paste0("enriched_premium_dataset_", frequency, ".csv")
  ),
  enriched_rds = file.path(
    output_dir, paste0("enriched_premium_dataset_", frequency, ".rds")
  ),
  new_indicator_files = list(
    monthly = file.path(new_indicator_dir, "Monthly",
                        "All_Indicators_Monthly.csv"),
    weekly = file.path(new_indicator_dir, "Weekly",
                       "All_Indicators_Weekly.csv")
  ),
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
  selected_future_dependent_variables = c("future_AR1", "future_pure_LC"),
  selected_specifications = c("Baseline", "MacroEventCategories"),
  selected_new_indicator_predictors = c(
    "DY_HL_Ratio",
    "EDC_HL_Ratio",
    "CVaR_HL_Ratio"
  ),
  overwrite_new_indicator_outputs = TRUE,
  nw_lag = 12L
)

ensure_dir(config$output_dir)

new_indicator_predictability <- run_new_indicator_predictability_stage(config)
message("NewIndicator predictability regressions: ", nrow(new_indicator_predictability$summary))
