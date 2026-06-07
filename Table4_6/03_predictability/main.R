stage_dir <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Table4_6/03_predictability"
TABLE4_STAGE_DIR <- stage_dir

source(file.path(stage_dir, "config.R"))
source(file.path(stage_dir, "functions.R"))

frequency <- normalize_frequency(Sys.getenv("CARDI_TEST_FREQUENCY",
                                            unset = "monthly"))
table4_dir <- predictability_table4_dir()
output_dir <- file.path(table4_dir, "Output", frequency)

# Selection parameters:
# - selected_future_dependent_variables choices:
#   "future_HC", "future_MC", "future_LC", "future_LC_HC",
#   "future_pure_LC", "future_AR1".
# - selected_cardi_predictor_roots choices:
#   "CARDI_5P", "CARDI_1P", "CARDI_10P",
#   "CARDI_5P_LogDiff", "CARDI_1P_LogDiff", "CARDI_10P_LogDiff".
#   The frequency suffix ("_M" or "_W") is matched automatically.
# - selected_specifications choices:
#   "Baseline", "Macro", "MacroEventDummy", "MacroEventCategories".
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
  regression_summary_file = file.path(
    output_dir, paste0("predictability_summary_", frequency, ".csv")
  ),
  predictability_rds = file.path(
    output_dir, paste0("predictability_models_", frequency, ".rds")
  ),
  selected_future_dependent_variables = c("future_AR1", "future_pure_LC"),
  selected_cardi_predictor_roots = c("CARDI_5P", "CARDI_1P", "CARDI_10P"),
  selected_specifications = c("Baseline", "MacroEventCategories"),
  overwrite_predictability_outputs = TRUE,
  nw_lag = 12L
)

ensure_dir(config$output_dir)

predictability <- run_predictability_stage(config)
message("Predictability regressions: ", nrow(predictability$summary))
