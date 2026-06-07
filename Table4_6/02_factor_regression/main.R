# Output: enriched_premium_dataset - LC premium after removing other factors

stage_dir <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Table4_6/02_factor_regression"
TABLE4_STAGE_DIR <- stage_dir

source(file.path(stage_dir, "config.R"))
source(file.path(stage_dir, "functions.R"))

frequency <- normalize_frequency(Sys.getenv("CARDI_TEST_FREQUENCY",
                                            unset = "monthly"))
table4_dir <- factor_table4_dir()
output_dir <- file.path(table4_dir, "Output", frequency)
data_dir <- file.path(table4_dir, "Data", "Processed")

config <- list(
  table4_dir = table4_dir,
  data_dir = data_dir,
  output_dir = output_dir,
  frequency = frequency,
  frequency_suffix = if (identical(frequency, "monthly")) "M" else "W",
  portfolio_premium_file = file.path(
    output_dir, paste0("portfolio_premiums_", frequency, ".csv")
  ),
  portfolio_premium_rds = file.path(
    output_dir, paste0("portfolio_premiums_", frequency, ".rds")
  ),
  fama_files = list(
    monthly = file.path(data_dir, "FamaFactors", "FamaFactors_Monthly.xlsx"),
    weekly = file.path(data_dir, "FamaFactors", "FamaFactors_Weekly.xlsx")
  ),
  cardi_files = list(
    monthly = file.path(data_dir, "CARDI", "Month_CARDI.xlsx"),
    weekly = file.path(data_dir, "CARDI", "Week_CARDI.xlsx")
  ),
  macro_files = list(
    monthly = file.path(data_dir, "Macro", "Month_Macro.xlsx"),
    weekly = file.path(data_dir, "Macro", "Week_Macro.xlsx")
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
  model_rds = file.path(
    output_dir, paste0("factor_models_", frequency, ".rds")
  ),
  var_window = if (identical(frequency, "monthly")) 24L else 52L,
  force_recompute_regression = FALSE
)

ensure_dir(config$output_dir)

enriched <- run_factor_regression_stage(config)
message("Enriched regression dataset rows: ", nrow(enriched))
