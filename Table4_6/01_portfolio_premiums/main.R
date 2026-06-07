# Generate portfolio premiums using only Table4-local inputs.

stage_dir <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Table4_6/01_portfolio_premiums"

TABLE4_STAGE_DIR <- stage_dir
source(file.path(stage_dir, "config.R"))
source(file.path(stage_dir, "functions.R"))

frequency <- normalize_frequency(Sys.getenv("CARDI_TEST_FREQUENCY",
                                            unset = "monthly"))
table4_dir <- portfolio_table4_dir()
output_dir <- file.path(table4_dir, "Output", frequency)

config <- list(
  table4_dir = table4_dir,
  output_dir = output_dir,
  frequency = frequency,
  frequency_suffix = if (identical(frequency, "monthly")) "M" else "W",
  date_start_source = "20140704",
  date_end_source = "20250127",
  input_dir = file.path(table4_dir, "Data", "Processed", "Input"),
  carbon_rank_file = file.path(table4_dir, "Data", "Processed",
                               "Carbon_Rank.rds"),
  reference_monthly_premium_file = NA_character_,
  fama_files = list(
    monthly = file.path(table4_dir, "Data", "Processed", "FamaFactors",
                        "FamaFactors_Monthly.xlsx"),
    weekly = file.path(table4_dir, "Data", "Processed", "FamaFactors",
                       "FamaFactors_Weekly.xlsx")
  ),
  portfolio_premium_file = file.path(
    output_dir, paste0("portfolio_premiums_", frequency, ".csv")
  ),
  portfolio_premium_rds = file.path(
    output_dir, paste0("portfolio_premiums_", frequency, ".rds")
  ),
  force_recompute_portfolio = FALSE
)

ensure_dir(config$output_dir)

portfolio_premiums <- run_portfolio_premiums_stage(config)
message("Portfolio premium rows: ", nrow(portfolio_premiums))
