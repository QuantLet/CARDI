# Main controller for the modular CARDI_Test workflow.
#
# Change CARDI_TEST_FREQUENCY to "monthly" or "weekly" and run:
#   Rscript R/CARDI_Test/main.R

wdir = "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
setwd(wdir)

options(stringsAsFactors = FALSE)

CARDI_TEST_FREQUENCY <- Sys.getenv("CARDI_TEST_FREQUENCY", unset = "weekly")

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))
} else {
  getwd()
}
if (!file.exists(file.path(script_dir, "config.R"))) {
  script_dir <- file.path(getwd(), "Code", "CARDI_Test")
}

source(file.path(script_dir, "config.R"))
source(file.path(script_dir, "functions_frequency.R"))
source(file.path(script_dir, "functions_load_data.R"))
source(file.path(script_dir, "functions_generate_data.R"))
source(file.path(script_dir, "functions_portfolio.R"))
source(file.path(script_dir, "functions_factor_regression.R"))
source(file.path(script_dir, "functions_predictability.R"))
source(file.path(script_dir, "functions_new_indicators_predictability.R"))

config <- cardi_test_config(CARDI_TEST_FREQUENCY)
ensure_dir(config$output_dir)

message("CARDI_Test workflow frequency: ", config$frequency)
message("Output folder: ", config$output_dir)

ensure_processed_frequency_inputs(config)

portfolio_premiums <- build_portfolio_premiums(config, config$frequency)
message("Portfolio premium rows: ", nrow(portfolio_premiums))

enriched <- run_factor_regression(config, portfolio_premiums)
message("Enriched regression dataset rows: ", nrow(enriched))

predictability <- run_predictability_outputs(config, enriched)
message("Predictability regressions: ", nrow(predictability$summary))

new_indicator_predictability <- run_new_indicators_predictability(config, enriched)
message("NewIndicator predictability regressions: ",
        nrow(new_indicator_predictability$summary))

message("Done. New modular outputs are saved under: ", config$output_dir)
