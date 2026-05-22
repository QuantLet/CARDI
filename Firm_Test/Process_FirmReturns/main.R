# =============================================================================
# File    : main.R
# Purpose : Orchestrate the full Process_FirmReturns pipeline: load inputs,
#           build firm-level returns and pure returns, aggregate HC/MC/LC pool
#           returns, construct relative volatility datasets, and run selected
#           predictability tests.
# Author  : CARDI Research Team
# Date    : 2026-05-16
# Dependencies:
#   - config.R
#   - functions_utils.R
#   - functions_load_data.R
#   - functions_returns.R
#   - functions_controls.R
#   - functions_pure_return.R
#   - functions_pool_returns.R
#   - functions_relative_volatility.R
#   - functions_predictability_CARDI.R
# =============================================================================

#' Resolve the directory containing this script
#'
#' @return Character path to the script directory when launched via `Rscript`,
#'   otherwise the current working directory.
.resolve_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  sf <- if (length(fa) > 0) sub("^--file=", "", fa[1]) else ""
  if (nzchar(sf) && file.exists(sf)) dirname(normalizePath(sf)) else getwd()
}

SCRIPT_DIR <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/Code/Firm_Test/Process_FirmReturns"
setwd(SCRIPT_DIR)

for (mod in c("config", "functions_utils", "functions_load_data",
              "functions_returns", "functions_controls", "functions_pure_return",
              "functions_pool_returns", "functions_relative_volatility",
              "functions_predictability_CARDI")) {
  source(file.path(SCRIPT_DIR, paste0(mod, ".R")))
}

cfg <- firm_returns_config()
args <- commandArgs(trailingOnly = TRUE)
if ("--force" %in% args || "--overwrite" %in% args) cfg$overwrite <- TRUE

#' Extract a command-line argument value by prefix
#'
#' @param prefix Prefix such as `"--pred-scope="`.
#'
#' @return Character value after the prefix, or `NULL` if absent.
arg_value <- function(prefix) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) return(NULL)
  sub(prefix, "", hit[1], fixed = TRUE)
}
#' Split a comma-separated command-line argument
#'
#' @param x Character scalar, or `NULL`.
#'
#' @return Character vector, or `NULL`.
split_arg <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}
pred_scope <- arg_value("--pred-scope=")
pred_family <- arg_value("--pred-family=")
pred_vars <- split_arg(arg_value("--pred-vars="))
pred_deps <- split_arg(arg_value("--pred-deps="))
pred_specs <- split_arg(arg_value("--pred-specs="))
if (!is.null(pred_scope)) cfg$predictability$scope <- pred_scope
if (!is.null(pred_family)) cfg$predictability$predictor_family <- pred_family
if (!is.null(pred_vars)) cfg$predictability$predictor_vars <- pred_vars
if (!is.null(pred_deps)) cfg$predictability$dependent_vars <- pred_deps
if (!is.null(pred_specs)) cfg$predictability$specifications <- pred_specs

ensure_dir(cfg$output_dir)
ensure_dir(cfg$predictability_dir)

cat("=================================================================\n")
cat(" Process_FirmReturns\n")
cat("  Project root:", cfg$project_root, "\n")
cat("  Module dir  :", cfg$module_dir, "\n")
cat("  Output dir  :", cfg$output_dir, "\n")
cat("  Overwrite   :", cfg$overwrite, "\n")
cat("  Pred scope  :", cfg$predictability$scope, "\n")
cat("  Pred family :", cfg$predictability$predictor_family, "\n")
cat("=================================================================\n")

validation_log <- NULL

# Shared inputs are cheap relative to return construction.
rf_daily <- load_fama_daily(cfg)
rf_monthly <- load_fama_monthly(cfg)
monthly_enrichment <- load_monthly_enrichment(cfg)
industry <- tryCatch(load_industry_codes(cfg), error = function(e) {
  warning("Industry codes unavailable: ", e$message)
  NULL
})
fi_wind <- tryCatch(load_fi_wind(cfg), error = function(e) {
  warning("Data_FI_WIND unavailable: ", e$message)
  NULL
})
carbon_rank <- tryCatch(load_carbon_rank(cfg), error = function(e) NULL)

firm_long_all <- NULL
annual_controls <- NULL
daily_df <- NULL
monthly_df <- NULL
pool_paths <- c(unlist(cfg$outputs$daily), unlist(cfg$outputs$monthly))
need_firm_build <- isTRUE(cfg$modules$process_firm_returns) &&
  (cfg$overwrite || !file.exists(cfg$outputs$firm_daily) || !file.exists(cfg$outputs$firm_month))
need_firm_build = TRUE
if (need_firm_build) {
  cat("\n--- Loading stock price/market-value pools ---\n")
  firm_long_all <- build_all_firms_long(cfg)
  validation_log <- append_validation(validation_log, "combined_stock_panel", firm_long_all,
                                      paste0("sorted_by_firm=", validate_sorted_by_firm(firm_long_all),
                                             "; carbon_types_missing=",
                                             sum(is.na(firm_long_all$CarbonType))))

  if (!is.null(fi_wind)) {
    annual_controls <- build_annual_controls(fi_wind, carbon_rank, firm_long_all, cfg)
    annual_controls <- winsorize_annual_controls(annual_controls, cfg$winsor_probs)
    validation_log <- append_validation(validation_log, "annual_controls", annual_controls,
                                        "previous-year merge uses Year + 1")
  }
}

if (isTRUE(cfg$modules$process_firm_returns)) {
  cat("\n--- Firm-level daily returns ---\n")
  daily_df <- read_or_build_rds(cfg$outputs$firm_daily, cfg$overwrite, function() {
    x <- compute_daily_log_returns(firm_long_all)
    x <- add_daily_excess_return(x, rf_daily, cfg$rf_scale)
    if (!is.null(annual_controls)) {
      x <- merge_lagged_controls(x, annual_controls, industry)
    } else {
      x$IndustryCode <- NA_character_
    }
    if (isTRUE(cfg$modules$estimate_pure_returns)) {
      x <- extract_pure_returns(x, "daily", cfg$min_pure_return_obs, cfg$min_industry_count)
    } else {
      x$PureReturn <- NA_real_
    }
    add_output_aliases(x)
  })
  validation_log <- append_validation(
    validation_log, "Firm_return_daily", daily_df,
    paste0("return_non_missing=", sum(!is.na(daily_df$Return)),
           "; rf_missing=", sum(is.na(daily_df$IndexRiskFreeRate)),
           "; pure_non_missing=", sum(!is.na(daily_df$PureReturn)))
  )

  cat("\n--- Firm-level monthly returns and within-month volatility ---\n")
  monthly_df <- read_or_build_rds(cfg$outputs$firm_month, cfg$overwrite, function() {
    month_end <- get_month_end_prices(firm_long_all)
    x <- compute_monthly_log_returns(month_end)
    x <- add_monthly_excess_return(x, rf_monthly, cfg$rf_scale)
    if (!is.null(annual_controls)) {
      x <- merge_lagged_controls(x, annual_controls, industry)
    } else {
      x$IndustryCode <- NA_character_
    }
    if (isTRUE(cfg$modules$estimate_pure_returns)) {
      x <- extract_pure_returns(x, "monthly", cfg$min_pure_return_obs, cfg$min_industry_count)
    } else {
      x$PureReturn <- NA_real_
    }
    daily_with_month <- daily_df
    daily_with_month$Month <- month_key(daily_with_month$Date)
    vol <- compute_firm_month_volatility(daily_with_month)
    x$Month <- month_key(x$Date)
    add_output_aliases(merge(x, vol, by = c("StockID", "Month"), all.x = TRUE))
  })
  validation_log <- append_validation(
    validation_log, "Firm_return_month", monthly_df,
    paste0("return_non_missing=", sum(!is.na(monthly_df$Return)),
           "; monthly_vol_non_missing=", sum(!is.na(monthly_df$vola_return)),
           "; pure_non_missing=", sum(!is.na(monthly_df$PureReturn)))
  )
} else {
  if (file.exists(cfg$outputs$firm_daily)) daily_df <- readRDS(cfg$outputs$firm_daily)
  if (file.exists(cfg$outputs$firm_month)) monthly_df <- readRDS(cfg$outputs$firm_month)
}

pool_outputs <- NULL
if (isTRUE(cfg$modules$build_pool_returns)) {
  cat("\n--- Pool-level daily/monthly returns and volatility ---\n")
  if (is.null(daily_df)) daily_df <- readRDS(cfg$outputs$firm_daily)
  if (is.null(monthly_df)) monthly_df <- readRDS(cfg$outputs$firm_month)
  if (!cfg$overwrite && all(file.exists(pool_paths))) {
    pool_outputs <- list(
      daily = lapply(cfg$outputs$daily, readRDS),
      monthly = lapply(cfg$outputs$monthly, readRDS)
    )
  } else {
    pool_outputs <- build_pool_outputs(daily_df, monthly_df, monthly_enrichment)
    for (ct in c("HC", "MC", "LC")) {
      save_rds_safe(pool_outputs$daily[[ct]], cfg$outputs$daily[[ct]], force = TRUE)
      save_rds_safe(pool_outputs$monthly[[ct]], cfg$outputs$monthly[[ct]], force = TRUE)
    }
  }
  for (ct in c("HC", "MC", "LC")) {
    validation_log <- append_validation(validation_log, paste0(ct, "_monthly_pool"),
                                        pool_outputs$monthly[[ct]],
                                        "pool volatility from daily pool series plus weighted average firm volatility")
  }
}

hc_firm_monthly <- NULL
hc_monthly <- NULL
if (isTRUE(cfg$modules$build_relative_volatility)) {
  cat("\n--- HC versus LC relative volatility ---\n")
  if (is.null(monthly_df)) monthly_df <- readRDS(cfg$outputs$firm_month)
  if (is.null(pool_outputs)) {
    pool_outputs <- list(monthly = list(
      HC = readRDS(cfg$outputs$monthly$HC),
      MC = readRDS(cfg$outputs$monthly$MC),
      LC = readRDS(cfg$outputs$monthly$LC)
    ))
  }
  alignment <- check_hc_lc_alignment(pool_outputs$monthly$HC, pool_outputs$monthly$LC)
  hc_firm_monthly <- read_or_build_rds(cfg$outputs$hc_firm_month, cfg$overwrite, function() {
    build_hc_firm_monthly(monthly_df, pool_outputs$monthly$LC, monthly_enrichment)
  })
  hc_monthly <- read_or_build_rds(cfg$outputs$hc_month, cfg$overwrite, function() {
    build_hc_monthly(pool_outputs$monthly$HC, pool_outputs$monthly$LC)
  })
  validation_log <- append_validation(validation_log, "HC_Firm_Monthly", hc_firm_monthly,
                                      paste0("common_hc_lc_months=", alignment$common_months,
                                             "; rela_non_missing=",
                                             sum(!is.na(hc_firm_monthly$Rela_vola_PurReturn))))
  validation_log <- append_validation(validation_log, "HC_Monthly", hc_monthly,
                                      paste0("common_hc_lc_months=", alignment$common_months,
                                             "; rela_non_missing=",
                                             sum(!is.na(hc_monthly$Rela_vola_PurReturn))))
}

if (isTRUE(cfg$modules$run_predictability_tests)) {
  cat("\n--- CARDI and other-indicator predictability ---\n")
  if (is.null(hc_firm_monthly)) hc_firm_monthly <- readRDS(cfg$outputs$hc_firm_month)
  if (is.null(hc_monthly)) hc_monthly <- readRDS(cfg$outputs$hc_month)
  pred <- run_all_predictability(hc_firm_monthly, hc_monthly, cfg)
  validation_log <- append_validation(validation_log, "predictability_firm", pred$firm,
                                      paste0("lagged_predictors_checked=",
                                             length(unique(pred$firm$independent_variable %||% character(0)))))
  validation_log <- append_validation(validation_log, "predictability_pool", pred$pool,
                                      paste0("lagged_predictors_checked=",
                                             length(unique(pred$pool$independent_variable %||% character(0)))))
}

save_csv_safe(validation_log, cfg$outputs$validation_log, force = TRUE)

cat("\n=================================================================\n")
cat(" Process_FirmReturns completed\n")
cat("  Key outputs in:", cfg$output_dir, "\n")
cat("  Predictability outputs in:", cfg$predictability_dir, "\n")
cat("=================================================================\n")
