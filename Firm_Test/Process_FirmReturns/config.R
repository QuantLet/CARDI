# =============================================================================
# File    : config.R
# Purpose : Central configuration for the Process_FirmReturns pipeline,
#           including input paths, output paths, module toggles, column maps,
#           and predictability-output filters.
# Author  : CARDI Research Team
# Date    : 2026-05-16
# Dependencies: Base R only
# =============================================================================

#' Build the firm-return workflow configuration
#'
#' @description
#' Resolves the project root from the current script location and returns a
#' single named configuration list used by every module in the workflow.
#' The list contains file paths, data-source conventions, column mappings,
#' empirical parameters, module toggles, and predictability-output filters.
#'
#' @return A named list with project paths, module settings, input/output
#'   locations, column names, control-variable mappings, and model parameters.
#'
#' @examples
#' \dontrun{
#' cfg <- firm_returns_config()
#' cfg$output_dir
#' }
firm_returns_config <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else ""
  script_dir <- if (nzchar(script_file) && file.exists(script_file)) {
    dirname(normalizePath(script_file, mustWork = TRUE))
  } else {
    getwd()
  }

  find_project_root <- function(start_dir) {
    current <- normalizePath(start_dir, mustWork = TRUE)
    repeat {
      if (dir.exists(file.path(current, "Data", "Processed")) &&
          dir.exists(file.path(current, "Code"))) return(current)
      parent <- dirname(current)
      if (identical(parent, current)) {
        stop("Could not locate project root from: ", start_dir)
      }
      current <- parent
    }
  }

  root <- find_project_root(script_dir)
  module_dir <- file.path(root, "Code", "Firm_Test", "Process_FirmReturns")
  output_dir <- file.path(root, "Output", "FirmReturns")
  pred_dir <- file.path(output_dir, "Predictability")

  outputs <- list(
    firm_daily = file.path(output_dir, "Firm_return_daily.rds"),
    firm_month = file.path(output_dir, "Firm_return_month.rds"),
    hc_firm_month = file.path(output_dir, "HC_Firm_Monthly.rds"),
    hc_month = file.path(output_dir, "HC_Monthly.rds"),
    daily = list(
      HC = file.path(output_dir, "HC_daily.rds"),
      MC = file.path(output_dir, "MC_daily.rds"),
      LC = file.path(output_dir, "LC_daily.rds")
    ),
    monthly = list(
      HC = file.path(output_dir, "HC_monthly.rds"),
      MC = file.path(output_dir, "MC_monthly.rds"),
      LC = file.path(output_dir, "LC_monthly.rds")
    ),
    validation_log = file.path(output_dir, "workflow_validation_summary.csv"),
    predictability_dir = pred_dir
  )

  list(
    project_root = root,
    module_dir = module_dir,
    output_dir = output_dir,
    predictability_dir = pred_dir,

    overwrite = FALSE,
    modules = list(
      process_firm_returns = TRUE,
      estimate_pure_returns = TRUE,
      build_pool_returns = TRUE,
      build_relative_volatility = TRUE,
      run_predictability_tests = TRUE
    ),

    # Predictability output controls.  These can also be overridden from
    # main.R with command-line flags:
    #   --pred-scope=firm|pool|both
    #   --pred-family=cardi|indicators|all
    #   --pred-vars=CARDI_5P_M,CARDI_1P_M,CARDI_10P_M,CARDI_5P_LogDiff_M,CARDI_1P_LogDiff_M,CARDI_10P_LogDiff_M
    #   --pred-deps=Rela_vola_Return,Rela_vola_PurReturn
    #   --pred-specs=NoControls,ControlsFirmFE,ControlsFirmFEYearTrend,ControlsFirmFEYearFE
    predictability = list(
      scope = "firm",
      predictor_family = "cardi",
      predictor_vars = c(
        "CARDI_5P_M", "CARDI_1P_M", "CARDI_10P_M",
        "CARDI_5P_LogDiff_M", "CARDI_1P_LogDiff_M", "CARDI_10P_LogDiff_M"
      ),
      dependent_vars = NULL,
      specifications = NULL,
      write_stata_tables = TRUE,
      write_predictor_summaries = TRUE,
      write_dataset_summaries = TRUE
    ),

    paths = list(
      input_dir = file.path(root, "Data", "Processed", "Input"),
      fama_daily = file.path(root, "Data", "Processed", "FamaFactors", "FamaFactors_Daily.xlsx"),
      fama_monthly = file.path(root, "Data", "Processed", "FamaFactors", "FamaFactors_Monthly.xlsx"),
      stocklist_industry = file.path(root, "Data", "raw", "Stocklist_industry.xlsx"),
      cardi_monthly = file.path(root, "Data", "Processed", "CARDI", "Month_CARDI.xlsx"),
      macro_monthly = file.path(root, "Data", "Processed", "Macro", "Month_Macro.csv"),
      fi_wind = file.path(root, "Data", "Processed", "Data_FI_WIND.rds"),
      carbon_rank = file.path(root, "Output", "Carbon_Rank.rds"),
      indicators_monthly = file.path(root, "Output", "NewIndicators", "Monthly", "All_Indicators_Monthly.csv"),
      events = file.path(root, "Data", "raw", "Important_Carbon_Events.xlsx")
    ),

    source_period = list(start = "20140704", end = "20250127"),
    carbon_groups = c(
      HighCarbonIntens = "HC",
      MedCarbonIntens = "MC",
      LowCarbonIntens = "LC"
    ),

    columns = list(
      date = "Date",
      month = "Month",
      firm_id = "StockID",
      price = "Price",
      market_value = "MktCap",
      return = "Return",
      excess_return = "ExReturn",
      pure_return = "PureReturn",
      risk_free = "IndexRiskFreeRate",
      carbon_type = "CarbonType",
      industry_code = "IndustryCode",
      industry_raw = "所属中上协行业代码",
      fi_firm_id = "ID",
      # controls = c("lag_logAsset", "lag_ROE", "lag_BM", "lag_CapExRatio", "lag_logEmissions"),
      controls = c("lag_logAsset", "lag_ROE", "lag_BM", "lag_CapExRatio"),
      dependent_relative_volatility = c("Rela_vola_Return", "Rela_vola_ExReturn", "Rela_vola_PurReturn")
    ),

    control_sources = list(
      asset = c("Asset", "TotalAsset", "TotalAssets"),
      equity = c("Equity", "BookValue", "BookEquity", "TotalEquity"),
      roe = c("ROE", "ROEA", "ReturnOnEquity"),
      capex = c("CapitalExpenditure", "CAPEX", "CapEx")
    ),

    winsor_probs = c(0.01, 0.99),
    rf_scale = 1.0,
    min_industry_count = 3L,
    min_pure_return_obs = 50L,
    nw_lag = 12L,
    min_pred_obs = 20L,
    firm_cluster_se = TRUE,
    outputs = outputs
  )
}
