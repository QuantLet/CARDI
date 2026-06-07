# =============================================================================
# File    : functions_controls.R
# Purpose : Build annual firm-level control variables from Data_FI_WIND.rds,
#           Carbon_Rank.rds, and the market-cap panels.
#
# Variables constructed
#   logAsset     : log(Total Asset)
#   ROE          : Return on Equity  (from Data_FI_WIND directly)
#   BM           : Book value (Equity_t) / Year-end MktCap_{t-1}
#   CapExRatio   : CapitalExpenditure_t / Total Asset_t
#   logEmissions : log(mean carbon emissions; static from Carbon_Rank)
#
# All controls are winsorised at the 1% and 99% levels.
# Merged as PREVIOUS-YEAR values: for return in year t, controls are from t-1.
#
# Author  : CARDI Research Team
# Date    : 2026-05-15
# Dependencies:
#   - functions_utils.R   (safe_log, winsorize_df, clean_stock_id, year_key)
# =============================================================================


# -----------------------------------------------------------------------------
# Reshape helper
# -----------------------------------------------------------------------------

#' Reshape one indicator from Data_FI_WIND wide format to long format
#'
#' Data_FI_WIND stores each indicator as a data.frame with columns:
#'   ID (stock code), "2000", "2001", ..., "2024"
#'
#' @param fi_wind       Named list from \code{load_fi_wind()}.
#' @param indicator_nm  Character scalar: name of the indicator (e.g. "Asset").
#'
#' @return data.frame with columns StockID (chr), Year (int), <indicator_nm>
#'   (num).
reshape_fi_indicator <- function(fi_wind, indicator_nm, out_name = indicator_nm) {
  if (!indicator_nm %in% names(fi_wind))
    stop("Indicator '", indicator_nm, "' not found in Data_FI_WIND. ",
         "Available: ", paste(names(fi_wind), collapse = ", "))

  wide     <- fi_wind[[indicator_nm]]
  all_cols <- names(wide)
  # Year columns are purely numeric strings
  year_cols     <- all_cols[grepl("^[0-9]{4}$", all_cols)]
  year_ints     <- as.integer(year_cols)

  n_firms <- nrow(wide)
  n_years <- length(year_cols)

  out <- data.frame(
    StockID = rep(clean_stock_id(wide$ID), times = n_years),
    Year    = rep(year_ints, each = n_firms),
    Value   = suppressWarnings(
      as.numeric(unlist(wide[, year_cols, drop = FALSE], use.names = FALSE))
    ),
    stringsAsFactors = FALSE
  )
  names(out)[3] <- out_name
  out
}


# -----------------------------------------------------------------------------
# Year-end market cap
# -----------------------------------------------------------------------------

#' Get the last (year-end) market cap per firm per calendar year
#'
#' Used for constructing the book-to-market ratio.
#'
#' @param firm_long_all  Combined long panel from \code{build_all_firms_long()},
#'   must contain columns Date, StockID, MktCap.
#'
#' @return data.frame with StockID (chr), Year (int), YearEndMktCap (num).
get_year_end_mktcap <- function(firm_long_all) {
  sub <- firm_long_all[is.finite(firm_long_all$MktCap) & firm_long_all$MktCap > 0,
                       c("Date", "StockID", "MktCap"), drop = FALSE]
  sub$Year <- year_key(sub$Date)

  # For each (StockID, Year) take the observation with the latest date.
  groups <- split(sub, list(sub$StockID, sub$Year), drop = TRUE)
  out_list <- lapply(groups, function(g) {
    g <- g[order(g$Date), , drop = FALSE]
    data.frame(StockID      = g$StockID[nrow(g)],
               Year         = g$Year[nrow(g)],
               YearEndMktCap = g$MktCap[nrow(g)],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out_list)
  out[order(out$StockID, out$Year), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# Main control construction
# -----------------------------------------------------------------------------

#' Build annual firm-level control variables
#'
#' Constructs logAsset, ROE, BM, CapExRatio, and logEmissions for every
#' (StockID, Year) combination available in Data_FI_WIND.
#'
#' Book-to-market definition:
#'   BM_year_t = Equity_t / YearEndMktCap_{t-1}
#' This follows the standard Fama-French convention: book value from the most
#' recent annual report divided by market cap at the end of the PREVIOUS year.
#'
#' @param fi_wind      Named list from \code{load_fi_wind()}.
#' @param carbon_rank  data.frame from \code{load_carbon_rank()}, or NULL.
#' @param firm_long_all Combined long panel (for year-end market caps).
#'
#' @return data.frame: StockID, Year, logAsset, ROE, BM, CapExRatio,
#'   logEmissions.  No winsorisation applied here (done in the next step).
#'   input: fi_wind, carbon_rank, firm_long_all, cfg
build_annual_controls <- function(fi_wind, carbon_rank, firm_long_all, config = NULL) {

  # -- 1. Reshape required indicators ----------------------------------------
  sources <- config$control_sources %||% list(
    asset = c("Asset"),
    equity = c("Equity"),
    roe = c("ROE"),
    capex = c("CapitalExpenditure")
  )
  indicator_map <- c(
    Asset = first_existing(sources$asset, names(fi_wind), label = "Asset indicator"),
    Equity = first_existing(sources$equity, names(fi_wind), label = "Equity indicator"),
    ROE = first_existing(sources$roe, names(fi_wind), label = "ROE indicator"),
    CapitalExpenditure = first_existing(sources$capex, names(fi_wind), label = "CapitalExpenditure indicator")
  )
  indicators <- names(indicator_map)
  ind_dfs <- lapply(indicators, function(nm) {
    reshape_fi_indicator(fi_wind, unname(indicator_map[[nm]]), out_name = nm)
  })
  names(ind_dfs) <- indicators

  # Merge all indicators on StockID + Year
  ctrl <- ind_dfs[["Asset"]]
  for (nm in indicators[-1]) {
    ctrl <- merge(ctrl, ind_dfs[[nm]], by = c("StockID", "Year"), all = TRUE)
  }

  # -- 2. logAsset -----------------------------------------------------------
  ctrl$logAsset <- safe_log(ctrl$Asset)

  # -- 3. CapEx ratio ---------------------------------------------------------
  ctrl$CapExRatio <- ifelse(
    is.finite(ctrl$Asset) & ctrl$Asset > 0,
    ctrl$CapitalExpenditure / ctrl$Asset,
    NA_real_
  )

  # -- 4. Book-to-Market: Equity_t / YearEndMktCap_{t-1} --------------------
  year_end_mc <- get_year_end_mktcap(firm_long_all)

  # Shift market-cap year by +1 so that MktCap_{t-1} aligns with Year=t.
  mc_prev       <- year_end_mc
  mc_prev$Year  <- mc_prev$Year + 1L
  names(mc_prev)[names(mc_prev) == "YearEndMktCap"] <- "PrevYearMktCap"

  ctrl <- merge(ctrl,
                mc_prev[, c("StockID", "Year", "PrevYearMktCap")],
                by = c("StockID", "Year"), all.x = TRUE)

  ctrl$BM <- ifelse(
    is.finite(ctrl$PrevYearMktCap) & ctrl$PrevYearMktCap > 0,
    ctrl$Equity / (ctrl$PrevYearMktCap*10^8),
    NA_real_
  )

  # -- 5. Log carbon emissions (static, from Carbon_Rank) --------------------
  ctrl$logEmissions <- NA_real_
  if (!is.null(carbon_rank)) {
    id_col <- first_existing(c("ID", "Symbol", "StockID", "证券代码"),
                             names(carbon_rank), required = FALSE,
                             label = "carbon rank stock id")
    emi_col <- first_existing(c("CarbonEmi_Mean", "CarbonEmission",
                                "CarbonEmissions", "Emission", "Emissions"),
                              names(carbon_rank), required = FALSE,
                              label = "carbon emissions")
    if (!is.na(id_col) && !is.na(emi_col)) {
      cr <- carbon_rank[, c(id_col, emi_col), drop = FALSE]
      cr$StockID <- clean_stock_id(cr[[id_col]])
      cr$logEmissions <- safe_log(suppressWarnings(as.numeric(cr[[emi_col]])))
    # Merge as a static cross-sectional variable (broadcast across all years)
      ctrl <- merge(ctrl,
                    cr[, c("StockID", "logEmissions")],
                    by = "StockID", all.x = TRUE, suffixes = c("", "_cr"))
      # Resolve duplicate column after merge
      if ("logEmissions_cr" %in% names(ctrl)) {
        ctrl$logEmissions <- ifelse(is.na(ctrl$logEmissions),
                                    ctrl$logEmissions_cr,
                                    ctrl$logEmissions)
        ctrl$logEmissions_cr <- NULL
      }
    }
  }

  # -- 6. Keep only required columns -----------------------------------------
  keep <- c("StockID", "Year", "logAsset", "ROE", "BM", "CapExRatio", "logEmissions")
  ctrl$ROE = ctrl$ROE / 100
  ctrl <- ctrl[, intersect(keep, names(ctrl)), drop = FALSE]
  ctrl[order(ctrl$StockID, ctrl$Year), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# Winsorisation
# -----------------------------------------------------------------------------

#' Winsorise the annual control variables
#'
#' Applied at the 1% and 99% levels across the full panel (pooled across all
#' stocks and years).
#'
#' @param controls  data.frame from \code{build_annual_controls()}.
#' @param probs     Length-2 quantile bounds (default c(0.01, 0.99)).
#'
#' @return The winsorised data.frame.
winsorize_annual_controls <- function(controls, probs = c(0.01, 0.99)) {
  # vars <- c("logAsset", "ROE", "BM", "CapExRatio", "logEmissions")
  vars <- c("logAsset", "ROE", "BM", "CapExRatio")
  winsorize_df(controls, intersect(vars, names(controls)), probs = probs)
}


# -----------------------------------------------------------------------------
# Merge controls into returns data
# -----------------------------------------------------------------------------

#' Merge previous-year control variables into a returns data frame
#'
#' For each return observation in year t, attaches the controls from year t-1
#' (i.e. controls are lagged by one year).  Industry codes are merged from a
#' separate lookup table.
#'
#' The returned data frame has all original columns from \code{returns_df}
#' plus lag_logAsset, lag_ROE, lag_BM, lag_CapExRatio, lag_logEmissions, and
#' IndustryCode.
#'
#' @param returns_df   data.frame with at least Date and StockID columns.
#' @param controls     Annual controls from \code{build_annual_controls()} /
#'   \code{winsorize_annual_controls()}.
#' @param industry_df  data.frame with StockID and IndustryCode (from
#'   \code{load_industry_codes()}), or NULL to omit industry codes.
#'
#' @return Enriched returns data.frame, sorted by StockID then Date.
merge_lagged_controls <- function(returns_df, controls, industry_df = NULL) {
  returns_df$Year <- year_key(returns_df$Date)

  # Shift controls by +1 year: controls in year t-1 apply to return in year t.
  ctrl_lag       <- controls
  ctrl_lag$Year  <- ctrl_lag$Year + 1L

  ctrl_vars <- intersect(c("logAsset", "ROE", "BM", "CapExRatio", "logEmissions"),
                         names(ctrl_lag))
  names(ctrl_lag)[names(ctrl_lag) %in% ctrl_vars] <- paste0("lag_", ctrl_vars)

  out <- merge(returns_df,
               ctrl_lag[, c("StockID", "Year", paste0("lag_", ctrl_vars))],
               by = c("StockID", "Year"), all.x = TRUE)

  # Merge industry codes (static cross-sectional look-up).
  if (!is.null(industry_df)) {
    out <- merge(out,
                 industry_df[, c("StockID", "IndustryCode"), drop = FALSE],
                 by = "StockID", all.x = TRUE)
  } else {
    out$IndustryCode <- NA_character_
  }

  out[order(out$StockID, out$Date), , drop = FALSE]
}
