# =============================================================================
# File    : functions_returns.R
# Purpose : Compute firm-level daily and monthly log returns and excess returns.
#
# Return definitions
#   Daily return    : log(P_t) - log(P_{t-1})
#   Monthly return  : log(P_last_day_of_month_t) - log(P_last_day_of_month_{t-1})
#   Excess return   : log return - IndexRiskFreeRate  (both in decimal form)
#
# Author  : CARDI Research Team
# Date    : 2026-05-15
# Dependencies:
#   - functions_utils.R   (parse_date, month_key, year_key, safe_log,
#                           pivot_panel_long, clean_stock_id)
#   - functions_load_data.R  (load_carbon_group_panels)
# =============================================================================


# -----------------------------------------------------------------------------
# Step 1 — Build the combined long panel from the three carbon groups
# -----------------------------------------------------------------------------

#' Convert a wide price + market-cap panel pair to long format
#'
#' @param prices_wide  Wide price panel (Date + stock columns).
#' @param mktcap_wide  Wide market-cap panel (same structure).
#' @param carbon_type  "HC", "MC", or "LC".
#'
#' @return data.frame: Date (Date), StockID (chr), Price (num), MktCap (num),
#'   CarbonType (chr).  Rows sorted by StockID then Date.
build_firm_long_panel <- function(prices_wide, mktcap_wide, carbon_type) {
  price_long       <- pivot_panel_long(prices_wide, "Price")
  mktcap_long      <- pivot_panel_long(mktcap_wide, "MktCap")

  # Zero / negative prices and market caps are data artefacts; set to NA.
  price_long$Price[!is.finite(price_long$Price) | price_long$Price <= 0] <- NA_real_
  mktcap_long$MktCap[!is.finite(mktcap_long$MktCap) | mktcap_long$MktCap <= 0] <- NA_real_

  out <- merge(price_long, mktcap_long, by = c("Date", "StockID"), all = TRUE)
  out$CarbonType <- carbon_type
  out[order(out$StockID, out$Date), , drop = FALSE]
}

#' Combine long panels from three carbon groups into a single universe panel
#'
#' Loads all three groups, calls \code{build_firm_long_panel} for each, and
#' rbinds them.  If the same StockID appears in multiple groups the first
#' occurrence (by the order HC > MC > LC) is kept for any overlapping dates.
#'
#' @param config Configuration list from \code{firm_returns_config()}.
#'
#' @return data.frame with columns Date, StockID, Price, MktCap, CarbonType.
build_all_firms_long <- function(config) {
  groups <- lapply(
    names(config$carbon_groups),
    function(g) load_carbon_group_panels(config, g)
  )

  panels <- lapply(groups, function(g)
    build_firm_long_panel(g$prices, g$mktcap, g$carbon_type)
  )

  out <- do.call(rbind, panels)

  # Resolve duplicate (StockID, Date) pairs — keep the first (highest carbon
  # type priority) occurrence.
  dup_key <- paste0(out$StockID, "_", as.character(out$Date))
  out     <- out[!duplicated(dup_key), , drop = FALSE]
  out[order(out$StockID, out$Date), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# Step 2 — Daily log returns
# -----------------------------------------------------------------------------

#' Compute daily log returns within each firm
#'
#' Return for firm i on day t: log(Price_{i,t}) - log(Price_{i,t-1}).
#' The first observation for each firm is NA (no prior-day price available).
#' Observations with a missing price receive NA return.
#'
#' @param firm_long  data.frame with at least Date, StockID, Price columns.
#'
#' @return The same data.frame with an additional Return column (numeric).
compute_daily_log_returns <- function(firm_long) {
  firm_long <- firm_long[order(firm_long$StockID, firm_long$Date), , drop = FALSE]

  stocks  <- unique(firm_long$StockID)
  ret_vec <- rep(NA_real_, nrow(firm_long))

  for (sid in stocks) {
    idx         <- which(firm_long$StockID == sid)
    log_prices  <- safe_log(firm_long$Price[idx])
    log_ret     <- c(NA_real_, diff(log_prices))
    ret_vec[idx] <- log_ret
  }

  firm_long$Return <- ret_vec
  firm_long
}


# -----------------------------------------------------------------------------
# Step 3 — Daily excess return
# -----------------------------------------------------------------------------

#' Merge the daily risk-free rate and compute excess return
#'
#' Excess return = Return - IndexRiskFreeRate  (both in decimal form).
#'
#' @param firm_long  Daily long panel with a Return column.
#' @param rf_daily   data.frame with Date and IndexRiskFreeRate (from
#'   \code{load_fama_daily()}).
#' @param rf_scale   Scalar multiplied to IndexRiskFreeRate before subtraction.
#'   Default 1.0 (no scaling needed; rates are already in decimal form).
#'
#' @return The input data.frame with IndexRiskFreeRate and ExReturn columns added.
add_daily_excess_return <- function(firm_long, rf_daily, rf_scale = 1.0) {
  rf_use <- rf_daily[, c("Date", "IndexRiskFreeRate"), drop = FALSE]
  rf_use$IndexRiskFreeRate <- rf_use$IndexRiskFreeRate * rf_scale

  out <- merge(firm_long, rf_use, by = "Date", all.x = TRUE)
  out$ExReturn <- out$Return - out$IndexRiskFreeRate
  out[order(out$StockID, out$Date), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# Step 4 — Monthly log returns (from last-day prices)
# -----------------------------------------------------------------------------

#' Extract the last positive trading-day price per firm per calendar month
#'
#' @param firm_long  Daily long panel (Date, StockID, Price, MktCap,
#'   CarbonType, ...).
#'
#' @return data.frame with one row per (StockID, Month): Date (last trading
#'   day), StockID, Month ("YYYY-MM"), Price (last positive price), MktCap
#'   (last positive market cap), CarbonType.
get_month_end_prices <- function(firm_long) {
  firm_long$Month <- month_key(firm_long$Date)

  # Keep only rows where price is positive (valid trading day).
  pos_rows <- firm_long[is.finite(firm_long$Price) & firm_long$Price > 0, ]

  stocks <- unique(pos_rows$StockID)
  out_list <- vector("list", length(stocks))

  for (i in seq_along(stocks)) {
    sid <- stocks[i]
    sub <- pos_rows[pos_rows$StockID == sid, ]
    sub <- sub[order(sub$Date), , drop = FALSE]

    months     <- unique(sub$Month)
    month_rows <- lapply(months, function(m) {
      sub_m <- sub[sub$Month == m, ]
      # Select the row with the latest date (last trading day of the month).
      sub_m[which.max(sub_m$Date), , drop = FALSE]
    })
    out_list[[i]] <- do.call(rbind, month_rows)
  }

  out <- do.call(rbind, out_list)
  out[order(out$StockID, out$Date), , drop = FALSE]
}

#' Compute monthly log returns from month-end prices
#'
#' Monthly return for firm i in month t:
#'   log(P_{i, last day of t}) - log(P_{i, last day of t-1})
#'
#' @param month_end_prices  Output of \code{get_month_end_prices()}.
#'
#' @return The input data.frame with an additional Return column (numeric).
compute_monthly_log_returns <- function(month_end_prices) {
  out <- month_end_prices[order(month_end_prices$StockID,
                                month_end_prices$Date), , drop = FALSE]

  stocks  <- unique(out$StockID)
  ret_vec <- rep(NA_real_, nrow(out))

  for (sid in stocks) {
    idx        <- which(out$StockID == sid)
    log_prices <- safe_log(out$Price[idx])
    log_ret    <- c(NA_real_, diff(log_prices))
    ret_vec[idx] <- log_ret
  }

  out$Return <- ret_vec
  out
}


# -----------------------------------------------------------------------------
# Step 5 — Monthly excess return
# -----------------------------------------------------------------------------

#' Merge the monthly risk-free rate and compute monthly excess return
#'
#' @param monthly_df  data.frame with a Month column and a Return column.
#' @param rf_monthly  data.frame with Month and IndexRiskFreeRate (from
#'   \code{load_fama_monthly()}).
#' @param rf_scale    Scalar multiplied to IndexRiskFreeRate.  Default 1.0.
#'
#' @return The input data.frame with IndexRiskFreeRate and ExReturn columns.
add_monthly_excess_return <- function(monthly_df, rf_monthly, rf_scale = 1.0) {
  rf_use <- rf_monthly[, c("Month", "IndexRiskFreeRate"), drop = FALSE]
  rf_use$IndexRiskFreeRate <- rf_use$IndexRiskFreeRate * rf_scale

  out <- merge(monthly_df, rf_use, by = "Month", all.x = TRUE)
  out$ExReturn <- out$Return - out$IndexRiskFreeRate
  out[order(out$StockID, out$Date), , drop = FALSE]
}
