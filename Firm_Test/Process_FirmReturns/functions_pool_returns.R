# =============================================================================
# File    : functions_pool_returns.R
# Purpose : Aggregate firm-level daily and monthly returns into HC/MC/LC
#           pool-level value-weighted returns and volatility measures, and
#           merge monthly CARDI/macro/event/indicator data.
# Author  : CARDI Research Team
# Date    : 2026-05-16
# Dependencies:
#   - functions_utils.R (month_key, ensure_dir)
#   - functions_load_data.R outputs for monthly enrichment
# =============================================================================

#' Compute a value-weighted mean with missing-value protection
#'
#' @param values Numeric vector of observations.
#' @param weights Numeric vector of non-negative weights.
#'
#' @return Weighted mean, or `NA_real_` if no valid value-weight pair exists.
weighted_mean_safe <- function(values, weights) {
  ok <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(ok)) return(NA_real_)
  sum(values[ok] * weights[ok], na.rm = TRUE) / sum(weights[ok], na.rm = TRUE)
}

#' Compute firm-month volatility from daily firm returns
#'
#' @param daily_df Daily firm-level panel with `Date`, `StockID`, `Return`,
#'   `ExReturn`, and `PureReturn`.
#'
#' @return Data frame keyed by `StockID` and `Month`, containing standard
#'   deviations of daily raw, excess, and pure returns.
compute_firm_month_volatility <- function(daily_df) {
  if (!"Month" %in% names(daily_df)) daily_df$Month <- month_key(daily_df$Date)
  keys <- split(daily_df, list(daily_df$StockID, daily_df$Month), drop = TRUE)
  rows <- lapply(keys, function(sub) {
    sd2 <- function(x) if (sum(!is.na(x)) >= 2) sd(x, na.rm = TRUE) else NA_real_
    data.frame(
      StockID = sub$StockID[1],
      Month = sub$Month[1],
      vola_return = sd2(sub$Return),
      vola_ExReturn = if ("ExReturn" %in% names(sub)) sd2(sub$ExReturn) else NA_real_,
      vola_PurReturn = if ("PureReturn" %in% names(sub)) sd2(sub$PureReturn) else NA_real_,
      n_daily_obs = sum(!is.na(sub$Return)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$StockID, out$Month), , drop = FALSE]
}

#' Compute daily value-weighted pool returns
#'
#' @param daily_df Daily firm-level panel.
#' @param carbon_type Carbon group to aggregate: `HC`, `MC`, or `LC`.
#'
#' @return Daily pool-level data frame with value-weighted raw, excess, and
#'   pure returns.
compute_pool_daily <- function(daily_df, carbon_type) {
  sub <- daily_df[daily_df$CarbonType == carbon_type, , drop = FALSE]
  dates <- sort(unique(sub$Date))
  rows <- lapply(dates, function(d) {
    x <- sub[sub$Date == d, , drop = FALSE]
    data.frame(
      date = d,
      Month = month_key(d),
      CarbonType = carbon_type,
      return = weighted_mean_safe(x$Return, x$MktCap),
      excess_return = weighted_mean_safe(x$ExReturn, x$MktCap),
      PureReturn = weighted_mean_safe(x$PureReturn, x$MktCap),
      TotalMktCap = sum(x$MktCap[is.finite(x$MktCap) & x$MktCap > 0], na.rm = TRUE),
      n_firms = sum(is.finite(x$MktCap) & x$MktCap > 0),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$date <- as.Date(out$date, origin = "1970-01-01")
  out[order(out$date), , drop = FALSE]
}

#' Convert daily pool returns to monthly pool volatility
#'
#' @param pool_daily Daily pool-level return series from `compute_pool_daily()`.
#'
#' @return Monthly pool-level data frame with end-of-month returns and standard
#'   deviations of daily pool returns within each month.
compute_pool_month_from_daily <- function(pool_daily) {
  months <- sort(unique(pool_daily$Month))
  rows <- lapply(months, function(m) {
    x <- pool_daily[pool_daily$Month == m, , drop = FALSE]
    sd2 <- function(v) if (sum(!is.na(v)) >= 2) sd(v, na.rm = TRUE) else NA_real_
    last <- x[which.max(x$date), , drop = FALSE]
    data.frame(
      Month = m,
      date = max(x$date, na.rm = TRUE),
      CarbonType = last$CarbonType[1],
      return = last$return[1],
      excess_return = last$excess_return[1],
      PureReturn = last$PureReturn[1],
      vola_return = sd2(x$return),
      vola_ExReturn = sd2(x$excess_return),
      vola_PurReturn = sd2(x$PureReturn),
      pool_daily_obs = nrow(x),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$date <- as.Date(out$date, origin = "1970-01-01")
  out[order(out$Month), , drop = FALSE]
}

#' Compute value-weighted average firm-level volatility by pool
#'
#' @param monthly_df Monthly firm panel containing firm volatility measures.
#' @param carbon_type Carbon group to aggregate: `HC`, `MC`, or `LC`.
#'
#' @return Monthly data frame with value-weighted average firm volatilities and
#'   value-weighted monthly returns.
compute_weighted_average_firm_volatility <- function(monthly_df, carbon_type) {
  sub <- monthly_df[monthly_df$CarbonType == carbon_type, , drop = FALSE]
  months <- sort(unique(sub$Month))
  rows <- lapply(months, function(m) {
    x <- sub[sub$Month == m, , drop = FALSE]
    data.frame(
      Month = m,
      CarbonType = carbon_type,
      wavg_month_return = weighted_mean_safe(x$Return, x$MktCap),
      wavg_month_ExReturn = weighted_mean_safe(x$ExReturn, x$MktCap),
      wavg_month_PureReturn = weighted_mean_safe(x$PureReturn, x$MktCap),
      wavg_firm_vola_return = weighted_mean_safe(x$vola_return, x$MktCap),
      wavg_firm_vola_ExReturn = weighted_mean_safe(x$vola_ExReturn, x$MktCap),
      wavg_firm_vola_PurReturn = weighted_mean_safe(x$vola_PurReturn, x$MktCap),
      firm_vol_n = sum(is.finite(x$MktCap) & x$MktCap > 0),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Merge monthly enrichment data into a pool or relative-volatility dataset
#'
#' @description
#' Merges CARDI, macro, alternative indicators, and event dummies by `Month`.
#' When multiple sources contain the same column name, existing non-missing
#' values are retained and missing values are filled from the newer source.
#'
#' @param df Base monthly data frame keyed by `Month`.
#' @param monthly_enrichment Named list from `load_monthly_enrichment()`.
#'
#' @return Enriched monthly data frame.
merge_monthly_enrichment <- function(df, monthly_enrichment) {
  out <- df
  for (nm in c("cardi", "macro", "indicators", "events")) {
    add <- monthly_enrichment[[nm]]
    if (is.null(add)) next
    add <- add[, !duplicated(names(add)), drop = FALSE]
    shared <- setdiff(intersect(names(out), names(add)), "Month")
    if (length(shared) > 0) {
      add_renamed <- add
      names(add_renamed)[match(shared, names(add_renamed))] <- paste0(shared, "__new")
      out <- merge(out, add_renamed, by = "Month", all.x = TRUE)
      for (col in shared) {
        new_col <- paste0(col, "__new")
        out[[col]] <- ifelse(is.na(out[[col]]), out[[new_col]], out[[col]])
        out[[new_col]] <- NULL
      }
    } else {
      out <- merge(out, add, by = "Month", all.x = TRUE)
    }
  }
  for (col in c("Event_dummy_M", "Event_Covid_M", "Event_China_M",
                "Event_International_M")) {
    if (!col %in% names(out)) out[[col]] <- 0L
  }
  event_cols <- grep("^Event_.*_M$", names(out), value = TRUE)
  for (col in event_cols) out[[col]][is.na(out[[col]])] <- 0
  out[order(out$Month), , drop = FALSE]
}

#' Build daily and monthly pool outputs for all carbon groups
#'
#' @param daily_df Daily firm-level return panel.
#' @param monthly_df Monthly firm-level return and volatility panel.
#' @param monthly_enrichment Named list of CARDI/macro/event/indicator data.
#'
#' @return Named list with `daily` and `monthly` elements; each contains `HC`,
#'   `MC`, and `LC` data frames.
build_pool_outputs <- function(daily_df, monthly_df, monthly_enrichment) {
  types <- c("HC", "MC", "LC")
  daily <- list()
  monthly <- list()
  for (ct in types) {
    daily[[ct]] <- compute_pool_daily(daily_df, ct)
    pool_month <- compute_pool_month_from_daily(daily[[ct]])
    firm_vol <- compute_weighted_average_firm_volatility(monthly_df, ct)
    month <- merge(pool_month, firm_vol, by = c("Month", "CarbonType"), all.x = TRUE)
    monthly[[ct]] <- merge_monthly_enrichment(month, monthly_enrichment)
  }
  list(daily = daily, monthly = monthly)
}
