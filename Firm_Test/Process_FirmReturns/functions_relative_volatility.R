# =============================================================================
# File    : functions_relative_volatility.R
# Purpose : Construct HC-versus-LC relative volatility datasets at firm and
#           pool levels for CARDI predictability regressions.
# Author  : CARDI Research Team
# Date    : 2026-05-16
# Dependencies:
#   - functions_utils.R        (safe_divide)
#   - functions_pool_returns.R (merge_monthly_enrichment)
# =============================================================================

#' Extract LC benchmark volatility columns
#'
#' @param lc_monthly LC pool-level monthly data frame.
#'
#' @return Data frame keyed by `Month`, with LC weighted-average firm
#'   volatility columns renamed as LC benchmark variables.
lc_reference_volatility <- function(lc_monthly) {
  keep <- c("Month", "wavg_firm_vola_return", "wavg_firm_vola_ExReturn",
            "wavg_firm_vola_PurReturn")
  lc <- lc_monthly[, intersect(keep, names(lc_monthly)), drop = FALSE]
  names(lc)[names(lc) == "wavg_firm_vola_return"] <- "LC_wavg_firm_vola_return"
  names(lc)[names(lc) == "wavg_firm_vola_ExReturn"] <- "LC_wavg_firm_vola_ExReturn"
  names(lc)[names(lc) == "wavg_firm_vola_PurReturn"] <- "LC_wavg_firm_vola_PurReturn"
  lc
}

#' Build HC firm-level relative-volatility dataset
#'
#' @param firm_monthly Firm-level monthly return and volatility panel.
#' @param lc_monthly LC monthly pool dataset containing benchmark volatility.
#' @param monthly_enrichment Optional named list of monthly CARDI/macro/event
#'   data to merge into the HC firm panel.
#'
#' @return HC-only firm-month data frame with `Rela_vola_Return`,
#'   `Rela_vola_ExReturn`, and `Rela_vola_PurReturn`.
build_hc_firm_monthly <- function(firm_monthly, lc_monthly, monthly_enrichment = NULL) {
  hc <- firm_monthly[firm_monthly$CarbonType == "HC", , drop = FALSE]
  lc <- lc_reference_volatility(lc_monthly)
  out <- merge(hc, lc, by = "Month", all.x = TRUE)
  out$Rela_vola_Return <- safe_divide(out$vola_return, out$LC_wavg_firm_vola_return)
  out$Rela_vola_ExReturn <- safe_divide(out$vola_ExReturn, out$LC_wavg_firm_vola_ExReturn)
  out$Rela_vola_PurReturn <- safe_divide(out$vola_PurReturn, out$LC_wavg_firm_vola_PurReturn)
  if (!is.null(monthly_enrichment)) out <- merge_monthly_enrichment(out, monthly_enrichment)
  out[order(out$StockID, out$Month), , drop = FALSE]
}

#' Build HC pool-level relative-volatility dataset
#'
#' @param hc_monthly HC pool-level monthly data frame.
#' @param lc_monthly LC pool-level monthly data frame.
#'
#' @return HC monthly pool data with HC/LC relative volatility variables.
build_hc_monthly <- function(hc_monthly, lc_monthly) {
  lc <- lc_reference_volatility(lc_monthly)
  names(lc)[names(lc) == "LC_wavg_firm_vola_return"] <- "LC_pool_wavg_firm_vola_return"
  names(lc)[names(lc) == "LC_wavg_firm_vola_ExReturn"] <- "LC_pool_wavg_firm_vola_ExReturn"
  names(lc)[names(lc) == "LC_wavg_firm_vola_PurReturn"] <- "LC_pool_wavg_firm_vola_PurReturn"
  out <- merge(hc_monthly, lc, by = "Month", all.x = TRUE)
  out$Rela_vola_Return <- safe_divide(out$wavg_firm_vola_return, out$LC_pool_wavg_firm_vola_return)
  out$Rela_vola_ExReturn <- safe_divide(out$wavg_firm_vola_ExReturn, out$LC_pool_wavg_firm_vola_ExReturn)
  out$Rela_vola_PurReturn <- safe_divide(out$wavg_firm_vola_PurReturn, out$LC_pool_wavg_firm_vola_PurReturn)
  out[order(out$Month), , drop = FALSE]
}

#' Check HC and LC month alignment
#'
#' @param hc_monthly HC monthly pool data frame.
#' @param lc_monthly LC monthly pool data frame.
#'
#' @return Named list with month counts and mismatch counts.
check_hc_lc_alignment <- function(hc_monthly, lc_monthly) {
  common <- intersect(hc_monthly$Month, lc_monthly$Month)
  list(
    hc_months = length(unique(hc_monthly$Month)),
    lc_months = length(unique(lc_monthly$Month)),
    common_months = length(unique(common)),
    hc_only = length(setdiff(unique(hc_monthly$Month), unique(lc_monthly$Month))),
    lc_only = length(setdiff(unique(lc_monthly$Month), unique(hc_monthly$Month)))
  )
}
