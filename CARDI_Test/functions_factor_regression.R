# Factor regression and enriched premium construction.

rolling_lower_tail_quantile <- function(x, prob, window) {
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    start <- i - window + 1L
    if (start < 1L) next
    values <- x[start:i]
    values <- values[is.finite(values)]
    if (length(values) >= max(6L, floor(window / 2))) {
      out[i] <- as.numeric(stats::quantile(values, probs = prob,
                                           na.rm = TRUE, names = FALSE,
                                           type = 7))
    }
  }
  out
}

add_ar1_residual <- function(data, source_col = "pure_LC_premium",
                             out_col = "AR1_Premium") {
  data$pure_LC_premium_lag1 <- c(NA_real_, head(data[[source_col]], -1))
  fit_data <- data[finite_complete(data, c(source_col,
                                           "pure_LC_premium_lag1")), ,
                   drop = FALSE]
  data[[out_col]] <- NA_real_
  if (nrow(fit_data) >= 10) {
    fit <- stats::lm(
      stats::as.formula(paste(source_col, "~ pure_LC_premium_lag1")),
      data = fit_data
    )
    data[[out_col]][as.integer(rownames(fit_data))] <- stats::residuals(fit)
  }
  data
}

run_factor_regression <- function(config, portfolio_premiums) {
  if (!isTRUE(config$force_recompute_regression) &&
      file.exists(config$enriched_rds)) {
    return(readRDS(config$enriched_rds))
  }

  factors <- load_fama_factors(config, config$frequency)
  cardi <- load_cardi_frequency(config, config$frequency)
  macro <- load_macro_frequency(config, config$frequency)

  portfolio_premiums$Period <- normalize_period_key(portfolio_premiums,
                                                    config$frequency)
  merged <- Reduce(
    function(x, y) merge(x, y, by = "Period", all = FALSE),
    list(portfolio_premiums, factors, cardi, macro)
  )
  merged <- merged[order(merged$Date), , drop = FALSE]
  write_new_csv(merged, config$merged_analysis_file)

  factor_vars <- c("MarketPremium", "SMB2", "HML2", "RMW2", "CMA2")
  reg_vars <- c("LC_HC_Premium", factor_vars)
  fit_data <- merged[finite_complete(merged, reg_vars), , drop = FALSE]
  if (nrow(fit_data) < length(reg_vars) + 5) {
    stop("Insufficient complete observations for factor regression.")
  }

  factor_fit <- stats::lm(
    LC_HC_Premium ~ MarketPremium + SMB2 + HML2 + RMW2 + CMA2,
    data = fit_data
  )

  merged$fitted_LC_HC_Premium <- NA_real_
  merged$pure_LC_premium <- NA_real_
  idx <- as.integer(rownames(fit_data))
  merged$fitted_LC_HC_Premium[idx] <- stats::fitted(factor_fit)
  merged$pure_LC_premium[idx] <- stats::residuals(factor_fit)

  merged$pure_LC_premium_VaR_10 <- rolling_lower_tail_quantile(
    merged$pure_LC_premium, 0.10, config$var_window
  )
  merged$pure_LC_premium_VaR_5 <- rolling_lower_tail_quantile(
    merged$pure_LC_premium, 0.05, config$var_window
  )
  merged$pure_LC_premium_VaR_1 <- rolling_lower_tail_quantile(
    merged$pure_LC_premium, 0.01, config$var_window
  )

  merged <- add_ar1_residual(merged)
  merged$AR1_Premium_VaR_10 <- rolling_lower_tail_quantile(
    merged$AR1_Premium, 0.10, config$var_window
  )
  merged$AR1_Premium_VaR_5 <- rolling_lower_tail_quantile(
    merged$AR1_Premium, 0.05, config$var_window
  )
  merged$AR1_Premium_VaR_1 <- rolling_lower_tail_quantile(
    merged$AR1_Premium, 0.01, config$var_window
  )

  save_new_rds(list(factor_fit = factor_fit), config$model_rds)
  write_new_csv(merged, config$enriched_file)
  save_new_rds(merged, config$enriched_rds)
  merged
}
