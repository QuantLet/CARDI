# =============================================================================
# File    : functions_portfolio.R
# Purpose : Function library for the Table8 CARDI portfolio workflow.
# =============================================================================

options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

parse_date <- function(x) {
  x <- as.character(x)
  out <- as.Date(x)
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- as.Date(x[missing], format = "%Y/%m/%d")
  }
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- as.Date(x[missing], format = "%Y%m%d")
  }
  out
}

clean_stock_id <- function(x) {
  gsub("^X", "", as.character(x))
}

read_numeric_panel <- function(path) {
  if (!file.exists(path)) {
    stop("Missing input file: ", path)
  }
  panel <- read.csv(path, check.names = FALSE)
  if (ncol(panel) < 2) {
    stop("Panel has fewer than two columns: ", path)
  }
  names(panel)[1] <- "Date"
  panel$Date <- parse_date(panel$Date)
  names(panel)[-1] <- clean_stock_id(names(panel)[-1])
  for (j in seq.int(2, ncol(panel))) {
    panel[[j]] <- suppressWarnings(as.numeric(panel[[j]]))
  }
  panel <- panel[!is.na(panel$Date), ]
  panel[order(panel$Date), ]
}

align_numeric_columns <- function(left, right) {
  cols <- intersect(names(left)[-1], names(right)[-1])
  if (length(cols) == 0) {
    stop("No shared stock columns between price and market-cap panels.")
  }
  list(
    left = left[, c("Date", cols), drop = FALSE],
    right = right[, c("Date", cols), drop = FALSE],
    cols = cols
  )
}

daily_value_weighted_returns <- function(price_panel, mktcap_panel) {
  aligned <- align_numeric_columns(price_panel, mktcap_panel)
  prices <- aligned$left
  mktcap <- aligned$right
  merged_dates <- intersect(prices$Date, mktcap$Date)
  prices <- prices[prices$Date %in% merged_dates, , drop = FALSE]
  mktcap <- mktcap[mktcap$Date %in% merged_dates, , drop = FALSE]
  prices <- prices[order(prices$Date), , drop = FALSE]
  mktcap <- mktcap[order(mktcap$Date), , drop = FALSE]

  price_matrix <- as.matrix(prices[, -1, drop = FALSE])
  cap_matrix <- as.matrix(mktcap[, -1, drop = FALSE])
  return_matrix <- diff(log(price_matrix))
  return_matrix[!is.finite(return_matrix)] <- NA_real_

  cap_for_returns <- cap_matrix[-1, , drop = FALSE]
  out <- numeric(nrow(return_matrix))
  for (i in seq_len(nrow(return_matrix))) {
    ok <- is.finite(return_matrix[i, ]) & is.finite(cap_for_returns[i, ]) &
      cap_for_returns[i, ] > 0
    if (any(ok)) {
      weights <- cap_for_returns[i, ok] / sum(cap_for_returns[i, ok])
      out[i] <- sum(return_matrix[i, ok] * weights)
    } else {
      out[i] <- NA_real_
    }
  }
  data.frame(Date = prices$Date[-1], Return = out)
}

monthly_compound_returns <- function(daily_returns, return_name) {
  daily_returns$Month <- format(daily_returns$Date, "%Y-%m")
  monthly <- aggregate(
    daily_returns$Return,
    by = list(Month = daily_returns$Month),
    FUN = function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0) return(NA_real_)
      exp(sum(x)) - 1
    }
  )
  names(monthly)[2] <- return_name
  monthly$Date <- as.Date(paste0(monthly$Month, "-01"))
  monthly[, c("Date", "Month", return_name)]
}

read_group_panels <- function(group_name) {
  group_dir <- file.path(
    config$input_dir,
    group_name,
    paste0(config$date_start_source, "-", config$date_end_source)
  )
  price_file <- file.path(
    group_dir,
    paste0(group_name, "_Price_", config$date_end_source, ".csv")
  )
  mktcap_file <- file.path(
    group_dir,
    paste0(group_name, "_Mktcap_", config$date_end_source, ".csv")
  )
  prices <- read_numeric_panel(price_file)
  mktcap <- read_numeric_panel(mktcap_file)
  list(prices = prices, mktcap = mktcap)
}

read_group_returns <- function(group_name) {
  panels <- read_group_panels(group_name)
  prices <- panels$prices
  mktcap <- panels$mktcap
  daily <- daily_value_weighted_returns(prices, mktcap)
  monthly <- monthly_compound_returns(daily, paste0(group_name, "_VW_Return"))
  list(daily = daily, monthly = monthly, prices = prices, mktcap = mktcap)
}

load_cardi_monthly <- function(cardi_file, signal_names) {
  cardi <- read.csv(cardi_file, check.names = FALSE)
  if (!"date" %in% names(cardi)) {
    stop("CARDI file must include a date column.")
  }
  missing_signals <- setdiff(signal_names, names(cardi))
  if (length(missing_signals) > 0) {
    stop("CARDI file missing signal column(s): ",
         paste(missing_signals, collapse = ", "))
  }
  cardi$date <- parse_date(cardi$date)
  cardi <- cardi[!is.na(cardi$date), c("date", signal_names), drop = FALSE]
  cardi <- cardi[order(cardi$date), , drop = FALSE]
  cardi$Month <- format(cardi$date, "%Y-%m")
  last_idx <- tapply(seq_len(nrow(cardi)), cardi$Month, tail, 1)
  monthly <- cardi[as.integer(last_idx), , drop = FALSE]
  monthly$Date <- as.Date(paste0(monthly$Month, "-01"))
  monthly[, c("Date", "Month", signal_names), drop = FALSE]
}

load_cardi_daily <- function(cardi_file, signal_name) {
  cardi <- read.csv(cardi_file, check.names = FALSE)
  if (!all(c("date", signal_name) %in% names(cardi))) {
    stop("CARDI file must include date and ", signal_name, " columns.")
  }
  cardi$date <- parse_date(cardi$date)
  cardi <- cardi[!is.na(cardi$date), c("date", signal_name), drop = FALSE]
  cardi[[signal_name]] <- suppressWarnings(as.numeric(cardi[[signal_name]]))
  cardi <- cardi[is.finite(cardi[[signal_name]]), , drop = FALSE]
  cardi[order(cardi$date), , drop = FALSE]
}

expanding_quantile <- function(x, prob) {
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    history <- x[seq_len(i)]
    history <- history[is.finite(history)]
    if (length(history) > 0) {
      out[i] <- as.numeric(quantile(history, probs = prob, na.rm = TRUE,
                                    names = FALSE, type = 7))
    }
  }
  out
}

make_strategy_returns <- function(monthly_returns, cardi_monthly, signal, prob,
                                  threshold_label) {
  merged <- merge(monthly_returns, cardi_monthly, by = c("Date", "Month"),
                  all = FALSE)
  merged <- merged[order(merged$Date), , drop = FALSE]
  signal_values <- merged[[signal]]
  merged$CARDI_lag <- c(NA_real_, head(signal_values, -1))
  merged$CARDI_threshold <- c(NA_real_,
                              head(expanding_quantile(signal_values, prob), -1))

  hc_minus_lc <- merged$HighCarbonIntens_VW_Return -
    merged$LowCarbonIntens_VW_Return
  lc_minus_hc <- -hc_minus_lc

  switch_direction <- ifelse(
    is.finite(merged$CARDI_lag) & is.finite(merged$CARDI_threshold) &
      merged$CARDI_lag < merged$CARDI_threshold,
    hc_minus_lc,
    ifelse(
      is.finite(merged$CARDI_lag) & is.finite(merged$CARDI_threshold) &
        merged$CARDI_lag > merged$CARDI_threshold,
      lc_minus_hc,
      NA_real_
    )
  )

  conditional_hc_lc <- ifelse(
    is.finite(merged$CARDI_lag) & is.finite(merged$CARDI_threshold) &
      merged$CARDI_lag < merged$CARDI_threshold,
    hc_minus_lc,
    0
  )
  conditional_lc_hc <- ifelse(
    is.finite(merged$CARDI_lag) & is.finite(merged$CARDI_threshold) &
      merged$CARDI_lag > merged$CARDI_threshold,
    lc_minus_hc,
    0
  )
  conditional_hc_lc[!is.finite(merged$CARDI_lag) |
                      !is.finite(merged$CARDI_threshold)] <- NA_real_
  conditional_lc_hc[!is.finite(merged$CARDI_lag) |
                      !is.finite(merged$CARDI_threshold)] <- NA_real_

  data.frame(
    Date = merged$Date,
    Month = merged$Month,
    CARDI_signal = signal,
    Threshold = threshold_label,
    CARDI = signal_values,
    CARDI_lag = merged$CARDI_lag,
    CARDI_threshold = merged$CARDI_threshold,
    HC_Return = merged$HighCarbonIntens_VW_Return,
    MC_Return = if ("MedCarbonIntens_VW_Return" %in% names(merged)) {
      merged$MedCarbonIntens_VW_Return
    } else {
      NA_real_
    },
    LC_Return = merged$LowCarbonIntens_VW_Return,
    Strategy_A_HC_minus_LC = hc_minus_lc,
    Strategy_B_LC_minus_HC = lc_minus_hc,
    Strategy_C_CARDI_switch = switch_direction,
    Strategy_D_conditional_HC_minus_LC = conditional_hc_lc,
    Strategy_E_conditional_LC_minus_HC = conditional_lc_hc
  )
}

performance_summary <- function(strategy_data) {
  strategy_cols <- grep("^Strategy_", names(strategy_data), value = TRUE)
  out <- do.call(rbind, lapply(strategy_cols, function(col) {
    x <- strategy_data[[col]]
    x <- x[is.finite(x)]
    n <- length(x)
    mean_monthly <- if (n > 0) mean(x) else NA_real_
    sd_monthly <- if (n > 1) sd(x) else NA_real_
    sharpe_annual <- if (is.finite(sd_monthly) && sd_monthly > 0) {
      sqrt(12) * mean_monthly / sd_monthly
    } else {
      NA_real_
    }
    cumulative <- if (n > 0) prod(1 + x) - 1 else NA_real_
    data.frame(
      CARDI_signal = unique(strategy_data$CARDI_signal),
      Threshold = unique(strategy_data$Threshold),
      Strategy = col,
      N_months = n,
      Mean_monthly = mean_monthly,
      Vol_monthly = sd_monthly,
      Sharpe_annualized = sharpe_annual,
      Cumulative_return = cumulative
    )
  }))
  row.names(out) <- NULL
  out
}

portfolio_metrics <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  mean_monthly <- if (n > 0) mean(x) else NA_real_
  sd_monthly <- if (n > 1) sd(x) else NA_real_
  data.frame(
    N_months = n,
    Mean_monthly = mean_monthly,
    Vol_monthly = sd_monthly,
    Sharpe_annualized = if (is.finite(sd_monthly) && sd_monthly > 0) {
      sqrt(12) * mean_monthly / sd_monthly
    } else {
      NA_real_
    },
    Cumulative_return = if (n > 0) prod(1 + x) - 1 else NA_real_
  )
}

make_oos_execution_signal <- function(strategies) {
  signal_name <- config$oos_execution_signal$primary_signal
  dat <- strategies[strategies$CARDI_signal == signal_name &
                      strategies$Threshold == "median", , drop = FALSE]
  dat <- dat[order(dat$Date), , drop = FALSE]
  dat <- dat[is.finite(dat$CARDI_lag) &
               is.finite(dat$Strategy_B_LC_minus_HC), , drop = FALSE]

  selected_indicator <- config$oos_execution_signal$selected_indicator
  selected_parameter <- config$oos_execution_signal$selected_parameter
  cardi <- dat$CARDI_lag
  strategy_b <- dat$Strategy_B_LC_minus_HC
  n_months <- nrow(dat)
  signal_value <- rep(NA_real_, n_months)
  selected_threshold <- rep(NA_real_, n_months)
  execute <- rep(0L, n_months)

  if (selected_indicator == "momentum_positive") {
    lag_k <- as.integer(sub("^lag=", "", selected_parameter))
    for (i in seq_len(n_months)) {
      if (i > lag_k) {
        signal_value[i] <- cardi[i] - cardi[i - lag_k]
        selected_threshold[i] <- 0
        execute[i] <- as.integer(signal_value[i] > 0)
      }
    }
  } else if (selected_indicator == "level_quantile_above") {
    q <- as.numeric(sub("^q=", "", selected_parameter))
    for (i in seq_len(n_months)) {
      signal_value[i] <- cardi[i]
      selected_threshold[i] <- as.numeric(
        quantile(cardi[seq_len(i)], probs = q, na.rm = TRUE,
                 names = FALSE, type = 7)
      )
      execute[i] <- as.integer(cardi[i] > selected_threshold[i])
    }
  } else if (selected_indicator == "zscore_above") {
    z_cutoff <- as.numeric(sub("^z>", "", selected_parameter))
    for (i in seq_len(n_months)) {
      mu <- mean(cardi[seq_len(i)], na.rm = TRUE)
      sigma <- stats::sd(cardi[seq_len(i)], na.rm = TRUE)
      selected_threshold[i] <- z_cutoff
      if (is.finite(sigma) && sigma > 0) {
        signal_value[i] <- (cardi[i] - mu) / sigma
        execute[i] <- as.integer(signal_value[i] > z_cutoff)
      }
    }
  } else {
    stop("Unsupported selected_indicator: ", selected_indicator)
  }

  oos_return <- ifelse(execute == 1, strategy_b, 0)

  out <- data.frame(
    Date = dat$Date,
    Month = dat$Month,
    CARDI_signal = signal_name,
    Indicator = selected_indicator,
    Parameter = selected_parameter,
    CARDI_lag = cardi,
    Signal_value = signal_value,
    Selected_threshold = selected_threshold,
    Execute_Strategy_B = execute,
    Strategy_B_LC_minus_HC = strategy_b,
    Strategy_F_CARDI_5P_OOS_execute_B = oos_return
  )
  out
}

make_oos_execution_threshold_grid <- function(strategies) {
  signal_name <- config$oos_execution_signal$primary_signal
  threshold_direction <- config$oos_execution_signal$threshold_direction
  dat <- strategies[strategies$CARDI_signal == signal_name &
                      strategies$Threshold == "median", , drop = FALSE]
  dat <- dat[order(dat$Date), , drop = FALSE]
  dat <- dat[is.finite(dat$CARDI_lag) &
               is.finite(dat$Strategy_B_LC_minus_HC), , drop = FALSE]
  do.call(rbind, lapply(config$oos_execution_signal$threshold_grid,
                        function(candidate_quantile) {
    candidate_return <- rep(NA_real_, nrow(dat))
    for (i in seq_len(nrow(dat))) {
      history <- dat[seq_len(i), , drop = FALSE]
      candidate_threshold <- as.numeric(
        quantile(history$CARDI_lag, probs = candidate_quantile,
                 na.rm = TRUE, names = FALSE, type = 7)
      )
      execute_i <- if (threshold_direction == "below") {
        dat$CARDI_lag[i] < candidate_threshold
      } else {
        dat$CARDI_lag[i] > candidate_threshold
      }
      candidate_return[i] <- ifelse(execute_i,
                                    dat$Strategy_B_LC_minus_HC[i],
                                    0)
    }
    metrics <- portfolio_metrics(candidate_return)
    data.frame(
      CARDI_signal = signal_name,
      Threshold_direction = threshold_direction,
      OOS_months = nrow(dat),
      Candidate_quantile = candidate_quantile,
      N_months = metrics$N_months,
      Mean_monthly = metrics$Mean_monthly,
      Vol_monthly = metrics$Vol_monthly,
      Sharpe_annualized = metrics$Sharpe_annualized,
      Cumulative_return = metrics$Cumulative_return,
      Active_share = mean(candidate_return != 0, na.rm = TRUE)
    )
  }))
}

rolling_mean_at <- function(x, i, window) {
  start <- max(1, i - window + 1)
  mean(x[start:i], na.rm = TRUE)
}

previous_month_key <- function(date_value) {
  current_month <- as.Date(paste0(format(date_value, "%Y-%m"), "-01"))
  format(seq(current_month, length = 2, by = "-1 month")[2], "%Y-%m")
}

month_end_signal <- function(signal_data) {
  signal_data$SignalMonth <- format(signal_data$date, "%Y-%m")
  idx <- tapply(seq_len(nrow(signal_data)), signal_data$SignalMonth, tail, 1)
  out <- signal_data[as.integer(idx), c("SignalMonth", "Signal_value"),
                     drop = FALSE]
  out[order(out$SignalMonth), , drop = FALSE]
}

make_oos_indicator_candidates <- function(strategies) {
  signal_name <- config$oos_execution_signal$primary_signal
  dat <- strategies[strategies$CARDI_signal == signal_name &
                      strategies$Threshold == "median", , drop = FALSE]
  dat <- dat[order(dat$Date), , drop = FALSE]
  dat <- dat[is.finite(dat$CARDI_lag) &
               is.finite(dat$Strategy_B_LC_minus_HC), , drop = FALSE]

  cardi <- dat$CARDI_lag
  strategy_b <- dat$Strategy_B_LC_minus_HC
  n_months <- nrow(dat)
  monthly_rows <- list()
  summary_rows <- list()
  idx <- 1

  add_candidate <- function(indicator, parameter, execute,
                            threshold_value = rep(NA_real_, n_months),
                            signal_value = rep(NA_real_, n_months)) {
    execute <- as.integer(is.finite(execute) & execute == 1)
    candidate_return <- ifelse(execute == 1, strategy_b, 0)
    metrics <- portfolio_metrics(candidate_return)
    monthly_rows[[idx]] <<- data.frame(
      Date = dat$Date,
      Month = dat$Month,
      CARDI_signal = signal_name,
      Indicator = indicator,
      Parameter = parameter,
      CARDI_lag = cardi,
      Signal_value = signal_value,
      Threshold_value = threshold_value,
      Execute_Strategy_B = execute,
      Strategy_B_LC_minus_HC = strategy_b,
      Candidate_return = candidate_return
    )
    summary_rows[[idx]] <<- data.frame(
      CARDI_signal = signal_name,
      Indicator = indicator,
      Parameter = parameter,
      N_months = metrics$N_months,
      Mean_monthly = metrics$Mean_monthly,
      Vol_monthly = metrics$Vol_monthly,
      Sharpe_annualized = metrics$Sharpe_annualized,
      Cumulative_return = metrics$Cumulative_return,
      Active_share = mean(execute == 1, na.rm = TRUE)
    )
    idx <<- idx + 1
  }

  for (q in config$oos_execution_signal$threshold_grid) {
    threshold <- rep(NA_real_, n_months)
    execute <- rep(0L, n_months)
    for (i in seq_len(n_months)) {
      threshold[i] <- as.numeric(quantile(cardi[seq_len(i)], probs = q,
                                          na.rm = TRUE, names = FALSE,
                                          type = 7))
      execute[i] <- as.integer(cardi[i] > threshold[i])
    }
    add_candidate("level_quantile_above", sprintf("q=%.2f", q), execute,
                  threshold, cardi)
  }

  for (z_cutoff in config$oos_execution_signal$zscore_grid) {
    z_value <- rep(NA_real_, n_months)
    execute <- rep(0L, n_months)
    for (i in seq_len(n_months)) {
      mu <- mean(cardi[seq_len(i)], na.rm = TRUE)
      sigma <- stats::sd(cardi[seq_len(i)], na.rm = TRUE)
      if (is.finite(sigma) && sigma > 0) {
        z_value[i] <- (cardi[i] - mu) / sigma
        execute[i] <- as.integer(z_value[i] > z_cutoff)
      }
    }
    add_candidate("zscore_above", sprintf("z>%.2f", z_cutoff), execute,
                  rep(z_cutoff, n_months), z_value)
  }

  for (lag_k in config$oos_execution_signal$momentum_lags) {
    momentum <- rep(NA_real_, n_months)
    execute <- rep(0L, n_months)
    for (i in seq_len(n_months)) {
      if (i > lag_k) {
        momentum[i] <- cardi[i] - cardi[i - lag_k]
        execute[i] <- as.integer(momentum[i] > 0)
      }
    }
    add_candidate("momentum_positive", paste0("lag=", lag_k), execute,
                  rep(0, n_months), momentum)
  }

  for (q in config$oos_execution_signal$level_momentum_quantiles) {
    for (lag_k in config$oos_execution_signal$level_momentum_lags) {
      threshold <- rep(NA_real_, n_months)
      momentum <- rep(NA_real_, n_months)
      execute <- rep(0L, n_months)
      for (i in seq_len(n_months)) {
        threshold[i] <- as.numeric(quantile(cardi[seq_len(i)], probs = q,
                                            na.rm = TRUE, names = FALSE,
                                            type = 7))
        if (i > lag_k) {
          momentum[i] <- cardi[i] - cardi[i - lag_k]
          execute[i] <- as.integer(cardi[i] > threshold[i] &&
                                     momentum[i] > 0)
        }
      }
      add_candidate("level_plus_momentum",
                    sprintf("q=%.2f;lag=%d", q, lag_k), execute,
                    threshold, momentum)
    }
  }

  for (short_window in config$oos_execution_signal$ma_short_windows) {
    for (long_window in config$oos_execution_signal$ma_long_windows) {
      if (short_window >= long_window) next
      ma_diff <- rep(NA_real_, n_months)
      execute <- rep(0L, n_months)
      for (i in seq_len(n_months)) {
        ma_short <- rolling_mean_at(cardi, i, short_window)
        ma_long <- rolling_mean_at(cardi, i, long_window)
        ma_diff[i] <- ma_short - ma_long
        execute[i] <- as.integer(ma_diff[i] > 0)
      }
      add_candidate("ma_crossover",
                    sprintf("short=%d;long=%d", short_window, long_window),
                    execute, rep(0, n_months), ma_diff)
    }
  }

  for (q in config$oos_execution_signal$acceleration_quantiles) {
    for (lag_k in config$oos_execution_signal$acceleration_lags) {
      threshold <- rep(NA_real_, n_months)
      acceleration <- rep(NA_real_, n_months)
      execute <- rep(0L, n_months)
      for (i in seq_len(n_months)) {
        threshold[i] <- as.numeric(quantile(cardi[seq_len(i)], probs = q,
                                            na.rm = TRUE, names = FALSE,
                                            type = 7))
        if (i > 2 * lag_k) {
          acceleration[i] <- (cardi[i] - cardi[i - lag_k]) -
            (cardi[i - lag_k] - cardi[i - 2 * lag_k])
          execute[i] <- as.integer(cardi[i] > threshold[i] &&
                                     acceleration[i] > 0)
        }
      }
      add_candidate("level_plus_acceleration",
                    sprintf("q=%.2f;lag=%d", q, lag_k), execute,
                    threshold, acceleration)
    }
  }

  monthly <- do.call(rbind, monthly_rows)
  summary <- do.call(rbind, summary_rows)
  summary$Eligible_active_share <- summary$Active_share >=
    config$oos_execution_signal$min_active_share
  summary <- summary[order(-summary$Sharpe_annualized,
                           -summary$Cumulative_return), , drop = FALSE]
  row.names(monthly) <- NULL
  row.names(summary) <- NULL
  list(monthly = monthly, summary = summary)
}

make_daily_oos_indicator_candidates <- function(cardi_daily, monthly_returns) {
  signal_name <- config$oos_execution_signal$primary_signal
  cardi <- cardi_daily[[signal_name]]
  dates <- cardi_daily$date
  n_daily <- length(cardi)

  monthly_base <- monthly_returns[, c("Date", "Month"), drop = FALSE]
  monthly_base$Strategy_B_LC_minus_HC <-
    monthly_returns$LowCarbonIntens_VW_Return -
    monthly_returns$HighCarbonIntens_VW_Return
  monthly_base$SignalMonth <- vapply(monthly_base$Date, previous_month_key,
                                     character(1))

  monthly_rows <- list()
  summary_rows <- list()
  idx <- 1

  add_daily_candidate <- function(indicator, parameter, daily_signal_value) {
    feature <- month_end_signal(data.frame(
      date = dates,
      Signal_value = daily_signal_value
    ))
    dat <- merge(monthly_base, feature, by = "SignalMonth", all = FALSE)
    dat <- dat[order(dat$Date), , drop = FALSE]
    dat <- dat[is.finite(dat$Signal_value) &
                 is.finite(dat$Strategy_B_LC_minus_HC), , drop = FALSE]
    execute <- as.integer(dat$Signal_value > 0)
    candidate_return <- ifelse(execute == 1,
                               dat$Strategy_B_LC_minus_HC,
                               0)
    metrics <- portfolio_metrics(candidate_return)
    monthly_rows[[idx]] <<- data.frame(
      Date = dat$Date,
      Month = dat$Month,
      CARDI_signal = signal_name,
      Indicator = indicator,
      Parameter = parameter,
      Signal_month = dat$SignalMonth,
      Signal_value = dat$Signal_value,
      Execute_Strategy_B = execute,
      Strategy_B_LC_minus_HC = dat$Strategy_B_LC_minus_HC,
      Candidate_return = candidate_return
    )
    summary_rows[[idx]] <<- data.frame(
      CARDI_signal = signal_name,
      Indicator = indicator,
      Parameter = parameter,
      OOS_start = min(dat$Date),
      OOS_end = max(dat$Date),
      N_months = metrics$N_months,
      Mean_monthly = metrics$Mean_monthly,
      Vol_monthly = metrics$Vol_monthly,
      Sharpe_annualized = metrics$Sharpe_annualized,
      Cumulative_return = metrics$Cumulative_return,
      Active_share = mean(execute == 1, na.rm = TRUE)
    )
    idx <<- idx + 1
  }

  add_monthly_candidate <- function(indicator, parameter, monthly_signal) {
    dat <- merge(monthly_base, monthly_signal, by.x = "SignalMonth",
                 by.y = "Month", all = FALSE)
    dat <- dat[order(dat$Date), , drop = FALSE]
    dat <- dat[is.finite(dat$Signal_value) &
                 is.finite(dat$Strategy_B_LC_minus_HC), , drop = FALSE]
    execute <- as.integer(dat$Signal_value > 0)
    candidate_return <- ifelse(execute == 1,
                               dat$Strategy_B_LC_minus_HC,
                               0)
    metrics <- portfolio_metrics(candidate_return)
    monthly_rows[[idx]] <<- data.frame(
      Date = dat$Date,
      Month = dat$Month,
      CARDI_signal = signal_name,
      Indicator = indicator,
      Parameter = parameter,
      Signal_month = dat$SignalMonth,
      Signal_value = dat$Signal_value,
      Execute_Strategy_B = execute,
      Strategy_B_LC_minus_HC = dat$Strategy_B_LC_minus_HC,
      Candidate_return = candidate_return
    )
    summary_rows[[idx]] <<- data.frame(
      CARDI_signal = signal_name,
      Indicator = indicator,
      Parameter = parameter,
      OOS_start = min(dat$Date),
      OOS_end = max(dat$Date),
      N_months = metrics$N_months,
      Mean_monthly = metrics$Mean_monthly,
      Vol_monthly = metrics$Vol_monthly,
      Sharpe_annualized = metrics$Sharpe_annualized,
      Cumulative_return = metrics$Cumulative_return,
      Active_share = mean(execute == 1, na.rm = TRUE)
    )
    idx <<- idx + 1
  }

  month_keys <- sort(unique(format(dates, "%Y-%m")))
  mean_percentile <- data.frame()
  for (month_key in month_keys) {
    month_start <- as.Date(paste0(month_key, "-01"))
    history <- cardi[dates < month_start]
    current <- cardi[format(dates, "%Y-%m") == month_key]
    if (length(history) > 20 && length(current) > 0) {
      percentile_rank <- vapply(
        current,
        function(value) mean(history <= value, na.rm = TRUE),
        numeric(1)
      )
      mean_percentile <- rbind(
        mean_percentile,
        data.frame(Month = month_key,
                   Mean_daily_percentile = mean(percentile_rank,
                                                na.rm = TRUE))
      )
    }
  }
  if (nrow(mean_percentile) > 1) {
    mean_percentile$Signal_value <- c(
      NA_real_,
      diff(mean_percentile$Mean_daily_percentile)
    )
    add_monthly_candidate(
      "daily_mean_percentile_momentum_positive",
      "lag=1",
      mean_percentile[, c("Month", "Signal_value"), drop = FALSE]
    )
    for (cutoff in c(0.45, 0.50, 0.55, 0.60)) {
      high_and_rising <- mean_percentile[, c("Month"), drop = FALSE]
      high_and_rising$Signal_value <- as.numeric(
        mean_percentile$Mean_daily_percentile > cutoff &
          mean_percentile$Signal_value > 0
      )
      add_monthly_candidate(
        "daily_mean_percentile_high_and_rising",
        sprintf("mean>%.2f;lag=1", cutoff),
        high_and_rising
      )
    }
  }

  for (lag_k in config$oos_execution_signal$daily_momentum_lags) {
    signal_value <- rep(NA_real_, n_daily)
    for (i in seq_len(n_daily)) {
      if (i > lag_k) signal_value[i] <- cardi[i] - cardi[i - lag_k]
    }
    add_daily_candidate("daily_momentum_positive",
                        paste0("lag=", lag_k), signal_value)
  }

  for (lag_k in config$oos_execution_signal$daily_momentum_lags) {
    signal_value <- rep(NA_real_, n_daily)
    for (i in seq_len(n_daily)) {
      if (i > lag_k) signal_value[i] <- cardi[i] / cardi[i - lag_k] - 1
    }
    add_daily_candidate("daily_pct_momentum_positive",
                        paste0("lag=", lag_k), signal_value)
  }

  for (short_window in config$oos_execution_signal$daily_ma_short_windows) {
    for (long_window in config$oos_execution_signal$daily_ma_long_windows) {
      if (short_window >= long_window) next
      signal_value <- rep(NA_real_, n_daily)
      for (i in seq_len(n_daily)) {
        signal_value[i] <- rolling_mean_at(cardi, i, short_window) -
          rolling_mean_at(cardi, i, long_window)
      }
      add_daily_candidate(
        "daily_ma_crossover",
        sprintf("short=%d;long=%d", short_window, long_window),
        signal_value
      )
    }
  }

  for (window in config$oos_execution_signal$daily_z_windows) {
    z_value <- rep(NA_real_, n_daily)
    for (i in seq_len(n_daily)) {
      start <- max(1, i - window + 1)
      history <- cardi[start:i]
      sigma <- stats::sd(history, na.rm = TRUE)
      if (is.finite(sigma) && sigma > 0) {
        z_value[i] <- (cardi[i] - mean(history, na.rm = TRUE)) / sigma
      }
    }
    for (cutoff in config$oos_execution_signal$daily_z_cutoffs) {
      add_daily_candidate("daily_zscore_above",
                          sprintf("window=%d;z>%.2f", window, cutoff),
                          z_value - cutoff)
    }
  }

  monthly <- do.call(rbind, monthly_rows)
  summary <- do.call(rbind, summary_rows)
  summary$Eligible_active_share <- summary$Active_share >=
    config$oos_execution_signal$min_active_share
  summary <- summary[order(-summary$Sharpe_annualized,
                           -summary$Cumulative_return), , drop = FALSE]
  row.names(monthly) <- NULL
  row.names(summary) <- NULL
  list(monthly = monthly, summary = summary)
}

select_daily_oos_execution_signal <- function(daily_candidates) {
  selected_indicator <- config$oos_execution_signal$selected_daily_indicator
  selected_parameter <- config$oos_execution_signal$selected_daily_parameter
  selected <- daily_candidates$monthly[
    daily_candidates$monthly$Indicator == selected_indicator &
      daily_candidates$monthly$Parameter == selected_parameter,
    ,
    drop = FALSE
  ]
  if (nrow(selected) == 0) {
    stop("Selected daily CARDI indicator not found: ",
         selected_indicator, " / ", selected_parameter)
  }
  names(selected)[names(selected) == "Candidate_return"] <-
    "Strategy_F_CARDI_5P_OOS_execute_B"
  selected
}

make_oos_execution_comparison <- function(oos_signal) {
  test <- oos_signal[is.finite(oos_signal$Strategy_F_CARDI_5P_OOS_execute_B), ,
                     drop = FALSE]
  candidate_metrics <- portfolio_metrics(
    test$Strategy_F_CARDI_5P_OOS_execute_B
  )
  benchmark_metrics <- portfolio_metrics(test$Strategy_B_LC_minus_HC)
  data.frame(
    Benchmark = "Strategy_B_LC_minus_HC",
    Candidate = "Strategy_F_CARDI_5P_OOS_execute_B",
    CARDI_signal = config$oos_execution_signal$primary_signal,
    OOS_start = min(test$Date),
    OOS_end = max(test$Date),
    OOS_months = nrow(test),
    Indicator = unique(test$Indicator),
    Parameter = unique(test$Parameter),
    Signal_frequency = if ("Signal_month" %in% names(test)) {
      "daily CARDI signal sampled at previous month end"
    } else {
      "monthly CARDI signal"
    },
    Min_active_share = config$oos_execution_signal$min_active_share,
    Benchmark_cumulative_return = benchmark_metrics$Cumulative_return,
    Candidate_cumulative_return = candidate_metrics$Cumulative_return,
    Difference_cumulative_return = candidate_metrics$Cumulative_return -
      benchmark_metrics$Cumulative_return,
    Benchmark_sharpe = benchmark_metrics$Sharpe_annualized,
    Candidate_sharpe = candidate_metrics$Sharpe_annualized,
    Difference_sharpe = candidate_metrics$Sharpe_annualized -
      benchmark_metrics$Sharpe_annualized,
    Benchmark_mean_monthly = benchmark_metrics$Mean_monthly,
    Candidate_mean_monthly = candidate_metrics$Mean_monthly,
    Benchmark_vol_monthly = benchmark_metrics$Vol_monthly,
    Candidate_vol_monthly = candidate_metrics$Vol_monthly,
    Candidate_active_share = mean(test$Execute_Strategy_B == 1,
                                  na.rm = TRUE),
    Note = "Out-of-sample rolling execution rule: selected indicator obeys the high/rising CARDI_5P -> execute Strategy B direction; returns are Strategy B when signal=1 and cash otherwise."
  )
}

newey_west_regression <- function(strategy_data, y_col, lag_months) {
  dat <- strategy_data[, c(y_col, "CARDI_lag"), drop = FALSE]
  dat <- dat[is.finite(dat[[y_col]]) & is.finite(dat$CARDI_lag), ,
             drop = FALSE]
  if (nrow(dat) < 10) {
    return(data.frame(
      CARDI_signal = unique(strategy_data$CARDI_signal),
      Threshold = unique(strategy_data$Threshold),
      Strategy = y_col,
      Term = c("(Intercept)", "CARDI_lag"),
      Estimate = NA_real_,
      Std_Error = NA_real_,
      t_value = NA_real_,
      p_value = NA_real_,
      N = nrow(dat),
      SE_type = "insufficient observations"
    ))
  }
  names(dat)[1] <- "Strategy_return"
  fit <- lm(Strategy_return ~ CARDI_lag, data = dat)
  term_names <- c("(Intercept)", "CARDI_lag")
  if (requireNamespace("sandwich", quietly = TRUE) &&
      requireNamespace("lmtest", quietly = TRUE)) {
    vc <- sandwich::NeweyWest(fit, lag = lag_months, prewhite = FALSE,
                              adjust = TRUE)
    test <- lmtest::coeftest(fit, vcov. = vc)
    data.frame(
      CARDI_signal = unique(strategy_data$CARDI_signal),
      Threshold = unique(strategy_data$Threshold),
      Strategy = y_col,
      Term = row.names(test),
      Estimate = test[, 1],
      Std_Error = test[, 2],
      t_value = test[, 3],
      p_value = test[, 4],
      N = nrow(dat),
      SE_type = paste0("Newey-West lag ", lag_months),
      row.names = NULL
    )
  } else {
    test <- summary(fit)$coefficients
    data.frame(
      CARDI_signal = unique(strategy_data$CARDI_signal),
      Threshold = unique(strategy_data$Threshold),
      Strategy = y_col,
      Term = term_names,
      Estimate = test[, 1],
      Std_Error = test[, 2],
      t_value = test[, 3],
      p_value = test[, 4],
      N = nrow(dat),
      SE_type = "ordinary least squares; install sandwich and lmtest for NW",
      row.names = NULL
    )
  }
}

combine_stock_panels <- function(...) {
  panels <- list(...)
  out <- panels[[1]]
  for (i in seq.int(2, length(panels))) {
    out <- merge(out, panels[[i]], by = "Date", all = FALSE)
  }
  duplicate_cols <- duplicated(names(out))
  if (any(duplicate_cols)) {
    out <- out[, !duplicate_cols, drop = FALSE]
  }
  out[order(out$Date), , drop = FALSE]
}

double_sort_ids <- function(ids, carbon_rank, low_prob = 0.30,
                            high_prob = 0.70) {
  carbon_rank$ID <- clean_stock_id(carbon_rank$ID)
  out <- merge(data.frame(ID = clean_stock_id(ids)), carbon_rank, by = "ID",
               all.x = TRUE)
  out <- out[is.finite(out$CarbonIntensity_Mean), , drop = FALSE]
  if (nrow(out) == 0) {
    return(list(Low = character(0), Medium = character(0), High = character(0)))
  }
  cuts <- quantile(out$CarbonIntensity_Mean,
                   probs = c(low_prob, high_prob),
                   na.rm = TRUE, names = FALSE)
  list(
    Low = out$ID[out$CarbonIntensity_Mean < cuts[1]],
    Medium = out$ID[out$CarbonIntensity_Mean >= cuts[1] &
                      out$CarbonIntensity_Mean <= cuts[2]],
    High = out$ID[out$CarbonIntensity_Mean > cuts[2]]
  )
}

safe_monthly_group_return <- function(price_panel, mktcap_panel, ids,
                                      return_name) {
  ids <- intersect(clean_stock_id(ids), names(price_panel)[-1])
  ids <- intersect(ids, names(mktcap_panel)[-1])
  if (length(ids) == 0) {
    months <- unique(format(price_panel$Date, "%Y-%m"))
    empty_monthly <- data.frame(
      Date = as.Date(paste0(months, "-01")),
      Month = months,
      value = NA_real_
    )[order(months), ]
    names(empty_monthly) <- c("Date", "Month", return_name)
    return(empty_monthly)
  }
  daily <- daily_value_weighted_returns(
    price_panel[, c("Date", ids), drop = FALSE],
    mktcap_panel[, c("Date", ids), drop = FALSE]
  )
  monthly_compound_returns(daily, return_name)
}

make_double_sort_returns <- function(price_panel, mktcap_panel, carbon_rank_file) {
  if (!file.exists(carbon_rank_file)) {
    warning("Carbon rank file not found; skipping double-sort portfolios: ",
            carbon_rank_file)
    return(NULL)
  }
  carbon_rank <- readRDS(carbon_rank_file)
  if (!all(c("ID", "CarbonIntensity_Mean") %in% names(carbon_rank))) {
    warning("Carbon rank file lacks ID and CarbonIntensity_Mean; skipping.")
    return(NULL)
  }

  avg_cap <- colMeans(mktcap_panel[, -1, drop = FALSE], na.rm = TRUE)
  avg_cap <- avg_cap[is.finite(avg_cap)]
  avg_cap <- sort(avg_cap)
  split_point <- floor(length(avg_cap) / 2)
  small_ids <- names(avg_cap)[seq_len(split_point)]
  large_ids <- names(avg_cap)[seq.int(split_point + 1, length(avg_cap))]

  small_carbon <- double_sort_ids(small_ids, carbon_rank)
  large_carbon <- double_sort_ids(large_ids, carbon_rank)
  groups <- list(
    Large_Low = large_carbon$Low,
    Small_Low = small_carbon$Low,
    Large_High = large_carbon$High,
    Small_High = small_carbon$High,
    Large_Medium = large_carbon$Medium,
    Small_Medium = small_carbon$Medium
  )

  monthly_groups <- lapply(names(groups), function(group_name) {
    safe_monthly_group_return(price_panel, mktcap_panel, groups[[group_name]],
                              group_name)
  })
  names(monthly_groups) <- names(groups)

  out <- Reduce(function(x, y) merge(x, y, by = c("Date", "Month"),
                                     all = TRUE),
                monthly_groups)
  out <- out[order(out$Date), , drop = FALSE]
  out$RC_Low_minus_High <- 0.5 * (out$Large_Low + out$Small_Low) -
    0.5 * (out$Large_High + out$Small_High)
  out
}

make_monthly_stock_panels <- function(price_panel, mktcap_panel) {
  price_cols <- names(price_panel)[-1]
  cap_cols <- names(mktcap_panel)[-1]
  stocks <- intersect(price_cols, cap_cols)
  prices <- price_panel[, c("Date", stocks), drop = FALSE]
  caps <- mktcap_panel[, c("Date", stocks), drop = FALSE]
  prices$Month <- format(prices$Date, "%Y-%m")
  caps$Month <- format(caps$Date, "%Y-%m")

  months <- sort(intersect(unique(prices$Month), unique(caps$Month)))
  monthly_returns <- data.frame(Month = months)
  month_end_caps <- data.frame(Month = months)

  for (stock in stocks) {
    returns <- rep(NA_real_, length(months))
    end_caps <- rep(NA_real_, length(months))
    for (i in seq_along(months)) {
      month_key <- months[i]
      p <- prices[prices$Month == month_key, c("Date", stock), drop = FALSE]
      p <- p[order(p$Date), , drop = FALSE]
      p_values <- p[[stock]]
      p_values <- p_values[is.finite(p_values) & p_values > 0]
      if (length(p_values) >= 2) {
        returns[i] <- tail(p_values, 1) / p_values[1] - 1
      }

      c <- caps[caps$Month == month_key, c("Date", stock), drop = FALSE]
      c <- c[order(c$Date), , drop = FALSE]
      c_values <- c[[stock]]
      c_values <- c_values[is.finite(c_values) & c_values > 0]
      if (length(c_values) > 0) {
        end_caps[i] <- tail(c_values, 1)
      }
    }
    monthly_returns[[stock]] <- returns
    month_end_caps[[stock]] <- end_caps
  }
  list(returns = monthly_returns, month_end_caps = month_end_caps)
}

weighted_group_return <- function(stock_ids, returns_row, lagged_cap_row) {
  ids <- intersect(stock_ids, names(returns_row))
  ids <- intersect(ids, names(lagged_cap_row))
  if (length(ids) == 0) return(NA_real_)
  returns <- as.numeric(returns_row[ids])
  caps <- as.numeric(lagged_cap_row[ids])
  ok <- is.finite(returns) & is.finite(caps) & caps > 0
  if (!any(ok)) return(NA_real_)
  weights <- caps[ok] / sum(caps[ok])
  sum(returns[ok] * weights)
}

make_dynamic_double_sort_returns <- function(price_panel, mktcap_panel,
                                             carbon_rank_file) {
  if (!file.exists(carbon_rank_file)) {
    stop("Carbon rank file not found: ", carbon_rank_file)
  }
  carbon_rank <- readRDS(carbon_rank_file)
  if (!all(c("ID", "CarbonIntensity_Mean") %in% names(carbon_rank))) {
    stop("Carbon rank file lacks ID and CarbonIntensity_Mean.")
  }
  carbon_rank$ID <- clean_stock_id(carbon_rank$ID)
  carbon_rank <- carbon_rank[is.finite(carbon_rank$CarbonIntensity_Mean), ,
                             drop = FALSE]

  panels <- make_monthly_stock_panels(price_panel, mktcap_panel)
  stock_returns <- panels$returns
  month_end_caps <- panels$month_end_caps
  months <- stock_returns$Month
  stocks <- intersect(names(stock_returns)[-1], names(month_end_caps)[-1])
  stocks <- intersect(stocks, carbon_rank$ID)

  out <- data.frame(
    Date = as.Date(paste0(months, "-01")),
    Month = months,
    Big_Low = NA_real_,
    Small_Low = NA_real_,
    Big_Medium = NA_real_,
    Small_Medium = NA_real_,
    Big_High = NA_real_,
    Small_High = NA_real_,
    RC_Low_minus_High = NA_real_,
    N_Big_Low = NA_integer_,
    N_Small_Low = NA_integer_,
    N_Big_Medium = NA_integer_,
    N_Small_Medium = NA_integer_,
    N_Big_High = NA_integer_,
    N_Small_High = NA_integer_
  )

  for (i in seq_along(months)) {
    if (i == 1) next
    lagged_caps <- month_end_caps[i - 1, stocks, drop = FALSE]
    current_returns <- stock_returns[i, stocks, drop = FALSE]
    caps <- as.numeric(lagged_caps[1, ])
    names(caps) <- stocks
    valid_stocks <- stocks[is.finite(caps) & caps > 0]
    valid_stocks <- valid_stocks[
      is.finite(as.numeric(current_returns[1, valid_stocks, drop = TRUE]))
    ]
    if (length(valid_stocks) < 6) next

    size_cutoff <- median(caps[valid_stocks], na.rm = TRUE)
    small_ids <- valid_stocks[caps[valid_stocks] <= size_cutoff]
    big_ids <- valid_stocks[caps[valid_stocks] > size_cutoff]

    small_carbon <- double_sort_ids(small_ids, carbon_rank)
    big_carbon <- double_sort_ids(big_ids, carbon_rank)
    groups <- list(
      Big_Low = big_carbon$Low,
      Small_Low = small_carbon$Low,
      Big_Medium = big_carbon$Medium,
      Small_Medium = small_carbon$Medium,
      Big_High = big_carbon$High,
      Small_High = small_carbon$High
    )

    for (group_name in names(groups)) {
      out[i, group_name] <- weighted_group_return(
        groups[[group_name]],
        current_returns,
        lagged_caps
      )
      out[i, paste0("N_", group_name)] <- length(groups[[group_name]])
    }
    out$RC_Low_minus_High[i] <- 0.5 * (out$Big_Low[i] + out$Small_Low[i]) -
      0.5 * (out$Big_High[i] + out$Small_High[i])
  }

  out[is.finite(out$RC_Low_minus_High), , drop = FALSE]
}

return_series_summary <- function(data, cols) {
  do.call(rbind, lapply(cols, function(col) {
    x <- data[[col]]
    x <- x[is.finite(x)]
    n <- length(x)
    mean_monthly <- if (n > 0) mean(x) else NA_real_
    sd_monthly <- if (n > 1) sd(x) else NA_real_
    t_stat <- if (is.finite(sd_monthly) && sd_monthly > 0) {
      mean_monthly / (sd_monthly / sqrt(n))
    } else {
      NA_real_
    }
    data.frame(
      Series = col,
      N_months = n,
      Mean_monthly = mean_monthly,
      Vol_monthly = sd_monthly,
      Sharpe_annualized = if (is.finite(sd_monthly) && sd_monthly > 0) {
        sqrt(12) * mean_monthly / sd_monthly
      } else {
        NA_real_
      },
      t_stat_mean = t_stat,
      Cumulative_return = if (n > 0) prod(1 + x) - 1 else NA_real_
    )
  }))
}

strategy_label <- function(x) {
  labels <- c(
    Strategy_A_HC_minus_LC = "Always long HC / short LC",
    Strategy_B_LC_minus_HC = "Always long LC / short HC",
    Strategy_C_CARDI_switch = "CARDI switching",
    Strategy_D_conditional_HC_minus_LC = "Conditional HC-LC only",
    Strategy_E_conditional_LC_minus_HC = "Conditional LC-HC only",
    Strategy_F_CARDI_5P_OOS_execute_B = "CARDI_5P OOS execute Strategy B"
  )
  out <- labels[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

make_cumulative_long <- function(data, strategy_cols) {
  out <- do.call(rbind, lapply(strategy_cols, function(col) {
    returns <- data[[col]]
    cumulative <- rep(NA_real_, length(returns))
    ok_seen <- FALSE
    wealth <- 1
    for (i in seq_along(returns)) {
      if (is.finite(returns[i])) {
        ok_seen <- TRUE
        wealth <- wealth * (1 + returns[i])
      }
      if (ok_seen) {
        cumulative[i] <- wealth - 1
      }
    }
    data.frame(
      Date = data$Date,
      Month = data$Month,
      Strategy = strategy_label(col),
      Cumulative_Return = cumulative
    )
  }))
  row.names(out) <- NULL
  out
}

cumulative_return_vector <- function(returns) {
  cumulative <- rep(NA_real_, length(returns))
  wealth <- 1
  for (i in seq_along(returns)) {
    if (is.finite(returns[i])) {
      wealth <- wealth * (1 + returns[i])
      cumulative[i] <- wealth - 1
    }
  }
  cumulative
}

cumulative_wealth_vector <- function(returns) {
  wealth_vector <- rep(NA_real_, length(returns))
  wealth <- 1
  for (i in seq_along(returns)) {
    wealth_vector[i] <- wealth
    if (is.finite(returns[i])) {
      wealth <- wealth * (1 + returns[i])
    }
  }
  wealth_vector
}

cardi_plot_theme <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "transparent",
                                             colour = NA),
    plot.background = ggplot2::element_rect(fill = "transparent",
                                            colour = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent",
                                                  colour = NA),
    legend.background = ggplot2::element_rect(fill = "transparent",
                                              colour = NA),
    legend.key = ggplot2::element_rect(fill = "transparent"),
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text.x = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 14),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.position = "bottom"
  )
}

save_line_plot <- function(plot_data, file_stub, y_label,
                           color_col = "Strategy",
                           group_col = "Strategy") {
  ensure_dir(config$figure_dir)
  png_file <- file.path(config$figure_dir, paste0(file_stub, ".png"))
  pdf_file <- file.path(config$figure_dir, paste0(file_stub, ".pdf"))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot_data$Line_alpha <- ifelse(
      plot_data[[color_col]] == "Other CARDI-based strategies",
      0.28,
      1
    )
    color_levels <- if (is.factor(plot_data[[color_col]])) {
      levels(plot_data[[color_col]])
    } else {
      unique(as.character(plot_data[[color_col]]))
    }
    color_values <- setNames(
      c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
        "#A6761D", "#666666")[seq_along(color_levels)],
      color_levels
    )
    gg <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = Date, y = Cumulative_Wealth,
                   color = .data[[color_col]],
                   group = .data[[group_col]])
    ) +
      ggplot2::geom_hline(yintercept = 1, color = "grey70", linewidth = 0.5) +
      ggplot2::geom_line(ggplot2::aes(alpha = Line_alpha),
                         linewidth = 1.2, na.rm = TRUE) +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_color_manual(values = color_values, drop = TRUE) +
      ggplot2::labs(x = "Time", y = y_label, color = NULL) +
      cardi_plot_theme()
    ggplot2::ggsave(png_file, gg, width = 9, height = 5.5, dpi = 320,
                    bg = "transparent")
    ggplot2::ggsave(pdf_file, gg, width = 9, height = 5.5,
                    bg = "transparent")
  } else {
    wide <- split(plot_data, plot_data[[color_col]])
    colors <- grDevices::hcl.colors(length(wide), palette = "Dark 3")
    draw_base <- function(device_file, device_fun) {
      device_fun(device_file, width = 9, height = 5.5)
      y_range <- range(plot_data$Cumulative_Wealth, na.rm = TRUE)
      plot(
        plot_data$Date, plot_data$Cumulative_Wealth,
        type = "n", xlab = "Time", ylab = y_label,
        main = "", ylim = y_range
      )
      abline(h = 1, col = "grey70")
      i <- 1
      for (nm in names(wide)) {
        lines(wide[[nm]]$Date, wide[[nm]]$Cumulative_Wealth,
              col = colors[i], lwd = 2)
        i <- i + 1
      }
      legend("topleft", legend = names(wide), col = colors, lwd = 2,
             bty = "n", cex = 0.8)
      grDevices::dev.off()
    }
    draw_base(png_file, grDevices::png)
    draw_base(pdf_file, grDevices::pdf)
  }
  c(png = png_file, pdf = pdf_file)
}

format_strategy_parameter <- function(parameter) {
  gsub(";", "; ", parameter, fixed = TRUE)
}

strategy_name_from_parts <- function(indicator, parameter) {
  paste(indicator, format_strategy_parameter(parameter), sep = " | ")
}

selected_strategy_names <- function(oos_execution_signal) {
  selected_name <- strategy_name_from_parts(
    unique(oos_execution_signal$Indicator)[1],
    unique(oos_execution_signal$Parameter)[1]
  )
  c(
    selected_name,
    "daily_ma_crossover | short=10; long=63",
    "daily_ma_crossover | short=10; long=42",
    "Baseline LC-HC",
    "Baseline HC-LC"
  )
}

find_daily_candidate_returns <- function(daily_candidates, indicator,
                                         parameter) {
  if (is.null(daily_candidates)) {
    stop("Daily CARDI candidate results are missing.")
  }
  dat <- daily_candidates$monthly[
    daily_candidates$monthly$Indicator == indicator &
      daily_candidates$monthly$Parameter == parameter,
    ,
    drop = FALSE
  ]
  if (nrow(dat) == 0) {
    stop("Missing selected daily CARDI strategy: ",
         strategy_name_from_parts(indicator, parameter))
  }
  dat
}

selected_strategy_return_series <- function(monthly_returns,
                                            oos_execution_signal,
                                            daily_candidates) {
  if (is.null(oos_execution_signal)) {
    stop("Selected CARDI OOS strategy results are missing.")
  }
  selected_levels <- selected_strategy_names(oos_execution_signal)
  best_name <- selected_levels[1]
  best <- oos_execution_signal[
    is.finite(oos_execution_signal$Strategy_F_CARDI_5P_OOS_execute_B),
    ,
    drop = FALSE
  ]
  ma_10_63 <- find_daily_candidate_returns(
    daily_candidates,
    "daily_ma_crossover",
    "short=10;long=63"
  )
  ma_10_42 <- find_daily_candidate_returns(
    daily_candidates,
    "daily_ma_crossover",
    "short=10;long=42"
  )

  out <- rbind(
    data.frame(
      Date = best$Date,
      Strategy = best_name,
      Strategy_type = "Selected CARDI strategy",
      Return = best$Strategy_F_CARDI_5P_OOS_execute_B,
      Active = best$Execute_Strategy_B
    ),
    data.frame(
      Date = ma_10_63$Date,
      Strategy = "daily_ma_crossover | short=10; long=63",
      Strategy_type = "Daily CARDI strategy",
      Return = ma_10_63$Candidate_return,
      Active = ma_10_63$Execute_Strategy_B
    ),
    data.frame(
      Date = ma_10_42$Date,
      Strategy = "daily_ma_crossover | short=10; long=42",
      Strategy_type = "Daily CARDI strategy",
      Return = ma_10_42$Candidate_return,
      Active = ma_10_42$Execute_Strategy_B
    ),
    data.frame(
      Date = monthly_returns$Date,
      Strategy = "Baseline LC-HC",
      Strategy_type = "Baseline",
      Return = monthly_returns$Strategy_B_LC_minus_HC,
      Active = 1
    ),
    data.frame(
      Date = monthly_returns$Date,
      Strategy = "Baseline HC-LC",
      Strategy_type = "Baseline",
      Return = -monthly_returns$Strategy_B_LC_minus_HC,
      Active = 1
    )
  )
  out <- out[is.finite(out$Return), , drop = FALSE]
  date_ranges <- aggregate(Date ~ Strategy, out, function(x) {
    c(start = min(x), end = max(x))
  })
  common_start <- max(as.Date(date_ranges$Date[, "start"],
                              origin = "1970-01-01"))
  common_end <- min(as.Date(date_ranges$Date[, "end"],
                            origin = "1970-01-01"))
  out <- out[out$Date >= common_start & out$Date <= common_end, ,
             drop = FALSE]
  common_dates <- Reduce(
    intersect,
    lapply(split(out$Date, out$Strategy), function(x) as.character(x))
  )
  out <- out[as.character(out$Date) %in% common_dates, , drop = FALSE]
  out$Strategy <- factor(out$Strategy, levels = selected_levels)
  out[order(out$Strategy, out$Date), , drop = FALSE]
}

make_wealth_plot_data <- function(selected_returns) {
  do.call(rbind, lapply(split(selected_returns, selected_returns$Strategy),
                        function(dat) {
    dat <- dat[order(dat$Date), , drop = FALSE]
    data.frame(
      Date = dat$Date,
      Strategy = unique(dat$Strategy),
      Line_ID = unique(as.character(dat$Strategy)),
      Cumulative_Wealth = cumulative_wealth_vector(dat$Return)
    )
  }))
}
# 
# plot_strategy_figures <- function(strategies, oos_execution_signal = NULL,
#                                   daily_candidates = NULL,
#                                   monthly_candidates = NULL,
#                                   monthly_returns = NULL) {
#   selected_returns <- selected_strategy_return_series(
#     monthly_returns,
#     oos_execution_signal,
#     daily_candidates
#   )
#   plot_data <- make_wealth_plot_data(selected_returns)
#   plot_data$Strategy <- factor(
#     plot_data$Strategy,
#     levels = selected_strategy_names(oos_execution_signal)
#   )
#   baseline_plot <- plot_data[
#     plot_data$Strategy %in% c(selected_strategy_names(oos_execution_signal)[1],
#                               "Baseline LC-HC"),
#     ,
#     drop = FALSE
#   ]
#   baseline_plot$Strategy <- factor(
#     baseline_plot$Strategy,
#     levels = c("Baseline LC-HC",
#                selected_strategy_names(oos_execution_signal)[1])
#   )
#   invisible(save_line_plot(
#     baseline_plot,
#     "baseline_lc_hc_vs_best_cardi_p5_oos",
#     "Cumulative wealth",
#     group_col = "Line_ID"
#   ))
#   invisible(save_line_plot(
#     plot_data,
#     "selected_cardi_strategies_cumulative_wealth",
#     "Cumulative wealth",
#     group_col = "Line_ID"
#   ))
# }

plot_strategy_figures <- function(strategies, oos_execution_signal = NULL,
                                  daily_candidates = NULL,
                                  monthly_candidates = NULL,
                                  monthly_returns = NULL) {
  selected_returns <- selected_strategy_return_series(
    monthly_returns,
    oos_execution_signal,
    daily_candidates
  )
  plot_data <- make_wealth_plot_data(selected_returns)
  plot_data$Strategy <- factor(
    plot_data$Strategy,
    levels = selected_strategy_names(oos_execution_signal)
  )
  
  # Update baseline plot to include Baseline LC-HC, Baseline HC-LC, and Best CARDI
  baseline_plot <- plot_data[
    plot_data$Strategy %in% c(selected_strategy_names(oos_execution_signal)[1],
                              "Baseline LC-HC",
                              "Baseline HC-LC"),
    ,
    drop = FALSE
  ]
  baseline_plot$Strategy <- factor(
    baseline_plot$Strategy,
    levels = c("Baseline HC-LC", "Baseline LC-HC",
               selected_strategy_names(oos_execution_signal)[1])
  )
  invisible(save_line_plot(
    baseline_plot,
    "baseline_lc_hc_vs_best_cardi_p5_oos",
    "Cumulative wealth",
    group_col = "Line_ID"
  ))
}

performance_table_row <- function(strategy_name, strategy_type, returns,
                                  dates = NULL, active_indicator = NULL) {
  ok <- is.finite(returns)
  returns <- returns[ok]
  if (!is.null(dates)) dates <- dates[ok]
  n <- length(returns)
  avg <- if (n > 0) mean(returns) else NA_real_
  sd_monthly <- if (n > 1) stats::sd(returns) else NA_real_
  t_stat <- if (is.finite(sd_monthly) && sd_monthly > 0) {
    avg / (sd_monthly / sqrt(n))
  } else {
    NA_real_
  }
  sharpe <- if (is.finite(sd_monthly) && sd_monthly > 0) {
    sqrt(12) * avg / sd_monthly
  } else {
    NA_real_
  }
  data.frame(
    Strategy = strategy_name,
    Strategy_type = strategy_type,
    Start = if (!is.null(dates) && length(dates) > 0) min(dates) else NA,
    End = if (!is.null(dates) && length(dates) > 0) max(dates) else NA,
    N_months = n,
    Average_monthly_return = avg,
    T_statistic = t_stat,
    Sharpe_ratio = sharpe,
    SD_monthly_return = sd_monthly,
    Cumulative_return = if (n > 0) prod(1 + returns) - 1 else NA_real_,
    Active_share = if (!is.null(active_indicator)) {
      mean(active_indicator[ok] == 1, na.rm = TRUE)
    } else {
      mean(returns != 0, na.rm = TRUE)
    }
  )
}

build_main_performance_table <- function(monthly_returns,
                                         oos_execution_signal,
                                         daily_candidates,
                                         monthly_candidates) {
  selected_returns <- selected_strategy_return_series(
    monthly_returns,
    oos_execution_signal,
    daily_candidates
  )
  split_returns <- split(selected_returns, selected_returns$Strategy)
  rows <- lapply(split_returns, function(dat) {
    performance_table_row(
      unique(as.character(dat$Strategy)),
      unique(dat$Strategy_type),
      dat$Return,
      dat$Date,
      dat$Active
    )
  })
  out <- do.call(rbind, rows)
  selected_order <- selected_strategy_names(oos_execution_signal)
  out$Strategy <- factor(out$Strategy, levels = selected_order)
  out <- out[order(out$Strategy), , drop = FALSE]
  out$Strategy <- as.character(out$Strategy)
  row.names(out) <- NULL
  out
}

required_strategy_result_names <- c(
  "monthly_returns",
  "strategies",
  "oos_execution_signal",
  "daily_oos_indicator_candidates",
  "oos_indicator_candidates",
  "portfolio_group_schema"
)

has_required_reporting_results <- function(results) {
  if (!is.list(results)) return(FALSE)
  if (!all(required_strategy_result_names %in% names(results))) return(FALSE)
  if (!identical(results$portfolio_group_schema,
                 config$portfolio_group_schema)) {
    return(FALSE)
  }
  if (!all(c("Date", "HighCarbonIntens_VW_Return",
             "MedCarbonIntens_VW_Return",
             "LowCarbonIntens_VW_Return",
             "Strategy_B_LC_minus_HC") %in%
           names(results$monthly_returns))) {
    return(FALSE)
  }
  if (!all(c("Date", "Indicator", "Parameter",
             "Strategy_B_LC_minus_HC",
             "Strategy_F_CARDI_5P_OOS_execute_B") %in%
           names(results$oos_execution_signal))) {
    return(FALSE)
  }
  for (candidate_name in c("daily_oos_indicator_candidates",
                           "oos_indicator_candidates")) {
    candidate_obj <- results[[candidate_name]]
    if (!is.list(candidate_obj) ||
        !all(c("summary", "monthly") %in% names(candidate_obj))) {
      return(FALSE)
    }
    if (!"Eligible_active_share" %in% names(candidate_obj$summary)) {
      return(FALSE)
    }
    if (!all(c("Date", "Indicator", "Parameter", "Candidate_return",
               "Execute_Strategy_B") %in% names(candidate_obj$monthly))) {
      return(FALSE)
    }
  }
  TRUE
}

load_strategy_results_for_reporting <- function(path) {
  if (!file.exists(path)) return(NULL)
  message("Loading cached strategy results: ", path)
  results <- readRDS(path)
  if (!has_required_reporting_results(results)) {
    message("Cached strategy results are incomplete; rebuilding portfolios.")
    return(NULL)
  }
  results
}

read_existing_csv <- function(file_name) {
  path <- file.path(config$output_dir, file_name)
  if (!file.exists(path)) return(NULL)
  read.csv(path, check.names = FALSE)
}

coerce_date_columns <- function(data) {
  for (date_col in intersect(names(data), c("Date", "OOS_start", "OOS_end"))) {
    data[[date_col]] <- parse_date(data[[date_col]])
  }
  data
}

load_legacy_reporting_results_from_csv <- function() {
  files <- c(
    "dynamic_double_sort_portfolios_monthly_returns.csv",
    "cardi_strategy_monthly_returns.csv",
    "cardi_5p_oos_execution_signal_monthly_returns.csv",
    "cardi_5p_daily_oos_indicator_candidate_summary.csv",
    "cardi_5p_daily_oos_indicator_candidate_monthly_returns.csv",
    "cardi_5p_oos_indicator_candidate_summary.csv",
    "cardi_5p_oos_indicator_candidate_monthly_returns.csv"
  )
  paths <- file.path(config$output_dir, files)
  if (!all(file.exists(paths))) return(NULL)

  message("Loading existing local strategy CSV results for reporting.")
  dynamic_returns <- coerce_date_columns(read_existing_csv(files[1]))
  strategies <- coerce_date_columns(read_existing_csv(files[2]))
  oos_execution_signal <- coerce_date_columns(read_existing_csv(files[3]))
  daily_summary <- coerce_date_columns(read_existing_csv(files[4]))
  daily_monthly <- coerce_date_columns(read_existing_csv(files[5]))
  monthly_summary <- coerce_date_columns(read_existing_csv(files[6]))
  monthly_monthly <- coerce_date_columns(read_existing_csv(files[7]))

  monthly_returns <- dynamic_returns[, c("Date", "Month"), drop = FALSE]
  monthly_returns$HighCarbonIntens_VW_Return <- 0.5 * (
    dynamic_returns$Big_High + dynamic_returns$Small_High
  )
  monthly_returns$MedCarbonIntens_VW_Return <- 0.5 * (
    dynamic_returns$Big_Medium + dynamic_returns$Small_Medium
  )
  monthly_returns$LowCarbonIntens_VW_Return <- 0.5 * (
    dynamic_returns$Big_Low + dynamic_returns$Small_Low
  )
  monthly_returns$Strategy_B_LC_minus_HC <-
    dynamic_returns$RC_Low_minus_High

  results <- list(
    monthly_returns = monthly_returns,
    strategies = strategies,
    oos_execution_signal = oos_execution_signal,
    daily_oos_indicator_candidates = list(
      summary = daily_summary,
      monthly = daily_monthly
    ),
    oos_indicator_candidates = list(
      summary = monthly_summary,
      monthly = monthly_monthly
    ),
    dynamic_double_sort_returns = dynamic_returns,
    portfolio_group_schema = config$portfolio_group_schema,
    loaded_from = "legacy_csv_outputs",
    created_at = Sys.time()
  )
  if (!has_required_reporting_results(results)) {
    message("Existing local CSV results are incomplete; rebuilding portfolios.")
    return(NULL)
  }
  results
}

construct_strategy_results <- function() {
  message("Loading high-, medium-, and low-carbon portfolio inputs...")
  hc <- read_group_returns("HighCarbonIntens")
  mc <- read_group_returns("MedCarbonIntens")
  lc <- read_group_returns("LowCarbonIntens")

  monthly_returns <- Reduce(
    function(x, y) merge(x, y, by = c("Date", "Month"), all = FALSE),
    list(hc$monthly, mc$monthly, lc$monthly)
  )
  monthly_returns <- monthly_returns[order(monthly_returns$Date), ,
                                     drop = FALSE]
  aggregate_hc_mc_lc_monthly_returns <- monthly_returns

  signal_names <- unique(c(config$signal, "CARDI_1P", "CARDI_5P",
                           "CARDI_10P"))
  message("Loading CARDI signals...")
  cardi_monthly <- load_cardi_monthly(config$cardi_file, signal_names)
  cardi_daily <- load_cardi_daily(
    config$cardi_file,
    config$oos_execution_signal$primary_signal
  )

  message("Constructing size-by-carbon double-sort portfolios...")
  universe_prices <- combine_stock_panels(hc$prices, mc$prices, lc$prices)
  universe_mktcap <- combine_stock_panels(hc$mktcap, mc$mktcap, lc$mktcap)
  double_sort_returns <- make_double_sort_returns(
    universe_prices,
    universe_mktcap,
    config$carbon_rank_file
  )
  dynamic_double_sort_returns <- make_dynamic_double_sort_returns(
    universe_prices,
    universe_mktcap,
    config$carbon_rank_file
  )
  monthly_returns <- dynamic_double_sort_returns[, c("Date", "Month"),
                                                 drop = FALSE]
  monthly_returns$HighCarbonIntens_VW_Return <- 0.5 * (
    dynamic_double_sort_returns$Big_High +
      dynamic_double_sort_returns$Small_High
  )
  monthly_returns$MedCarbonIntens_VW_Return <- 0.5 * (
    dynamic_double_sort_returns$Big_Medium +
      dynamic_double_sort_returns$Small_Medium
  )
  monthly_returns$LowCarbonIntens_VW_Return <- 0.5 * (
    dynamic_double_sort_returns$Big_Low +
      dynamic_double_sort_returns$Small_Low
  )
  monthly_returns$Strategy_B_LC_minus_HC <-
    dynamic_double_sort_returns$RC_Low_minus_High

  strategy_sets <- list()
  for (signal in signal_names) {
    for (threshold_name in names(config$threshold_probs)) {
      strategy_sets[[paste(signal, threshold_name, sep = "_")]] <-
        make_strategy_returns(
          monthly_returns = monthly_returns,
          cardi_monthly = cardi_monthly,
          signal = signal,
          prob = config$threshold_probs[[threshold_name]],
          threshold_label = threshold_name
        )
    }
  }

  strategies <- do.call(rbind, strategy_sets)
  row.names(strategies) <- NULL

  summary_tables <- do.call(rbind, lapply(strategy_sets, performance_summary))
  row.names(summary_tables) <- NULL

  monthly_oos_execution_signal <- make_oos_execution_signal(strategies)
  oos_execution_threshold_grid <- make_oos_execution_threshold_grid(strategies)
  oos_indicator_candidates <- make_oos_indicator_candidates(strategies)
  daily_oos_indicator_candidates <- make_daily_oos_indicator_candidates(
    cardi_daily,
    monthly_returns
  )
  oos_execution_signal <- select_daily_oos_execution_signal(
    daily_oos_indicator_candidates
  )
  oos_execution_comparison <- make_oos_execution_comparison(
    oos_execution_signal
  )

  list(
    monthly_returns = monthly_returns,
    strategies = strategies,
    strategy_sets = strategy_sets,
    summary_tables = summary_tables,
    monthly_oos_execution_signal = monthly_oos_execution_signal,
    oos_execution_threshold_grid = oos_execution_threshold_grid,
    oos_indicator_candidates = oos_indicator_candidates,
    daily_oos_indicator_candidates = daily_oos_indicator_candidates,
    oos_execution_signal = oos_execution_signal,
    oos_execution_comparison = oos_execution_comparison,
    double_sort_returns = double_sort_returns,
    dynamic_double_sort_returns = dynamic_double_sort_returns,
    aggregate_hc_mc_lc_monthly_returns = aggregate_hc_mc_lc_monthly_returns,
    portfolio_group_schema = config$portfolio_group_schema,
    created_at = Sys.time()
  )
}

save_strategy_results_for_reporting <- function(results, path) {
  message("Saving strategy results for reporting: ", path)
  saveRDS(results, path)
}

load_monthly_fama_factor_data <- function(path) {
  if (!file.exists(path)) {
    stop("Missing monthly Fama factor file: ", path)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package readxl is required to read: ", path)
  }
  factors <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  if (!"Date" %in% names(factors)) {
    stop("Monthly Fama factor file must include Date.")
  }
  factors$Date <- parse_date(factors$Date)
  if ("FrequencyID" %in% names(factors)) {
    factors$Month <- as.character(factors$FrequencyID)
  } else {
    factors$Month <- format(factors$Date, "%Y-%m")
  }
  numeric_cols <- intersect(
    c("IndexRiskFreeRate", "MarketPremium", "SMB2", "HML2", "RMW2", "CMA2"),
    names(factors)
  )
  for (col in numeric_cols) {
    factors[[col]] <- suppressWarnings(as.numeric(factors[[col]]))
  }
  factors
}

build_portfolio_premium_data <- function(results) {
  monthly_returns <- results$monthly_returns
  required_cols <- c("Date", "Month", "HighCarbonIntens_VW_Return",
                     "MedCarbonIntens_VW_Return",
                     "LowCarbonIntens_VW_Return",
                     "Strategy_B_LC_minus_HC")
  missing_cols <- setdiff(required_cols, names(monthly_returns))
  if (length(missing_cols) > 0) {
    stop("Monthly portfolio returns are missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  factors <- load_monthly_fama_factor_data(config$fama_monthly_file)
  if (!"IndexRiskFreeRate" %in% names(factors)) {
    stop("Monthly Fama factor file must include IndexRiskFreeRate.")
  }
  rf <- factors[, c("Month", "Date", "IndexRiskFreeRate"), drop = FALSE]
  names(rf)[names(rf) == "Date"] <- "FamaFactorDate"

  merged <- merge(monthly_returns[, required_cols, drop = FALSE],
                  rf, by = "Month", all.x = TRUE)
  merged <- merged[order(merged$Date), , drop = FALSE]

  out <- data.frame(
    Date = merged$Date,
    Month = merged$Month,
    FamaFactorDate = merged$FamaFactorDate,
    HC_Return = merged$HighCarbonIntens_VW_Return,
    MC_Return = merged$MedCarbonIntens_VW_Return,
    LC_Return = merged$LowCarbonIntens_VW_Return,
    LC_HC_Return = merged$Strategy_B_LC_minus_HC,
    IndexRiskFreeRate = merged$IndexRiskFreeRate
  )

  # Portfolio risk premiums subtract the monthly index risk-free return from
  # each long-only carbon portfolio. The long-short LC-HC premium is the
  # difference between LC and HC premiums, so the risk-free leg cancels out.
  out$HC_Premium <- out$HC_Return - out$IndexRiskFreeRate
  out$MC_Premium <- out$MC_Return - out$IndexRiskFreeRate
  out$LC_Premium <- out$LC_Return - out$IndexRiskFreeRate
  out$LC_HC_Premium <- out$LC_Premium - out$HC_Premium
  out
}

save_portfolio_premium_data <- function(results) {
  message("Saving monthly portfolio risk-premium dataset...")
  premiums <- build_portfolio_premium_data(results)
  write.csv(premiums, config$portfolio_premium_csv, row.names = FALSE)
  saveRDS(premiums, config$portfolio_premium_rds)
  premiums
}

build_reporting_outputs <- function(results) {
  main_performance_table <- build_main_performance_table(
    results$monthly_returns,
    results$oos_execution_signal,
    results$daily_oos_indicator_candidates,
    results$oos_indicator_candidates
  )
  selected_strategies <- config$strategy_performance$selected_strategies
  main_performance_table <- main_performance_table[
    main_performance_table$Strategy %in% selected_strategies,
    ,
    drop = FALSE
  ]
  main_performance_table$Strategy <- factor(
    main_performance_table$Strategy,
    levels = selected_strategies
  )
  main_performance_table <- main_performance_table[
    order(main_performance_table$Strategy),
    ,
    drop = FALSE
  ]
  main_performance_table$Strategy <- as.character(main_performance_table$Strategy)

  write.csv(main_performance_table,
            file.path(config$output_dir,
                      "cardi_strategy_main_performance_comparison.csv"),
            row.names = FALSE)

  message("Saving portfolio performance figures...")
  plot_strategy_figures(results$strategies, results$oos_execution_signal,
                        results$daily_oos_indicator_candidates,
                        results$oos_indicator_candidates,
                        results$monthly_returns)
  invisible(main_performance_table)
}

save_extra_strategy_outputs <- function(results) {
  extra_dir <- file.path(config$output_dir, "Extra_Strategy_Results")
  ensure_dir(extra_dir)

  write.csv(results$aggregate_hc_mc_lc_monthly_returns,
            file.path(extra_dir, "aggregate_hc_mc_lc_monthly_returns.csv"),
            row.names = FALSE)
  if (!is.null(results$double_sort_returns)) {
    write.csv(results$double_sort_returns,
              file.path(extra_dir, "double_sort_portfolios_monthly_returns.csv"),
              row.names = FALSE)
  }
  write.csv(results$dynamic_double_sort_returns,
            file.path(extra_dir, "dynamic_double_sort_portfolios_monthly_returns.csv"),
            row.names = FALSE)
  write.csv(results$strategies,
            file.path(extra_dir, "cardi_strategy_monthly_returns.csv"),
            row.names = FALSE)
  write.csv(results$summary_tables,
            file.path(extra_dir, "cardi_strategy_performance_summary.csv"),
            row.names = FALSE)
  write.csv(results$monthly_oos_execution_signal,
            file.path(extra_dir, "cardi_5p_monthly_oos_execution_signal_monthly_returns.csv"),
            row.names = FALSE)
  write.csv(results$oos_execution_threshold_grid,
            file.path(extra_dir, "cardi_5p_oos_execution_threshold_grid.csv"),
            row.names = FALSE)
  write.csv(results$oos_execution_comparison,
            file.path(extra_dir, "cardi_5p_oos_execution_comparison.csv"),
            row.names = FALSE)
  write.csv(results$oos_indicator_candidates$summary,
            file.path(extra_dir, "cardi_5p_oos_indicator_candidate_summary.csv"),
            row.names = FALSE)
  write.csv(results$oos_indicator_candidates$monthly,
            file.path(extra_dir, "cardi_5p_oos_indicator_candidate_monthly_returns.csv"),
            row.names = FALSE)
  write.csv(results$daily_oos_indicator_candidates$summary,
            file.path(extra_dir, "cardi_5p_daily_oos_indicator_candidate_summary.csv"),
            row.names = FALSE)
  write.csv(results$daily_oos_indicator_candidates$monthly,
            file.path(extra_dir, "cardi_5p_daily_oos_indicator_candidate_monthly_returns.csv"),
            row.names = FALSE)
  invisible(extra_dir)
}

load_or_construct_strategy_results <- function() {
  ensure_dir(config$output_dir)
  ensure_dir(config$figure_dir)
  strategy_results <- load_strategy_results_for_reporting(
    config$strategy_results_rds
  )
  if (is.null(strategy_results)) {
    strategy_results <- load_legacy_reporting_results_from_csv()
  }
  if (is.null(strategy_results)) {
    message("No complete local strategy results found; running portfolios.")
    strategy_results <- construct_strategy_results()
    save_strategy_results_for_reporting(strategy_results,
                                        config$strategy_results_rds)
  } else {
    if (!file.exists(config$strategy_results_rds)) {
      save_strategy_results_for_reporting(strategy_results,
                                          config$strategy_results_rds)
    }
    message("Complete local strategy results found; running CSV/figures only.")
  }
  strategy_results
}

run_table8_portfolio_main <- function() {
  strategy_results <- load_or_construct_strategy_results()
  portfolio_premium_data <- save_portfolio_premium_data(strategy_results)
  main_performance_table <- build_reporting_outputs(strategy_results)
  message("CARDI portfolio strategy complete.")
  message("Outputs written to: ", config$output_dir)
  invisible(list(
    strategy_results = strategy_results,
    portfolio_premium_data = portfolio_premium_data,
    main_performance_table = main_performance_table
  ))
}

run_table8_extra_strategy_outputs <- function() {
  strategy_results <- load_or_construct_strategy_results()
  extra_dir <- save_extra_strategy_outputs(strategy_results)
  message("Extra strategy outputs written to: ", extra_dir)
  invisible(strategy_results)
}
