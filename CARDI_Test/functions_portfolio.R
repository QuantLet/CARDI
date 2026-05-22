# Portfolio construction functions. The monthly branch can reuse the existing
# reference output; the generic constructor supports monthly or weekly runs.

make_period_stock_panels <- function(price_panel, mktcap_panel, frequency) {
  frequency <- normalize_frequency(frequency)
  stocks <- intersect(names(price_panel)[-1], names(mktcap_panel)[-1])
  prices <- price_panel[, c("Date", stocks), drop = FALSE]
  caps <- mktcap_panel[, c("Date", stocks), drop = FALSE]
  prices$Period <- period_id(prices$Date, frequency)
  caps$Period <- period_id(caps$Date, frequency)

  periods <- sort(intersect(unique(prices$Period), unique(caps$Period)))
  period_dates <- as.Date(vapply(periods, function(p) {
    as.character(max(prices$Date[prices$Period == p], na.rm = TRUE))
  }, character(1)))

  returns <- data.frame(Period = periods, Date = as.Date(period_dates),
                        stringsAsFactors = FALSE)
  end_caps <- data.frame(Period = periods, Date = as.Date(period_dates),
                         stringsAsFactors = FALSE)

  for (stock in stocks) {
    stock_returns <- rep(NA_real_, length(periods))
    stock_caps <- rep(NA_real_, length(periods))
    for (i in seq_along(periods)) {
      key <- periods[i]
      pdat <- prices[prices$Period == key, c("Date", stock), drop = FALSE]
      pdat <- pdat[order(pdat$Date), , drop = FALSE]
      p_values <- pdat[[stock]]
      p_values <- p_values[is.finite(p_values) & p_values > 0]
      if (length(p_values) >= 2) {
        stock_returns[i] <- tail(p_values, 1) / p_values[1] - 1
      }

      cdat <- caps[caps$Period == key, c("Date", stock), drop = FALSE]
      cdat <- cdat[order(cdat$Date), , drop = FALSE]
      c_values <- cdat[[stock]]
      c_values <- c_values[is.finite(c_values) & c_values > 0]
      if (length(c_values) > 0) stock_caps[i] <- tail(c_values, 1)
    }
    returns[[stock]] <- stock_returns
    end_caps[[stock]] <- stock_caps
  }

  list(returns = returns, period_end_caps = end_caps)
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
  cuts <- stats::quantile(out$CarbonIntensity_Mean,
                          probs = c(low_prob, high_prob),
                          na.rm = TRUE, names = FALSE)
  list(
    Low = out$ID[out$CarbonIntensity_Mean < cuts[1]],
    Medium = out$ID[out$CarbonIntensity_Mean >= cuts[1] &
                    out$CarbonIntensity_Mean <= cuts[2]],
    High = out$ID[out$CarbonIntensity_Mean > cuts[2]]
  )
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
                                             carbon_rank_file, frequency) {
  if (!file.exists(carbon_rank_file)) {
    stop("Carbon rank file not found: ", carbon_rank_file)
  }
  carbon_rank <- readRDS(carbon_rank_file)
  check_required_columns(carbon_rank, c("ID", "CarbonIntensity_Mean"),
                         "Carbon rank file")
  carbon_rank$ID <- clean_stock_id(carbon_rank$ID)
  carbon_rank <- carbon_rank[is.finite(carbon_rank$CarbonIntensity_Mean), ,
                             drop = FALSE]

  panels <- make_period_stock_panels(price_panel, mktcap_panel, frequency)
  stock_returns <- panels$returns
  period_end_caps <- panels$period_end_caps
  periods <- stock_returns$Period
  stocks <- intersect(names(stock_returns)[-(1:2)],
                      names(period_end_caps)[-(1:2)])
  stocks <- intersect(stocks, carbon_rank$ID)

  out <- data.frame(
    Date = stock_returns$Date,
    Period = periods,
    Big_Low = NA_real_,
    Small_Low = NA_real_,
    Big_Medium = NA_real_,
    Small_Medium = NA_real_,
    Big_High = NA_real_,
    Small_High = NA_real_,
    LC_HC_Return = NA_real_,
    N_Big_Low = NA_integer_,
    N_Small_Low = NA_integer_,
    N_Big_Medium = NA_integer_,
    N_Small_Medium = NA_integer_,
    N_Big_High = NA_integer_,
    N_Small_High = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(periods)) {
    if (i == 1) next
    lagged_caps <- period_end_caps[i - 1, stocks, drop = FALSE]
    current_returns <- stock_returns[i, stocks, drop = FALSE]
    caps <- as.numeric(lagged_caps[1, ])
    names(caps) <- stocks
    valid_stocks <- stocks[is.finite(caps) & caps > 0]
    valid_stocks <- valid_stocks[
      is.finite(as.numeric(current_returns[1, valid_stocks, drop = TRUE]))
    ]
    if (length(valid_stocks) < 6) next

    size_cutoff <- stats::median(caps[valid_stocks], na.rm = TRUE)
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
        groups[[group_name]], current_returns, lagged_caps
      )
      out[i, paste0("N_", group_name)] <- length(groups[[group_name]])
    }
    out$LC_HC_Return[i] <- 0.5 * (out$Big_Low[i] + out$Small_Low[i]) -
      0.5 * (out$Big_High[i] + out$Small_High[i])
  }

  out[is.finite(out$LC_HC_Return), , drop = FALSE]
}

construct_portfolio_returns <- function(config, frequency = config$frequency) {
  message("Constructing ", frequency, " HC/MC/LC double-sort returns...")
  hc <- load_group_panels(config, "HighCarbonIntens")
  mc <- load_group_panels(config, "MedCarbonIntens")
  lc <- load_group_panels(config, "LowCarbonIntens")
  universe_prices <- combine_stock_panels(hc$prices, mc$prices, lc$prices)
  universe_mktcap <- combine_stock_panels(hc$mktcap, mc$mktcap, lc$mktcap)

  dynamic <- make_dynamic_double_sort_returns(
    universe_prices, universe_mktcap, config$carbon_rank_file, frequency
  )

  out <- dynamic[, c("Date", "Period"), drop = FALSE]
  out$HC_Return <- 0.5 * (dynamic$Big_High + dynamic$Small_High)
  out$MC_Return <- 0.5 * (dynamic$Big_Medium + dynamic$Small_Medium)
  out$LC_Return <- 0.5 * (dynamic$Big_Low + dynamic$Small_Low)
  out$LC_HC_Return <- dynamic$LC_HC_Return
  out
}

build_portfolio_premiums <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)

  if (!isTRUE(config$force_recompute_portfolio) &&
      file.exists(config$portfolio_premium_rds)) {
    return(readRDS(config$portfolio_premium_rds))
  }
  if (!isTRUE(config$force_recompute_portfolio) &&
      file.exists(config$portfolio_premium_file)) {
    return(read.csv(config$portfolio_premium_file, check.names = FALSE))
  }

  if (identical(frequency, "monthly")) {
    reference <- load_reference_monthly_premiums(config)
    if (!is.null(reference)) {
      out <- reference
    } else {
      returns <- construct_portfolio_returns(config, frequency)
      factors <- load_fama_factors(config, frequency)
      out <- merge(returns, factors[, c("Period", "IndexRiskFreeRate"),
                                    drop = FALSE],
                   by = "Period", all.x = TRUE)
    }
  } else {
    returns <- construct_portfolio_returns(config, frequency)
    factors <- load_fama_factors(config, frequency)
    out <- merge(returns, factors[, c("Period", "IndexRiskFreeRate"),
                                  drop = FALSE],
                 by = "Period", all.x = TRUE)
  }

  if (!"IndexRiskFreeRate" %in% names(out)) {
    stop("Portfolio premium calculation requires IndexRiskFreeRate.")
  }
  out$HC_Premium <- out$HC_Return - out$IndexRiskFreeRate
  out$MC_Premium <- out$MC_Return - out$IndexRiskFreeRate
  out$LC_Premium <- out$LC_Return - out$IndexRiskFreeRate
  out$LC_HC_Premium <- out$LC_Premium - out$HC_Premium
  if (identical(frequency, "monthly") && !"Month" %in% names(out)) {
    out$Month <- out$Period
  }
  out <- out[order(out$Date), , drop = FALSE]

  save_new_dataset(out, config$portfolio_premium_file,
                   config$portfolio_premium_rds)
  out
}
