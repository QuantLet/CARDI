# Self-contained stage functions.

# Auto-split Table 4 module: 03_frequency_dates.R

parse_date <- function(x) {
  # Fast paths: return early when the input is already a Date type.
  if (inherits(x, "Date"))                       return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))

  # Excel stores dates as the number of days since 1899-12-30.
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))

  x_chr <- trimws(as.character(x))

  # Try the ISO 8601 default first ("YYYY-MM-DD"); suppressWarnings prevents
  # noisy NA-coercion messages when the format does not match.
  out <- suppressWarnings(as.Date(x_chr))

  # Iterate over additional formats.  For monthly-only strings we append "-01"
  # to make them parseable as a full date (first of the month).
  for (fmt in c("%Y/%m/%d", "%Y%m%d", "%Y-%m", "%Y/%m")) {
    missing <- is.na(out)
    if (!any(missing)) break          # All entries already parsed; stop early.

    candidate <- x_chr[missing]

    # Monthly shorthand ("YYYY-MM" or "YYYY/MM") needs a day component.
    if (fmt %in% c("%Y-%m", "%Y/%m")) {
      candidate <- paste0(candidate, "-01")
      fmt       <- paste0(fmt, "-%d")   # Augment format to match the added day.
    }

    out[missing] <- suppressWarnings(as.Date(candidate, format = fmt))
  }
  out
}

normalize_frequency <- function(frequency) {
  frequency <- tolower(frequency)
  if (frequency %in% c("m", "month", "monthly")) return("monthly")
  if (frequency %in% c("w", "week",  "weekly"))  return("weekly")
  stop("Unsupported frequency: ", frequency)
}

frequency_suffix <- function(frequency) {
  frequency <- normalize_frequency(frequency)
  if (identical(frequency, "monthly")) "M" else "W"
}

period_id <- function(date, frequency) {
  frequency <- normalize_frequency(frequency)
  date      <- parse_date(date)

  if (identical(frequency, "monthly")) {
    return(format(date, "%Y-%m"))
  }
  # %G gives the ISO 8601 week-numbering year (may differ from calendar year
  # near year boundaries); %V gives the zero-padded ISO week number.
  format(date, "%G-%V")
}

period_start_date <- function(period, frequency) {
  frequency <- normalize_frequency(frequency)

  if (identical(frequency, "monthly")) {
    # Append "-01" to get the first day of the month.
    return(as.Date(paste0(substr(as.character(period), 1, 7), "-01")))
  }

  # ISO week: append "-1" (Monday = weekday 1 in %u) and parse with %G-%V-%u.
  as.Date(paste0(as.character(period), "-1"), format = "%G-%V-%u")
}

normalize_period_key <- function(data, frequency) {
  # Check columns in priority order to handle the variety of naming conventions
  # used across the project's input files.
  if ("Period"      %in% names(data)) return(as.character(data$Period))
  if ("FrequencyID" %in% names(data)) return(as.character(data$FrequencyID))
  if ("Month"       %in% names(data)) return(substr(as.character(data$Month), 1, 7))
  if ("Week"        %in% names(data)) return(as.character(data$Week))

  # Fall back to computing the period from a raw Date column.
  if ("Date" %in% names(data)) return(period_id(data$Date, frequency))

  stop(
    "Cannot construct period key: no Period, FrequencyID, Month, Week, ",
    "or Date column found."
  )
}


# Auto-split Table 4 module: 04_data_utilities.R

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

as_numeric_columns <- function(data, cols) {
  # Only touch columns that actually exist to avoid spurious errors.
  for (col in intersect(cols, names(data))) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }
  data
}

check_required_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      label, " is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

first_existing_path <- function(paths, label) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop(
      "Missing ", label, ". Checked:\n  ",
      paste(paths, collapse = "\n  ")
    )
  }
  existing[1]
}

clean_stock_id <- function(x) {
  x <- as.character(x)
  sub("\\.0$", "", x)
}

finite_complete <- function(data, cols) {
  # An empty column list means no restriction: all rows pass.
  if (length(cols) == 0) return(rep(TRUE, nrow(data)))

  mat <- data[, cols, drop = FALSE]

  # complete.cases checks for NA/NaN; the apply check additionally excludes Inf.
  stats::complete.cases(mat) &
    apply(mat, 1, function(row) {
      all(is.finite(suppressWarnings(as.numeric(row))))
    })
}


# Auto-split Table 4 module: 05_output_writers.R

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

save_new_dataset <- function(data, csv_path, rds_path = NULL) {
  ensure_dir(dirname(csv_path))

  # Guard: refuse to overwrite the CSV.
  if (file.exists(csv_path)) {
    stop("Refusing to overwrite existing file: ", csv_path)
  }
  write.csv(data, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

  # Guard: refuse to overwrite the RDS (if a path was supplied).
  if (!is.null(rds_path)) {
    if (file.exists(rds_path)) {
      stop("Refusing to overwrite existing file: ", rds_path)
    }
    saveRDS(data, rds_path)
  }

  invisible(data)
}

write_new_csv <- function(data, path) {
  ensure_dir(dirname(path))

  if (file.exists(path)) {
    stop("Refusing to overwrite existing file: ", path)
  }
  write.csv(data, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

save_new_rds <- function(object, path) {
  ensure_dir(dirname(path))

  if (file.exists(path)) {
    stop("Refusing to overwrite existing file: ", path)
  }
  saveRDS(object, path)
  invisible(path)
}


# Auto-split Table 4 module: 06_input_readers.R

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing required R package: ", pkg)
  }
}

read_excel_as_df <- function(path) {
  require_package("readxl")
  as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
}

read_numeric_panel <- function(path) {
  if (!file.exists(path)) stop("Missing panel file: ", path)

  data <- read.csv(path, check.names = FALSE)

  # Some files omit the "Date" header for the first column.
  if (!"Date" %in% names(data)) names(data)[1] <- "Date"

  data$Date <- parse_date(data$Date)

  # Coerce every non-date column to numeric (missing values become NA).
  for (col in setdiff(names(data), "Date")) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }

  # Remove rows where the date could not be parsed.
  data <- data[!is.na(data$Date), , drop = FALSE]
  data <- data[order(data$Date), , drop = FALSE]

  # Standardise stock ID column names (e.g. "600000.0" -> "600000").
  names(data)[-1] <- clean_stock_id(names(data)[-1])
  data
}


# Auto-split Table 4 module: 07_stock_panel_loaders.R

load_group_panels <- function(config, group_name) {
  # Build the folder path using the date range that identifies the panel files.
  group_dir <- file.path(
    config$input_dir,
    group_name,
    paste0(config$date_start_source, "-", config$date_end_source)
  )

  price_file  <- file.path(
    group_dir,
    paste0(group_name, "_Price_",  config$date_end_source, ".csv")
  )
  mktcap_file <- file.path(
    group_dir,
    paste0(group_name, "_Mktcap_", config$date_end_source, ".csv")
  )

  list(
    prices = read_numeric_panel(price_file),
    mktcap = read_numeric_panel(mktcap_file)
  )
}

combine_stock_panels <- function(...) {
  panels <- list(...)
  out    <- panels[[1]]

  # Sequentially inner-join each additional panel on Date.
  for (i in seq.int(2, length(panels))) {
    out <- merge(out, panels[[i]], by = "Date", all = FALSE)
  }

  # Drop any duplicate stock-ID columns introduced by the merge.
  out <- out[, !duplicated(names(out)), drop = FALSE]
  out[order(out$Date), , drop = FALSE]
}


# Auto-split Table 4 module: 08_frequency_data_loaders.R

load_fama_factors <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)
  path      <- config$fama_files[[frequency]]
  if (!file.exists(path)) stop("Missing Fama factor file: ", path)

  data <- read_excel_as_df(path)

  required <- c("Date", "MarketPremium", "SMB2", "HML2", "RMW2", "CMA2")
  check_required_columns(data, required, paste(frequency, "Fama factor file"))

  data$Date   <- parse_date(data$Date)
  data$Period <- normalize_period_key(data, frequency)

  # Cast all factor columns to numeric; IndexRiskFreeRate is optional.
  numeric_cols <- c("MarketPremium", "SMB2", "HML2", "RMW2", "CMA2",
                    "IndexRiskFreeRate")
  data <- as_numeric_columns(data, numeric_cols)

  # Rename Date to FactorDate so it does not collide with the portfolio Date
  # column after merging.
  names(data)[names(data) == "Date"] <- "FactorDate"

  data[, c("Period", "FactorDate", intersect(numeric_cols, names(data))),
       drop = FALSE]
}

load_cardi_frequency <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)
  suffix    <- frequency_suffix(frequency)

  path <- first_existing_path(
    config$cardi_files[[frequency]],
    paste(frequency, "CARDI file")
  )

  data <- read_excel_as_df(path)

  # Build the expected column names for this frequency suffix.
  required <- paste0(
    c("CARDI_5P", "CARDI_1P", "CARDI_10P",
      "CARDI_5P_LogDiff", "CARDI_1P_LogDiff", "CARDI_10P_LogDiff"),
    "_", suffix
  )
  check_required_columns(data, required, paste(frequency, "CARDI file"))

  # Rename Date column to CARDIDate to avoid collisions when merging.
  if ("Date" %in% names(data)) data$CARDIDate <- parse_date(data$Date)
  if (!"CARDIDate" %in% names(data)) data$CARDIDate <- NA

  data$Period <- normalize_period_key(data, frequency)
  data        <- as_numeric_columns(data, required)

  data[, c("Period", "CARDIDate", required), drop = FALSE]
}

load_macro_frequency <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)
  suffix    <- frequency_suffix(frequency)

  path <- config$macro_files[[frequency]]
  if (!file.exists(path)) stop("Missing macro file: ", path)

  data <- read_excel_as_df(path)

  # Construct the expected column names by appending the frequency suffix.
  required <- c(
    paste0("CarbonVol_", suffix, "_Shenzhen"),
    paste0("CarbonVol_", suffix, "_Guangdong"),
    paste0("CarbonVol_", suffix, "_Hubei"),
    paste0("RealEstate_Premium_", suffix),
    paste0("Slope_", suffix),
    paste0("TED_", suffix),
    paste0("TY3M_Change_", suffix),
    paste0("MarketVol_", suffix),
    paste0("Event_dummy_", suffix),
    paste0("Event_Covid_", suffix),
    paste0("Event_China_", suffix),
    paste0("Event_International_", suffix)
  )
  check_required_columns(data, required, paste(frequency, "macro file"))

  # Rename Date to MacroDate to avoid collisions after merging.
  if ("Date" %in% names(data)) data$MacroDate <- parse_date(data$Date)
  if (!"MacroDate" %in% names(data)) data$MacroDate <- NA

  data$Period <- normalize_period_key(data, frequency)
  data        <- as_numeric_columns(data, required)

  data[, c("Period", "MacroDate", required), drop = FALSE]
}

load_reference_monthly_premiums <- function(config) {
  path <- config$reference_monthly_premium_file

  # Return NULL instead of stopping: the caller can fall back to building
  # portfolio premiums from the daily panels.
  if (!file.exists(path)) return(NULL)

  data <- read.csv(path, check.names = FALSE)

  required <- c("Date", "Month", "HC_Return", "MC_Return", "LC_Return",
                "LC_HC_Return", "IndexRiskFreeRate", "HC_Premium",
                "MC_Premium", "LC_Premium", "LC_HC_Premium")
  check_required_columns(data, required, "Reference monthly premium file")

  data$Date   <- parse_date(data$Date)
  data$Period <- normalize_period_key(data, "monthly")
  data        <- as_numeric_columns(
    data, setdiff(required, c("Date", "Month"))
  )

  # Return only the standard columns in a consistent order.
  data[, unique(c("Date", "Period", "Month", required)), drop = FALSE]
}


# Auto-split Table 4 module: 10_portfolio_period_panels.R

make_period_stock_panels <- function(price_panel, mktcap_panel, frequency) {
  frequency <- normalize_frequency(frequency)

  # Only keep stocks that appear in both the price and market-cap panels.
  stocks <- intersect(names(price_panel)[-1], names(mktcap_panel)[-1])

  prices <- price_panel[,  c("Date", stocks), drop = FALSE]
  caps   <- mktcap_panel[, c("Date", stocks), drop = FALSE]

  # Assign a period key to every daily row.
  prices$Period <- period_id(prices$Date, frequency)
  caps$Period   <- period_id(caps$Date,   frequency)

  # Identify periods that have observations in both panels.
  periods <- sort(intersect(unique(prices$Period), unique(caps$Period)))

  # The period-end date is the latest trading date observed within the period.
  period_dates <- as.Date(vapply(periods, function(p) {
    as.character(max(prices$Date[prices$Period == p], na.rm = TRUE))
  }, character(1)))

  # Initialise output frames: one row per period.
  returns   <- data.frame(Period = periods, Date = as.Date(period_dates),
                          stringsAsFactors = FALSE)
  end_caps  <- data.frame(Period = periods, Date = as.Date(period_dates),
                          stringsAsFactors = FALSE)

  for (stock in stocks) {
    stock_returns <- rep(NA_real_, length(periods))
    stock_caps    <- rep(NA_real_, length(periods))

    for (i in seq_along(periods)) {
      key  <- periods[i]

      # --- Period return ---
      pdat     <- prices[prices$Period == key, c("Date", stock), drop = FALSE]
      pdat     <- pdat[order(pdat$Date), , drop = FALSE]
      p_values <- pdat[[stock]]
      # Use only positive (tradeable) prices; negative or zero prices are data
      # errors.
      p_values <- p_values[is.finite(p_values) & p_values > 0]
      if (length(p_values) >= 2) {
        # Buy at the first observed price, sell at the last.
        stock_returns[i] <- tail(p_values, 1) / p_values[1] - 1
      }

      # --- Period-end market cap ---
      cdat     <- caps[caps$Period == key, c("Date", stock), drop = FALSE]
      cdat     <- cdat[order(cdat$Date), , drop = FALSE]
      c_values <- cdat[[stock]]
      c_values <- c_values[is.finite(c_values) & c_values > 0]
      if (length(c_values) > 0) stock_caps[i] <- tail(c_values, 1)
    }

    returns[[stock]]  <- stock_returns
    end_caps[[stock]] <- stock_caps
  }

  list(returns = returns, period_end_caps = end_caps)
}


# Auto-split Table 4 module: 11_portfolio_sorting.R

double_sort_ids <- function(ids, carbon_rank, low_prob = 0.30,
                            high_prob = 0.70) {
  # Normalise ID format so IDs can be matched regardless of ".0" suffix.
  carbon_rank$ID <- clean_stock_id(carbon_rank$ID)

  out <- merge(
    data.frame(ID = clean_stock_id(ids)),
    carbon_rank,
    by  = "ID",
    all.x = TRUE
  )
  # Drop stocks that have no carbon-intensity record.
  out <- out[is.finite(out$CarbonIntensity_Mean), , drop = FALSE]

  if (nrow(out) == 0) {
    return(list(Low = character(0), Medium = character(0), High = character(0)))
  }

  # Compute the two quantile cutpoints across the stocks in this group.
  cuts <- stats::quantile(
    out$CarbonIntensity_Mean,
    probs  = c(low_prob, high_prob),
    na.rm  = TRUE,
    names  = FALSE
  )

  list(
    Low    = out$ID[out$CarbonIntensity_Mean <  cuts[1]],
    Medium = out$ID[out$CarbonIntensity_Mean >= cuts[1] &
                    out$CarbonIntensity_Mean <= cuts[2]],
    High   = out$ID[out$CarbonIntensity_Mean >  cuts[2]]
  )
}


# Auto-split Table 4 module: 12_portfolio_returns.R

weighted_group_return <- function(stock_ids, returns_row, lagged_cap_row) {
  # Restrict to stocks present in both the returns and the lagged-cap frames.
  ids <- intersect(stock_ids, names(returns_row))
  ids <- intersect(ids, names(lagged_cap_row))
  if (length(ids) == 0) return(NA_real_)

  returns <- as.numeric(returns_row[ids])
  caps    <- as.numeric(lagged_cap_row[ids])

  # Keep only stocks with finite returns AND positive lagged market cap.
  ok <- is.finite(returns) & is.finite(caps) & caps > 0
  if (!any(ok)) return(NA_real_)

  # Normalise caps to sum to 1 so the weighted sum equals the portfolio return.
  weights <- caps[ok] / sum(caps[ok])
  sum(returns[ok] * weights)
}

make_dynamic_double_sort_returns <- function(price_panel, mktcap_panel,
                                             carbon_rank_file, frequency) {
  if (!file.exists(carbon_rank_file)) {
    stop("Carbon rank file not found: ", carbon_rank_file)
  }

  carbon_rank <- readRDS(carbon_rank_file)
  check_required_columns(
    carbon_rank, c("ID", "CarbonIntensity_Mean"), "Carbon rank file"
  )
  carbon_rank$ID <- clean_stock_id(carbon_rank$ID)
  # Keep only stocks with a valid carbon-intensity measure.
  carbon_rank <- carbon_rank[is.finite(carbon_rank$CarbonIntensity_Mean), ,
                             drop = FALSE]

  panels          <- make_period_stock_panels(price_panel, mktcap_panel,
                                             frequency)
  stock_returns   <- panels$returns
  period_end_caps <- panels$period_end_caps
  periods         <- stock_returns$Period

  # Universe is the intersection of stocks in both panels and the carbon rank.
  stocks <- intersect(names(stock_returns)[-(1:2)],
                      names(period_end_caps)[-(1:2)])
  stocks <- intersect(stocks, carbon_rank$ID)

  # Pre-allocate the output data frame.
  out <- data.frame(
    Date          = stock_returns$Date,
    Period        = periods,
    Big_Low       = NA_real_,  Small_Low    = NA_real_,
    Big_Medium    = NA_real_,  Small_Medium = NA_real_,
    Big_High      = NA_real_,  Small_High   = NA_real_,
    LC_HC_Return  = NA_real_,
    N_Big_Low     = NA_integer_,  N_Small_Low    = NA_integer_,
    N_Big_Medium  = NA_integer_,  N_Small_Medium = NA_integer_,
    N_Big_High    = NA_integer_,  N_Small_High   = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(periods)) {
    # Skip period 1: no lagged data available for value-weighting.
    if (i == 1) next

    # Lagged caps (from previous period) are used for size sorting and
    # value-weighting in the current period — this preserves the information
    # timing constraint.
    lagged_caps     <- period_end_caps[i - 1, stocks, drop = FALSE]
    current_returns <- stock_returns[i,       stocks, drop = FALSE]

    caps         <- as.numeric(lagged_caps[1, ])
    names(caps)  <- stocks

    # Keep only stocks with finite lagged cap > 0 AND finite current return.
    valid_stocks <- stocks[is.finite(caps) & caps > 0]
    valid_stocks <- valid_stocks[
      is.finite(as.numeric(current_returns[1, valid_stocks, drop = TRUE]))
    ]

    # Require at least 6 stocks to have meaningful size and carbon sorts.
    if (length(valid_stocks) < 6) next

    # Size sort: split at the cross-sectional median of lagged market cap.
    size_cutoff <- stats::median(caps[valid_stocks], na.rm = TRUE)
    small_ids   <- valid_stocks[caps[valid_stocks] <= size_cutoff]
    big_ids     <- valid_stocks[caps[valid_stocks] >  size_cutoff]

    # Carbon sort within each size group.
    small_carbon <- double_sort_ids(small_ids, carbon_rank)
    big_carbon   <- double_sort_ids(big_ids,   carbon_rank)

    groups <- list(
      Big_Low       = big_carbon$Low,
      Small_Low     = small_carbon$Low,
      Big_Medium    = big_carbon$Medium,
      Small_Medium  = small_carbon$Medium,
      Big_High      = big_carbon$High,
      Small_High    = small_carbon$High
    )

    # Compute value-weighted returns and record group sizes.
    for (group_name in names(groups)) {
      out[i, group_name] <- weighted_group_return(
        groups[[group_name]], current_returns, lagged_caps
      )
      out[i, paste0("N_", group_name)] <- length(groups[[group_name]])
    }

    # LC-HC long-short return: average of Big/Small Low minus average of
    # Big/Small High.  The 0.5 weight reflects the equal blend of size groups.
    out$LC_HC_Return[i] <- 0.5 * (out$Big_Low[i]  + out$Small_Low[i]) -
                           0.5 * (out$Big_High[i] + out$Small_High[i])
  }

  # Drop periods where the LC-HC return could not be computed (typically
  # because too few stocks passed the filters).
  out[is.finite(out$LC_HC_Return), , drop = FALSE]
}

construct_portfolio_returns <- function(config, frequency = config$frequency) {
  message("Constructing ", frequency, " HC/MC/LC double-sort returns...")

  # Load daily price and market-cap panels for all three carbon groups.
  hc <- load_group_panels(config, "HighCarbonIntens")
  mc <- load_group_panels(config, "MedCarbonIntens")
  lc <- load_group_panels(config, "LowCarbonIntens")

  universe_prices <- combine_stock_panels(hc$prices, mc$prices, lc$prices)
  universe_mktcap <- combine_stock_panels(hc$mktcap, mc$mktcap, lc$mktcap)

  dynamic <- make_dynamic_double_sort_returns(
    universe_prices, universe_mktcap, config$carbon_rank_file, frequency
  )

  # Collapse six double-sort groups into three summary portfolio returns.
  out              <- dynamic[, c("Date", "Period"), drop = FALSE]
  out$HC_Return    <- 0.5 * (dynamic$Big_High   + dynamic$Small_High)
  out$MC_Return    <- 0.5 * (dynamic$Big_Medium + dynamic$Small_Medium)
  out$LC_Return    <- 0.5 * (dynamic$Big_Low    + dynamic$Small_Low)
  out$LC_HC_Return <- dynamic$LC_HC_Return
  out
}

build_portfolio_premiums <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)

  # Load cached output first unless recomputation is explicitly requested.
  if (!isTRUE(config$force_recompute_portfolio) &&
      file.exists(config$portfolio_premium_rds)) {
    return(readRDS(config$portfolio_premium_rds))
  }
  if (!isTRUE(config$force_recompute_portfolio) &&
      file.exists(config$portfolio_premium_file)) {
    return(read.csv(config$portfolio_premium_file, check.names = FALSE))
  }

  returns <- construct_portfolio_returns(config, frequency)
  factors <- load_fama_factors(config, frequency)
  out <- merge(
    returns,
    factors[, c("Period", "IndexRiskFreeRate"), drop = FALSE],
    by = "Period",
    all.x = TRUE
  )

  if (!"IndexRiskFreeRate" %in% names(out)) {
    stop("Portfolio premium calculation requires IndexRiskFreeRate.")
  }

  out$HC_Premium    <- out$HC_Return    - out$IndexRiskFreeRate
  out$MC_Premium    <- out$MC_Return    - out$IndexRiskFreeRate
  out$LC_Premium    <- out$LC_Return    - out$IndexRiskFreeRate
  out$LC_HC_Premium <- out$LC_Premium   - out$HC_Premium

  if (identical(frequency, "monthly") && !"Month" %in% names(out)) {
    out$Month <- out$Period
  }

  out <- out[order(out$Date), , drop = FALSE]

  save_new_dataset(
    out,
    config$portfolio_premium_file,
    config$portfolio_premium_rds
  )

  out
}

run_portfolio_premiums_stage <- function(config) {
  build_portfolio_premiums(config, config$frequency)
}
