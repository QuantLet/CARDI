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


# Auto-split Table 4 module: 13_factor_regression_helpers.R

rolling_lower_tail_quantile <- function(x, prob, window) {
  out <- rep(NA_real_, length(x))

  for (i in seq_along(x)) {
    start <- i - window + 1L
    # Skip if the window has not yet accumulated enough observations.
    if (start < 1L) next

    values <- x[start:i]
    values <- values[is.finite(values)]

    # Require at least half the window (but no fewer than 6) to be finite.
    min_obs <- max(6L, floor(window / 2))
    if (length(values) >= min_obs) {
      out[i] <- as.numeric(stats::quantile(
        values, probs = prob, na.rm = TRUE, names = FALSE, type = 7
      ))
    }
  }
  out
}

add_ar1_residual <- function(data, source_col = "pure_LC_premium",
                             out_col = "AR1_Premium") {
  # Construct the lag by shifting the series forward by one period.
  data$pure_LC_premium_lag1 <- c(NA_real_, head(data[[source_col]], -1))

  # Identify rows where both current value and lag are finite.
  fit_data <- data[finite_complete(data, c(source_col,
                                           "pure_LC_premium_lag1")), ,
                   drop = FALSE]

  data[[out_col]] <- NA_real_

  if (nrow(fit_data) >= 10) {
    fit <- stats::lm(
      stats::as.formula(paste(source_col, "~ pure_LC_premium_lag1")),
      data = fit_data
    )
    # Write residuals back into the original row positions using the integer
    # row names preserved in fit_data.
    data[[out_col]][as.integer(rownames(fit_data))] <- stats::residuals(fit)
  }

  data
}


# Auto-split Table 4 module: 14_factor_regression_pipeline.R

run_factor_regression <- function(config, portfolio_premiums) {
  # --- Cache check ---
  if (!isTRUE(config$force_recompute_regression) &&
      file.exists(config$enriched_rds)) {
    return(readRDS(config$enriched_rds))
  }

  # --- Load frequency-level covariates ---
  factors <- load_fama_factors(config, config$frequency)
  cardi   <- load_cardi_frequency(config, config$frequency)
  macro   <- load_macro_frequency(config, config$frequency)

  # --- Merge all datasets on the common Period key ---
  # Assign the Period key to portfolio_premiums in case it was not already set.
  portfolio_premiums$Period <- normalize_period_key(portfolio_premiums,
                                                    config$frequency)

  # Sequential inner join: periods present in all four datasets are retained.
  merged <- Reduce(
    function(x, y) merge(x, y, by = "Period", all = FALSE),
    list(portfolio_premiums, factors, cardi, macro)
  )
  merged <- merged[order(merged$Date), , drop = FALSE]

  # Save the pre-regression merged dataset as an auditable intermediate file.
  write_new_csv(merged, config$merged_analysis_file)

  # --- Fama–French five-factor regression on LC-HC Premium ---
  factor_vars <- c("MarketPremium", "SMB2", "HML2", "RMW2", "CMA2")
  reg_vars    <- c("LC_HC_Premium", factor_vars)

  # Filter to rows with complete, finite values for all regression variables.
  fit_data <- merged[finite_complete(merged, reg_vars), , drop = FALSE]

  if (nrow(fit_data) < length(reg_vars) + 5) {
    stop("Insufficient complete observations for factor regression.")
  }

  factor_fit <- stats::lm(
    LC_HC_Premium ~ MarketPremium + SMB2 + HML2 + RMW2 + CMA2,
    data = fit_data
  )

  # --- Attach fitted values and residuals to the full merged data frame ---
  merged$fitted_LC_HC_Premium <- NA_real_
  merged$pure_LC_premium      <- NA_real_

  # Use integer row names from fit_data to correctly place results back into
  # the (potentially larger) merged data frame.
  idx <- as.integer(rownames(fit_data))
  merged$fitted_LC_HC_Premium[idx] <- stats::fitted(factor_fit)
  merged$pure_LC_premium[idx]      <- stats::residuals(factor_fit)

  # --- Rolling VaR for pure_LC_premium ---
  merged$pure_LC_premium_VaR_10 <- rolling_lower_tail_quantile(
    merged$pure_LC_premium, 0.10, config$var_window
  )
  merged$pure_LC_premium_VaR_5  <- rolling_lower_tail_quantile(
    merged$pure_LC_premium, 0.05, config$var_window
  )
  merged$pure_LC_premium_VaR_1  <- rolling_lower_tail_quantile(
    merged$pure_LC_premium, 0.01, config$var_window
  )

  # --- AR(1) adjustment and its rolling VaR ---
  merged <- add_ar1_residual(merged)
  merged$AR1_Premium_VaR_10 <- rolling_lower_tail_quantile(
    merged$AR1_Premium, 0.10, config$var_window
  )
  merged$AR1_Premium_VaR_5  <- rolling_lower_tail_quantile(
    merged$AR1_Premium, 0.05, config$var_window
  )
  merged$AR1_Premium_VaR_1  <- rolling_lower_tail_quantile(
    merged$AR1_Premium, 0.01, config$var_window
  )

  # --- Persist outputs ---
  # Save only the model object (not the full data frame) so the RDS stays small.
  save_new_rds(list(factor_fit = factor_fit), config$model_rds)
  write_new_csv(merged, config$enriched_file)
  save_new_rds(merged,  config$enriched_rds)

  merged
}


load_portfolio_premiums_input <- function(config) {
  if (file.exists(config$portfolio_premium_rds)) {
    return(readRDS(config$portfolio_premium_rds))
  }
  if (file.exists(config$portfolio_premium_file)) {
    return(read.csv(config$portfolio_premium_file, check.names = FALSE))
  }
  stop("Portfolio premium output is missing. Run 01_portfolio_premiums/main.R first.")
}

run_factor_regression_stage <- function(config) {
  portfolio_premiums <- load_portfolio_premiums_input(config)
  run_factor_regression(config, portfolio_premiums)
}

