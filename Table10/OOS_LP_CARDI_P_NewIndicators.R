# =============================================================================
# OOS_LP_CARDI_P_NewIndicators.R
#
# Purpose:
#   Test whether CARDI_5P predicts future changes in Eigenvector_HL_Ratio
#   using daily local projections across multiple horizons.
#
# Inputs:
#   Data/NewIndicators/Daily/All_Indicators_Daily.csv
#   Data/Processed/FRM_Carbon_risk.csv
# Output:
#   Output/CARDI_Indicator_Compare/ForwardInfo/CARDI_5P_LocalProjection_Daily.csv
# =============================================================================

options(stringsAsFactors = FALSE)

# ----------------------------- Configuration ------------------------------

project_root <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Table10"
setwd(project_root)

parse_env_int_vec <- function(name, default) {
  raw <- Sys.getenv(name, unset = default)
  out <- suppressWarnings(as.integer(trimws(strsplit(raw, ",")[[1]])))
  out <- out[is.finite(out) & out >= 1]
  if (length(out) == 0) stop(name, " must contain positive integer values.")
  out
}

config <- list(
  cardi_base_name = "CARDI_5P",
  selected_indicator_cols = c("Eigenvector_HL_Ratio"),
  frequency = "daily",
  transform = Sys.getenv("CARDI_COMPARE_TRANSFORM", unset = "none"),
  forecast_lag = as.integer(Sys.getenv("CARDI_COMPARE_FORECAST_LAG",
                                       unset = "1")),
  initial_window = as.integer(Sys.getenv("CARDI_COMPARE_DAILY_INITIAL",
                                         unset = "252")),
  horizons = parse_env_int_vec("CARDI_COMPARE_DAILY_HORIZONS", "1,5,10,20"),
  nw_lag = as.integer(Sys.getenv("CARDI_COMPARE_NW_LAG_DAILY", unset = "20")),
  min_observations = as.integer(Sys.getenv("CARDI_COMPARE_MIN_OBS",
                                           unset = "30")),
  indicator_file = file.path(
    "Data", "NewIndicators", "Daily", "All_Indicators_Daily.csv"
  ),
  cardi_file = file.path("Data", "Processed", "FRM_Carbon_risk.csv"),
  output_dir = file.path("Output", "CARDI_Indicator_Compare", "ForwardInfo")
)

if (!config$transform %in% c("none", "diff", "logdiff")) {
  stop("CARDI_COMPARE_TRANSFORM must be one of: none, diff, logdiff.")
}
if (!is.finite(config$forecast_lag) || config$forecast_lag < 1) {
  stop("CARDI_COMPARE_FORECAST_LAG must be a positive integer.")
}

# ------------------------------- Utilities --------------------------------

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing required package: ", pkg)
  }
}

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))

  x_chr <- trimws(as.character(x))
  out <- suppressWarnings(as.Date(x_chr))
  for (fmt in c("%Y/%m/%d", "%Y%m%d", "%Y-%m-%d", "%Y-%m", "%Y/%m")) {
    missing <- is.na(out)
    if (!any(missing)) break
    candidate <- x_chr[missing]
    if (fmt %in% c("%Y-%m", "%Y/%m")) {
      candidate <- paste0(candidate, "-01")
      fmt <- paste0(fmt, "-%d")
    }
    out[missing] <- suppressWarnings(as.Date(candidate, format = fmt))
  }
  out
}

period_key <- function(date, frequency) {
  date <- parse_date(date)
  if (identical(frequency, "monthly")) return(format(date, "%Y-%m"))
  as.character(date)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

finite_rows <- function(data, cols) {
  stats::complete.cases(data[, cols, drop = FALSE]) &
    apply(data[, cols, drop = FALSE], 1, function(row) {
      all(is.finite(safe_numeric(row)))
    })
}

lead_vec <- function(x, h) {
  c(tail(x, -h), rep(NA_real_, h))
}

lag_vec <- function(x, lag) {
  if (lag == 0) return(x)
  c(rep(NA_real_, lag), head(x, -lag))
}

transform_series <- function(x, method) {
  x <- safe_numeric(x)
  if (identical(method, "none")) return(x)
  if (identical(method, "diff")) return(c(NA_real_, diff(x)))
  if (identical(method, "logdiff")) {
    x[!is.finite(x) | x <= 0] <- NA_real_
    return(c(NA_real_, diff(log(x))))
  }
  stop("Unsupported transform: ", method)
}

choose_cardi_column <- function(data, base_name, frequency) {
  suffix <- if (identical(frequency, "monthly")) "_M" else ""
  candidates <- unique(c(
    paste0(base_name, suffix),
    base_name,
    paste0(base_name, "_", toupper(substr(frequency, 1, 1))),
    grep(paste0("^", gsub("_", ".*", base_name), "($|_)"),
         names(data), value = TRUE)
  ))
  candidates <- candidates[candidates %in% names(data)]
  if (length(candidates) == 0) {
    cardi_like <- grep("CARDI.*P", names(data), value = TRUE,
                       ignore.case = TRUE)
    stop("Could not find requested CARDI column for ", frequency,
         ". Requested base name: ", base_name,
         ". CARDI-like columns available: ", paste(cardi_like, collapse = ", "))
  }
  candidates[1]
}

load_monthly_cardi <- function(path) {
  require_package("readxl")
  data <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  if ("FrequencyID" %in% names(data)) {
    data$Period <- as.character(data$FrequencyID)
  } else if ("YearMonth" %in% names(data)) {
    data$Period <- substr(as.character(data$YearMonth), 1, 7)
  } else if ("Month" %in% names(data)) {
    data$Period <- substr(as.character(data$Month), 1, 7)
  } else if ("Date" %in% names(data)) {
    data$Period <- period_key(data$Date, "monthly")
  } else {
    stop("Monthly CARDI file has no recognizable date/month column.")
  }
  data
}

load_daily_cardi <- function(path) {
  data <- read.csv(path, check.names = FALSE)
  date_candidates <- c("date", "Date", "TradingDate", "trading_date", "Trddt")
  date_col <- date_candidates[date_candidates %in% names(data)][1]
  if (is.na(date_col)) stop("Daily CARDI file has no recognizable date column.")
  data$Period <- period_key(data[[date_col]], "daily")
  data
}

load_indicator_data <- function(path, frequency) {
  data <- read.csv(path, check.names = FALSE)
  ratio_cols <- grep("_HL_Ratio$", names(data), value = TRUE)
  if (length(ratio_cols) == 0) {
    stop("No *_HL_Ratio columns found in indicator file: ", path)
  }
  missing_selected <- setdiff(config$selected_indicator_cols, ratio_cols)
  if (length(missing_selected) > 0) {
    stop("Selected indicator column(s) not found in ", path, ": ",
         paste(missing_selected, collapse = ", "))
  }
  ratio_cols <- intersect(config$selected_indicator_cols, ratio_cols)
  if (identical(frequency, "monthly")) {
    if ("YearMonth" %in% names(data)) {
      data$Period <- substr(as.character(data$YearMonth), 1, 7)
    } else if ("Date" %in% names(data)) {
      data$Period <- period_key(data$Date, "monthly")
    } else {
      stop("Monthly indicator file must contain YearMonth or Date.")
    }
  } else {
    if (!"Date" %in% names(data)) stop("Daily indicator file must contain Date.")
    data$Period <- period_key(data$Date, "daily")
  }
  data <- data[, c("Period", ratio_cols), drop = FALSE]
  for (col in ratio_cols) data[[col]] <- safe_numeric(data[[col]])
  data
}

load_frequency_data <- function(frequency) {
  if (!identical(frequency, "daily")) stop("This Table10 version only supports daily data.")
  indicators <- load_indicator_data(config$indicator_file, "daily")
  cardi <- load_daily_cardi(config$cardi_file)

  cardi_col <- choose_cardi_column(cardi, config$cardi_base_name, frequency)
  cardi[[cardi_col]] <- safe_numeric(cardi[[cardi_col]])
  cardi <- cardi[, c("Period", cardi_col), drop = FALSE]
  names(cardi)[names(cardi) == cardi_col] <- "CARDI"

  merged <- merge(indicators, cardi, by = "Period", all = FALSE)
  merged <- merged[order(merged$Period), , drop = FALSE]

  list(
    data = merged,
    indicator_cols = setdiff(names(indicators), "Period"),
    cardi_column_used = cardi_col
  )
}

nw_for_lm <- function(fit, term, lag) {
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    co <- summary(fit)$coefficients
    return(c(coef = unname(co[term, "Estimate"]),
             t = unname(co[term, "t value"]),
             p = unname(co[term, "Pr(>|t|)"])))
  }
  vc <- sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)
  coef <- stats::coef(fit)
  se <- sqrt(diag(vc))
  if (is.null(names(se)) || length(names(se)) != length(coef) ||
      any(!nzchar(names(se)))) {
    names(se) <- names(coef)
  }
  shared <- intersect(names(coef), names(se))
  if (!term %in% shared) return(c(coef = NA_real_, t = NA_real_, p = NA_real_))
  t_val <- unname(coef[term]) / unname(se[term])
  p_val <- 2 * stats::pt(abs(t_val), df = stats::df.residual(fit),
                         lower.tail = FALSE)
  c(coef = unname(coef[term]), t = unname(t_val), p = unname(p_val))
}

run_local_projection_one <- function(y, cardi, horizon, lag, nw_lag,
                                     min_observations) {
  y <- safe_numeric(y)
  cardi <- safe_numeric(cardi)
  target <- lead_vec(y, horizon) - y
  data <- data.frame(target = target, cardi_current = cardi)
  for (l in 0:lag) {
    data[[paste0("indicator_lag", l)]] <- lag_vec(y, l)
  }
  data <- data[finite_rows(data, names(data)), , drop = FALSE]
  if (nrow(data) < max(min_observations, lag + 8)) return(NULL)

  rhs <- c("cardi_current", paste0("indicator_lag", 0:lag))
  fit <- stats::lm(
    stats::as.formula(paste("target ~", paste(rhs, collapse = " + "))),
    data = data
  )
  test <- nw_for_lm(fit, "cardi_current", nw_lag)
  list(
    coefficient = test["coef"],
    nw_t_stat = test["t"],
    nw_p_value = test["p"],
    adjusted_r_squared = summary(fit)$adj.r.squared,
    nobs = stats::nobs(fit)
  )
}

sig_flags <- function(p) {
  c(
    significant_10pct = is.finite(p) && p < 0.10,
    significant_5pct = is.finite(p) && p < 0.05,
    significant_1pct = is.finite(p) && p < 0.01
  )
}

run_frequency_analysis <- function(frequency) {
  obj <- load_frequency_data(frequency)
  data <- obj$data
  indicator_cols <- obj$indicator_cols
  horizons <- config$horizons
  initial_window <- config$initial_window
  nw_lag <- config$nw_lag

  data$CARDI_transformed <- transform_series(data$CARDI, config$transform)

  lp_rows <- list()

  for (indicator in indicator_cols) {
    indicator_series <- transform_series(data[[indicator]], config$transform)

    for (h in horizons) {
      lp <- run_local_projection_one(
        y = indicator_series,
        cardi = data$CARDI_transformed,
        horizon = h,
        lag = config$forecast_lag,
        nw_lag = nw_lag,
        min_observations = config$min_observations
      )

      if (!is.null(lp)) {
        flags <- sig_flags(lp$nw_p_value)
        lp_rows[[length(lp_rows) + 1L]] <- data.frame(
          frequency = frequency,
          indicator_name = indicator,
          cardi_column_used = obj$cardi_column_used,
          horizon = h,
          forecast_lag = config$forecast_lag,
          transformation = config$transform,
          dependent_variable = paste0("future_", h, "_period_change_in_", indicator),
          cardi_coefficient = unname(lp$coefficient),
          newey_west_t_stat = unname(lp$nw_t_stat),
          newey_west_p_value = unname(lp$nw_p_value),
          adjusted_r_squared = unname(lp$adjusted_r_squared),
          n_observations = unname(lp$nobs),
          positive_coefficient =
            is.finite(unname(lp$coefficient)) && unname(lp$coefficient) > 0,
          significant_10pct = unname(flags["significant_10pct"]),
          significant_5pct = unname(flags["significant_5pct"]),
          significant_1pct = unname(flags["significant_1pct"]),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  list(
    lp = if (length(lp_rows) > 0) do.call(rbind, lp_rows) else data.frame()
  )
}

write_frequency_outputs <- function(frequency, result) {
  if (!identical(frequency, "daily")) stop("This Table10 version only writes daily output.")
  dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  cardi_label <- gsub("[^A-Za-z0-9]+", "_", config$cardi_base_name)

  paths <- list(
    lp = file.path(config$output_dir, paste0(cardi_label, "_LocalProjection_Daily.csv"))
  )

  write.csv(result$lp, paths$lp, row.names = FALSE, fileEncoding = "UTF-8")
  paths
}

# --------------------------------- Run -------------------------------------

for (frequency in config$frequency) {
  message("Running local projection tests for ", frequency, " data...")
  result <- run_frequency_analysis(frequency)
  paths <- write_frequency_outputs(frequency, result)
  message("  LP output: ", paths$lp)
}

message("CARDI daily local-projection analysis complete.")
