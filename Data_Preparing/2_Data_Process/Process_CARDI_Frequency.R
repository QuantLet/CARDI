# ============================================================
# Generate Monthly and Weekly CARDI Variables
# Purpose:
#   Aggregate daily CARDI series to monthly and weekly frequency and calculate
#   log differences of the aggregated CARDI variables.
# Outputs:
#   Data/Processed/CARDI/Month_CARDI.csv
#   Data/Processed/CARDI/Month_CARDI.xlsx
#   Data/Processed/CARDI/Week_CARDI.csv
#   Data/Processed/CARDI/Week_CARDI.xlsx
#   Data/Processed/CARDI/CARDI_Frequency_Validation.csv
# ============================================================

options(stringsAsFactors = FALSE)

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))
} else {
  getwd()
}

find_project_root <- function(start_dir) {
  current <- normalizePath(start_dir, mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, "Data", "Processed")) &&
        dir.exists(file.path(current, "Code", "Data_Process"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate project root from: ", start_dir)
    }
    current <- parent
  }
}

project_root <- find_project_root(script_dir)
setwd(project_root)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package readxl is required.")
}

config <- list(
  cardi_file = file.path("Data", "Processed", "FRM_Carbon_risk.csv"),
  fama_monthly_file = file.path("Data", "Processed", "FamaFactors",
                                "FamaFactors_Monthly.xlsx"),
  fama_weekly_file = file.path("Data", "Processed", "FamaFactors",
                               "FamaFactors_Weekly.xlsx"),
  output_dir = file.path("Data", "Processed", "CARDI")
)

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- unlist(config[names(config) != "output_dir"])
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required input file(s): ",
       paste(missing_files, collapse = ", "))
}

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x_chr <- trimws(as.character(x))
  out <- suppressWarnings(as.Date(x_chr, format = "%Y-%m-%d"))
  for (fmt in c("%Y/%m/%d", "%Y%m%d", "%Y-%m-%d")) {
    missing <- is.na(out)
    if (any(missing)) {
      out[missing] <- suppressWarnings(as.Date(x_chr[missing], format = fmt))
    }
  }
  out
}

month_id <- function(date) {
  format(date, "%Y-%m")
}

week_id <- function(date) {
  format(date, "%G-%V")
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

read_fama_reference <- function(path) {
  dat <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  required <- c("Date", "FrequencyID")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0) {
    stop("Fama reference file missing column(s): ",
         paste(missing, collapse = ", "))
  }
  dat$Date <- parse_date(dat$Date)
  dat$FrequencyID <- as.character(dat$FrequencyID)
  dat <- dat[!is.na(dat$Date) & !is.na(dat$FrequencyID),
             c("Date", "FrequencyID"), drop = FALSE]
  dat <- dat[!duplicated(dat$FrequencyID), , drop = FALSE]
  dat[order(dat$Date), , drop = FALSE]
}

detect_date_column <- function(dat) {
  candidates <- c("date", "Date", "TradingDate", "trading_date",
                  "Trddt", "日期")
  found <- candidates[candidates %in% names(dat)]
  if (length(found) == 0) {
    stop("Could not detect a date column in CARDI file.")
  }
  found[1]
}

read_cardi_daily <- function(path) {
  dat <- read.csv(path, check.names = FALSE)
  date_col <- detect_date_column(dat)
  required <- c("CARDI_5P", "CARDI_1P", "CARDI_10P")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0) {
    stop("CARDI file missing required column(s): ",
         paste(missing, collapse = ", "))
  }
  dat$Date <- parse_date(dat[[date_col]])
  for (col in required) {
    dat[[col]] <- suppressWarnings(as.numeric(dat[[col]]))
  }
  dat <- dat[!is.na(dat$Date), c("Date", required), drop = FALSE]
  dat <- dat[order(dat$Date), , drop = FALSE]
  dat
}

assign_reference_period <- function(date, frequency, period_ref) {
  raw_id <- if (identical(frequency, "M")) month_id(date) else week_id(date)
  out <- raw_id
  missing_ref <- !out %in% period_ref$FrequencyID
  if (any(missing_ref)) {
    out[missing_ref] <- vapply(
      date[missing_ref],
      function(obs_date) {
        next_idx <- which(period_ref$Date >= obs_date)[1]
        if (is.na(next_idx)) return(NA_character_)
        period_ref$FrequencyID[next_idx]
      },
      character(1)
    )
  }
  out
}

carry_forward_positive <- function(x) {
  # Log differences require positive values. This function leaves the reported
  # average CARDI columns unchanged, but for the log-difference calculation it
  # replaces missing, zero, negative, or infinite values with the previous
  # finite positive value.
  out <- rep(NA_real_, length(x))
  last_positive <- NA_real_
  for (i in seq_along(x)) {
    if (is.finite(x[i]) && x[i] > 0) {
      last_positive <- x[i]
      out[i] <- x[i]
    } else {
      out[i] <- last_positive
    }
  }
  out
}

add_log_diff <- function(dat, avg_cols, suffix) {
  for (col in avg_cols) {
    adjusted <- carry_forward_positive(dat[[col]])
    out_col <- sub(paste0("_", suffix, "$"), paste0("_LogDiff_", suffix), col)
    dat[[out_col]] <- c(NA_real_, diff(log(adjusted)))
  }
  dat
}

aggregate_cardi <- function(cardi_daily, period_ref, frequency, suffix) {
  dat <- cardi_daily
  dat$FrequencyID <- assign_reference_period(dat$Date, frequency, period_ref)
  dat <- dat[!is.na(dat$FrequencyID), , drop = FALSE]

  # Aggregate daily CARDI to period-level CARDI by taking the mean of finite
  # daily observations inside each month/week.
  avg <- aggregate(
    dat[, c("CARDI_5P", "CARDI_1P", "CARDI_10P")],
    by = list(FrequencyID = dat$FrequencyID),
    FUN = safe_mean
  )
  names(avg)[names(avg) == "CARDI_5P"] <- paste0("CARDI_5P_", suffix)
  names(avg)[names(avg) == "CARDI_1P"] <- paste0("CARDI_1P_", suffix)
  names(avg)[names(avg) == "CARDI_10P"] <- paste0("CARDI_10P_", suffix)

  out <- merge(period_ref, avg, by = "FrequencyID", all.x = FALSE)
  out <- out[order(out$Date), , drop = FALSE]
  out <- out[!duplicated(out$FrequencyID), , drop = FALSE]

  avg_cols <- c(paste0("CARDI_5P_", suffix),
                paste0("CARDI_1P_", suffix),
                paste0("CARDI_10P_", suffix))

  # Log differences are calculated after sorting by period. Non-positive or
  # missing period averages are replaced with the previous finite positive
  # average only for the log-difference calculation.
  out <- add_log_diff(out, avg_cols, suffix)
  ordered_cols <- c(
    "Date", "FrequencyID",
    avg_cols,
    paste0("CARDI_5P_LogDiff_", suffix),
    paste0("CARDI_1P_LogDiff_", suffix),
    paste0("CARDI_10P_LogDiff_", suffix)
  )
  out[, ordered_cols, drop = FALSE]
}

write_outputs <- function(dat, name) {
  csv_path <- file.path(config$output_dir, paste0(name, ".csv"))
  xlsx_path <- file.path(config$output_dir, paste0(name, ".xlsx"))
  write.csv(dat, csv_path, row.names = FALSE)
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(dat, xlsx_path)
  }
}

validate_output <- function(dat, name, avg_cols, logdiff_cols) {
  data.frame(
    Dataset = name,
    Rows = nrow(dat),
    Columns = ncol(dat),
    StartDate = min(dat$Date, na.rm = TRUE),
    EndDate = max(dat$Date, na.rm = TRUE),
    DuplicateFrequencyID = sum(duplicated(dat$FrequencyID)),
    MissingAverageCells = sum(is.na(dat[, avg_cols, drop = FALSE])),
    MissingLogDiffCells = sum(is.na(dat[, logdiff_cols, drop = FALSE])),
    NonPositiveAverageCells = sum(dat[, avg_cols, drop = FALSE] <= 0,
                                  na.rm = TRUE)
  )
}

message("Reading CARDI input and Fama date references...")
cardi_daily <- read_cardi_daily(config$cardi_file)
monthly_ref <- read_fama_reference(config$fama_monthly_file)
weekly_ref <- read_fama_reference(config$fama_weekly_file)

message("Constructing Month_CARDI...")
month_cardi <- aggregate_cardi(cardi_daily, monthly_ref, "M", "M")

message("Constructing Week_CARDI...")
week_cardi <- aggregate_cardi(cardi_daily, weekly_ref, "W", "W")

if (any(duplicated(month_cardi$FrequencyID))) {
  stop("Month_CARDI has duplicate FrequencyID values.")
}
if (any(duplicated(week_cardi$FrequencyID))) {
  stop("Week_CARDI has duplicate FrequencyID values.")
}

write_outputs(month_cardi, "Month_CARDI")
write_outputs(week_cardi, "Week_CARDI")

validation <- rbind(
  validate_output(
    month_cardi,
    "Month_CARDI",
    c("CARDI_5P_M", "CARDI_1P_M", "CARDI_10P_M"),
    c("CARDI_5P_LogDiff_M", "CARDI_1P_LogDiff_M", "CARDI_10P_LogDiff_M")
  ),
  validate_output(
    week_cardi,
    "Week_CARDI",
    c("CARDI_5P_W", "CARDI_1P_W", "CARDI_10P_W"),
    c("CARDI_5P_LogDiff_W", "CARDI_1P_LogDiff_W", "CARDI_10P_LogDiff_W")
  )
)
validation$InputRows <- nrow(cardi_daily)
validation$InputStartDate <- min(cardi_daily$Date, na.rm = TRUE)
validation$InputEndDate <- max(cardi_daily$Date, na.rm = TRUE)
validation$DateColumnDetected <- detect_date_column(read.csv(
  config$cardi_file,
  check.names = FALSE,
  nrows = 1
))
validation$NonPositiveHandling <- paste(
  "Reported averages unchanged;",
  "log differences use previous finite positive average when needed"
)
write.csv(validation,
          file.path(config$output_dir, "CARDI_Frequency_Validation.csv"),
          row.names = FALSE)

message("CARDI frequency generation complete.")
message("Outputs written to: ", config$output_dir)
