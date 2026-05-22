# Shared date and frequency helpers for the CARDI_Test workflow.

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))

  x_chr <- trimws(as.character(x))
  out <- suppressWarnings(as.Date(x_chr))
  for (fmt in c("%Y/%m/%d", "%Y%m%d", "%Y-%m", "%Y/%m")) {
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

normalize_frequency <- function(frequency) {
  frequency <- tolower(frequency)
  if (frequency %in% c("m", "month", "monthly")) return("monthly")
  if (frequency %in% c("w", "week", "weekly")) return("weekly")
  stop("Unsupported frequency: ", frequency)
}

frequency_suffix <- function(frequency) {
  frequency <- normalize_frequency(frequency)
  if (identical(frequency, "monthly")) "M" else "W"
}

period_id <- function(date, frequency) {
  frequency <- normalize_frequency(frequency)
  date <- parse_date(date)
  if (identical(frequency, "monthly")) {
    return(format(date, "%Y-%m"))
  }
  format(date, "%G-%V")
}

period_start_date <- function(period, frequency) {
  frequency <- normalize_frequency(frequency)
  if (identical(frequency, "monthly")) {
    return(as.Date(paste0(substr(as.character(period), 1, 7), "-01")))
  }
  as.Date(paste0(as.character(period), "-1"), format = "%G-%V-%u")
}

normalize_period_key <- function(data, frequency) {
  if ("Period" %in% names(data)) return(as.character(data$Period))
  if ("FrequencyID" %in% names(data)) return(as.character(data$FrequencyID))
  if ("Month" %in% names(data)) return(substr(as.character(data$Month), 1, 7))
  if ("Week" %in% names(data)) return(as.character(data$Week))
  if ("Date" %in% names(data)) return(period_id(data$Date, frequency))
  stop("Cannot construct period key: no Period, FrequencyID, Month, Week, or Date column.")
}

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

as_numeric_columns <- function(data, cols) {
  for (col in intersect(cols, names(data))) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }
  data
}

check_required_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing required column(s): ",
         paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

first_existing_path <- function(paths, label) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop("Missing ", label, ". Checked:\n  ",
         paste(paths, collapse = "\n  "))
  }
  existing[1]
}

clean_stock_id <- function(x) {
  x <- as.character(x)
  sub("\\.0$", "", x)
}

finite_complete <- function(data, cols) {
  if (length(cols) == 0) return(rep(TRUE, nrow(data)))
  mat <- data[, cols, drop = FALSE]
  stats::complete.cases(mat) &
    apply(mat, 1, function(row) {
      all(is.finite(suppressWarnings(as.numeric(row))))
    })
}
