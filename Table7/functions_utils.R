# =============================================================================
# File    : functions_utils.R
# Purpose : General-purpose utility functions shared across all modules in the
#           Process_FirmReturns workflow.  Covers date parsing, winsorisation,
#           safe I/O, and small numeric helpers.
# Author  : CARDI Research Team
# Date    : 2026-05-15
# Dependencies: Base R only (stats package)
# =============================================================================


# -----------------------------------------------------------------------------
# Package helper
# -----------------------------------------------------------------------------

#' Assert that a package is installed
require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Missing required R package: ", pkg,
         ". Install with: install.packages('", pkg, "')")
}

#' Return left-hand value unless it is NULL
#'
#' @param a Preferred value.
#' @param b Fallback value.
#'
#' @return `a` unless `a` is `NULL`; otherwise `b`.
`%||%` <- function(a, b) if (!is.null(a)) a else b


# -----------------------------------------------------------------------------
# Date utilities
# -----------------------------------------------------------------------------

#' Parse heterogeneous date representations to Date
#'
#' Handles: Date / POSIXct / POSIXlt objects already; Excel serial numerics;
#' and character strings in YYYY-MM-DD, YYYY/MM/DD, YYYYMMDD, YYYY-MM, YYYY/MM.
parse_date <- function(x) {
  if (inherits(x, "Date"))                          return(x)
  if (inherits(x, c("POSIXct", "POSIXlt")))        return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))

  x_chr <- trimws(as.character(x))
  out   <- suppressWarnings(as.Date(x_chr))        # Try ISO 8601 first

  for (fmt in c("%Y/%m/%d", "%Y%m%d", "%Y-%m", "%Y/%m")) {
    missing <- is.na(out)
    if (!any(missing)) break
    candidate <- x_chr[missing]
    if (fmt %in% c("%Y-%m", "%Y/%m")) {
      candidate <- paste0(candidate, "-01")
      fmt       <- paste0(fmt, "-%d")
    }
    out[missing] <- suppressWarnings(as.Date(candidate, format = fmt))
  }
  out
}

#' Return "YYYY-MM" month key for a date vector
#'
#' @param date Date-like vector accepted by `parse_date()`.
#'
#' @return Character vector of month keys.
month_key <- function(date) format(parse_date(date), "%Y-%m")

#' Return the calendar year as an integer for a date vector
#'
#' @param date Date-like vector accepted by `parse_date()`.
#'
#' @return Integer vector of calendar years.
year_key <- function(date) as.integer(format(parse_date(date), "%Y"))


# -----------------------------------------------------------------------------
# Numeric helpers
# -----------------------------------------------------------------------------

#' Natural log that returns NA for non-positive or non-finite inputs
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector of natural logs, with invalid inputs set to `NA`.
safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)

#' Winsorise a numeric vector at specified quantile bounds
#'
#' @param x     Numeric vector.
#' @param probs Length-2 numeric: lower and upper quantile bounds.
winsorize <- function(x, probs = c(0.01, 0.99)) {
  bounds <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, bounds[1]), bounds[2])
}

#' Winsorise selected columns of a data frame in place
#'
#' @param df Data frame.
#' @param cols Character vector of columns to winsorise.
#' @param probs Length-2 numeric vector of lower/upper quantile bounds.
#'
#' @return Modified data frame.
winsorize_df <- function(df, cols, probs = c(0.01, 0.99)) {
  for (col in intersect(cols, names(df))) {
    df[[col]] <- winsorize(df[[col]], probs = probs)
  }
  df
}

#' Return a logical vector: TRUE for rows where all cols are finite & complete
#'
#' @param df Data frame.
#' @param cols Character vector of columns to check.
#'
#' @return Logical vector aligned with rows of `df`.
finite_complete <- function(df, cols) {
  if (length(cols) == 0) return(rep(TRUE, nrow(df)))
  mat <- df[, intersect(cols, names(df)), drop = FALSE]
  stats::complete.cases(mat) &
    apply(mat, 1, function(row) all(is.finite(suppressWarnings(as.numeric(row)))))
}

#' Normalise common Chinese A-share identifiers to 6-digit stock codes.
#'
#' @param x Character or numeric stock identifiers, optionally with exchange
#'   suffixes such as `.SH` or `.SZ`.
#'
#' @return Character vector of six-digit stock codes.
clean_stock_code <- function(x) {
  x <- as.character(x)
  x <- sub("\\.(SH|SZ|BJ)$", "", x, ignore.case = TRUE)
  x <- sub("\\.0$", "", x)
  x <- gsub("[^0-9]", "", x)
  num <- suppressWarnings(as.integer(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(num)
  out[ok] <- formatC(num[ok], width = 6, flag = "0")
  out
}

clean_stock_id <- clean_stock_code

#' Coerce named columns of a data frame to numeric (warnings suppressed)
#'
#' @param df Data frame.
#' @param cols Character vector of columns to coerce.
#'
#' @return Modified data frame.
as_numeric_cols <- function(df, cols) {
  for (col in intersect(cols, names(df)))
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df
}


# -----------------------------------------------------------------------------
# Validation helpers
# -----------------------------------------------------------------------------

#' Stop with an informative message if required columns are missing
#'
#' @param df Data frame to check.
#' @param required Character vector of required column names.
#' @param label Human-readable data-frame label for error messages.
#'
#' @return Invisibly returns `TRUE`.
check_cols <- function(df, required, label = "data frame") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0)
    stop(label, " is missing columns: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

#' Return the first available column candidate
#'
#' @param candidates Preferred column names in priority order.
#' @param available Available column names.
#' @param required Logical. If `TRUE`, error when no candidate is found.
#' @param label Human-readable label for error messages.
#'
#' @return Character scalar containing the selected column name, or `NA`.
first_existing <- function(candidates, available, required = TRUE, label = "column") {
  hit <- intersect(candidates, available)
  if (length(hit) > 0) return(hit[1])
  if (required) stop("Cannot find ", label, ". Tried: ", paste(candidates, collapse = ", "))
  NA_character_
}

#' Divide safely with zero and missing-value protection
#'
#' @param num Numeric numerator.
#' @param den Numeric denominator.
#'
#' @return Numeric ratio, with invalid divisions set to `NA`.
safe_divide <- function(num, den) {
  ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)
}


# -----------------------------------------------------------------------------
# Wide panel  <->  long format
# -----------------------------------------------------------------------------

#' Pivot a wide panel (Date + stock columns) to long format
#'
#' @param panel_wide  data.frame with a Date column and one column per stock.
#' @param value_name  Name for the value column in the output.
#'
#' @return data.frame with columns: Date, StockID, <value_name>
pivot_panel_long <- function(panel_wide, value_name = "Value") {
  date_col <- panel_wide[["Date"]]
  stocks   <- setdiff(names(panel_wide), "Date")

  n_dates  <- length(date_col)
  n_stocks <- length(stocks)

  out <- data.frame(
    Date    = rep(date_col,  times = n_stocks),
    StockID = rep(stocks, each  = n_dates),
    stringsAsFactors = FALSE
  )
  out[[value_name]] <- unlist(panel_wide[, stocks, drop = FALSE], use.names = FALSE)
  out
}


# -----------------------------------------------------------------------------
# Safe I/O
# -----------------------------------------------------------------------------

#' Create a directory (and parents) if it does not exist
#'
#' @param path Directory path.
#'
#' @return Invisibly returns `path`.
ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

#' Save an RDS file; skip if it already exists and force = FALSE
#'
#' @param obj R object to save.
#' @param path Destination `.rds` path.
#' @param force Logical. If `FALSE`, skip when the file already exists.
#'
#' @return Invisibly returns `path`.
save_rds_safe <- function(obj, path, force = FALSE) {
  if (!force && file.exists(path)) {
    message("[skip] Already exists: ", basename(path))
    return(invisible(path))
  }
  ensure_dir(dirname(path))
  saveRDS(obj, path)
  message("[save] ", path)
  invisible(path)
}

#' Read an existing RDS or build and save it
#'
#' @param path Destination/source `.rds` path.
#' @param overwrite Logical. If `TRUE`, rebuild even when the file exists.
#' @param builder Zero-argument function that creates the object.
#'
#' @return Existing or newly built R object.
read_or_build_rds <- function(path, overwrite, builder) {
  if (!overwrite && file.exists(path)) {
    message("[load] ", path)
    return(readRDS(path))
  }
  obj <- builder()
  save_rds_safe(obj, path, force = TRUE)
  obj
}

#' Write a CSV file; skip if it already exists and force = FALSE
#'
#' @param df Data frame to write.
#' @param path Destination CSV path.
#' @param force Logical. If `FALSE`, skip when the file already exists.
#'
#' @return Invisibly returns `path`.
save_csv_safe <- function(df, path, force = FALSE) {
  if (!force && file.exists(path)) {
    message("[skip] Already exists: ", basename(path))
    return(invisible(path))
  }
  ensure_dir(dirname(path))
  utils::write.csv(df, path, row.names = FALSE)
  message("[save] ", path)
  invisible(path)
}

#' Append one row to the workflow validation log
#'
#' @param log Existing validation-log data frame, or `NULL`.
#' @param step Character step name.
#' @param data Optional data frame used to compute row, firm, and month counts.
#' @param notes Character notes for the validation row.
#' @param count Optional row count override.
#'
#' @return Updated validation-log data frame.
append_validation <- function(log, step, data = NULL, notes = "", count = NULL) {
  n <- if (!is.null(count)) count else if (!is.null(data)) nrow(data) else NA_integer_
  row <- data.frame(
    step = step,
    n_rows = n,
    n_firms = if (!is.null(data) && "StockID" %in% names(data)) length(unique(data$StockID)) else NA_integer_,
    n_months = if (!is.null(data) && "Month" %in% names(data)) length(unique(data$Month)) else NA_integer_,
    notes = notes,
    stringsAsFactors = FALSE
  )
  if (is.null(log)) row else rbind(log, row)
}

#' Check whether each firm's dates are sorted
#'
#' @param df Data frame with `StockID` and `Date`.
#'
#' @return Logical scalar.
validate_sorted_by_firm <- function(df) {
  if (!all(c("StockID", "Date") %in% names(df))) return(FALSE)
  keys <- split(df$Date, df$StockID)
  all(vapply(keys, function(x) all(diff(as.numeric(x)) >= 0), logical(1)))
}

#' Add user-friendly alias columns to firm output panels
#'
#' @param df Firm-level output data frame.
#'
#' @return Data frame with lower-case alias columns such as `date`, `price`,
#'   and `market_value` added when source columns exist.
add_output_aliases <- function(df) {
  aliases <- list(
    date = "Date",
    firm_identifier = "StockID",
    price = "Price",
    market_value = "MktCap",
    return = "Return",
    excess_return = "ExReturn",
    industry_code = "IndustryCode"
  )
  for (new in names(aliases)) {
    old <- aliases[[new]]
    if (old %in% names(df) && !new %in% names(df)) df[[new]] <- df[[old]]
  }
  df
}
