# =============================================================================
# File    : functions_load_data.R
# Purpose : Load raw and processed inputs used by the firm-return workflow:
#           stock price panels, market-value panels, risk-free rates, CARDI,
#           macro controls, event dummies, industry codes, and firm controls.
# Author  : CARDI Research Team
# Date    : 2026-05-16
# Dependencies:
#   - functions_utils.R (parse_date, clean_stock_id, month_key,
#                        as_numeric_cols)
#   - readxl           (Excel workbook input)
# =============================================================================

#' Read an Excel worksheet as a data frame
#'
#' @param path Character. Path to an `.xlsx` file.
#' @param sheet Sheet index or name passed to `readxl::read_excel()`.
#'
#' @return A base `data.frame`.
read_excel_df <- function(path, sheet = 1) {
  require_pkg("readxl")
  as.data.frame(readxl::read_excel(path, sheet = sheet), stringsAsFactors = FALSE)
}

#' Read a wide Date-by-firm CSV panel
#'
#' @description
#' Reads a CSV whose first column is a date and remaining columns are firm
#' identifiers. Dates are parsed, firm IDs are normalized, and value columns
#' are coerced to numeric.
#'
#' @param path Character. Path to a wide CSV panel.
#'
#' @return A data frame with `Date` plus one numeric column per firm.
read_panel_csv <- function(path) {
  if (!file.exists(path)) stop("Missing panel CSV: ", path)
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Date" %in% names(df)) names(df)[1] <- "Date"
  df$Date <- parse_date(df$Date)
  for (col in setdiff(names(df), "Date")) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df <- df[!is.na(df$Date), , drop = FALSE]
  df <- df[order(df$Date), , drop = FALSE]
  names(df)[-1] <- clean_stock_id(names(df)[-1])
  df
}

#' Load price and market-cap panels for one carbon group
#'
#' @param config Configuration list from `firm_returns_config()`.
#' @param group_name One of `HighCarbonIntens`, `MedCarbonIntens`, or
#'   `LowCarbonIntens`.
#'
#' @return A list with `prices`, `mktcap`, and `carbon_type`.
load_carbon_group_panels <- function(config, group_name) {
  ct_map <- config$carbon_groups
  if (!group_name %in% names(ct_map)) stop("Unknown carbon group: ", group_name)
  group_dir <- file.path(
    config$paths$input_dir,
    group_name,
    paste0(config$source_period$start, "-", config$source_period$end)
  )
  price_file <- file.path(group_dir, paste0(group_name, "_Price_", config$source_period$end, ".csv"))
  mktcap_file <- file.path(group_dir, paste0(group_name, "_Mktcap_", config$source_period$end, ".csv"))
  list(prices = read_panel_csv(price_file),
       mktcap = read_panel_csv(mktcap_file),
       carbon_type = unname(ct_map[[group_name]]))
}

#' Load daily risk-free rates
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame with `Date` and `IndexRiskFreeRate`.
load_fama_daily <- function(config) {
  path <- config$paths$fama_daily
  if (!file.exists(path)) stop("Missing daily Fama factor file: ", path)
  df <- read_excel_df(path)
  df$Date <- parse_date(df$Date)
  df[[config$columns$risk_free]] <- suppressWarnings(as.numeric(df[[config$columns$risk_free]]))
  df <- df[!is.na(df$Date), , drop = FALSE]
  df <- aggregate(df[[config$columns$risk_free]], list(Date = df$Date), mean, na.rm = TRUE)
  names(df)[2] <- config$columns$risk_free
  df[order(df$Date), , drop = FALSE]
}

#' Load monthly risk-free rates
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame with `Month` and `IndexRiskFreeRate`.
load_fama_monthly <- function(config) {
  path <- config$paths$fama_monthly
  if (!file.exists(path)) stop("Missing monthly Fama factor file: ", path)
  df <- read_excel_df(path)
  df$Date <- parse_date(df$Date)
  df$Month <- if ("FrequencyID" %in% names(df)) as.character(df$FrequencyID) else month_key(df$Date)
  df[[config$columns$risk_free]] <- suppressWarnings(as.numeric(df[[config$columns$risk_free]]))
  out <- aggregate(df[[config$columns$risk_free]], list(Month = df$Month), mean, na.rm = TRUE)
  names(out)[2] <- config$columns$risk_free
  out[order(out$Month), , drop = FALSE]
}

#' Load monthly CARDI variables
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame keyed by `Month`, with CARDI level and log-difference
#'   columns.
load_cardi_monthly <- function(config) {
  path <- config$paths$cardi_monthly
  if (!file.exists(path)) stop("Missing monthly CARDI file: ", path)
  df <- read_excel_df(path)
  df$Date <- parse_date(df$Date)
  df$Month <- if ("FrequencyID" %in% names(df)) as.character(df$FrequencyID) else month_key(df$Date)
  cardi_cols <- grep("^CARDI_", names(df), value = TRUE)
  as_numeric_cols(df[order(df$Month), unique(c("Month", "Date", cardi_cols)), drop = FALSE], cardi_cols)
}

#' Load monthly macro controls
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame keyed by `Month`, including macro controls and any event
#'   dummies present in the source file.
load_macro_monthly <- function(config) {
  path <- config$paths$macro_monthly
  if (!file.exists(path)) stop("Missing monthly macro file: ", path)
  df <- if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    read_excel_df(path)
  }
  df$Date <- parse_date(df$Date)
  df$Month <- if ("FrequencyID" %in% names(df)) as.character(df$FrequencyID) else month_key(df$Date)
  df[order(df$Month), , drop = FALSE]
}

#' Load monthly alternative indicators
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame keyed by `Month`, or `NULL` if the file is absent.
load_other_indicators_monthly <- function(config) {
  path <- config$paths$indicators_monthly
  if (!file.exists(path)) return(NULL)
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if ("YearMonth" %in% names(df)) {
    df$Month <- as.character(df$YearMonth)
  } else if ("Date" %in% names(df)) {
    df$Month <- month_key(parse_date(df$Date))
  } else if (!"Month" %in% names(df)) {
    stop("Cannot identify monthly key in indicators file: ", path)
  }
  df[order(df$Month), , drop = FALSE]
}

#' Load important carbon events as monthly dummies
#'
#' @description
#' Converts event dates to month keys and creates the three event-category
#' indicators used in predictability regressions: Covid, China, and
#' International.
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame with `Month`, `Event_dummy_M`, `Event_Covid_M`,
#'   `Event_China_M`, and `Event_International_M`, or `NULL` if absent.
load_events_monthly <- function(config) {
  path <- config$paths$events
  if (!file.exists(path)) return(NULL)
  df <- read_excel_df(path)
  if (!"Date" %in% names(df)) stop("Events file must contain Date.")
  df$Date <- parse_date(df$Date)
  df$Month <- month_key(df$Date)
  df <- df[!is.na(df$Month), , drop = FALSE]
  months <- sort(unique(df$Month))
  out <- data.frame(Month = months, Event_dummy_M = 1L, stringsAsFactors = FALSE)
  event_types <- c("Covid", "China", "International")
  for (tp in event_types) {
    hit <- unique(df$Month[tolower(as.character(df$Type)) == tolower(tp)])
    out[[paste0("Event_", tp, "_M")]] <- as.integer(out$Month %in% hit)
  }
  out
}

#' Load static industry codes
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame with `StockID` and `IndustryCode`.
load_industry_codes <- function(config) {
  path <- config$paths$stocklist_industry
  if (!file.exists(path)) stop("Missing Stocklist_industry.xlsx: ", path)
  df <- read_excel_df(path)
  stock_col <- if ("证券代码" %in% names(df)) "证券代码" else names(df)[1]
  ind_cols <- grep(config$columns$industry_raw, names(df), value = TRUE, fixed = TRUE)
  if (length(ind_cols) == 0) {
    ind_cols <- grep("所属中上协行业代码", names(df), value = TRUE)
  }
  if (length(ind_cols) == 0) stop("Cannot find industry code column in ", path)
  top <- grep("大类行业", ind_cols, value = TRUE)
  ind_col <- if (length(top) > 0) top[1] else ind_cols[1]
  out <- data.frame(
    StockID = clean_stock_id(df[[stock_col]]),
    IndustryCode = as.character(df[[ind_col]]),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$StockID) & nzchar(out$StockID) &
               !is.na(out$IndustryCode) & nzchar(out$IndustryCode), ]
  out[!duplicated(out$StockID), , drop = FALSE]
}

#' Load annual firm financial data
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return R object stored in `Data_FI_WIND.rds`, typically a named list of
#'   wide annual financial-indicator panels.
load_fi_wind <- function(config) {
  path <- config$paths$fi_wind
  if (!file.exists(path)) stop("Missing Data_FI_WIND.rds: ", path)
  readRDS(path)
}

#' Load firm-level carbon-emissions ranks
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Data frame from `Carbon_Rank.rds`, or `NULL` if the file is absent.
load_carbon_rank <- function(config) {
  path <- config$paths$carbon_rank
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

#' Load all monthly enrichment datasets
#'
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Named list with `cardi`, `macro`, `indicators`, and `events`.
load_monthly_enrichment <- function(config) {
  cardi <- load_cardi_monthly(config)
  macro <- tryCatch(load_macro_monthly(config), error = function(e) NULL)
  indicators <- tryCatch(load_other_indicators_monthly(config), error = function(e) NULL)
  events <- tryCatch(load_events_monthly(config), error = function(e) NULL)
  list(cardi = cardi, macro = macro, indicators = indicators, events = events)
}
