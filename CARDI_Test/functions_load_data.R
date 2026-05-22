# Loading utilities. These functions read existing processed data and never
# modify reference files.

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
  if (!"Date" %in% names(data)) names(data)[1] <- "Date"
  data$Date <- parse_date(data$Date)
  for (col in setdiff(names(data), "Date")) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }
  data <- data[!is.na(data$Date), , drop = FALSE]
  data <- data[order(data$Date), , drop = FALSE]
  names(data)[-1] <- clean_stock_id(names(data)[-1])
  data
}

load_group_panels <- function(config, group_name) {
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
  list(
    prices = read_numeric_panel(price_file),
    mktcap = read_numeric_panel(mktcap_file)
  )
}

combine_stock_panels <- function(...) {
  panels <- list(...)
  out <- panels[[1]]
  for (i in seq.int(2, length(panels))) {
    out <- merge(out, panels[[i]], by = "Date", all = FALSE)
  }
  out <- out[, !duplicated(names(out)), drop = FALSE]
  out[order(out$Date), , drop = FALSE]
}

load_fama_factors <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)
  path <- config$fama_files[[frequency]]
  if (!file.exists(path)) stop("Missing Fama factor file: ", path)
  data <- read_excel_as_df(path)
  required <- c("Date", "MarketPremium", "SMB2", "HML2", "RMW2", "CMA2")
  check_required_columns(data, required, paste(frequency, "Fama factor file"))
  data$Date <- parse_date(data$Date)
  data$Period <- normalize_period_key(data, frequency)
  numeric_cols <- c("MarketPremium", "SMB2", "HML2", "RMW2", "CMA2",
                    "IndexRiskFreeRate")
  data <- as_numeric_columns(data, numeric_cols)
  keep <- c("Period", "Date", intersect(numeric_cols, names(data)))
  names(data)[names(data) == "Date"] <- "FactorDate"
  data[, c("Period", "FactorDate", intersect(numeric_cols, names(data))),
       drop = FALSE]
}

load_cardi_frequency <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)
  suffix <- frequency_suffix(frequency)
  path <- first_existing_path(config$cardi_files[[frequency]],
                              paste(frequency, "CARDI file"))
  data <- read_excel_as_df(path)
  required <- paste0(
    rep(c("CARDI_5P", "CARDI_1P", "CARDI_10P",
          "CARDI_5P_LogDiff", "CARDI_1P_LogDiff",
          "CARDI_10P_LogDiff"), each = 1),
    "_", suffix
  )
  check_required_columns(data, required, paste(frequency, "CARDI file"))
  if ("Date" %in% names(data)) data$CARDIDate <- parse_date(data$Date)
  if (!"CARDIDate" %in% names(data)) data$CARDIDate <- NA
  data$Period <- normalize_period_key(data, frequency)
  data <- as_numeric_columns(data, required)
  data[, c("Period", "CARDIDate", required), drop = FALSE]
}

load_macro_frequency <- function(config, frequency = config$frequency) {
  frequency <- normalize_frequency(frequency)
  suffix <- frequency_suffix(frequency)
  path <- config$macro_files[[frequency]]
  if (!file.exists(path)) stop("Missing macro file: ", path)
  data <- read_excel_as_df(path)
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
  if ("Date" %in% names(data)) data$MacroDate <- parse_date(data$Date)
  if (!"MacroDate" %in% names(data)) data$MacroDate <- NA
  data$Period <- normalize_period_key(data, frequency)
  data <- as_numeric_columns(data, required)
  data[, c("Period", "MacroDate", required), drop = FALSE]
}

load_reference_monthly_premiums <- function(config) {
  path <- config$reference_monthly_premium_file
  if (!file.exists(path)) return(NULL)
  data <- read.csv(path, check.names = FALSE)
  required <- c("Date", "Month", "HC_Return", "MC_Return", "LC_Return",
                "LC_HC_Return", "IndexRiskFreeRate", "HC_Premium",
                "MC_Premium", "LC_Premium", "LC_HC_Premium")
  check_required_columns(data, required, "Reference monthly premium file")
  data$Date <- parse_date(data$Date)
  data$Period <- normalize_period_key(data, "monthly")
  data <- as_numeric_columns(data, setdiff(required, c("Date", "Month")))
  data[, unique(c("Date", "Period", "Month", required)), drop = FALSE]
}
