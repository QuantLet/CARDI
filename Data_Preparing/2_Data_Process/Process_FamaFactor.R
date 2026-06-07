# Process Fama factor files and reconstruct market risk premia

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
    if (dir.exists(file.path(current, "Data", "raw")) &&
        dir.exists(file.path(current, "Code"))) {
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

required_packages <- c("readxl", "writexl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing required R package(s): ", paste(missing_packages, collapse = ", "))
}

config <- list(
  fama_dir = file.path(project_root, "Data", "raw", "Famafactor"),
  market_dir = file.path(project_root, "Data", "raw", "MarketReturns"),
  macro_dir = file.path(project_root, "Data", "raw", "macro"),
  output_dir = file.path(project_root, "Data", "Processed", "FamaFactors"),
  target_markettype_id = "P9709",
  target_markettype_label = "沪深A股和创业板",
  market_return_type = 1,
  market_return_type_label = "上证A股"
)

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x <- trimws(as.character(x))
  out <- as.Date(x)
  missing <- is.na(out)
  if (any(missing)) out[missing] <- as.Date(x[missing], format = "%Y/%m/%d")
  missing <- is.na(out)
  if (any(missing)) out[missing] <- as.Date(x[missing], format = "%Y%m%d")
  out
}

to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)))))
}

clean_key <- function(x) trimws(as.character(x))

read_csmar_excel <- function(path) {
  if (!file.exists(path)) stop("Missing input file: ", path)
  sheets <- readxl::excel_sheets(path)
  sheet <- sheets[1]
  header_rows <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    n_max = 3,
    .name_repair = "minimal"
  )
  column_names <- make.unique(as.character(unlist(header_rows[1, ])))
  labels <- setNames(as.character(unlist(header_rows[2, ])), column_names)
  units <- setNames(as.character(unlist(header_rows[3, ])), column_names)
  data <- readxl::read_excel(
    path,
    sheet = sheet,
    skip = 3,
    col_names = column_names,
    .name_repair = "unique",
    guess_max = 10000
  )
  list(
    path = path,
    sheet = sheet,
    columns = column_names,
    labels = labels,
    units = units,
    data = as.data.frame(data),
    row_count = nrow(data)
  )
}

choose_column <- function(columns, preferred, patterns, context) {
  exact <- preferred[preferred %in% columns]
  if (length(exact) > 0) return(exact[1])
  for (pattern in patterns) {
    hit <- grep(pattern, columns, value = TRUE, ignore.case = TRUE)
    if (length(hit) > 0) {
      message("Column mapping in ", context, ": using ", hit[1],
              " for requested ", preferred[1])
      return(hit[1])
    }
  }
  stop("Could not find required column for ", preferred[1], " in ", context,
       ". Available columns: ", paste(columns, collapse = ", "))
}

select_fama_factor_columns <- function(fama_obj) {
  labels <- fama_obj$labels
  total_weighted <- names(labels)[grepl("总市值加权", labels, fixed = TRUE)]
  non_market <- total_weighted[!grepl("^RiskPremium", total_weighted)]
  needed_patterns <- c("^SMB", "^HML", "^RMW", "^CMA")
  unique(unlist(lapply(needed_patterns, function(pattern) {
    grep(pattern, non_market, value = TRUE)
  }), use.names = FALSE))
}

prepare_fama <- function(fama_obj, date_col, frequency) {
  data <- fama_obj$data
  portfolio_col <- choose_column(
    names(data), "Portfolios", c("Portfolio", "Port"),
    paste0("Fama ", frequency)
  )
  markettype_col <- choose_column(
    names(data), "MarkettypeID", c("Markettype", "Market"),
    paste0("Fama ", frequency)
  )
  date_col <- choose_column(
    names(data), date_col, c("TradingDate", "TradingWeek", "TradingMonth"),
    paste0("Fama ", frequency)
  )
  factor_cols <- select_fama_factor_columns(fama_obj)
  if (length(factor_cols) == 0) {
    stop("No non-market Fama factor columns with 总市值加权 label found in ",
         fama_obj$path)
  }

  data[[portfolio_col]] <- to_numeric(data[[portfolio_col]])
  data$MarkettypeID <- clean_key(data[[markettype_col]])
  data <- data[
    data[[portfolio_col]] == 1 &
      data$MarkettypeID == config$target_markettype_id,
    ,
    drop = FALSE
  ]
  data$FrequencyID <- clean_key(data[[date_col]])
  data$JoinKey <- data$FrequencyID
  if (frequency == "Daily") {
    data$Date <- parse_date(data[[date_col]])
    data$JoinKey <- as.character(data$Date)
  } else if (frequency == "Monthly") {
    data$Date <- as.Date(paste0(data$FrequencyID, "-01"))
  } else {
    data$Date <- as.Date(NA)
  }
  for (col in factor_cols) data[[col]] <- to_numeric(data[[col]])

  out <- data[, c("Date", "FrequencyID", "JoinKey", "MarkettypeID",
                  factor_cols), drop = FALSE]
  list(
    data = out,
    factor_cols = factor_cols,
    rows_after_filter = nrow(out)
  )
}

prepare_market <- function(market_obj, date_col, return_col, frequency) {
  data <- market_obj$data
  markettype_col <- choose_column(
    names(data), "Markettype", c("Markettype", "Market"),
    paste0("Market returns ", frequency)
  )
  date_col <- choose_column(
    names(data), date_col, c("Trdt", "Trddt", "TradingWeek", "Trdwnt", "Trdmnt"),
    paste0("Market returns ", frequency)
  )
  return_col <- choose_column(
    names(data), return_col, c("retwdtl", "wdtl"),
    paste0("Market returns ", frequency)
  )

  data$Markettype <- to_numeric(data[[markettype_col]])
  data$FrequencyID <- clean_key(data[[date_col]])
  data$JoinKey <- data$FrequencyID
  if (frequency == "Daily") {
    data$Date <- parse_date(data[[date_col]])
    data$JoinKey <- as.character(data$Date)
  } else if (frequency == "Monthly") {
    data$Date <- as.Date(paste0(data$FrequencyID, "-01"))
  } else {
    data$Date <- as.Date(NA)
  }
  data$MarketReturn <- to_numeric(data[[return_col]])
  data <- data[data$Markettype == config$market_return_type, ,
               drop = FALSE]
  data[, c("Date", "FrequencyID", "JoinKey", "Markettype", "MarketReturn"),
       drop = FALSE]
}

read_regular_excel <- function(path) {
  if (!file.exists(path)) stop("Missing input file: ", path)
  sheets <- readxl::excel_sheets(path)
  sheet <- sheets[1]
  data <- readxl::read_excel(
    path,
    sheet = sheet,
    .name_repair = "unique",
    guess_max = 10000
  )
  list(
    path = path,
    sheet = sheet,
    columns = names(data),
    data = as.data.frame(data),
    row_count = nrow(data)
  )
}

annual_percent_to_return <- function(rate_percent, frequency) {
  rate_annual <- rate_percent / 100
  exponent <- switch(
    frequency,
    Daily = 1 / 365,
    Weekly = 7 / 365,
    Monthly = 1 / 12
  )
  (1 + rate_annual) ^ exponent - 1
}

run_market_premium_tests <- function(output_dir) {
  rf_percent <- 5
  expected_weekly <- (1 + 0.05) ^ (7 / 365) - 1
  expected_monthly <- (1 + 0.05) ^ (1 / 12) - 1
  actual_weekly <- annual_percent_to_return(rf_percent, "Weekly")
  actual_monthly <- annual_percent_to_return(rf_percent, "Monthly")
  test_return <- 0.10
  actual_premium <- test_return - actual_weekly
  expected_premium <- test_return - expected_weekly
  tests <- data.frame(
    Test = c(
      "weekly compound RF conversion",
      "monthly compound RF conversion",
      "market premium subtraction"
    ),
    Expected = c(expected_weekly, expected_monthly, expected_premium),
    Actual = c(actual_weekly, actual_monthly, actual_premium)
  )
  tests$Pass <- abs(tests$Expected - tests$Actual) < 1e-14
  write.csv(
    tests,
    file.path(output_dir, "MarketPremium_ConversionTests.csv"),
    row.names = FALSE
  )
  if (!all(tests$Pass)) {
    stop("Market premium conversion self-test failed.")
  }
  tests
}

prepare_index_returns <- function(index_obj) {
  data <- index_obj$data
  date_col <- choose_column(
    names(data), "日期", c("date", "日期"), "000001.SH close price"
  )
  close_col <- choose_column(
    names(data), "收盘价(元)", c("收盘", "close"), "000001.SH close price"
  )
  data$Date <- parse_date(data[[date_col]])
  data$Close <- to_numeric(data[[close_col]])
  data <- data[!is.na(data$Date) & is.finite(data$Close), ,
               drop = FALSE]
  data <- data[order(data$Date), , drop = FALSE]
  data$DailyReturn <- c(NA_real_, data$Close[-1] / head(data$Close, -1) - 1)
  data$WeekKey <- format(data$Date, "%G-%V")
  data$MonthKey <- format(data$Date, "%Y-%m")
  data[, c("Date", "Close", "DailyReturn", "WeekKey", "MonthKey")]
}

compound_period_returns <- function(index_returns, frequency) {
  if (frequency == "Daily") {
    out <- data.frame(
      Date = index_returns$Date,
      FrequencyID = as.character(index_returns$Date),
      JoinKey = as.character(index_returns$Date),
      MarketReturn = index_returns$DailyReturn,
      MarketClose = index_returns$Close
    )
    return(out)
  }
  key_col <- if (frequency == "Weekly") "WeekKey" else "MonthKey"
  split_data <- split(index_returns, index_returns[[key_col]])
  rows <- lapply(split_data, function(dat) {
    dat <- dat[order(dat$Date), , drop = FALSE]
    returns <- dat$DailyReturn[is.finite(dat$DailyReturn)]
    market_return <- if (length(returns) > 0) prod(1 + returns) - 1 else NA_real_
    data.frame(
      Date = max(dat$Date, na.rm = TRUE),
      FrequencyID = dat[[key_col]][1],
      JoinKey = dat[[key_col]][1],
      MarketReturn = market_return,
      MarketClose = tail(dat$Close, 1)
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out[order(out$Date), , drop = FALSE]
}

prepare_macro_risk_free <- function(macro_obj, frequency) {
  data <- macro_obj$data
  date_col <- choose_column(
    names(data), "指标名称", c("date", "日期", "指标名称"),
    "macro risk-free rate"
  )
  rf_col <- choose_column(
    names(data), "中债国债到期收益率:3个月",
    c("3个月", "三个月"), "macro risk-free rate"
  )
  data$RfDate <- parse_date(data[[date_col]])
  data$RiskFreeAnnualPercent <- to_numeric(data[[rf_col]])
  data <- data[!is.na(data$RfDate) & is.finite(data$RiskFreeAnnualPercent), ,
               drop = FALSE]
  data <- data[order(data$RfDate), , drop = FALSE]
  data$RiskFreeRate <- annual_percent_to_return(
    data$RiskFreeAnnualPercent,
    frequency
  )
  if (frequency == "Daily") {
    data$JoinKey <- as.character(data$RfDate)
    out <- data[, c("JoinKey", "RfDate", "RiskFreeAnnualPercent",
                    "RiskFreeRate"), drop = FALSE]
  } else if (frequency == "Weekly") {
    data$WeekKey <- format(data$RfDate, "%G-%V")
    out <- last_observation_by_key(data, "WeekKey",
                                   c("RiskFreeAnnualPercent",
                                     "RiskFreeRate"))
    names(out)[names(out) == "WeekKey"] <- "JoinKey"
  } else {
    data$MonthKey <- format(data$RfDate, "%Y-%m")
    out <- last_observation_by_key(data, "MonthKey",
                                   c("RiskFreeAnnualPercent",
                                     "RiskFreeRate"))
    names(out)[names(out) == "MonthKey"] <- "JoinKey"
  }
  attr(out, "construction") <- switch(
    frequency,
    Daily = paste(
      "Daily market premium uses the annualized 3-month government bond",
      "yield from macro.xlsx, divides percent by 100, and converts with",
      "(1 + rf_annual)^(1/365) - 1."
    ),
    Weekly = paste(
      "Weekly market premium uses the annualized 3-month government bond",
      "yield from the last available macro date in each ISO week and converts",
      "with (1 + rf_annual)^(7/365) - 1."
    ),
    Monthly = paste(
      "Monthly market premium uses the annualized 3-month government bond",
      "yield from the last available macro date in each month and converts",
      "with (1 + rf_annual)^(1/12) - 1."
    )
  )
  out
}

prepare_macro_market_premium <- function(index_returns, macro_obj, frequency) {
  market <- compound_period_returns(index_returns, frequency)
  rf <- prepare_macro_risk_free(macro_obj, frequency)
  rf_construction <- attr(rf, "construction")
  merged <- merge(
    market,
    rf[, c("JoinKey", "RfDate", "RiskFreeAnnualPercent", "RiskFreeRate"),
       drop = FALSE],
    by = "JoinKey",
    all.x = TRUE
  )
  merged$MarketPremium <- merged$MarketReturn - merged$RiskFreeRate
  merged <- merged[order(merged$Date), , drop = FALSE]
  attr(merged, "riskfree_construction") <- rf_construction
  attr(merged, "market_construction") <- paste(
    "MarketReturn is calculated from 000001.SH close price at",
    frequency,
    "frequency."
  )
  merged
}

last_observation_by_key <- function(data, key_col, value_cols) {
  data <- data[order(data[[key_col]], data$RfDate), , drop = FALSE]
  idx <- tapply(seq_len(nrow(data)), data[[key_col]], tail, 1)
  out <- data[as.integer(idx), c(key_col, "RfDate", value_cols), drop = FALSE]
  row.names(out) <- NULL
  out
}

prepare_risk_free <- function(rf_obj, frequency) {
  data <- rf_obj$data
  date_col <- choose_column(
    names(data), "Clsdt", c("Clsdt", "Date"), "Risk-free rates"
  )
  nrrdata_col <- choose_column(
    names(data), "Nrrdata", c("Nrrdata"), "Risk-free rates"
  )
  rf_col <- switch(
    frequency,
    Daily = choose_column(names(data), "Nrrdaydt", c("Nrrday"),
                          "Risk-free rates"),
    Weekly = choose_column(names(data), "Nrrwkdt", c("Nrrwk"),
                           "Risk-free rates"),
    Monthly = choose_column(names(data), "Nrrmtdt", c("Nrrmt"),
                            "Risk-free rates")
  )

  data$RfDate <- parse_date(data[[date_col]])
  data$Nrrdata <- to_numeric(data[[nrrdata_col]])
  data$RiskFreeRate_SourcePercent <- to_numeric(data[[rf_col]])
  data$RiskFreeRate <- data$RiskFreeRate_SourcePercent / 100

  if (frequency == "Daily") {
    data$JoinKey <- as.character(data$RfDate)
    out <- data[, c("JoinKey", "RfDate", "Nrrdata",
                    "RiskFreeRate_SourcePercent", "RiskFreeRate"),
                drop = FALSE]
    attr(out, "construction") <- paste(
      "Daily RF uses Clsdt and Nrrdaydt; Nrrdaydt is labeled in percent",
      "and is divided by 100 before subtracting from decimal market returns."
    )
  } else if (frequency == "Weekly") {
    data$WeekKey <- format(data$RfDate, "%G-%V")
    out <- last_observation_by_key(data, "WeekKey",
                                   c("Nrrdata", "RiskFreeRate_SourcePercent",
                                     "RiskFreeRate"))
    names(out)[names(out) == "WeekKey"] <- "JoinKey"
    attr(out, "construction") <- paste(
      "Weekly RF uses Nrrwkdt from the last available Clsdt in each ISO",
      "week (%G-%V), aligned to TradingWeek/Trdwnt; Nrrwkdt is labeled in",
      "percent and is divided by 100 before subtracting from decimal market",
      "returns."
    )
  } else {
    data$MonthKey <- format(data$RfDate, "%Y-%m")
    out <- last_observation_by_key(data, "MonthKey",
                                   c("Nrrdata", "RiskFreeRate_SourcePercent",
                                     "RiskFreeRate"))
    names(out)[names(out) == "MonthKey"] <- "JoinKey"
    attr(out, "construction") <- paste(
      "Monthly RF uses Nrrmtdt from the last available Clsdt in each",
      "calendar month, aligned to Trdmnt; Nrrmtdt is labeled in percent",
      "and is divided by 100 before subtracting from decimal market returns."
    )
  }
  out
}

deduplicate_keys <- function(data, key_cols, label) {
  duplicated_key <- duplicated(data[, key_cols, drop = FALSE])
  if (any(duplicated_key)) {
    message(label, ": dropping ", sum(duplicated_key),
            " duplicated key rows after keeping first occurrence.")
    data <- data[!duplicated_key, , drop = FALSE]
  }
  data
}

process_frequency <- function(spec, rf_obj, market_premium_data) {
  message("\nProcessing ", spec$frequency, " Fama factors...")
  fama_obj <- read_csmar_excel(file.path(config$fama_dir, spec$fama_file))
  market_obj <- read_csmar_excel(file.path(config$market_dir, spec$market_file))

  fama <- prepare_fama(fama_obj, spec$fama_date_col, spec$frequency)
  market <- prepare_market(market_obj, spec$market_date_col,
                           spec$market_return_col, spec$frequency)
  rf <- prepare_risk_free(rf_obj, spec$frequency)
  rf_construction <- attr(rf, "construction")

  fama_data <- deduplicate_keys(
    fama$data,
    c("JoinKey", "MarkettypeID"),
    paste0(spec$frequency, " Fama")
  )
  market <- deduplicate_keys(
    market,
    "JoinKey",
    paste0(spec$frequency, " Markettype 1 market return")
  )
  rf <- deduplicate_keys(rf, "JoinKey", paste0(spec$frequency, " risk-free"))

  merged <- merge(
    fama_data,
    market[, c("JoinKey", "Markettype", "MarketReturn"), drop = FALSE],
    by = "JoinKey",
    all.x = TRUE
  )
  unmatched_market <- sum(is.na(merged$MarketReturn))

  merged <- merge(
    merged,
    rf[, c("JoinKey", "RfDate", "Nrrdata", "RiskFreeRate_SourcePercent",
           "RiskFreeRate"), drop = FALSE],
    by = "JoinKey",
    all.x = TRUE
  )
  unmatched_rf <- sum(is.na(merged$RiskFreeRate))
  merged$RiskPremium <- merged$MarketReturn - merged$RiskFreeRate

  mp <- market_premium_data[, c("JoinKey", "MarketReturn", "RiskFreeRate",
                                "MarketPremium", "MarketClose", "RfDate",
                                "RiskFreeAnnualPercent"), drop = FALSE]
  names(mp) <- c("JoinKey", "IndexMarketReturn", "IndexRiskFreeRate",
                 "MarketPremium", "IndexClose", "MacroRfDate",
                 "MacroRiskFreeAnnualPercent")
  merged <- merge(merged, mp, by = "JoinKey", all.x = TRUE)
  unmatched_index_market <- sum(is.na(merged$IndexMarketReturn))
  unmatched_macro_rf <- sum(is.na(merged$IndexRiskFreeRate))
  missing_market_premium <- sum(is.na(merged$MarketPremium))

  if (spec$frequency %in% c("Weekly", "Monthly")) {
    merged$Date <- merged$RfDate
  }

  output <- merged[, c("Date", "FrequencyID", "MarkettypeID",
                       "Markettype", "MarketReturn",
                       "RiskFreeRate", "RiskPremium",
                       "IndexMarketReturn", "IndexRiskFreeRate",
                       "MarketPremium",
                       fama$factor_cols), drop = FALSE]
  rows_before_market_premium_filter <- nrow(output)
  output <- output[is.finite(output$IndexRiskFreeRate) &
                     is.finite(output$MarketPremium), ,
                   drop = FALSE]
  output <- output[order(output$Date, output$MarkettypeID), , drop = FALSE]

  output_path <- file.path(config$output_dir, spec$output_file)
  writexl::write_xlsx(output, output_path)

  validation_detail <- merged[, c("JoinKey", "Date", "FrequencyID",
                                  "MarkettypeID", "Markettype",
                                  "MarketReturn", "RfDate",
                                  "RiskFreeRate", "RiskPremium",
                                  "IndexMarketReturn", "MacroRfDate",
                                  "IndexRiskFreeRate", "MarketPremium",
                                  "IndexClose",
                                  "MacroRiskFreeAnnualPercent"),
                              drop = FALSE]
  validation_detail$MissingMarketReturn <- is.na(validation_detail$MarketReturn)
  validation_detail$MissingRiskFreeRate <- is.na(validation_detail$RiskFreeRate)
  validation_detail$MissingRiskPremium <- is.na(validation_detail$RiskPremium)
  validation_detail$MissingIndexMarketReturn <-
    is.na(validation_detail$IndexMarketReturn)
  validation_detail$MissingMacroRiskFreeRate <-
    is.na(validation_detail$IndexRiskFreeRate)
  validation_detail$MissingMarketPremium <- is.na(validation_detail$MarketPremium)
  validation_detail_path <- file.path(
    config$output_dir,
    paste0("FamaFactors_", spec$frequency, "_ValidationDetail.csv")
  )
  write.csv(validation_detail, validation_detail_path, row.names = FALSE)

  date_values <- output$Date[!is.na(output$Date)]
  market_construction <- paste(
    "MarketReturn uses TRD market return where Markettype = 1",
    "(上证A股), as requested."
  )

  validation <- data.frame(
    Frequency = spec$frequency,
    Fama_input_rows = fama_obj$row_count,
    Market_input_rows = market_obj$row_count,
    Riskfree_input_rows = rf_obj$row_count,
    Fama_Portfolios1_P9709_rows = fama$rows_after_filter,
    Rows_before_market_premium_filter = rows_before_market_premium_filter,
    Output_rows = nrow(output),
    Date_start = if (length(date_values) > 0) min(date_values) else as.Date(NA),
    Date_end = if (length(date_values) > 0) max(date_values) else as.Date(NA),
    Missing_RiskPremium = sum(is.na(output$RiskPremium)),
    Missing_MarketPremium = sum(is.na(output$MarketPremium)),
    Unmatched_market_rows = unmatched_market,
    Unmatched_riskfree_rows = unmatched_rf,
    Unmatched_index_market_rows = unmatched_index_market,
    Unmatched_macro_riskfree_rows = unmatched_macro_rf,
    Riskfree_last_Clsdt_matched = spec$frequency %in% c("Weekly", "Monthly"),
    MacroRiskfree_last_date_matched = spec$frequency %in% c("Weekly", "Monthly"),
    Riskfree_construction = rf_construction,
    MacroRiskfree_construction = attr(market_premium_data,
                                      "riskfree_construction"),
    Market_return_construction = market_construction,
    Index_market_return_construction = attr(market_premium_data,
                                            "market_construction"),
    Final_columns = paste(names(output), collapse = " | "),
    Output_file = output_path,
    Validation_detail_file = validation_detail_path
  )

  list(output = output, validation = validation)
}

specs <- list(
  list(
    frequency = "Daily",
    fama_file = "STK_MKT_FIVEFACDAY.xlsx",
    fama_date_col = "TradingDate",
    market_file = "TRD_Dalym.xlsx",
    # The request uses Trdt; the workbook column is Trddt.
    market_date_col = "Trddt",
    market_return_col = "Dretwdtl",
    output_file = "FamaFactors_Daily.xlsx"
  ),
  list(
    frequency = "Weekly",
    fama_file = "STK_MKT_FIVEFACWEEK.xlsx",
    fama_date_col = "TradingWeek",
    market_file = "TRD_Weekm.xlsx",
    # TradingWeek is represented by Trdwnt in the workbook.
    market_date_col = "Trdwnt",
    market_return_col = "Wretwdtl",
    output_file = "FamaFactors_Weekly.xlsx"
  ),
  list(
    frequency = "Monthly",
    fama_file = "STK_MKT_FIVEFACMONTH.xlsx",
    fama_date_col = "TradingMonth",
    market_file = "TRD_Mont.xlsx",
    market_date_col = "Trdmnt",
    market_return_col = "Mretwdtl",
    output_file = "FamaFactors_Monthly.xlsx"
  )
)

message("Reading risk-free rate workbook...")
rf_obj <- read_csmar_excel(file.path(config$market_dir, "TRDNEW_Nrrate.xlsx"))
message("Reading macro market premium inputs...")
index_obj <- read_regular_excel(file.path(config$macro_dir, "000001.SH.xlsx"))
macro_obj <- read_regular_excel(file.path(config$macro_dir, "macro.xlsx"))
conversion_tests <- run_market_premium_tests(config$output_dir)
index_returns <- prepare_index_returns(index_obj)
market_premium_by_frequency <- setNames(
  lapply(specs, function(spec) {
    prepare_macro_market_premium(index_returns, macro_obj, spec$frequency)
  }),
  vapply(specs, `[[`, character(1), "frequency")
)

results <- Map(
  function(spec) {
    process_frequency(
      spec,
      rf_obj = rf_obj,
      market_premium_data = market_premium_by_frequency[[spec$frequency]]
    )
  },
  specs
)
validation <- do.call(rbind, lapply(results, `[[`, "validation"))

validation_path <- file.path(config$output_dir, "FamaFactors_Validation.csv")
write.csv(validation, validation_path, row.names = FALSE)

message("\nValidation summary:")
print(validation)
message("\nOutputs written to: ", config$output_dir)
