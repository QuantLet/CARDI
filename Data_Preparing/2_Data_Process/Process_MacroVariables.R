# ============================================================
# Generate Monthly and Weekly Macro Variables
# Purpose:
#   Create Month_Macro and Week_Macro datasets from carbon market prices,
#   sector indexes, monetary-policy variables, carbon events, and market index.
# Outputs:
#   Data/Processed/Macro/Month_Macro.csv
#   Data/Processed/Macro/Month_Macro.xlsx
#   Data/Processed/Macro/Week_Macro.csv
#   Data/Processed/Macro/Week_Macro.xlsx
#   Data/Processed/Macro/MacroVariables_Validation.csv
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
    if (dir.exists(file.path(current, "Data", "raw")) &&
        dir.exists(file.path(current, "Data", "Processed"))) {
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

required_packages <- c("readxl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing required R package(s): ",
       paste(missing_packages, collapse = ", "))
}

config <- list(
  carbon_file = file.path("Data", "raw", "CarbonMarketData",
                          "CNE_CEmissRightTrade.xlsx"),
  bank_file = file.path("Data", "raw", "macro", "Bank_801780.SI.xlsx"),
  realestate_file = file.path("Data", "raw", "macro",
                              "RealEstate_801180.SI.xlsx"),
  monetary_file = file.path("Data", "raw", "macro", "macro.xlsx"),
  event_file = file.path("Data", "raw", "Important_Carbon_Events.xlsx"),
  market_index_file = file.path("Data", "raw", "macro", "000001.SH.xlsx"),
  fama_monthly_file = file.path("Data", "Processed", "FamaFactors",
                                "FamaFactors_Monthly.xlsx"),
  fama_weekly_file = file.path("Data", "Processed", "FamaFactors",
                               "FamaFactors_Weekly.xlsx"),
  output_dir = file.path("Data", "Processed", "Macro")
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
  formats <- c("%Y/%m/%d", "%Y%m%d", "%Y-%m-%d")
  for (fmt in formats) {
    missing <- is.na(out)
    if (any(missing)) {
      out[missing] <- suppressWarnings(as.Date(x_chr[missing], format = fmt))
    }
  }
  missing <- is.na(out)
  if (any(missing)) {
    serial <- suppressWarnings(as.numeric(x_chr[missing]))
    ok <- is.finite(serial) & serial > 20000 & serial < 70000
    parsed <- rep(as.Date(NA), length(serial))
    parsed[ok] <- as.Date(serial[ok], origin = "1899-12-30")
    out[missing] <- parsed
  }
  out
}

as_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

month_id <- function(date) {
  format(date, "%Y-%m")
}

week_id <- function(date) {
  format(date, "%G-%V")
}

finite_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  stats::sd(x)
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

read_carbon_daily <- function(path) {
  raw <- as.data.frame(readxl::read_excel(path, col_types = "text"),
                       stringsAsFactors = FALSE)
  required <- c("TradingDate", "CityName", "ClosePrice", "AvgPrice", "Amount")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Carbon file missing column(s): ", paste(missing, collapse = ", "))
  }
  raw <- raw[, required, drop = FALSE]
  raw$Date <- parse_date(raw$TradingDate)
  raw$ClosePrice <- as_num(raw$ClosePrice)
  raw$AvgPrice <- as_num(raw$AvgPrice)
  raw$Amount <- as_num(raw$Amount)
  raw <- raw[!is.na(raw$Date), , drop = FALSE]

  city_map <- c("深圳" = "Shenzhen", "广东" = "Guangdong", "湖北" = "Hubei")
  raw <- raw[raw$CityName %in% names(city_map), , drop = FALSE]
  raw$Market <- unname(city_map[raw$CityName])
  raw$Price <- raw$ClosePrice
  raw$Price[!is.finite(raw$Price) | raw$Price <= 0] <-
    raw$AvgPrice[!is.finite(raw$Price) | raw$Price <= 0]
  raw <- raw[is.finite(raw$Price) & raw$Price > 0, , drop = FALSE]

  # Preserve the existing project convention: when several trading products
  # exist for the same city-date, keep the first city-date observation.
  raw$.row_id <- seq_len(nrow(raw))
  raw <- raw[order(raw$Market, raw$Date, raw$.row_id), , drop = FALSE]
  raw <- raw[!duplicated(paste(raw$Market, raw$Date)), , drop = FALSE]

  out <- raw[, c("Date", "Market", "Price", "Amount"), drop = FALSE]
  out <- out[order(out$Market, out$Date), , drop = FALSE]
  do.call(rbind, lapply(split(out, out$Market), function(dat) {
    dat <- dat[order(dat$Date), , drop = FALSE]
    dat$DailyReturn <- c(NA_real_,
                         log(dat$Price[-1]) - log(head(dat$Price, -1)))
    dat
  }))
}

read_price_index <- function(path, label) {
  raw <- as.data.frame(readxl::read_excel(path, col_types = "text"),
                       stringsAsFactors = FALSE)
  date_col <- if ("日期" %in% names(raw)) {
    "日期"
  } else if ("...2" %in% names(raw)) {
    "...2"
  } else {
    stop("Could not identify date column for ", label)
  }
  close_cols <- grep("收盘价", names(raw), value = TRUE)
  price_col <- if (length(close_cols) > 0) {
    close_cols[1]
  } else if ("LogPrice" %in% names(raw)) {
    "LogPrice"
  } else {
    stop("Could not identify close price column for ", label)
  }

  out <- data.frame(
    Date = parse_date(raw[[date_col]]),
    Price = as_num(raw[[price_col]])
  )
  if (identical(price_col, "LogPrice")) {
    out$Price <- exp(out$Price)
  }
  out <- out[!is.na(out$Date) & is.finite(out$Price) & out$Price > 0,
             , drop = FALSE]
  out <- out[order(out$Date), , drop = FALSE]
  out <- out[!duplicated(out$Date), , drop = FALSE]
  out$DailyReturn <- c(NA_real_,
                       log(out$Price[-1]) - log(head(out$Price, -1)))
  out
}

read_monetary_daily <- function(path) {
  raw <- as.data.frame(readxl::read_excel(path, col_types = "text"),
                       stringsAsFactors = FALSE)
  if (ncol(raw) < 4) {
    stop("Monetary policy file has fewer than 4 columns: ", path)
  }
  dat <- raw[, 1:4, drop = FALSE]
  names(dat) <- c("Date", "TY_3m", "TY_10Y", "Shibor_3m")
  dat$Date <- parse_date(dat$Date)
  for (col in c("TY_3m", "TY_10Y", "Shibor_3m")) {
    dat[[col]] <- as_num(dat[[col]])
  }
  dat <- dat[!is.na(dat$Date), , drop = FALSE]
  dat$Slope <- dat$TY_10Y - dat$TY_3m
  dat$TED <- dat$Shibor_3m - dat$TY_3m
  dat[order(dat$Date), , drop = FALSE]
}

read_events <- function(path) {
  raw <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  required <- c("Date", "Type")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Event file missing column(s): ", paste(missing, collapse = ", "))
  }
  raw$Date <- parse_date(raw$Date)
  raw$Type <- trimws(as.character(raw$Type))
  raw <- raw[!is.na(raw$Date), , drop = FALSE]
  raw[, c("Date", "Type"), drop = FALSE]
}

add_period_id <- function(dat, frequency) {
  dat$PeriodID <- if (identical(frequency, "M")) {
    month_id(dat$Date)
  } else {
    week_id(dat$Date)
  }
  dat
}

make_market_period_panel <- function(daily, period_ref, frequency, markets,
                                     vol_prefix, return_prefix) {
  daily <- add_period_id(daily, frequency)
  out <- period_ref[, c("FrequencyID"), drop = FALSE]

  for (market in markets) {
    dat <- daily[daily$Market == market, , drop = FALSE]
    vol <- aggregate(
      dat$DailyReturn,
      by = list(FrequencyID = dat$PeriodID),
      FUN = finite_sd
    )
    names(vol)[2] <- paste0(vol_prefix, market)

    dat <- dat[order(dat$PeriodID, dat$Date), , drop = FALSE]
    last_idx <- tapply(seq_len(nrow(dat)), dat$PeriodID, tail, 1)
    end_price <- dat[as.integer(last_idx), c("PeriodID", "Price"),
                     drop = FALSE]
    names(end_price) <- c("FrequencyID", "EndPrice")
    end_price <- merge(period_ref[, c("FrequencyID"), drop = FALSE],
                       end_price, by = "FrequencyID", all.x = TRUE)
    end_price[[paste0(return_prefix, market)]] <- c(
      NA_real_,
      log(end_price$EndPrice[-1]) - log(head(end_price$EndPrice, -1))
    )
    end_price <- end_price[, c("FrequencyID", paste0(return_prefix, market)),
                           drop = FALSE]

    out <- merge(out, vol, by = "FrequencyID", all.x = TRUE)
    out <- merge(out, end_price, by = "FrequencyID", all.x = TRUE)
  }
  out
}

make_price_return <- function(daily, period_ref, frequency, output_col) {
  daily <- add_period_id(daily, frequency)
  daily <- daily[order(daily$PeriodID, daily$Date), , drop = FALSE]
  last_idx <- tapply(seq_len(nrow(daily)), daily$PeriodID, tail, 1)
  end_price <- daily[as.integer(last_idx), c("PeriodID", "Price"),
                     drop = FALSE]
  names(end_price) <- c("FrequencyID", "EndPrice")
  out <- merge(period_ref[, c("FrequencyID"), drop = FALSE],
               end_price, by = "FrequencyID", all.x = TRUE)
  out[[output_col]] <- c(
    NA_real_,
    log(out$EndPrice[-1]) - log(head(out$EndPrice, -1))
  )
  out[, c("FrequencyID", output_col), drop = FALSE]
}

make_return_vol <- function(daily, period_ref, frequency, output_col) {
  daily <- add_period_id(daily, frequency)
  vol <- aggregate(
    daily$DailyReturn,
    by = list(FrequencyID = daily$PeriodID),
    FUN = finite_sd
  )
  names(vol)[2] <- output_col
  merge(period_ref[, c("FrequencyID"), drop = FALSE],
        vol, by = "FrequencyID", all.x = TRUE)
}

make_monetary_period <- function(daily, period_ref, frequency, suffix) {
  daily <- add_period_id(daily, frequency)
  slope <- aggregate(
    daily$Slope,
    by = list(FrequencyID = daily$PeriodID),
    FUN = safe_mean
  )
  names(slope)[2] <- paste0("Slope_", suffix)
  ted <- aggregate(
    daily$TED,
    by = list(FrequencyID = daily$PeriodID),
    FUN = safe_mean
  )
  names(ted)[2] <- paste0("TED_", suffix)

  daily <- daily[order(daily$PeriodID, daily$Date), , drop = FALSE]
  last_idx <- tapply(seq_len(nrow(daily)), daily$PeriodID, tail, 1)
  ty_last <- daily[as.integer(last_idx), c("PeriodID", "TY_3m"),
                   drop = FALSE]
  names(ty_last) <- c("FrequencyID", "TY_3m_End")
  ty_last <- merge(period_ref[, c("FrequencyID"), drop = FALSE],
                   ty_last, by = "FrequencyID", all.x = TRUE)
  ty_last[[paste0("TY3M_Change_", suffix)]] <- c(
    NA_real_,
    ty_last$TY_3m_End[-1] - head(ty_last$TY_3m_End, -1)
  )
  ty_last <- ty_last[, c("FrequencyID", paste0("TY3M_Change_", suffix)),
                     drop = FALSE]

  out <- merge(period_ref[, c("FrequencyID"), drop = FALSE],
               slope, by = "FrequencyID", all.x = TRUE)
  out <- merge(out, ted, by = "FrequencyID", all.x = TRUE)
  merge(out, ty_last, by = "FrequencyID", all.x = TRUE)
}

make_event_period <- function(events, period_ref, frequency, suffix) {
  events <- add_period_id(events, frequency)
  valid_periods <- period_ref$FrequencyID
  missing_period <- !events$PeriodID %in% valid_periods
  if (any(missing_period)) {
    events$PeriodID[missing_period] <- vapply(
      events$Date[missing_period],
      function(event_date) {
        next_idx <- which(period_ref$Date >= event_date)[1]
        if (is.na(next_idx)) return(NA_character_)
        period_ref$FrequencyID[next_idx]
      },
      character(1)
    )
  }
  events <- events[!is.na(events$PeriodID), , drop = FALSE]
  event_rows <- data.frame(
    FrequencyID = events$PeriodID,
    EventDummy = 1,
    Covid = as.numeric(events$Type == "Covid"),
    China = as.numeric(events$Type == "China"),
    International = as.numeric(events$Type %in%
                                 c("International", "Covid"))
  )
  agg <- aggregate(
    event_rows[, c("EventDummy", "Covid", "China", "International")],
    by = list(FrequencyID = event_rows$FrequencyID),
    FUN = function(x) as.numeric(any(x == 1, na.rm = TRUE))
  )
  out <- merge(period_ref[, c("FrequencyID"), drop = FALSE],
               agg, by = "FrequencyID", all.x = TRUE)
  event_cols <- c("EventDummy", "Covid", "China", "International")
  for (col in event_cols) {
    out[[col]][is.na(out[[col]])] <- 0
  }
  names(out)[names(out) == "EventDummy"] <- paste0("Event_dummy_", suffix)
  names(out)[names(out) == "Covid"] <- paste0("Event_Covid_", suffix)
  names(out)[names(out) == "China"] <- paste0("Event_China_", suffix)
  names(out)[names(out) == "International"] <-
    paste0("Event_International_", suffix)
  out
}

assemble_macro <- function(period_ref, frequency, suffix, carbon_daily,
                           bank_daily, realestate_daily, monetary_daily,
                           market_daily, events) {
  carbon <- make_market_period_panel(
    daily = carbon_daily,
    period_ref = period_ref,
    frequency = frequency,
    markets = c("Shenzhen", "Guangdong", "Hubei"),
    vol_prefix = paste0("CarbonVol_", suffix, "_"),
    return_prefix = paste0("CarbonReturn_", suffix, "_")
  )
  bank <- make_price_return(bank_daily, period_ref, frequency,
                            paste0("BankReturn_", suffix))
  realestate <- make_price_return(realestate_daily, period_ref, frequency,
                                  paste0("RealEstateReturn_", suffix))
  monetary <- make_monetary_period(monetary_daily, period_ref, frequency,
                                   suffix)
  market_vol <- make_return_vol(market_daily, period_ref, frequency,
                                paste0("MarketVol_", suffix))
  event <- make_event_period(events, period_ref, frequency, suffix)

  out <- period_ref
  for (piece in list(carbon, bank, realestate, monetary, market_vol, event)) {
    out <- merge(out, piece, by = "FrequencyID", all.x = TRUE)
  }
  out <- out[order(out$Date), , drop = FALSE]
  out[[paste0("RealEstate_Premium_", suffix)]] <-
    out[[paste0("RealEstateReturn_", suffix)]] -
    out[[paste0("BankReturn_", suffix)]]

  ordered_cols <- c(
    "Date", "FrequencyID",
    paste0("CarbonVol_", suffix, "_Shenzhen"),
    paste0("CarbonVol_", suffix, "_Guangdong"),
    paste0("CarbonVol_", suffix, "_Hubei"),
    paste0("CarbonReturn_", suffix, "_Shenzhen"),
    paste0("CarbonReturn_", suffix, "_Guangdong"),
    paste0("CarbonReturn_", suffix, "_Hubei"),
    paste0("BankReturn_", suffix),
    paste0("RealEstateReturn_", suffix),
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

validate_output <- function(dat, name, expected_cols) {
  missing_cols <- setdiff(expected_cols, names(dat))
  duplicate_dates <- sum(duplicated(dat$Date))
  data.frame(
    Dataset = name,
    Rows = nrow(dat),
    Columns = ncol(dat),
    StartDate = min(dat$Date, na.rm = TRUE),
    EndDate = max(dat$Date, na.rm = TRUE),
    DuplicateDates = duplicate_dates,
    MissingExpectedColumns = if (length(missing_cols) == 0) {
      ""
    } else {
      paste(missing_cols, collapse = ";")
    },
    MissingCells = sum(is.na(dat)),
    EventDummyCount = sum(
      dat[[grep("Event_dummy", names(dat), value = TRUE)]],
      na.rm = TRUE
    )
  )
}

message("Reading input data...")
monthly_ref <- read_fama_reference(config$fama_monthly_file)
weekly_ref <- read_fama_reference(config$fama_weekly_file)
carbon_daily <- read_carbon_daily(config$carbon_file)
bank_daily <- read_price_index(config$bank_file, "bank sector index")
realestate_daily <- read_price_index(config$realestate_file,
                                     "real estate sector index")
monetary_daily <- read_monetary_daily(config$monetary_file)
market_daily <- read_price_index(config$market_index_file,
                                 "Shanghai Composite index")
events <- read_events(config$event_file)

message("Constructing monthly macro dataset...")
month_macro <- assemble_macro(
  period_ref = monthly_ref,
  frequency = "M",
  suffix = "M",
  carbon_daily = carbon_daily,
  bank_daily = bank_daily,
  realestate_daily = realestate_daily,
  monetary_daily = monetary_daily,
  market_daily = market_daily,
  events = events
)

message("Constructing weekly macro dataset...")
week_macro <- assemble_macro(
  period_ref = weekly_ref,
  frequency = "W",
  suffix = "W",
  carbon_daily = carbon_daily,
  bank_daily = bank_daily,
  realestate_daily = realestate_daily,
  monetary_daily = monetary_daily,
  market_daily = market_daily,
  events = events
)

expected_month_cols <- names(month_macro)
expected_week_cols <- names(week_macro)

if (any(duplicated(month_macro$Date))) {
  stop("Month_Macro has duplicate Date values.")
}
if (any(duplicated(week_macro$Date))) {
  stop("Week_Macro has duplicate Date values.")
}

write_outputs(month_macro, "Month_Macro")
write_outputs(week_macro, "Week_Macro")

validation <- rbind(
  validate_output(month_macro, "Month_Macro", expected_month_cols),
  validate_output(week_macro, "Week_Macro", expected_week_cols)
)
validation$InputCarbonRows <- nrow(carbon_daily)
validation$InputBankRows <- nrow(bank_daily)
validation$InputRealEstateRows <- nrow(realestate_daily)
validation$InputMonetaryRows <- nrow(monetary_daily)
validation$InputMarketRows <- nrow(market_daily)
validation$InputEventRows <- nrow(events)
validation$ReturnConvention <- "log price difference"
validation$CarbonPriceConvention <- "ClosePrice; AvgPrice fallback if ClosePrice missing"
validation$CarbonDuplicateConvention <- "first city-date observation"
validation$EventTypeConvention <- "Event_dummy any event; Covid counted as International"
write.csv(validation,
          file.path(config$output_dir, "MacroVariables_Validation.csv"),
          row.names = FALSE)

message("Macro variable generation complete.")
message("Outputs written to: ", config$output_dir)
