# =============================================================================
# 01_Volatility_Index.R
# Rolling Volatility Index for High-Carbon (HC) and Low-Carbon (LC) Pools
#
# Two volatility series are computed for each pool:
#
#   Vol      - Cross-sectional mean of 63-day rolling std of individual
#              stock log returns.
#              Source: price CSV → log returns → rolling std → mean
#
#   VolVaR   - Cross-sectional mean of 63-day rolling std of individual
#              stock VaR (quantile-regression fitted values at tau=5%).
#              Source: Output/{channel}/FitQr/Volatility_VaR5.csv
#                      (pre-computed by FRM pipeline)
#              Fallback: recompute from FitQr_{channel}.rds if CSV absent
#
# Inputs:
#   Data/Processed/Input/HighCarbonIntens/20140704-20250127/HighCarbonIntens_Price_20250127.csv
#   Data/Processed/Input/LowCarbonIntens/20140704-20250127/LowCarbonIntens_Price_20250127.csv
#   Output/HighCarbonIntens/FitQr/Volatility_VaR5.csv
#   Output/LowCarbonIntens/FitQr/Volatility_VaR5.csv
#
# Outputs:
#   Output/NewIndicators/Daily/Volatility_All.csv
#   Output/NewIndicators/Monthly/Volatility_Monthly.csv
# =============================================================================

rm(list = ls(all = TRUE))

libraries <- c("dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) install.packages(x))
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# ---- Parameters ----
wdir              <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
date_start_source <- 20140704
date_end_source   <- 20250127
s                 <- 63     # rolling window (trading days)

setwd(wdir)

dir.create("Output/NewIndicators/Daily",   showWarnings = FALSE, recursive = TRUE)
dir.create("Output/NewIndicators/Monthly", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Part 1 — Return-based Volatility
# =============================================================================

compute_vol_index <- function(channel) {

  input_path <- paste0("Data/Processed/Input/", channel, "/",
                       date_start_source, "-", date_end_source, "/",
                       channel, "_Price_", date_end_source, ".csv")

  prices <- read.csv(input_path, header = TRUE, check.names = FALSE)
  colnames(prices)[1] <- "Date"
  prices$Date <- as.character(prices$Date)

  price_mat <- as.matrix(prices[, -1])
  mode(price_mat) <- "numeric"

  log_ret <- diff(log(price_mat))
  log_ret[is.na(log_ret) | is.infinite(log_ret)] <- 0
  dates_ret <- prices$Date[-1]

  N         <- nrow(log_ret)
  vol_index <- rep(NA_real_, N)

  for (i in seq(s, N)) {
    window_ret <- log_ret[(i - s + 1):i, , drop = FALSE]
    vol_index[i] <- mean(apply(window_ret, 2, sd, na.rm = TRUE), na.rm = TRUE)
  }

  data.frame(Date = dates_ret, Vol = vol_index, stringsAsFactors = FALSE)
}

# =============================================================================
# Part 2 — Volatility of VaR
#
# Primary: read Volatility_VaR5.csv (pre-computed rolling std of per-stock VaR
#          at tau=5%, produced by the main FRM pipeline).
#          CSV layout: [row_index | date | stock_1 | stock_2 | ...]
#
# Fallback: reshape FitQr_{channel}.rds → wide matrix of daily VaR values →
#           rolling 63-day std per stock → cross-sectional mean.
# =============================================================================

compute_volvar_index <- function(channel) {

  csv_path <- paste0("Output/", channel, "/FitQr/Volatility_VaR5.csv")
  rds_path <- paste0("Output/", channel, "/FitQr/FitQr_", channel, ".rds")

  # ---- Primary: pre-computed CSV ----
  if (file.exists(csv_path)) {
    cat("  Reading pre-computed Volatility_VaR5.csv for", channel, "...\n")

    var_vol <- read.csv(csv_path, header = TRUE, check.names = FALSE)

    # Drop leading row-index column if present (unnamed first column)
    if (colnames(var_vol)[1] %in% c("", "X", "V1") ||
        grepl("^[0-9]+$", as.character(var_vol[1, 1]))) {
      var_vol <- var_vol[, -1]
    }

    colnames(var_vol)[1] <- "Date"
    var_vol$Date <- as.character(var_vol$Date)

    num_mat <- as.matrix(var_vol[, -1])
    mode(num_mat) <- "numeric"

    # Cross-sectional mean of per-stock VaR volatilities
    volvar_index <- apply(num_mat, 1, mean, na.rm = TRUE)
    volvar_index[is.nan(volvar_index)] <- NA_real_

    return(data.frame(Date = var_vol$Date, VolVaR = volvar_index,
                      stringsAsFactors = FALSE))
  }

  # ---- Fallback: compute from FitQr RDS ----
  if (!file.exists(rds_path)) {
    warning(paste("Neither Volatility_VaR5.csv nor FitQr RDS found for", channel))
    return(NULL)
  }

  cat("  Computing VolVaR from FitQr RDS for", channel, "...\n")
  FitQr <- readRDS(rds_path)
  N_h   <- length(FitQr)

  # Reshape list → wide matrix (dates × stocks)
  stock_names <- unique(unlist(lapply(FitQr, function(x) colnames(x))))
  N_names     <- length(stock_names)
  wide_mat    <- matrix(NA_real_, N_h, N_names)
  colnames(wide_mat) <- stock_names

  for (k in seq_len(N_names)) {
    for (t in seq_len(N_h)) {
      sn <- stock_names[k]
      if (sn %in% colnames(FitQr[[t]])) {
        wide_mat[t, k] <- FitQr[[t]][1, sn]   # single fitted VaR value per date
      }
    }
  }

  dates_fitqr <- names(FitQr)

  # Drop stocks with >10% missing
  na_ratio <- colMeans(is.na(wide_mat))
  wide_mat <- wide_mat[, na_ratio < 0.1, drop = FALSE]

  # Rolling 63-day std of each stock's VaR series
  var_vol_mat <- matrix(NA_real_, N_h, ncol(wide_mat))
  for (i in seq(s, N_h)) {
    win <- wide_mat[(i - s + 1):i, , drop = FALSE]
    var_vol_mat[i, ] <- apply(win, 2, sd, na.rm = TRUE)
  }

  # Cross-sectional mean
  volvar_index <- apply(var_vol_mat, 1, mean, na.rm = TRUE)
  volvar_index[is.nan(volvar_index)] <- NA_real_

  data.frame(Date = dates_fitqr, VolVaR = volvar_index,
             stringsAsFactors = FALSE)
}

# =============================================================================
# Compute both series for HC and LC
# =============================================================================

cat("Computing HC Return Volatility...\n")
Vol_HC <- compute_vol_index("HighCarbonIntens")
colnames(Vol_HC)[2] <- "Vol_HC"

cat("Computing LC Return Volatility...\n")
Vol_LC <- compute_vol_index("LowCarbonIntens")
colnames(Vol_LC)[2] <- "Vol_LC"

cat("Computing HC VaR Volatility...\n")
VolVaR_HC <- compute_volvar_index("HighCarbonIntens")
if (!is.null(VolVaR_HC)) colnames(VolVaR_HC)[2] <- "VolVaR_HC"

cat("Computing LC VaR Volatility...\n")
VolVaR_LC <- compute_volvar_index("LowCarbonIntens")
if (!is.null(VolVaR_LC)) colnames(VolVaR_LC)[2] <- "VolVaR_LC"

# =============================================================================
# Merge and compute HC/LC ratios
# =============================================================================

Vol_All <- merge(Vol_HC, Vol_LC, by = "Date", all = TRUE)
Vol_All$Vol_HL_Ratio <- Vol_All$Vol_HC / Vol_All$Vol_LC

if (!is.null(VolVaR_HC) && !is.null(VolVaR_LC)) {
  VolVaR_All <- merge(VolVaR_HC, VolVaR_LC, by = "Date", all = TRUE)
  VolVaR_All$VolVaR_HL_Ratio <- VolVaR_All$VolVaR_HC / VolVaR_All$VolVaR_LC
  Vol_All <- merge(Vol_All, VolVaR_All, by = "Date", all = TRUE)
}

# =============================================================================
# Save daily outputs
# =============================================================================

write.csv(Vol_All, "Output/NewIndicators/Daily/Volatility_All.csv",
          row.names = FALSE, quote = FALSE)
cat("Saved: Output/NewIndicators/Daily/Volatility_All.csv\n")

# =============================================================================
# Monthly aggregation (mean within calendar month)
# =============================================================================

Vol_All$YearMonth <- substr(Vol_All$Date, 1, 7)

numeric_cols <- names(Vol_All)[sapply(Vol_All, is.numeric)]

Vol_Monthly <- Vol_All %>%
  group_by(YearMonth) %>%
  summarise(across(all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

write.csv(Vol_Monthly, "Output/NewIndicators/Monthly/Volatility_Monthly.csv",
          row.names = FALSE, quote = FALSE)
cat("Saved: Output/NewIndicators/Monthly/Volatility_Monthly.csv\n")
cat("Volatility Index: DONE\n")
