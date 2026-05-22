# =============================================================================
# 02_DY_Index.R
# Diebold-Yilmaz (2012) Total Connectedness Index (TCI)
# for High-Carbon (HC) and Low-Carbon (LC) Pools
#
# Method: Rolling-window VAR with generalised FEVD (Diebold & Yilmaz 2012).
#         Uses ConnectednessApproach package.
#         To keep computation tractable, selects top J stocks by average
#         market capitalisation within each pool.
#
# Inputs:
#   Data/Processed/Input/HighCarbonIntens/20140704-20250127/HighCarbonIntens_Price_20250127.csv
#   Data/Processed/Input/HighCarbonIntens/20140704-20250127/HighCarbonIntens_Mktcap_20250127.csv
#   (same for LowCarbonIntens)
#
# Outputs:
#   Output/NewIndicators/Daily/DY_HC.csv
#   Output/NewIndicators/Daily/DY_LC.csv
#   Output/NewIndicators/Monthly/DY_Monthly.csv
# =============================================================================

rm(list = ls(all = TRUE))

libraries <- c("dplyr", "zoo", "ConnectednessApproach")
missing_libraries <- libraries[!vapply(libraries, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_libraries) > 0) {
  stop("Missing required package(s): ",
       paste(missing_libraries, collapse = ", "),
       ". Please install them before running this script.")
}
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))

# ---- Parameters ----
wdir             <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
date_start_source <- 20140704
date_end_source   <- 20250127
J                 <- as.integer(Sys.getenv("DY_J", unset = "50"))
nlag              <- as.integer(Sys.getenv("DY_NLAG", unset = "1"))
nfore             <- as.integer(Sys.getenv("DY_NFORE", unset = "10"))
window_size       <- as.integer(Sys.getenv("DY_WINDOW", unset = "200"))
dy_engine         <- Sys.getenv("DY_ENGINE", unset = "robust")
output_suffix     <- Sys.getenv("DY_OUTPUT_SUFFIX", unset = "")

if (!is.finite(J) || J < 2) stop("DY_J must be an integer >= 2.")
if (!is.finite(nlag) || nlag < 1) stop("DY_NLAG must be an integer >= 1.")
if (!is.finite(nfore) || nfore < 1) stop("DY_NFORE must be an integer >= 1.")
if (!is.finite(window_size) || window_size < 30) {
  stop("DY_WINDOW must be an integer >= 30.")
}
if (!dy_engine %in% c("robust", "package")) {
  stop("DY_ENGINE must be either 'robust' or 'package'.")
}

setwd(wdir)

dir.create("Output/NewIndicators/Daily",   showWarnings = FALSE, recursive = TRUE)
dir.create("Output/NewIndicators/Monthly", showWarnings = FALSE, recursive = TRUE)

safe_numeric_matrix <- function(df) {
  out <- as.matrix(df)
  mode(out) <- "numeric"
  out
}

extract_tci_values <- function(dca) {
  if (is.null(dca$TCI)) stop("ConnectednessApproach output has no TCI element.")
  tci <- dca$TCI

  if (is.data.frame(tci)) {
    numeric_cols <- names(tci)[vapply(tci, is.numeric, logical(1))]
    if ("TCI" %in% numeric_cols) {
      return(as.numeric(tci[["TCI"]]))
    }
    if (length(numeric_cols) == 0) {
      stop("TCI data frame has no numeric column.")
    }
    return(as.numeric(tci[[numeric_cols[1]]]))
  }

  if (is.matrix(tci)) {
    if ("TCI" %in% colnames(tci)) return(as.numeric(tci[, "TCI"]))
    if (ncol(tci) == 1) return(as.numeric(tci[, 1]))
    return(as.numeric(tci[, 1]))
  }

  as.numeric(tci)
}

safe_ratio <- function(x, y) {
  out <- x / y
  out[!is.finite(out)] <- NA_real_
  out
}

output_file <- function(folder, stem) {
  file.path(folder, paste0(stem, output_suffix, ".csv"))
}

robust_var_fit <- function(x, nlag) {
  x_mat <- zoo::coredata(x)
  k <- ncol(x_mat)
  if (nrow(x_mat) <= nlag + 2L) {
    stop("Not enough observations for VAR fit.")
  }

  z <- stats::embed(x_mat, nlag + 1L)
  y_mat <- z[, seq_len(k), drop = FALSE]
  x_lag <- z[, -seq_len(k), drop = FALSE]
  x_design <- cbind(Intercept = 1, x_lag)

  coef_mat <- qr.coef(qr(x_design), y_mat)
  coef_mat[!is.finite(coef_mat)] <- 0
  B <- t(coef_mat[-1, , drop = FALSE])
  residuals <- y_mat - x_design %*% coef_mat

  residuals[!is.finite(residuals)] <- 0
  Q <- crossprod(residuals) / nrow(residuals)
  if (any(!is.finite(Q))) {
    Q <- diag(k)
  }
  Q <- Q + diag(1e-10, k)

  list(B = B, Q = Q)
}

robust_connectedness <- function(data_zoo, nlag, nfore, window_size) {
  k <- ncol(data_zoo)
  total_obs <- nrow(data_zoo)
  rolling_width <- window_size - nlag
  if (rolling_width <= nlag + 2L) {
    stop("window_size is too small relative to nlag.")
  }
  t0 <- total_obs - rolling_width + 1L
  if (t0 <= 0) {
    stop("Not enough observations for rolling DY connectedness.")
  }

  B_t <- array(NA_real_, c(k, k * nlag, t0))
  Q_t <- array(NA_real_, c(k, k, t0))

  for (i in seq_len(t0)) {
    window_data <- data_zoo[i:(i + rolling_width - 1L), ]
    fit <- robust_var_fit(window_data, nlag)
    B_t[, , i] <- fit$B
    Q_t[, , i] <- fit$Q
    if (i %% 100 == 0 || i == t0) {
      cat("    Robust VAR window", i, "/", t0, "\n")
    }
  }

  dates <- as.character(zoo::index(data_zoo))
  date <- dates[(length(dates) - dim(Q_t)[3] + 1):length(dates)]
  nms <- colnames(data_zoo)
  dimnames(Q_t)[[1]] <- dimnames(Q_t)[[2]] <- nms
  dimnames(Q_t)[[3]] <- as.character(date)

  ConnectednessApproach:::TimeConnectedness(
    Phi = B_t,
    Sigma = Q_t,
    nfore = nfore,
    generalized = TRUE,
    corrected = FALSE
  )
}

run_dy_connectedness <- function(data_zoo) {
  if (identical(dy_engine, "robust")) {
    return(robust_connectedness(data_zoo, nlag, nfore, window_size))
  }

  tryCatch(
    ConnectednessApproach(
      data_zoo,
      nlag        = nlag,
      nfore       = nfore,
      window.size = window_size,
      model       = "VAR",
      connectedness = "Time",
      Connectedness_config = list(
        TimeConnectedness = list(generalized = TRUE)
      )
    ),
    error = function(e) {
      message("  Package VAR failed: ", conditionMessage(e))
      message("  Falling back to robust fixed-dimension VAR loop.")
      robust_connectedness(data_zoo, nlag, nfore, window_size)
    }
  )
}

# ---- Helper: compute DY TCI for one pool ----
compute_dy_index <- function(channel) {

  input_path <- paste0("Data/Processed/Input/", channel, "/",
                       date_start_source, "-", date_end_source)

  prices <- read.csv(paste0(input_path, "/", channel, "_Price_", date_end_source, ".csv"),
                     header = TRUE, check.names = FALSE)
  colnames(prices)[1] <- "Date"

  mktcap <- read.csv(paste0(input_path, "/", channel, "_Mktcap_", date_end_source, ".csv"),
                     header = TRUE, check.names = FALSE)
  colnames(mktcap)[1] <- "Date"

  # Align mktcap rows to price dates (skip first row to match returns)
  common_stocks <- intersect(colnames(prices)[-1], colnames(mktcap)[-1])
  prices  <- prices[,  c("Date", common_stocks)]
  mktcap  <- mktcap[,  c("Date", common_stocks)]

  # Select top J by average market cap across all dates
  mktcap_mat <- safe_numeric_matrix(mktcap[, -1])
  avg_mktcap <- colMeans(mktcap_mat, na.rm = TRUE)
  avg_mktcap <- avg_mktcap[is.finite(avg_mktcap) & avg_mktcap > 0]
  if (length(avg_mktcap) < 2) {
    warning(paste(channel, ": fewer than 2 stocks with positive market cap."))
    return(data.frame(Date = prices$Date[-1], DY_TCI = NA_real_))
  }
  top_j <- names(sort(avg_mktcap, decreasing = TRUE))[seq_len(min(J, length(avg_mktcap)))]

  prices <- prices[, c("Date", top_j)]

  # Log returns
  price_mat <- safe_numeric_matrix(prices[, -1])
  price_mat[!is.finite(price_mat) | price_mat <= 0] <- NA_real_
  log_ret <- diff(log(price_mat))
  log_ret[is.na(log_ret) | is.infinite(log_ret)] <- 0
  dates_ret <- prices$Date[-1]

  # Annualised volatility return (as in reference: (r^2 * 0.361 * 365)^0.5 * 100)
  vol_ret <- (log_ret^2 * 0.361 * 365)^0.5 * 100

  # Filter stocks: remove any column with >33% zeros across the FULL series
  # (Chinese stocks often have trading halts → all-zero windows cause VAR to fail)
  zero_ratio <- colMeans(vol_ret == 0, na.rm = TRUE)
  vol_ret    <- vol_ret[, zero_ratio <= 1/3, drop = FALSE]

  if (ncol(vol_ret) < 2) {
    warning(paste(channel, ": fewer than 2 valid stocks after filtering."))
    return(data.frame(Date = dates_ret, DY_TCI = NA_real_))
  }

  # Cap at J stocks (re-select after filtering)
  if (ncol(vol_ret) > J) vol_ret <- vol_ret[, 1:J, drop = FALSE]
  if (nrow(vol_ret) <= window_size + nlag) {
    stop(channel, ": not enough observations for DY connectedness. ",
         "Need more than window_size + nlag observations; have ",
         nrow(vol_ret), ".")
  }
  cat("  Using", ncol(vol_ret), "stocks for", channel, "\n")

  # Use the actual return dates as the time index so TCI alignment is traceable.
  time_idx  <- as.Date(dates_ret)
  data_zoo  <- zoo::zoo(vol_ret, order.by = time_idx)

  cat("  Running ConnectednessApproach for", channel, "...\n")
  dca <- run_dy_connectedness(data_zoo)

  tci_values <- extract_tci_values(dca)
  tci_values <- tci_values[is.finite(tci_values)]
  # Align TCI dates to the last `nrow(tci_values)` actual dates
  n_tci <- length(tci_values)
  if (n_tci == 0) {
    warning(paste(channel, ": ConnectednessApproach returned no finite TCI values."))
    return(data.frame(Date = dates_ret, DY_TCI = NA_real_))
  }
  if (n_tci > length(dates_ret)) {
    stop(channel, ": TCI length exceeds return-date length.")
  }
  aligned_dates <- dates_ret[(length(dates_ret) - n_tci + 1):length(dates_ret)]

  result <- data.frame(
    Date    = aligned_dates,
    DY_TCI  = as.numeric(tci_values),
    stringsAsFactors = FALSE
  )
  return(result)
}

# ---- Compute for HC and LC ----
cat("Computing HC DY Index...\n")
DY_HC <- compute_dy_index("HighCarbonIntens")
colnames(DY_HC)[2] <- "DY_TCI_HC"

cat("Computing LC DY Index...\n")
DY_LC <- compute_dy_index("LowCarbonIntens")
colnames(DY_LC)[2] <- "DY_TCI_LC"

# ---- Merge and compute HC/LC ratio ----
DY_All <- merge(DY_HC, DY_LC, by = "Date", all = TRUE)
DY_All$DY_HL_Ratio <- safe_ratio(DY_All$DY_TCI_HC, DY_All$DY_TCI_LC)

# ---- Save daily outputs ----
write.csv(DY_HC,  output_file("Output/NewIndicators/Daily", "DY_HC"),
          row.names = FALSE, quote = FALSE)
write.csv(DY_LC,  output_file("Output/NewIndicators/Daily", "DY_LC"),
          row.names = FALSE, quote = FALSE)
write.csv(DY_All, output_file("Output/NewIndicators/Daily", "DY_All"),
          row.names = FALSE, quote = FALSE)
cat("Saved daily DY outputs.\n")

# ---- Monthly aggregation ----
DY_All$YearMonth <- substr(DY_All$Date, 1, 7)

DY_Monthly <- DY_All %>%
  filter(!is.na(DY_TCI_HC), !is.na(DY_TCI_LC)) %>%
  group_by(YearMonth) %>%
  summarise(
    DY_TCI_HC  = mean(DY_TCI_HC[is.finite(DY_TCI_HC)], na.rm = TRUE),
    DY_TCI_LC  = mean(DY_TCI_LC[is.finite(DY_TCI_LC)], na.rm = TRUE),
    DY_HL_Ratio = mean(DY_HL_Ratio[is.finite(DY_HL_Ratio)], na.rm = TRUE),
    .groups = "drop"
  )

write.csv(DY_Monthly, output_file("Output/NewIndicators/Monthly", "DY_Monthly"),
          row.names = FALSE, quote = FALSE)
cat("Saved monthly DY outputs.\n")
cat("DY Index: DONE\n")
