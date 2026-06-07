# =============================================================================
# 03_EDC_CVaR_Index.R
# Extreme Downside Co-movement (EDC) and Conditional Value-at-Risk (CVaR)
# Indices for High-Carbon (HC) and Low-Carbon (LC) Pools
#
# Methods:
#   EDC  - Rolling cosine similarity of negative excess returns across all
#           stock pairs (Ref_EDC_EDH.R, "EDC_simp").
#          For each pair (k, j):
#            EDC_kj = sum(d_k * d_j) / (||d_k|| * ||d_j||)
#          where d_k = min(r_k - mean(r_k), 0)
#          Pool EDC = mean over all k≠j pairs
#
#   CVaR - Rolling Expected Shortfall (ES) at tau=5% for each stock,
#           then cross-sectional mean across stocks.
#          Uses cvar::ES() with empirical CDF.
#
# Inputs:
#   Data/Processed/Input/HighCarbonIntens/20140704-20250127/HighCarbonIntens_Price_20250127.csv
#   Data/Processed/Input/LowCarbonIntens/20140704-20250127/LowCarbonIntens_Price_20250127.csv
#
# Outputs:
#   Output/NewIndicators/Daily/EDC_All.csv
#   Output/NewIndicators/Daily/CVaR_All.csv
#   Output/NewIndicators/Monthly/EDC_Monthly.csv
#   Output/NewIndicators/Monthly/CVaR_Monthly.csv
# =============================================================================

rm(list = ls(all = TRUE))

libraries <- c("dplyr", "cvar")
lapply(libraries, function(x) if (!(x %in% installed.packages())) install.packages(x))
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# ---- Parameters ----
wdir              <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
date_start_source <- 20140704
date_end_source   <- 20250127
s                 <- 63     # rolling window (trading days, matches reference winsize=62 ~ 63)
tau               <- 0.05   # CVaR quantile level

setwd(wdir)

dir.create("Output/NewIndicators/Daily",   showWarnings = FALSE, recursive = TRUE)
dir.create("Output/NewIndicators/Monthly", showWarnings = FALSE, recursive = TRUE)

# ---- Helper: ES (CVaR) for a single series ----
fun_cvar <- function(x) {
  # Returns positive CVaR (Expected Shortfall as loss magnitude).
  # cvar::ES() returns a positive loss value; no negation needed.
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 5) return(NA_real_)
  tryCatch(
    cvar::ES(x, tau, dist.type = "cdf"),
    error = function(e) NA_real_
  )
}

# ---- Helper: compute EDC and CVaR for one pool ----
compute_edc_cvar <- function(channel) {

  input_path <- paste0("Data/Processed/Input/", channel, "/",
                       date_start_source, "-", date_end_source, "/",
                       channel, "_Price_", date_end_source, ".csv")

  prices <- read.csv(input_path, header = TRUE, check.names = FALSE)
  colnames(prices)[1] <- "Date"

  price_mat <- as.matrix(prices[, -1])
  mode(price_mat) <- "numeric"

  log_ret <- diff(log(price_mat))
  log_ret[is.na(log_ret) | is.infinite(log_ret)] <- 0
  dates_ret <- prices$Date[-1]

  N  <- nrow(log_ret)
  NS <- ncol(log_ret)

  edc_index  <- rep(NA_real_, N)
  cvar_index <- rep(NA_real_, N)

  cat("  Processing", channel, ":", N, "days,", NS, "stocks...\n")

  for (i in seq(s, N)) {

    window_ret <- log_ret[(i - s + 1):i, , drop = FALSE]

    # ---- CVaR: mean ES across stocks ----
    cvar_vals  <- apply(window_ret, 2, fun_cvar)
    cvar_index[i] <- mean(cvar_vals, na.rm = TRUE)

    # ---- EDC: cosine similarity of negative excess returns ----
    col_means  <- colMeans(window_ret, na.rm = TRUE)
    excess_ret <- sweep(window_ret, 2, col_means, "-")
    # Keep only downside deviations (clip positives to 0)
    down_ret   <- pmin(excess_ret, 0)

    # Remove stocks with all-zero downside (no tail events in window)
    col_norms  <- sqrt(colSums(down_ret^2))
    active     <- which(col_norms > 0)

    if (length(active) < 2) {
      edc_index[i] <- NA_real_
      next
    }

    down_active  <- down_ret[, active, drop = FALSE]
    norms_active <- col_norms[active]

    # Fully vectorised: normalise columns, then gram matrix = cosine sim matrix
    normed      <- sweep(down_active, 2, norms_active, "/")
    gram        <- crossprod(normed)          # K x K cosine similarity matrix
    upper_vals  <- gram[upper.tri(gram)]      # upper triangle (k != j pairs)
    edc_index[i] <- if (length(upper_vals) > 0) mean(upper_vals, na.rm = TRUE) else NA_real_

    if (i %% 200 == 0) cat("    Day", i, "/", N, "\n")
  }

  data.frame(
    Date       = dates_ret,
    EDC_Index  = edc_index,
    CVaR_Index = cvar_index,
    stringsAsFactors = FALSE
  )
}

# ---- Compute for HC and LC ----
cat("Computing HC EDC and CVaR...\n")
Result_HC <- compute_edc_cvar("HighCarbonIntens")

cat("Computing LC EDC and CVaR...\n")
Result_LC <- compute_edc_cvar("LowCarbonIntens")

# ---- Merge and compute HC/LC ratios ----
EDC_HC  <- Result_HC[, c("Date", "EDC_Index")]
CVaR_HC <- Result_HC[, c("Date", "CVaR_Index")]
colnames(EDC_HC)[2]  <- "EDC_HC"
colnames(CVaR_HC)[2] <- "CVaR_HC"

EDC_LC  <- Result_LC[, c("Date", "EDC_Index")]
CVaR_LC <- Result_LC[, c("Date", "CVaR_Index")]
colnames(EDC_LC)[2]  <- "EDC_LC"
colnames(CVaR_LC)[2] <- "CVaR_LC"

EDC_All  <- merge(EDC_HC,  EDC_LC,  by = "Date", all = TRUE)
CVaR_All <- merge(CVaR_HC, CVaR_LC, by = "Date", all = TRUE)

EDC_All$EDC_HL_Ratio   <- EDC_All$EDC_HC   / EDC_All$EDC_LC
CVaR_All$CVaR_HL_Ratio <- CVaR_All$CVaR_HC / CVaR_All$CVaR_LC

# ---- Save daily outputs ----
write.csv(EDC_All,  "Output/NewIndicators/Daily/EDC_All.csv",  row.names = FALSE, quote = FALSE)
write.csv(CVaR_All, "Output/NewIndicators/Daily/CVaR_All.csv", row.names = FALSE, quote = FALSE)
cat("Saved daily EDC and CVaR outputs.\n")

# ---- Monthly aggregation ----
monthly_agg <- function(df, val_cols) {
  df$YearMonth <- substr(df$Date, 1, 7)
  df %>%
    filter(rowSums(!is.na(df[, val_cols, drop = FALSE])) > 0) %>%
    group_by(YearMonth) %>%
    summarise(across(all_of(val_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}

EDC_Monthly  <- monthly_agg(EDC_All,  c("EDC_HC", "EDC_LC", "EDC_HL_Ratio"))
CVaR_Monthly <- monthly_agg(CVaR_All, c("CVaR_HC", "CVaR_LC", "CVaR_HL_Ratio"))

write.csv(EDC_Monthly,  "Output/NewIndicators/Monthly/EDC_Monthly.csv",
          row.names = FALSE, quote = FALSE)
write.csv(CVaR_Monthly, "Output/NewIndicators/Monthly/CVaR_Monthly.csv",
          row.names = FALSE, quote = FALSE)
cat("Saved monthly EDC and CVaR outputs.\n")
cat("EDC and CVaR Indices: DONE\n")
