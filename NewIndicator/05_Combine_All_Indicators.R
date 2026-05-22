# =============================================================================
# 05_Combine_All_Indicators.R
# Combine all new risk indicators into a single dataset
#
# Reads the daily output CSVs from scripts 01–04 and merges them by Date.
# Computes HC/LC ratios for every indicator and aggregates to monthly frequency.
#
# Prerequisites: Run scripts 01–04 first.
#
# Inputs:
#   Output/NewIndicators/Daily/Volatility_All.csv
#   Output/NewIndicators/Daily/DY_All.csv
#   Output/NewIndicators/Daily/EDC_All.csv
#   Output/NewIndicators/Daily/CVaR_All.csv
#   Output/NewIndicators/Daily/Centrality_All.csv
#
# Outputs:
#   Output/NewIndicators/Daily/All_Indicators_Daily.csv
#   Output/NewIndicators/Monthly/All_Indicators_Monthly.csv
# =============================================================================

rm(list = ls(all = TRUE))

libraries <- c("dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) install.packages(x))
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
setwd(wdir)

# ---- Load individual indicator files ----
load_indicator <- function(path) {
  if (!file.exists(path)) {
    warning(paste("File not found, skipping:", path))
    return(NULL)
  }
  df <- read.csv(path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  df$Date <- as.character(df$Date)
  # Drop YearMonth column if present (will re-create)
  df <- df[, !colnames(df) %in% "YearMonth"]
  df
}

Vol_All  <- load_indicator("Output/NewIndicators/Daily/Volatility_All.csv")
DY_All   <- load_indicator("Output/NewIndicators/Daily/DY_All.csv")
EDC_All  <- load_indicator("Output/NewIndicators/Daily/EDC_All.csv")
CVaR_All <- load_indicator("Output/NewIndicators/Daily/CVaR_All.csv")
Cent_All <- load_indicator("Output/NewIndicators/Daily/Centrality_All.csv")

# ---- Merge all by Date (left-join from Volatility as the widest series) ----
combine_list <- Filter(Negate(is.null), list(Vol_All, DY_All, EDC_All, CVaR_All, Cent_All))

All_Daily <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), combine_list)
All_Daily <- All_Daily[order(All_Daily$Date), ]

cat("Combined daily dataset:", nrow(All_Daily), "rows,", ncol(All_Daily), "columns\n")
cat("Date range:", All_Daily$Date[1], "to", All_Daily$Date[nrow(All_Daily)], "\n")

# ---- Save combined daily dataset ----
write.csv(All_Daily, "Output/NewIndicators/Daily/All_Indicators_Daily.csv",
          row.names = FALSE, quote = FALSE)
cat("Saved: Output/NewIndicators/Daily/All_Indicators_Daily.csv\n")

# ---- Monthly aggregation ----
All_Daily$YearMonth <- substr(All_Daily$Date, 1, 7)

numeric_cols <- names(All_Daily)[sapply(All_Daily, is.numeric)]

All_Monthly <- All_Daily %>%
  group_by(YearMonth) %>%
  summarise(across(all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

write.csv(All_Monthly, "Output/NewIndicators/Monthly/All_Indicators_Monthly.csv",
          row.names = FALSE, quote = FALSE)
cat("Saved: Output/NewIndicators/Monthly/All_Indicators_Monthly.csv\n")

# ---- Summary table of available columns ----
cat("\n=== Available indicators in combined dataset ===\n")
ratio_cols <- grep("_Ratio$", colnames(All_Daily), value = TRUE)
hc_cols    <- grep("_HC$",    colnames(All_Daily), value = TRUE)
lc_cols    <- grep("_LC$",    colnames(All_Daily), value = TRUE)

cat("HC series:", paste(hc_cols, collapse = ", "), "\n")
cat("LC series:", paste(lc_cols, collapse = ", "), "\n")
cat("HC/LC ratios:", paste(ratio_cols, collapse = ", "), "\n")
cat("Combine All Indicators: DONE\n")
