# =============================================================================
# CARDI_Construct.R
#
# Purpose:
#   Construct daily CARDI ratios from local FRM outputs for high- and
#   low-carbon-intensity firms.
#
# Inputs:
#   Output/HighCarbonIntens/Lambda/FRM_HighCarbonIntens_index.csv
#   Output/LowCarbonIntens/Lambda/FRM_LowCarbonIntens_index.csv
#   Output/*/Sensitivity/tau=1/s=63/Lambda/FRM_*_index.csv
#   Output/*/Sensitivity/tau=10/s=63/Lambda/FRM_*_index.csv
#
# Outputs:
#   Output/FRM_Carbon_risk.csv
#   Output/FRM_Carbon_risk.rds
# =============================================================================

rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

# ----------------------------- Configuration ---------------------------------

project_root <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Data_Preparing/1_Cardi_Construction"
setwd(project_root)

output_dir <- file.path(project_root, "Output")
channel <- c("HighCarbonIntens", "LowCarbonIntens")

tau_files <- list(
  `5` = function(group) file.path(output_dir, group, "Lambda",
                                  paste0("FRM_", group, "_index.csv")),
  `1` = function(group) file.path(output_dir, group, "Sensitivity", "tau=1",
                                  "s=63", "Lambda",
                                  paste0("FRM_", group, "_index.csv")),
  `10` = function(group) file.path(output_dir, group, "Sensitivity", "tau=10",
                                   "s=63", "Lambda",
                                   paste0("FRM_", group, "_index.csv"))
)

csv_output <- file.path(output_dir, "FRM_Carbon_risk.csv")
rds_output <- file.path(output_dir, "FRM_Carbon_risk.rds")

# ------------------------------- Utilities -----------------------------------

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x), tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d"))
}

read_frm_index <- function(path, tau, group) {
  if (!file.exists(path)) stop("Missing local FRM input file: ", path, call. = FALSE)
  dat <- read.csv(path, check.names = FALSE)
  if (ncol(dat) < 2) stop("FRM input has fewer than two columns: ", path, call. = FALSE)

  names(dat)[1] <- "Date"
  dat$Date <- parse_date(dat$Date)
  value_col <- names(dat)[2]
  out <- dat[, c("Date", value_col), drop = FALSE]
  names(out)[2] <- paste0("FRM_", tau, "_", group)
  out <- out[!is.na(out$Date), , drop = FALSE]
  out[order(out$Date), , drop = FALSE]
}

merge_by_date <- function(left, right) {
  merge(left, right, by = "Date", all = FALSE)
}

# ---------------------------------- Main --------------------------------------

frm_by_group <- list()

for (group in channel) {
  frm_parts <- lapply(names(tau_files), function(tau) {
    read_frm_index(tau_files[[tau]](group), tau, group)
  })
  frm_by_group[[group]] <- Reduce(merge_by_date, frm_parts)
}

frm_reg <- Reduce(merge_by_date, frm_by_group)
frm_reg <- frm_reg[order(frm_reg$Date), , drop = FALSE]

frm_reg$CARDI_5P <- frm_reg$FRM_5_HighCarbonIntens / frm_reg$FRM_5_LowCarbonIntens
frm_reg$CARDI_10P <- frm_reg$FRM_10_HighCarbonIntens / frm_reg$FRM_10_LowCarbonIntens
frm_reg$CARDI_1P <- frm_reg$FRM_1_HighCarbonIntens / frm_reg$FRM_1_LowCarbonIntens
frm_reg$Year <- as.integer(format(frm_reg$Date, "%Y"))
frm_reg$Month <- as.integer(format(frm_reg$Date, "%m"))

frm_reg <- frm_reg[, c(
  "Date", "Year", "Month",
  "FRM_5_HighCarbonIntens", "FRM_1_HighCarbonIntens", "FRM_10_HighCarbonIntens",
  "FRM_5_LowCarbonIntens", "FRM_1_LowCarbonIntens", "FRM_10_LowCarbonIntens",
  "CARDI_5P", "CARDI_1P", "CARDI_10P"
)]

write.csv(frm_reg, csv_output, row.names = FALSE, quote = FALSE)
saveRDS(frm_reg, rds_output)

cat("\nCARDI construction complete.\n")
cat("Rows: ", nrow(frm_reg), "\n", sep = "")
cat("Saved CSV: ", csv_output, "\n", sep = "")
cat("Saved RDS: ", rds_output, "\n", sep = "")
