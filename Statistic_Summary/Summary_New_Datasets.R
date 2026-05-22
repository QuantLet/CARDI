################################################################################
# Summary_New_Datasets.R
#
# Purpose: Add summary statistics for monthly indicator data, HC firm monthly
#          volatility/financial data, and daily CARDI event data.
# Author: CARDI project workflow
# Date: 2026-05-19
# Dependencies: openxlsx is optional for Excel output.
################################################################################

rm(list = ls())

# ----------------------------- Configuration ---------------------------------

project_root <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
output_dir <- file.path(project_root, "Output", "Statistic_Summary")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dataset_paths <- list(
  Monthly_Indicator_Analysis = c(
    file.path(project_root, "Code", "CARDI_Test", "output", "monthly", "new_indicator_analysis_dataset_monthly.csv"),
    file.path(project_root, "new_indicator_analysis_dataset_monthly.csv")
  ),
  HC_Firm_Monthly = file.path(project_root, "Output", "FirmReturns", "HC_Firm_Monthly.rds"),
  SCARDI_Event = file.path(project_root, "Output", "Event_test", "FRM_Event.csv")
)

# ------------------------------- Utilities -----------------------------------

first_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop("None of these input paths exist: ", paste(paths, collapse = "; "), call. = FALSE)
  }
  existing[1]
}

read_dataset <- function(path) {
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    readRDS(path)
  } else {
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  }
}

is_date_like_name <- function(x) {
  grepl("(^date$|date|month|yearmonth|frequencyid)", x, ignore.case = TRUE)
}

looks_like_date_values <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(TRUE)
  if (!is.character(x)) return(FALSE)
  sample_x <- x[!is.na(x) & x != ""]
  if (length(sample_x) == 0) return(FALSE)
  sample_x <- head(sample_x, 50)
  sample_for_parse <- ifelse(grepl("^\\d{4}[-/]\\d{1,2}$", sample_x), paste0(sample_x, "-01"), sample_x)
  sample_for_parse <- gsub("/", "-", sample_for_parse)
  parsed <- tryCatch(suppressWarnings(as.Date(sample_for_parse, tryFormats = c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y-%m", "%Y/%m"
  ))), error = function(e) rep(as.Date(NA), length(sample_for_parse)))
  mean(!is.na(parsed)) > 0.8
}

is_date_like_column <- function(name, x) {
  is_date_like_name(name) || looks_like_date_values(x)
}

safe_numeric <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

summarize_one_variable <- function(x, var_name, dataset_name) {
  numeric_x <- safe_numeric(x)
  numeric_share <- mean(!is.na(numeric_x) | is.na(x))
  use_numeric_stats <- is.numeric(x) || is.integer(x) || numeric_share > 0.8

  if (use_numeric_stats) {
    finite_x <- numeric_x[is.finite(numeric_x)]
    obs <- length(finite_x)
    return(data.frame(
      Dataset = dataset_name,
      Var = var_name,
      Type = "numeric",
      Obs = obs,
      Missing = sum(is.na(x) | !is.finite(numeric_x)),
      Unique = length(unique(finite_x)),
      Mean = if (obs > 0) mean(finite_x) else NA_real_,
      Std = if (obs > 1) stats::sd(finite_x) else NA_real_,
      Min = if (obs > 0) min(finite_x) else NA_real_,
      Max = if (obs > 0) max(finite_x) else NA_real_,
      Median = if (obs > 0) stats::median(finite_x) else NA_real_,
      P25 = if (obs > 0) as.numeric(stats::quantile(finite_x, 0.25, na.rm = TRUE)) else NA_real_,
      P75 = if (obs > 0) as.numeric(stats::quantile(finite_x, 0.75, na.rm = TRUE)) else NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  non_missing <- x[!is.na(x) & x != ""]
  data.frame(
    Dataset = dataset_name,
    Var = var_name,
    Type = class(x)[1],
    Obs = length(non_missing),
    Missing = length(x) - length(non_missing),
    Unique = length(unique(non_missing)),
    Mean = NA_real_,
    Std = NA_real_,
    Min = NA_real_,
    Max = NA_real_,
    Median = NA_real_,
    P25 = NA_real_,
    P75 = NA_real_,
    stringsAsFactors = FALSE
  )
}

summarize_dataset <- function(data, dataset_name) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Requested SCARDI naming: report FRM_High_Low as CARDI.
  if (dataset_name == "SCARDI_Event" && "FRM_High_Low" %in% names(data)) {
    names(data)[names(data) == "FRM_High_Low"] <- "CARDI"
  }

  keep_cols <- names(data)[!vapply(names(data), function(nm) {
    is_date_like_column(nm, data[[nm]])
  }, logical(1))]

  if (length(keep_cols) == 0) {
    return(data.frame(
      Dataset = dataset_name,
      Var = NA_character_,
      Type = NA_character_,
      Obs = NA_integer_,
      Missing = NA_integer_,
      Unique = NA_integer_,
      Mean = NA_real_,
      Std = NA_real_,
      Min = NA_real_,
      Max = NA_real_,
      Median = NA_real_,
      P25 = NA_real_,
      P75 = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  out <- lapply(keep_cols, function(var_name) {
    summarize_one_variable(data[[var_name]], var_name, dataset_name)
  })
  do.call(rbind, out)
}

write_outputs <- function(summary_list, output_dir) {
  combined <- do.call(rbind, summary_list)

  for (dataset_name in names(summary_list)) {
    utils::write.csv(
      summary_list[[dataset_name]],
      file = file.path(output_dir, paste0("Summary_", dataset_name, ".csv")),
      row.names = FALSE,
      fileEncoding = "GB18030"
    )
  }

  combined_csv <- file.path(output_dir, "Summary_New_Datasets_All.csv")
  utils::write.csv(combined, file = combined_csv, row.names = FALSE, fileEncoding = "GB18030")

  excel_file <- NA_character_
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    excel_file <- file.path(output_dir, "Summary_New_Datasets_All.xlsx")
    wb <- openxlsx::createWorkbook()
    for (dataset_name in names(summary_list)) {
      openxlsx::addWorksheet(wb, dataset_name)
      openxlsx::writeData(wb, dataset_name, summary_list[[dataset_name]])
      openxlsx::freezePane(wb, dataset_name, firstRow = TRUE)
    }
    openxlsx::addWorksheet(wb, "All")
    openxlsx::writeData(wb, "All", combined)
    openxlsx::freezePane(wb, "All", firstRow = TRUE)
    openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
  } else {
    message("Package 'openxlsx' is not installed. Excel output skipped; CSV files were saved.")
  }

  list(combined_csv = combined_csv, excel_file = excel_file)
}

# ---------------------------------- Main --------------------------------------

summary_list <- list()
resolved_paths <- data.frame(Dataset = character(), Path = character(), stringsAsFactors = FALSE)

for (dataset_name in names(dataset_paths)) {
  input_path <- first_existing_path(dataset_paths[[dataset_name]])
  dat <- read_dataset(input_path)
  summary_list[[dataset_name]] <- summarize_dataset(dat, dataset_name)
  resolved_paths <- rbind(
    resolved_paths,
    data.frame(Dataset = dataset_name, Path = input_path, stringsAsFactors = FALSE)
  )
}

saved <- write_outputs(summary_list, output_dir)

utils::write.csv(
  resolved_paths,
  file = file.path(output_dir, "Summary_New_Datasets_Input_Paths.csv"),
  row.names = FALSE
)

cat("\nSummary statistics complete.\n")
cat("Input datasets:\n")
for (i in seq_len(nrow(resolved_paths))) {
  cat(" - ", resolved_paths$Dataset[i], ": ", resolved_paths$Path[i], "\n", sep = "")
}
cat("\nSaved combined CSV: ", saved$combined_csv, "\n", sep = "")
if (!is.na(saved$excel_file)) cat("Saved Excel workbook: ", saved$excel_file, "\n", sep = "")
