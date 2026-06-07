# =============================================================================
# Shapley_Macro.R
#
# Purpose:
#   Generate only the macro Shapley-value figure for Figure 7.
#
# Inputs:
#   Data/Processed/FRM_Carbon_risk.csv
#   Data/Processed/Input/HighCarbonIntens/20140704-20250127/HighCarbonIntens_Macro_20250127.csv
#
# Output:
#   Output/Feature_Macro.png
# =============================================================================

rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

# ----------------------------- Configuration ---------------------------------

project_root <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Figure7"
setwd(project_root)

config <- list(
  cardi_file = file.path("Data", "Processed", "FRM_Carbon_risk.csv"),
  macro_file = file.path(
    "Data", "Processed", "Input", "HighCarbonIntens",
    "20140704-20250127", "HighCarbonIntens_Macro_20250127.csv"
  ),
  cardi_target = "CARDI_5P",
  output_file = file.path("Output", "Feature_Macro.png"),
  xgb_nrounds = 100,
  random_seed = 20250607
)

dir.create(dirname(config$output_file), recursive = TRUE, showWarnings = FALSE)

# ------------------------------ Dependencies ---------------------------------

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing required package: ", pkg, call. = FALSE)
  }
  invisible(TRUE)
}

invisible(lapply(c("xgboost", "SHAPforxgboost", "ggplot2"), require_package))

# ------------------------------- Utilities -----------------------------------

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  as.Date(as.character(x), tryFormats = c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%m/%d/%Y", "%d/%m/%Y"
  ))
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

load_cardi <- function(path, target_col) {
  if (!file.exists(path)) stop("Missing CARDI file: ", path, call. = FALSE)
  cardi <- read.csv(path, check.names = FALSE)
  date_col <- intersect(c("date", "Date", "DATE"), names(cardi))[1]
  if (is.na(date_col)) stop("CARDI file must include a date column.", call. = FALSE)
  if (!target_col %in% names(cardi)) {
    stop("CARDI file is missing target column: ", target_col, call. = FALSE)
  }
  out <- data.frame(
    Date = parse_date(cardi[[date_col]]),
    CARDI_target = safe_numeric(cardi[[target_col]])
  )
  out <- out[!is.na(out$Date) & is.finite(out$CARDI_target), , drop = FALSE]
  out[order(out$Date), , drop = FALSE]
}

load_macro <- function(path) {
  if (!file.exists(path)) stop("Missing macro file: ", path, call. = FALSE)
  macro <- read.csv(path, check.names = FALSE)
  date_col <- intersect(c("Date", "date", "DATE"), names(macro))[1]
  if (is.na(date_col)) stop("Macro file must include a date column.", call. = FALSE)
  names(macro)[names(macro) == date_col] <- "Date"
  macro$Date <- parse_date(macro$Date)
  macro <- macro[!is.na(macro$Date), , drop = FALSE]
  macro_cols <- setdiff(names(macro), "Date")
  for (col in macro_cols) macro[[col]] <- safe_numeric(macro[[col]])
  macro[order(macro$Date), , drop = FALSE]
}

build_shapley_figure_data <- function(cardi, macro) {
  set.seed(config$random_seed)
  dat <- merge(cardi, macro, by = "Date", all = FALSE)
  feature_cols <- setdiff(names(dat), c("Date", "CARDI_target"))
  dat <- dat[stats::complete.cases(dat[, c("CARDI_target", feature_cols), drop = FALSE]), ,
             drop = FALSE]
  if (nrow(dat) < 20) stop("Insufficient complete observations for Shapley model.")

  x_train <- as.matrix(dat[, feature_cols, drop = FALSE])
  y_train <- dat$CARDI_target

  model <- xgboost::xgboost(
    data = x_train,
    label = y_train,
    nrounds = config$xgb_nrounds,
    objective = "reg:squarederror",
    seed = config$random_seed,
    verbose = 0
  )

  shap_values <- SHAPforxgboost::shap.prep(xgb_model = model, X_train = x_train)
  feature_importance <- stats::aggregate(abs(value) ~ variable, data = shap_values, FUN = mean)
  names(feature_importance)[names(feature_importance) == "abs(value)"] <- "Shapley_Mean"
  feature_importance$Shapley_Mean <- round(feature_importance$Shapley_Mean, 4)
  feature_importance[order(feature_importance$Shapley_Mean), , drop = FALSE]
}

save_shapley_figure <- function(feature_importance, output_file) {
  label_pad <- max(feature_importance$Shapley_Mean, na.rm = TRUE) * 0.035
  x_limit <- max(feature_importance$Shapley_Mean, na.rm = TRUE) + label_pad * 6
  p <- ggplot2::ggplot(
    feature_importance,
    ggplot2::aes(x = reorder(variable, Shapley_Mean), y = Shapley_Mean, fill = variable)
  ) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(label = Shapley_Mean, y = Shapley_Mean + label_pad),
      hjust = 0,
      size = 5
    ) +
    ggplot2::scale_y_continuous(limits = c(0, x_limit), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "Macro variables", y = "Shapley Value") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 15, angle = 30, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 15),
      axis.title = ggplot2::element_text(size = 15, face = "bold"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 35, face = "bold"),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::margin(t = 10, r = 35, b = 10, l = 10),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "none"
    )

  grDevices::png(output_file, width = 900, height = 600, bg = "transparent")
  print(p)
  grDevices::dev.off()
  invisible(output_file)
}

# ---------------------------------- Main --------------------------------------

cardi <- load_cardi(config$cardi_file, config$cardi_target)
macro <- load_macro(config$macro_file)
feature_importance <- build_shapley_figure_data(cardi, macro)
saved_figure <- save_shapley_figure(feature_importance, config$output_file)

cat("\nMacro Shapley figure complete.\n")
cat("Saved figure:\n")
cat(" - ", saved_figure, "\n", sep = "")
