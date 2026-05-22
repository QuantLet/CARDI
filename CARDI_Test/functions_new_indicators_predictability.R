# Predictability tests for NewIndicator HC/LC ratio variables.

new_indicator_source_file <- function(config) {
  if (identical(config$frequency, "monthly")) {
    return(config$new_indicator_files$monthly)
  }
  config$new_indicator_files$weekly
}

new_indicator_period_key <- function(data, frequency) {
  if ("YearMonth" %in% names(data)) return(substr(as.character(data$YearMonth), 1, 7))
  if ("YearWeek" %in% names(data)) return(as.character(data$YearWeek))
  if ("Period" %in% names(data)) return(as.character(data$Period))
  if ("FrequencyID" %in% names(data)) return(as.character(data$FrequencyID))
  if ("Week" %in% names(data)) return(as.character(data$Week))
  if ("Date" %in% names(data)) return(period_id(data$Date, frequency))
  stop("NewIndicator file must contain YearMonth, YearWeek, Period, FrequencyID, Week, or Date.")
}

load_new_indicator_ratios <- function(config) {
  path <- new_indicator_source_file(config)
  if (!file.exists(path)) {
    warning("NewIndicator source file is missing for ", config$frequency,
            ": ", path,
            "\nSkipping NewIndicator predictability module.")
    return(NULL)
  }

  data <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  data$Period <- new_indicator_period_key(data, config$frequency)
  ratio_cols <- grep("_HL_Ratio$", names(data), value = TRUE)
  if (length(ratio_cols) == 0) {
    warning("No *_HL_Ratio columns found in NewIndicator file: ", path)
    return(NULL)
  }

  data <- as_numeric_columns(data, ratio_cols)
  out <- data[, c("Period", ratio_cols), drop = FALSE]
  out <- out[!is.na(out$Period) & nzchar(out$Period), , drop = FALSE]
  out <- out[!duplicated(out$Period), , drop = FALSE]
  out[order(out$Period), , drop = FALSE]
}

run_new_indicators_predictability <- function(config, enriched) {
  if (!isTRUE(config$run_new_indicators_predictability)) {
    return(list(summary = data.frame(), models = list(), data = enriched))
  }

  if (file.exists(config$new_indicator_predictability_rds) &&
      file.exists(config$new_indicator_summary_file)) {
    return(readRDS(config$new_indicator_predictability_rds))
  }

  indicators <- load_new_indicator_ratios(config)
  if (is.null(indicators)) {
    return(list(summary = data.frame(), models = list(), data = enriched))
  }

  enriched$Period <- normalize_period_key(enriched, config$frequency)
  merged <- merge(enriched, indicators, by = "Period", all = FALSE)
  merged <- merged[order(merged$Date), , drop = FALSE]

  ratio_cols <- setdiff(names(indicators), "Period")
  ratio_cols <- intersect(ratio_cols, names(merged))
  grid <- fit_predictability_grid_for_predictors(
    config = config,
    enriched = merged,
    predictors = ratio_cols,
    predictor_label = "NewIndicator"
  )

  grid$source_file <- new_indicator_source_file(config)
  grid$selected_indicators <- ratio_cols

  write_new_csv(merged, config$new_indicator_analysis_file)
  write_new_csv(grid$summary, config$new_indicator_summary_file)

  comparison <- grid$summary
  if (nrow(comparison) > 0) {
    comparison <- comparison[
      order(comparison$dependent_variable,
            comparison$NewIndicator_predictor,
            comparison$specification),
      ,
      drop = FALSE
    ]
  }
  write_new_csv(comparison, config$new_indicator_comparison_file)

  write_predictability_tables(
    config = config,
    grid = grid,
    file_prefix = "new_indicator_regression"
  )

  save_new_rds(grid, config$new_indicator_predictability_rds)
  grid
}
