# Self-contained stage functions.

# Auto-split Table 4 module: 04_data_utilities.R

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

as_numeric_columns <- function(data, cols) {
  # Only touch columns that actually exist to avoid spurious errors.
  for (col in intersect(cols, names(data))) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }
  data
}

check_required_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      label, " is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

first_existing_path <- function(paths, label) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop(
      "Missing ", label, ". Checked:\n  ",
      paste(paths, collapse = "\n  ")
    )
  }
  existing[1]
}

clean_stock_id <- function(x) {
  x <- as.character(x)
  sub("\\.0$", "", x)
}

finite_complete <- function(data, cols) {
  # An empty column list means no restriction: all rows pass.
  if (length(cols) == 0) return(rep(TRUE, nrow(data)))

  mat <- data[, cols, drop = FALSE]

  # complete.cases checks for NA/NaN; the apply check additionally excludes Inf.
  stats::complete.cases(mat) &
    apply(mat, 1, function(row) {
      all(is.finite(suppressWarnings(as.numeric(row))))
    })
}


# Auto-split Table 4 module: 05_output_writers.R

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

save_new_dataset <- function(data, csv_path, rds_path = NULL) {
  ensure_dir(dirname(csv_path))

  # Guard: refuse to overwrite the CSV.
  if (file.exists(csv_path)) {
    stop("Refusing to overwrite existing file: ", csv_path)
  }
  write.csv(data, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

  # Guard: refuse to overwrite the RDS (if a path was supplied).
  if (!is.null(rds_path)) {
    if (file.exists(rds_path)) {
      stop("Refusing to overwrite existing file: ", rds_path)
    }
    saveRDS(data, rds_path)
  }

  invisible(data)
}

write_new_csv <- function(data, path) {
  ensure_dir(dirname(path))

  if (file.exists(path)) {
    stop("Refusing to overwrite existing file: ", path)
  }
  write.csv(data, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

save_new_rds <- function(object, path) {
  ensure_dir(dirname(path))

  if (file.exists(path)) {
    stop("Refusing to overwrite existing file: ", path)
  }
  saveRDS(object, path)
  invisible(path)
}


# Auto-split Table 4 module: 06_input_readers.R

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing required R package: ", pkg)
  }
}

read_excel_as_df <- function(path) {
  require_package("readxl")
  as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
}

read_numeric_panel <- function(path) {
  if (!file.exists(path)) stop("Missing panel file: ", path)

  data <- read.csv(path, check.names = FALSE)

  # Some files omit the "Date" header for the first column.
  if (!"Date" %in% names(data)) names(data)[1] <- "Date"

  data$Date <- parse_date(data$Date)

  # Coerce every non-date column to numeric (missing values become NA).
  for (col in setdiff(names(data), "Date")) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }

  # Remove rows where the date could not be parsed.
  data <- data[!is.na(data$Date), , drop = FALSE]
  data <- data[order(data$Date), , drop = FALSE]

  # Standardise stock ID column names (e.g. "600000.0" -> "600000").
  names(data)[-1] <- clean_stock_id(names(data)[-1])
  data
}


# Auto-split Table 4 module: 15_predictability_hac.R

nw_test_for_model <- function(fit, lag = 12L) {
  require_package("sandwich")

  # Estimate the Newey-West covariance matrix.
  vcov_nw <- sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE,
                                 adjust = TRUE)
  coefs   <- stats::coef(fit)

  # Extract standard errors from the diagonal of the NW covariance matrix.
  se_raw <- sqrt(diag(vcov_nw))

  # Initialise SEs as NA so we can safely handle cases where the NW covariance
  # matrix contains fewer terms than the full coefficient vector (can happen
  # with rank-deficient models).
  se <- rep(NA_real_, length(coefs))
  names(se) <- names(coefs)
  shared_terms     <- intersect(names(se), names(se_raw))
  se[shared_terms] <- se_raw[shared_terms]

  t_value <- coefs / se
  p_value <- 2 * stats::pt(abs(t_value), df = stats::df.residual(fit),
                           lower.tail = FALSE)

  data.frame(
    term     = names(coefs),
    estimate = as.numeric(coefs),
    nw_se    = as.numeric(se),
    nw_t     = as.numeric(t_value),
    nw_p     = as.numeric(p_value),
    row.names = NULL
  )
}


# Auto-split Table 4 module: 16_predictability_variables.R

predictability_variable_lists <- function(frequency) {
  suffix <- frequency_suffix(frequency)

  # CARDI predictors: level and log-difference variants at three quantile tails.
  cardi <- paste0(
    c("CARDI_5P", "CARDI_1P", "CARDI_10P",
      "CARDI_5P_LogDiff", "CARDI_1P_LogDiff", "CARDI_10P_LogDiff"),
    "_", suffix
  )

  # Macro controls: carbon-market volatility, financial conditions, and
  # market-wide volatility.
  macro <- c(
    paste0("CarbonVol_", suffix, "_Shenzhen"),
    paste0("CarbonVol_", suffix, "_Guangdong"),
    paste0("CarbonVol_", suffix, "_Hubei"),
    paste0("RealEstate_Premium_", suffix),
    paste0("Slope_", suffix),
    paste0("TED_", suffix),
    paste0("TY3M_Change_", suffix),
    paste0("MarketVol_", suffix)
  )

  events_general  <- paste0("Event_dummy_", suffix)
  events_category <- paste0(c("Event_Covid", "Event_China",
                              "Event_International"), "_", suffix)

  list(
    dependent       = c("HC_Premium", "MC_Premium", "LC_Premium",
                        "LC_HC_Premium", "pure_LC_premium", "AR1_Premium"),
    cardi           = cardi,
    macro           = macro,
    events_general  = events_general,
    events_category = events_category
  )
}

short_name <- function(x) {
  out <- gsub("_Premium",        "",         x)
  out <- gsub("pure_LC_premium", "PureLC",   out, fixed = TRUE)
  out <- gsub("LC_HC",           "LCHC",     out, fixed = TRUE)
  out <- gsub("CARDI_",          "CARDI",    out, fixed = TRUE)
  out <- gsub("_LogDiff",        "LogDiff",  out, fixed = TRUE)
  out <- gsub("_",               "",         out, fixed = TRUE)
  out
}

future_dep_selected <- function(future_dep, selected) {
  if (is.null(selected) || length(selected) == 0) return(TRUE)
  any(future_dep == selected | startsWith(future_dep, selected))
}

predictor_selected <- function(predictor, selected_roots) {
  if (is.null(selected_roots) || length(selected_roots) == 0) return(TRUE)
  any(predictor == selected_roots |
        startsWith(predictor, paste0(selected_roots, "_")))
}


# Auto-split Table 4 module: 17_predictability_grid.R

fit_predictability_grid <- function(config, enriched) {
  lists            <- predictability_variable_lists(config$frequency)
  cardi_predictors <- intersect(lists$cardi, names(enriched))
  cardi_predictors <- cardi_predictors[
    vapply(cardi_predictors,
           predictor_selected,
           logical(1),
           selected_roots = config$selected_cardi_predictor_roots)
  ]

  if (length(cardi_predictors) == 0) {
    stop("No CARDI predictors found for frequency: ", config$frequency)
  }

  fit_predictability_grid_for_predictors(
    config          = config,
    enriched        = enriched,
    predictors      = cardi_predictors,
    predictor_label = "CARDI"
  )
}

fit_predictability_grid_for_predictors <- function(config, enriched, predictors,
                                                   predictor_label = "Predictor") {
  lists      <- predictability_variable_lists(config$frequency)
  predictors <- intersect(predictors, names(enriched))

  if (length(predictors) == 0) {
    stop("No predictors found for predictability test.")
  }

  # Build the four control-variable sets, retaining only columns present in
  # the enriched dataset to handle cases where some controls are unavailable.
  specs <- list(
    Baseline            = character(0),
    Macro               = intersect(lists$macro, names(enriched)),
    MacroEventDummy     = intersect(c(lists$macro, lists$events_general),
                                    names(enriched)),
    MacroEventCategories = intersect(c(lists$macro, lists$events_category),
                                     names(enriched))
  )
  if (!is.null(config$selected_specifications) &&
      length(config$selected_specifications) > 0) {
    specs <- specs[intersect(config$selected_specifications, names(specs))]
  }
  if (length(specs) == 0) {
    stop("No predictability specifications selected.")
  }

  # Sort data chronologically before constructing lead (future) variables.
  enriched <- enriched[order(enriched$Date), , drop = FALSE]

  results <- list()  # Stores individual regression result objects.
  rows    <- list()  # Accumulates summary-table rows.

  dependent_vars <- intersect(lists$dependent, names(enriched))
  selected_future <- config$selected_future_dependent_variables
  dependent_vars <- dependent_vars[
    vapply(paste0("future_", dependent_vars),
           future_dep_selected,
           logical(1),
           selected = selected_future)
  ]

  for (dep in dependent_vars) {
    future_dep <- paste0("future_", dep)

    # Shift the dependent variable forward by one period to create the
    # one-period-ahead outcome.  The last observation becomes NA because it
    # has no future value.
    enriched[[future_dep]] <- c(tail(enriched[[dep]], -1), NA_real_)

    for (pred in predictors) {
      for (spec_name in names(specs)) {
        controls <- specs[[spec_name]]
        rhs      <- c(pred, controls)
        keep     <- c(future_dep, rhs)

        # Restrict to complete, finite rows for this regression.
        fit_data <- enriched[finite_complete(enriched, keep), , drop = FALSE]

        # Skip if there are not enough observations for a reliable estimate
        # (minimum: k regressors + 8 residual d.f.).
        if (nrow(fit_data) < length(rhs) + 8) next

        form <- stats::as.formula(
          paste(future_dep, "~", paste(rhs, collapse = " + "))
        )
        fit      <- stats::lm(form, data = fit_data)
        ordinary <- summary(fit)$coefficients
        nw       <- nw_test_for_model(fit, config$nw_lag)

        # Skip if the predictor was dropped (e.g. collinearity).
        if (!pred %in% rownames(ordinary) || !pred %in% nw$term) next

        pred_nw  <- nw[nw$term == pred, , drop = FALSE]
        reg_name <- paste(short_name(dep), short_name(pred), spec_name,
                          sep = "_")
        pred_coef <- unname(stats::coef(fit)[pred])

        row <- data.frame(
          regression_name          = reg_name,
          dependent_variable       = future_dep,
          predictor_family         = predictor_label,
          predictor_name           = pred,
          specification            = spec_name,
          predictor_coefficient    = pred_coef,
          ordinary_t_stat          = ordinary[pred, "t value"],
          ordinary_p_value         = ordinary[pred, "Pr(>|t|)"],
          newey_west_t_stat        = pred_nw$nw_t,
          newey_west_p_value       = pred_nw$nw_p,
          r_squared                = summary(fit)$r.squared,
          adjusted_r_squared       = summary(fit)$adj.r.squared,
          n_observations           = stats::nobs(fit),
          controls_included        = paste(controls, collapse = "; "),
          positive_coefficient     = pred_coef > 0,
          # Significance flags using ordinary p-value.
          ordinary_p_lt_10         = ordinary[pred, "Pr(>|t|)"] < 0.10,
          ordinary_p_lt_05         = ordinary[pred, "Pr(>|t|)"] < 0.05,
          ordinary_p_lt_01         = ordinary[pred, "Pr(>|t|)"] < 0.01,
          # Significance flags using Newey-West p-value.
          nw_p_lt_10               = pred_nw$nw_p < 0.10,
          nw_p_lt_05               = pred_nw$nw_p < 0.05,
          nw_p_lt_01               = pred_nw$nw_p < 0.01,
          stringsAsFactors = FALSE
        )

        # Add predictor-family-specific columns (e.g. "CARDI_predictor").
        row[[paste0(predictor_label, "_predictor")]]   <- pred
        row[[paste0(predictor_label, "_coefficient")]] <- pred_coef

        rows[[length(rows) + 1L]]    <- row
        results[[reg_name]] <- list(fit      = fit,
                                    nw       = nw,
                                    dep      = future_dep,
                                    pred     = pred,
                                    spec     = spec_name,
                                    controls = controls)
      }
    }
  }

  summary_table <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()

  list(
    summary    = summary_table,
    models     = results,
    data       = enriched,
    specs      = specs,
    predictors = predictors
  )
}


# Auto-split Table 4 module: 18_predictability_tables.R

star_for_p <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

fmt_coef <- function(x, p) {
  if (!is.finite(x)) return("")
  paste0(sprintf("%.3f", x), star_for_p(p))
}

fmt_se_text <- function(x) {
  if (!is.finite(x)) return("")
  paste0("\t(", sprintf("%.3f", x), ")")
}

write_dependent_variable_table <- function(path, future_dep, models) {
  model_names <- names(models)

  # Collect all variables that appear in any model (union across all fits).
  variables <- unique(unlist(lapply(models, function(m) {
    c(names(stats::coef(m$fit)), m$controls)
  })))
  # Place the intercept last (after all regressors) following Stata convention.
  variables         <- unique(c(setdiff(variables, "(Intercept)"), "(Intercept)"))
  display_variables <- ifelse(variables == "(Intercept)", "Constant", variables)

  # Header rows.
  header1 <- c("VARIABLES", paste0("(", seq_along(models), ")"))
  header2 <- c("", rep(future_dep, length(models)))
  table   <- list(header1, header2)

  # Variable rows: one coefficient row + one SE row per variable.
  for (i in seq_along(variables)) {
    var      <- variables[i]
    coef_row <- c(display_variables[i])
    se_row   <- c("")

    for (model in models) {
      nw  <- model$nw
      hit <- nw[nw$term == var, , drop = FALSE]

      if (nrow(hit) == 0) {
        # Variable not in this model (omitted as a control or dropped due to
        # collinearity).
        coef_row <- c(coef_row, "")
        se_row   <- c(se_row,   "")
      } else {
        coef_row <- c(coef_row, fmt_coef(hit$estimate[1], hit$nw_p[1]))
        se_row   <- c(se_row,   fmt_se_text(hit$nw_se[1]))
      }
    }

    table[[length(table) + 1L]] <- coef_row
    table[[length(table) + 1L]] <- se_row
  }

  # Footer rows summarising fit statistics.
  table[[length(table) + 1L]] <- c(
    "Observations",
    vapply(models, function(m) as.character(stats::nobs(m$fit)), character(1))
  )
  table[[length(table) + 1L]] <- c(
    "R-squared",
    vapply(models, function(m) sprintf("%.3f", summary(m$fit)$r.squared),
           character(1))
  )
  table[[length(table) + 1L]] <- c(
    "Adjusted R-squared",
    vapply(models, function(m) sprintf("%.3f", summary(m$fit)$adj.r.squared),
           character(1))
  )
  table[[length(table) + 1L]] <- c(
    "Controls",
    vapply(models, function(m) if (length(m$controls) == 0) "NO" else "YES",
           character(1))
  )
  # Significance-star legend rows (these fill only the first column).
  table[[length(table) + 1L]] <- c("Newey-West errors in parentheses",
                                   rep("", length(models)))
  table[[length(table) + 1L]] <- c("*** p<0.01, ** p<0.05, * p<0.1",
                                   rep("", length(models)))

  # Pad all rows to the same length before converting to a data frame.
  max_len <- max(vapply(table, length, integer(1)))
  table   <- lapply(table, function(row) c(row, rep("", max_len - length(row))))

  out <- as.data.frame(do.call(rbind, table), stringsAsFactors = FALSE)

  if (file.exists(path)) {
    stop("Refusing to overwrite existing file: ", path)
  }
  utils::write.table(out, path, sep = ",", row.names = FALSE, col.names = FALSE,
                     quote = TRUE, fileEncoding = "UTF-8")
}

write_predictability_tables <- function(config, grid, file_prefix = "regression",
                                        file_suffix = "") {
  if (length(grid$models) == 0) return(invisible(NULL))

  by_dep <- split(names(grid$models),
                  vapply(grid$models, function(x) x$dep, character(1)))

  for (future_dep in names(by_dep)) {
    model_keys <- by_dep[[future_dep]]
    models     <- grid$models[model_keys]
    file       <- file.path(
      config$output_dir,
      paste0(file_prefix, "_", future_dep, file_suffix, ".csv")
    )
    write_dependent_variable_table(file, future_dep, models)
  }

  invisible(NULL)
}

cleanup_predictability_outputs <- function(config, selected_future_deps) {
  if (!isTRUE(config$overwrite_predictability_outputs)) {
    return(invisible(NULL))
  }

  unlink(c(config$regression_summary_file, config$predictability_rds))

  existing_tables <- list.files(
    config$output_dir,
    pattern = "^regression_future_.*\\.csv$",
    full.names = TRUE
  )
  if (length(existing_tables) == 0) return(invisible(NULL))

  keep_files <- file.path(
    config$output_dir,
    paste0("regression_", selected_future_deps, ".csv")
  )
  unlink(setdiff(existing_tables, keep_files))
  unlink(intersect(existing_tables, keep_files))

  invisible(NULL)
}
run_predictability_outputs <- function(config, enriched) {
  # Return cached results if both key outputs already exist.
  if (!isTRUE(config$overwrite_predictability_outputs) &&
      file.exists(config$predictability_rds) &&
      file.exists(config$regression_summary_file)) {
    return(readRDS(config$predictability_rds))
  }

  grid <- fit_predictability_grid(config, enriched)
  selected_future_deps <- unique(vapply(grid$models, function(x) x$dep,
                                        character(1)))
  cleanup_predictability_outputs(config, selected_future_deps)

  write_new_csv(grid$summary, config$regression_summary_file)

  # Write one formatted table per future dependent variable.
  by_dep <- split(names(grid$models),
                  vapply(grid$models, function(x) x$dep, character(1)))
  for (future_dep in names(by_dep)) {
    model_keys <- by_dep[[future_dep]]
    models     <- grid$models[model_keys]
    file       <- file.path(
      config$output_dir,
      paste0("regression_", future_dep, ".csv")
    )
    write_dependent_variable_table(file, future_dep, models)
  }

  save_new_rds(grid, config$predictability_rds)
  grid
}


load_enriched_input <- function(config) {
  if (file.exists(config$enriched_rds)) {
    return(readRDS(config$enriched_rds))
  }
  if (file.exists(config$enriched_file)) {
    return(read.csv(config$enriched_file, check.names = FALSE))
  }
  stop("Enriched dataset is missing. Run 02_factor_regression/main.R first.")
}

run_predictability_stage <- function(config) {
  enriched <- load_enriched_input(config)
  run_predictability_outputs(config, enriched)
}
