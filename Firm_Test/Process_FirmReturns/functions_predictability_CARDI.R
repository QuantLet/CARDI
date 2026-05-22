# =============================================================================
# File    : functions_predictability_CARDI.R
# Purpose : Estimate firm-level and pool-level predictability regressions that
#           test whether lagged CARDI or other indicators forecast future HC/LC
#           relative volatility.  Writes both machine-readable summaries and
#           Stata-style regression tables.
# Author  : CARDI Research Team
# Date    : 2026-05-16
# Dependencies:
#   - functions_utils.R (parse_date, ensure_dir, save_csv_safe, `%||%`)
#   - sandwich         (Newey-West and clustered covariance estimators)
# =============================================================================

#' Return conventional significance stars for a p-value
#'
#' @param p Numeric scalar p-value.
#'
#' @return Character scalar: `***`, `**`, `*`, or empty string.
star_for_p <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

#' Format a coefficient for regression tables
#'
#' @param x Numeric coefficient.
#' @param p Numeric p-value used for significance stars.
#'
#' @return Character scalar formatted to three decimals plus stars.
fmt_coef <- function(x, p) {
  if (!is.finite(x)) return("")
  paste0(sprintf("%.3f", x), star_for_p(p))
}

#' Format a standard error for regression tables
#'
#' @param x Numeric standard error.
#'
#' @return Character scalar formatted as a parenthesized standard error.
fmt_se_text <- function(x) {
  if (!is.finite(x)) return("")
  paste0("\t(", sprintf("%.3f", x), ")")
}

#' Compute Newey-West inference for an OLS model
#'
#' @param fit Fitted `lm` object.
#' @param lag Integer Newey-West lag length.
#'
#' @return Data frame with coefficient estimates, standard errors,
#'   t-statistics, and p-values.
nw_test <- function(fit, lag = 12L) {
  require_pkg("sandwich")
  vc <- sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)
  co <- stats::coef(fit)
  se <- sqrt(diag(vc))[names(co)]
  t <- co / se
  p <- 2 * stats::pt(abs(t), df = stats::df.residual(fit), lower.tail = FALSE)
  data.frame(term = names(co), estimate = as.numeric(co), se = as.numeric(se),
             t_stat = as.numeric(t), p_value = as.numeric(p), row.names = NULL)
}

#' Compute cluster-robust inference for an OLS model
#'
#' @param fit Fitted `lm` object.
#' @param cluster Cluster identifier vector aligned with the model frame.
#'
#' @return Data frame with coefficient estimates, standard errors,
#'   t-statistics, and p-values.
cluster_test <- function(fit, cluster) {
  require_pkg("sandwich")
  vc <- sandwich::vcovCL(fit, cluster = cluster, type = "HC1")
  co <- stats::coef(fit)
  se <- sqrt(diag(vc))[names(co)]
  t <- co / se
  p <- 2 * stats::pt(abs(t), df = stats::df.residual(fit), lower.tail = FALSE)
  data.frame(term = names(co), estimate = as.numeric(co), se = as.numeric(se),
             t_stat = as.numeric(t), p_value = as.numeric(p), row.names = NULL)
}

#' Select predictors to test
#'
#' @param data Regression-ready data frame.
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Character vector of predictor column names after applying
#'   `predictor_family` and `predictor_vars` filters.
candidate_predictors <- function(data, config = NULL) {
  cardi <- grep("^CARDI_.*_M$", names(data), value = TRUE)
  indicators <- setdiff(
    grep("(_HC$|_LC$|_HL_Ratio$|^Vol_|^VolVaR_|^DY_|^EDC_|^CVaR_|Degree|Closeness|Betweenness|Eigenvector)",
         names(data), value = TRUE),
    cardi
  )
  family <- tolower(config$predictability$predictor_family %||% "all")
  out <- switch(family,
                cardi = cardi,
                indicator = indicators,
                indicators = indicators,
                all = unique(c(cardi, indicators)),
                unique(c(cardi, indicators)))
  explicit <- config$predictability$predictor_vars %||% NULL
  if (!is.null(explicit)) out <- intersect(out, explicit)
  intersect(unique(out), names(data))
}

#' Identify macro-control columns
#'
#' @param data Regression-ready data frame.
#'
#' @return Character vector of available macro-control column names.
macro_control_vars <- function(data) {
  grep("^(CarbonVol|RealEstate_Premium|Slope|TED|TY3M_Change|MarketVol)_M$",
       names(data), value = TRUE)
}

#' Identify event-control columns
#'
#' @param data Regression-ready data frame.
#'
#' @return Character vector containing available Covid, China, and
#'   International event controls.
event_control_vars <- function(data) {
  intersect(c("Event_Covid_M", "Event_China_M", "Event_International_M"), names(data))
}

#' Standardize event columns after cached merge variants
#'
#' @description
#' Older cached datasets may contain `.x`/`.y` event columns. This helper
#' coalesces them back to the canonical event-control names used by the
#' predictability models.
#'
#' @param data Data frame that may contain event columns.
#'
#' @return Data frame with canonical event controls.
standardize_event_columns <- function(data) {
  for (base in c("Event_Covid_M", "Event_China_M", "Event_International_M")) {
    candidates <- intersect(c(base, paste0(base, ".x"), paste0(base, ".y")), names(data))
    if (length(candidates) == 0) {
      data[[base]] <- 0L
      next
    }
    vals <- rep(NA_real_, nrow(data))
    for (col in candidates) {
      vals <- ifelse(is.na(vals), suppressWarnings(as.numeric(data[[col]])), vals)
    }
    vals[is.na(vals)] <- 0
    data[[base]] <- vals
  }
  data
}

#' Identify firm-level control columns
#'
#' @param data Regression-ready firm-month data frame.
#'
#' @return Character vector of lagged firm controls present in `data`.
firm_control_vars <- function(data) {
  # intersect(c("lag_logAsset", "lag_ROE", "lag_BM", "lag_CapExRatio", "lag_logEmissions"), names(data))
  intersect(c("lag_logAsset", "lag_ROE", "lag_BM", "lag_CapExRatio"), names(data))
}

#' Add annual fixed-effect key and continuous year trend
#'
#' @param data Data frame keyed by `Month` in `YYYY-MM` format.
#'
#' @return Data frame with integer `Year` and numeric `YearTrend`.
add_year_control <- function(data) {
  month_date <- parse_date(paste0(data$Month, "-01"))
  data$Year <- as.integer(format(month_date, "%Y"))
  years <- sort(unique(data$Year[!is.na(data$Year)]))
  data$YearTrend <- match(data$Year, years)
  data
}

#' Add a lagged monthly predictor
#'
#' @description
#' Creates one lagged predictor by month, then merges it back to either the
#' pool-month data or every firm-month observation. This avoids accidentally
#' lagging CARDI within firm rows when CARDI is a month-level variable.
#'
#' @param data Data frame with `Month` and `pred_var`.
#' @param pred_var Predictor column to lag.
#' @param lag Number of months to lag.
#'
#' @return List with lagged data and the new lagged-column name.
add_lagged_predictor <- function(data, pred_var, lag = 1L) {
  data <- data[order(data$Month), , drop = FALSE]
  lag_col <- paste0("lag", lag, "_", pred_var)
  month_pred <- aggregate(data[[pred_var]], list(Month = data$Month), function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else x[1]
  })
  names(month_pred)[2] <- pred_var
  month_pred <- month_pred[order(month_pred$Month), , drop = FALSE]
  month_pred[[lag_col]] <- c(rep(NA_real_, lag), head(month_pred[[pred_var]], -lag))
  data <- merge(data, month_pred[, c("Month", lag_col), drop = FALSE], by = "Month", all.x = TRUE)
  list(data = data, lag_col = lag_col)
}

#' Report the sample period of a fitted model frame
#'
#' @param data Model data containing `Month`.
#'
#' @return Character scalar of the form `YYYY-MM to YYYY-MM`.
sample_period <- function(data) {
  m <- sort(unique(data$Month[!is.na(data$Month)]))
  if (length(m) == 0) return(NA_character_)
  paste0(min(m), " to ", max(m))
}

#' Build predictability-control specifications
#'
#' @param data Regression-ready data frame.
#' @param firm_level Logical. If `TRUE`, include firm controls and firm FE.
#'
#' @return Named list of specifications. For firm-level data: no controls/no
#'   FE; controls plus firm FE; controls plus firm FE plus year trend; controls
#'   plus firm FE plus year FE.
predictability_control_specs <- function(data, firm_level = FALSE) {
  macro <- macro_control_vars(data)
  events <- event_control_vars(data)
  firm <- firm_control_vars(data)
  year_trend <- intersect("YearTrend", names(data))

  if (firm_level) {
    controls <- c(macro, events, firm)
    specs <- list(
      NoControls = list(
        controls = character(0),
        fixed_effects = character(0)
      ),
      ControlsFirmFE = list(
        controls = controls,
        fixed_effects = "firm"
      ),
      ControlsFirmFEYearTrend = list(
        controls = c(controls, year_trend),
        fixed_effects = "firm"
      ),
      ControlsFirmFEYearFE = list(
        controls = controls,
        fixed_effects = c("firm", "year")
      )
    )
  } else {
    controls <- c(macro, events)
    specs <- list(
      NoControls = list(
        controls = character(0),
        fixed_effects = character(0)
      ),
      Controls = list(
        controls = controls,
        fixed_effects = character(0)
      ),
      ControlsYearTrend = list(
        controls = c(controls, year_trend),
        fixed_effects = character(0)
      ),
      ControlsYearFE = list(
        controls = controls,
        fixed_effects = "year"
      )
    )
  }

  specs
}

#' Fit one predictability regression
#'
#' @param data Regression-ready data frame.
#' @param dep_var Dependent variable name.
#' @param pred_var Predictor variable name before lagging.
#' @param controls Character vector of controls.
#' @param dataset_label Output dataset label.
#' @param specification Specification label.
#' @param lag Predictor lag length in months.
#' @param fixed_effects Character vector containing any of `firm` and `year`.
#' @param cluster Optional cluster column name.
#' @param nw_lag Newey-West lag used when no cluster is requested.
#' @param min_obs Minimum complete observations required.
#'
#' @return List containing the fitted model, inference table, metadata, and a
#'   one-row summary, or `NULL` when the model cannot be fit.
fit_predictability_one <- function(data, dep_var, pred_var, controls,
                                   dataset_label, specification, lag = 1L,
                                   fixed_effects = character(0), cluster = NULL,
                                   nw_lag = 12L, min_obs = 20L) {
  lagged <- add_lagged_predictor(data, pred_var, lag = lag)
  d <- lagged$data
  lag_col <- lagged$lag_col
  rhs <- c(lag_col, controls)
  if ("firm" %in% fixed_effects && "StockID" %in% names(d)) rhs <- c(rhs, "factor(StockID)")
  if ("year" %in% fixed_effects && "Year" %in% names(d)) rhs <- c(rhs, "factor(Year)")

  needed <- unique(c(dep_var, lag_col, controls,
                     if ("StockID" %in% names(d)) "StockID",
                     "Month",
                     if ("year" %in% fixed_effects) "Year"))
  complete <- stats::complete.cases(d[, intersect(needed, names(d)), drop = FALSE])
  for (v in intersect(c(dep_var, lag_col, controls), names(d))) {
    if (is.numeric(d[[v]])) complete <- complete & is.finite(d[[v]])
  }
  fit_data <- d[complete, , drop = FALSE]
  if (nrow(fit_data) < max(length(rhs) + 8L, min_obs)) return(NULL)

  form <- stats::as.formula(paste(dep_var, "~", paste(rhs, collapse = " + ")))
  fit <- tryCatch(stats::lm(form, data = fit_data), error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  infer <- tryCatch({
    if (!is.null(cluster) && cluster %in% names(fit_data)) {
      cluster_test(fit, fit_data[[cluster]])
    } else {
      nw_test(fit, lag = nw_lag)
    }
  }, error = function(e) {
    co <- summary(fit)$coefficients
    data.frame(term = rownames(co), estimate = co[, 1], se = co[, 2],
               t_stat = co[, 3], p_value = co[, 4], row.names = NULL)
  })

  pred_row <- infer[infer$term == lag_col, , drop = FALSE]
  pred_est <- if (nrow(pred_row) == 0) NA_real_ else pred_row$estimate[1]
  pred_t <- if (nrow(pred_row) == 0) NA_real_ else pred_row$t_stat[1]
  pred_p <- if (nrow(pred_row) == 0) NA_real_ else pred_row$p_value[1]
  s <- summary(fit)
  controls <- unique(controls)
  summary_row <- data.frame(
    regression_name = paste(dataset_label, dep_var, pred_var, specification, sep = "__"),
    dataset = dataset_label,
    dependent_variable = dep_var,
    independent_variable = pred_var,
    lagged_independent_variable = lag_col,
    specification = specification,
    coefficient = pred_est,
    t_statistic = pred_t,
    p_value = pred_p,
    r_squared = s$r.squared,
    adjusted_r_squared = s$adj.r.squared,
    n_observations = stats::nobs(fit),
    sample_period = sample_period(fit_data),
    controls_included = paste(controls, collapse = "; "),
    firm_fixed_effects = "firm" %in% fixed_effects,
    year_trend_control = "YearTrend" %in% controls,
    year_fixed_effects = "year" %in% fixed_effects,
    inference = if (!is.null(cluster)) paste0("clustered by ", cluster) else paste0("Newey-West lag ", nw_lag),
    positive_coefficient = pred_est > 0,
    p_lt_10 = pred_p < 0.10,
    p_lt_05 = pred_p < 0.05,
    p_lt_01 = pred_p < 0.01,
    stringsAsFactors = FALSE
  )

  list(
    fit = fit,
    infer = infer,
    dep = dep_var,
    predictor = pred_var,
    lagged_predictor = lag_col,
    specification = specification,
    controls = controls,
    fixed_effects = fixed_effects,
    cluster = cluster,
    summary = summary_row
  )
}

#' Run all predictability regressions for one dataset
#'
#' @param data Regression-ready firm-level or pool-level dataset.
#' @param dataset_label Label used in output names.
#' @param config Configuration list from `firm_returns_config()`.
#' @param firm_level Logical. If `TRUE`, run firm-level specifications with
#'   clustered standard errors by firm.
#'
#' @return List with `summary` and `models`.
#' data = hc_firm_monthly
#' dataset_label = "HC_Firm_Monthly"
#'  config, firm_level = TRUE
run_predictability_dataset <- function(data, dataset_label, config, firm_level = FALSE) {
  data <- standardize_event_columns(data)
  data <- add_year_control(data)
  requested_dep <- config$predictability$dependent_vars %||%
    config$columns$dependent_relative_volatility
  dep_vars <- intersect(requested_dep, names(data))
  pred_vars <- candidate_predictors(data, config)
  if (length(dep_vars) == 0 || length(pred_vars) == 0) {
    return(list(summary = data.frame(), models = list()))
  }

  specs <- predictability_control_specs(data, firm_level = firm_level)
  requested_specs <- config$predictability$specifications %||% names(specs)
  specs <- specs[intersect(requested_specs, names(specs))]
  if (length(specs) == 0) return(list(summary = data.frame(), models = list()))
  cluster <- if (firm_level && isTRUE(config$firm_cluster_se)) "StockID" else NULL

  rows <- list()
  models <- list()
  for (dep in dep_vars) {
    for (pred in pred_vars) {
      for (spec_nm in names(specs)) {
        spec <- specs[[spec_nm]]
        result <- fit_predictability_one(
          data = data,
          dep_var = dep,
          pred_var = pred,
          controls = setdiff(intersect(spec$controls, names(data)), pred),
          dataset_label = dataset_label,
          specification = spec_nm,
          lag = 1L,
          fixed_effects = spec$fixed_effects,
          cluster = cluster,
          nw_lag = config$nw_lag,
          min_obs = config$min_pred_obs
        )
        if (is.null(result)) next
        key <- result$summary$regression_name
        rows[[length(rows) + 1L]] <- result$summary
        models[[key]] <- result
      }
    }
  }

  list(
    summary = if (length(rows) == 0) data.frame() else do.call(rbind, rows),
    models = models
  )
}

#' Write a Stata-style regression table for one dependent variable
#'
#' @param path Destination CSV path.
#' @param dep_var Dependent variable represented by the table.
#' @param models Named list of fitted model-result objects.
#' @param inference_label Footer text describing the standard errors.
#'
#' @return Invisibly returns `path`.
write_regression_table <- function(path, dep_var, models, inference_label) {
  if (length(models) == 0) return(invisible(NULL))
  variables <- unique(unlist(lapply(models, function(m) {
    c(m$lagged_predictor, m$controls)
  })))
  variables <- variables[!grepl("^factor[(](StockID|Year)[)]", variables)]
  header1 <- c("VARIABLES", paste0("(", seq_along(models), ")"))
  header2 <- c("", rep(dep_var, length(models)))
  table <- list(header1, header2)

  for (var in variables) {
    coef_row <- c(var)
    se_row <- c("")
    for (model in models) {
      hit <- model$infer[model$infer$term == var, , drop = FALSE]
      if (nrow(hit) == 0) {
        coef_row <- c(coef_row, "")
        se_row <- c(se_row, "")
      } else {
        coef_row <- c(coef_row, fmt_coef(hit$estimate[1], hit$p_value[1]))
        se_row <- c(se_row, fmt_se_text(hit$se[1]))
      }
    }
    table[[length(table) + 1L]] <- coef_row
    table[[length(table) + 1L]] <- se_row
  }

  table[[length(table) + 1L]] <- c("Observations", vapply(models, function(m) as.character(stats::nobs(m$fit)), character(1)))
  table[[length(table) + 1L]] <- c("R-squared", vapply(models, function(m) sprintf("%.3f", summary(m$fit)$r.squared), character(1)))
  table[[length(table) + 1L]] <- c("Adjusted R-squared", vapply(models, function(m) sprintf("%.3f", summary(m$fit)$adj.r.squared), character(1)))
  table[[length(table) + 1L]] <- c("Controls", vapply(models, function(m) if (length(m$controls) == 0) "NO" else "YES", character(1)))
  table[[length(table) + 1L]] <- c("Firm FE", vapply(models, function(m) if ("firm" %in% m$fixed_effects) "YES" else "NO", character(1)))
  table[[length(table) + 1L]] <- c("Year trend", vapply(models, function(m) if ("YearTrend" %in% m$controls) "YES" else "NO", character(1)))
  table[[length(table) + 1L]] <- c("Year FE", vapply(models, function(m) if ("year" %in% m$fixed_effects) "YES" else "NO", character(1)))
  table[[length(table) + 1L]] <- c(inference_label, rep("", length(models)))
  table[[length(table) + 1L]] <- c("*** p<0.01, ** p<0.05, * p<0.1", rep("", length(models)))

  max_len <- max(vapply(table, length, integer(1)))
  table <- lapply(table, function(row) c(row, rep("", max_len - length(row))))
  out <- as.data.frame(do.call(rbind, table), stringsAsFactors = FALSE)
  ensure_dir(dirname(path))
  utils::write.table(out, path, sep = ",", row.names = FALSE, col.names = FALSE,
                     quote = TRUE, fileEncoding = "UTF-8")
  invisible(path)
}

#' Write predictability output files
#'
#' @param grid List returned by `run_predictability_dataset()`.
#' @param dataset_label Dataset label used in output names.
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return Invisibly returns `NULL`.
#' firm, "HC_Firm_Monthly", config
write_predictability_outputs <- function(grid, dataset_label, config) {
  if (nrow(grid$summary) == 0) return(invisible(NULL))
  ensure_dir(config$outputs$predictability_dir)

  if (isTRUE(config$predictability$write_dataset_summaries)) {
    summary_path <- file.path(
      config$outputs$predictability_dir,
      paste0("predictability_summary_", dataset_label, ".csv")
    )
    save_csv_safe(grid$summary, summary_path, force = TRUE)
  }

  if (isTRUE(config$predictability$write_stata_tables)) {
    by_dep <- split(names(grid$models), vapply(grid$models, function(x) x$dep, character(1)))
    inference_label <- if (identical(dataset_label, "HC_Firm_Monthly")) {
      "Clustered standard errors in parentheses"
    } else {
      "Newey-West errors in parentheses"
    }
    for (dep in names(by_dep)) {
      models <- grid$models[by_dep[[dep]]]
      path <- file.path(config$outputs$predictability_dir,
                        paste0("regression_", dataset_label, "_", dep, ".csv"))
      write_regression_table(path, dep, models, inference_label)
    }
  }

  if (isTRUE(config$predictability$write_predictor_summaries)) {
    by_pred <- split(grid$summary, grid$summary$independent_variable)
    for (pred in names(by_pred)) {
      safe_pred <- gsub("[^A-Za-z0-9_]+", "_", pred)
      suffix <- if (identical(dataset_label, "HC_Firm_Monthly")) {
        "HC_Firm_Monthly_predictability.csv"
      } else {
        "HC_Pool_Monthly_predictability.csv"
      }
      save_csv_safe(by_pred[[pred]], file.path(config$outputs$predictability_dir,
                                               paste0(safe_pred, "_", suffix)),
                    force = TRUE)
    }
  }
  invisible(NULL)
}

#' Run requested firm-level and pool-level predictability tests
#'
#' @param hc_firm_monthly HC firm-month relative-volatility dataset.
#' @param hc_monthly HC pool-month relative-volatility dataset.
#' @param config Configuration list from `firm_returns_config()`.
#'
#' @return List with firm and pool summaries and model objects.
#' hc_firm_monthly, hc_monthly, cfg
run_all_predictability <- function(hc_firm_monthly, hc_monthly, config) {
  scope <- tolower(config$predictability$scope %||% "both")
  run_firm <- scope %in% c("both", "firm", "firms", "firm_level")
  run_pool <- scope %in% c("both", "pool", "pool_level")
  firm <- list(summary = data.frame(), models = list())
  pool <- list(summary = data.frame(), models = list())
  if (run_firm) {
    firm <- run_predictability_dataset(hc_firm_monthly, "HC_Firm_Monthly", config, firm_level = TRUE)
    write_predictability_outputs(firm, "HC_Firm_Monthly", config)
  }
  if (run_pool) {
    pool <- run_predictability_dataset(hc_monthly, "HC_Monthly", config, firm_level = FALSE)
    write_predictability_outputs(pool, "HC_Monthly", config)
  }
  list(firm = firm$summary, pool = pool$summary, firm_models = firm$models, pool_models = pool$models)
}
