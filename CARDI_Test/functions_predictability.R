# CARDI predictability regressions, translated from the Stata reporting logic.

nw_test_for_model <- function(fit, lag = 12L) {
  require_package("sandwich")
  vcov_nw <- sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE,
                                 adjust = TRUE)
  coefs <- stats::coef(fit)
  se_raw <- sqrt(diag(vcov_nw))
  se <- rep(NA_real_, length(coefs))
  names(se) <- names(coefs)
  shared_terms <- intersect(names(se), names(se_raw))
  se[shared_terms] <- se_raw[shared_terms]
  t_value <- coefs / se
  p_value <- 2 * stats::pt(abs(t_value), df = stats::df.residual(fit),
                           lower.tail = FALSE)
  data.frame(
    term = names(coefs),
    estimate = as.numeric(coefs),
    nw_se = as.numeric(se),
    nw_t = as.numeric(t_value),
    nw_p = as.numeric(p_value),
    row.names = NULL
  )
}

predictability_variable_lists <- function(frequency) {
  suffix <- frequency_suffix(frequency)
  cardi <- paste0(
    c("CARDI_5P", "CARDI_1P", "CARDI_10P",
      "CARDI_5P_LogDiff", "CARDI_1P_LogDiff", "CARDI_10P_LogDiff"),
    "_", suffix
  )
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
  events_general <- paste0("Event_dummy_", suffix)
  events_category <- paste0(c("Event_Covid", "Event_China",
                              "Event_International"), "_", suffix)
  list(
    dependent = c("HC_Premium", "MC_Premium", "LC_Premium",
                  "LC_HC_Premium", "pure_LC_premium", "AR1_Premium"),
    cardi = cardi,
    macro = macro,
    events_general = events_general,
    events_category = events_category
  )
}

short_name <- function(x) {
  out <- gsub("_Premium", "", x)
  out <- gsub("pure_LC_premium", "PureLC", out, fixed = TRUE)
  out <- gsub("LC_HC", "LCHC", out, fixed = TRUE)
  out <- gsub("CARDI_", "CARDI", out, fixed = TRUE)
  out <- gsub("_LogDiff", "LogDiff", out, fixed = TRUE)
  out <- gsub("_", "", out, fixed = TRUE)
  out
}

fit_predictability_grid <- function(config, enriched) {
  lists <- predictability_variable_lists(config$frequency)
  cardi_predictors <- intersect(lists$cardi, names(enriched))
  if (length(cardi_predictors) == 0) {
    stop("No CARDI predictors found for frequency: ", config$frequency)
  }

  fit_predictability_grid_for_predictors(
    config = config,
    enriched = enriched,
    predictors = cardi_predictors,
    predictor_label = "CARDI"
  )
}

fit_predictability_grid_for_predictors <- function(config, enriched, predictors,
                                                   predictor_label = "Predictor") {
  lists <- predictability_variable_lists(config$frequency)
  predictors <- intersect(predictors, names(enriched))
  if (length(predictors) == 0) {
    stop("No predictors found for predictability test.")
  }

  specs <- list(
    Baseline = character(0),
    Macro = intersect(lists$macro, names(enriched)),
    MacroEventDummy = intersect(c(lists$macro, lists$events_general),
                                names(enriched)),
    MacroEventCategories = intersect(c(lists$macro, lists$events_category),
                                     names(enriched))
  )

  enriched <- enriched[order(enriched$Date), , drop = FALSE]
  results <- list()
  rows <- list()

  for (dep in intersect(lists$dependent, names(enriched))) {
    future_dep <- paste0("future_", dep)
    enriched[[future_dep]] <- c(tail(enriched[[dep]], -1), NA_real_)

    for (pred in predictors) {
      for (spec_name in names(specs)) {
        controls <- specs[[spec_name]]
        rhs <- c(pred, controls)
        keep <- c(future_dep, rhs)
        fit_data <- enriched[finite_complete(enriched, keep), , drop = FALSE]
        if (nrow(fit_data) < length(rhs) + 8) next

        form <- stats::as.formula(
          paste(future_dep, "~", paste(rhs, collapse = " + "))
        )
        fit <- stats::lm(form, data = fit_data)
        ordinary <- summary(fit)$coefficients
        nw <- nw_test_for_model(fit, config$nw_lag)
        if (!pred %in% rownames(ordinary) || !pred %in% nw$term) next
        pred_nw <- nw[nw$term == pred, , drop = FALSE]
        reg_name <- paste(short_name(dep), short_name(pred), spec_name,
                          sep = "_")
        pred_coef <- unname(stats::coef(fit)[pred])
        row <- data.frame(
          regression_name = reg_name,
          dependent_variable = future_dep,
          predictor_family = predictor_label,
          predictor_name = pred,
          specification = spec_name,
          predictor_coefficient = pred_coef,
          ordinary_t_stat = ordinary[pred, "t value"],
          ordinary_p_value = ordinary[pred, "Pr(>|t|)"],
          newey_west_t_stat = pred_nw$nw_t,
          newey_west_p_value = pred_nw$nw_p,
          r_squared = summary(fit)$r.squared,
          adjusted_r_squared = summary(fit)$adj.r.squared,
          n_observations = stats::nobs(fit),
          controls_included = paste(controls, collapse = "; "),
          positive_coefficient = pred_coef > 0,
          ordinary_p_lt_10 = ordinary[pred, "Pr(>|t|)"] < 0.10,
          ordinary_p_lt_05 = ordinary[pred, "Pr(>|t|)"] < 0.05,
          ordinary_p_lt_01 = ordinary[pred, "Pr(>|t|)"] < 0.01,
          nw_p_lt_10 = pred_nw$nw_p < 0.10,
          nw_p_lt_05 = pred_nw$nw_p < 0.05,
          nw_p_lt_01 = pred_nw$nw_p < 0.01,
          stringsAsFactors = FALSE
        )
        row[[paste0(predictor_label, "_predictor")]] <- pred
        row[[paste0(predictor_label, "_coefficient")]] <- pred_coef
        rows[[length(rows) + 1L]] <- row
        results[[reg_name]] <- list(fit = fit, nw = nw, dep = future_dep,
                                    pred = pred, spec = spec_name,
                                    controls = controls)
      }
    }
  }

  summary_table <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  list(summary = summary_table, models = results, data = enriched,
       specs = specs, predictors = predictors)
}

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
  variables <- unique(unlist(lapply(models, function(m) {
    c(names(stats::coef(m$fit)), m$controls)
  })))
  variables <- unique(c(setdiff(variables, "(Intercept)"), "(Intercept)"))
  display_variables <- ifelse(variables == "(Intercept)", "Constant",
                              variables)

  header1 <- c("VARIABLES", paste0("(", seq_along(models), ")"))
  header2 <- c("", rep(future_dep, length(models)))
  table <- list(header1, header2)

  for (i in seq_along(variables)) {
    var <- variables[i]
    coef_row <- c(display_variables[i])
    se_row <- c("")
    for (model in models) {
      nw <- model$nw
      hit <- nw[nw$term == var, , drop = FALSE]
      if (nrow(hit) == 0) {
        coef_row <- c(coef_row, "")
        se_row <- c(se_row, "")
      } else {
        coef_row <- c(coef_row, fmt_coef(hit$estimate[1], hit$nw_p[1]))
        se_row <- c(se_row, fmt_se_text(hit$nw_se[1]))
      }
    }
    table[[length(table) + 1L]] <- coef_row
    table[[length(table) + 1L]] <- se_row
  }

  table[[length(table) + 1L]] <- c("Observations",
                                   vapply(models, function(m) {
                                     as.character(stats::nobs(m$fit))
                                   }, character(1)))
  table[[length(table) + 1L]] <- c("R-squared",
                                   vapply(models, function(m) {
                                     sprintf("%.3f", summary(m$fit)$r.squared)
                                   }, character(1)))
  table[[length(table) + 1L]] <- c("Adjusted R-squared",
                                   vapply(models, function(m) {
                                     sprintf("%.3f", summary(m$fit)$adj.r.squared)
                                   }, character(1)))
  table[[length(table) + 1L]] <- c("Controls",
                                   vapply(models, function(m) {
                                     if (length(m$controls) == 0) "NO" else "YES"
                                   }, character(1)))
  table[[length(table) + 1L]] <- c("Newey-West errors in parentheses",
                                   rep("", length(models)))
  table[[length(table) + 1L]] <- c("*** p<0.01, ** p<0.05, * p<0.1",
                                   rep("", length(models)))

  max_len <- max(vapply(table, length, integer(1)))
  table <- lapply(table, function(row) c(row, rep("", max_len - length(row))))
  out <- as.data.frame(do.call(rbind, table), stringsAsFactors = FALSE)
  if (file.exists(path)) {
    stop("Refusing to overwrite existing file: ", path)
  }
  utils::write.table(out, path, sep = ",", row.names = FALSE, col.names = FALSE,
                     quote = TRUE, fileEncoding = "UTF-8")
}

run_predictability_outputs <- function(config, enriched) {
  if (file.exists(config$predictability_rds) &&
      file.exists(config$regression_summary_file)) {
    return(readRDS(config$predictability_rds))
  }

  grid <- fit_predictability_grid(config, enriched)
  write_new_csv(grid$summary, config$regression_summary_file)

  by_dep <- split(names(grid$models),
                  vapply(grid$models, function(x) x$dep, character(1)))
  for (future_dep in names(by_dep)) {
    model_keys <- by_dep[[future_dep]]
    models <- grid$models[model_keys]
    file <- file.path(config$output_dir,
                      paste0("regression_", future_dep, ".csv"))
    write_dependent_variable_table(file, future_dep, models)
  }

  save_new_rds(grid, config$predictability_rds)
  grid
}

write_predictability_tables <- function(config, grid, file_prefix = "regression",
                                        file_suffix = "") {
  if (length(grid$models) == 0) return(invisible(NULL))

  by_dep <- split(names(grid$models),
                  vapply(grid$models, function(x) x$dep, character(1)))
  for (future_dep in names(by_dep)) {
    model_keys <- by_dep[[future_dep]]
    models <- grid$models[model_keys]
    file <- file.path(
      config$output_dir,
      paste0(file_prefix, "_", future_dep, file_suffix, ".csv")
    )
    write_dependent_variable_table(file, future_dep, models)
  }
  invisible(NULL)
}
