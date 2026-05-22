################################################################################
# Event_test.R
#
# Purpose: Test whether regulatory carbon-related events are associated with
#          structural changes in daily CARDI series.
# Author: CARDI project workflow
# Date: 2026-05-19
# Dependencies: readxl, openxlsx, ggplot2, sandwich, lmtest;
#               MSwM is optional for Markov-switching models.
#
# Notes:
# - This script does not install packages automatically. If a package is missing,
#   install it with install.packages("<package_name>") and re-run the script.
# - Regression-based intervention tests report Newey-West standard errors for
#   the main event coefficients.
################################################################################

rm(list = ls(all = TRUE))

# ----------------------------- Configuration ---------------------------------

project_root <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
cardi_file <- file.path(project_root, "Data", "Processed", "FRM_Carbon_risk.csv")
event_file <- file.path(project_root, "Data", "raw", "Important_Carbon_Events.xlsx")

figure_dir <- file.path(project_root, "Output", "Figure")
table_dir <- file.path(project_root, "Output", "Event_test")

event_windows <- c(30L, 60L)
reference_event_windows <- c(7L, 30L, 60L)
event_cluster_gap_days <- 30L
# Newey-West lag used for all regression-based event coefficients.
nw_lag <- 5L
# Regime-switch dates are defined as upward crossings of these high-risk probabilities.
markov_thresholds <- c(0.5, 0.8)

cardi_column_map <- c(
  CARDI_1P = "FRM_High_Low_1",
  CARDI_5P = "FRM_High_Low_5",
  CARDI_10P = "FRM_High_Low_10"
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------ Dependencies ---------------------------------

require_package <- function(pkg, required = TRUE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0(
      "Package '", pkg, "' is required for this step. ",
      "Install it with install.packages('", pkg, "')."
    )
    if (required) stop(msg, call. = FALSE)
    message(msg)
    return(FALSE)
  }
  TRUE
}

required_packages <- c("readxl", "openxlsx", "ggplot2", "sandwich", "lmtest")
invisible(lapply(required_packages, require_package))
has_mswm <- require_package("MSwM", required = FALSE)

# ------------------------------- Utilities -----------------------------------

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  x_chr <- as.character(x)
  # CARDI/event inputs have appeared in both YYYY/MM/DD and Excel-derived formats.
  parsed <- as.Date(x_chr, tryFormats = c(
    "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y", "%Y%m%d"
  ))
  parsed
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

stars <- function(p_value) {
  ifelse(is.na(p_value), "",
    ifelse(p_value < 0.01, "***",
      ifelse(p_value < 0.05, "**",
        ifelse(p_value < 0.10, "*", "")
      )
    )
  )
}

conclusion <- function(p_value, level) {
  ifelse(is.na(p_value), NA_character_,
    ifelse(p_value < level, "Reject no break", "Do not reject")
  )
}

write_workbook <- function(path, sheets) {
  wb <- openxlsx::createWorkbook()
  for (sheet_name in names(sheets)) {
    openxlsx::addWorksheet(wb, sheet_name)
    dat <- sheets[[sheet_name]]
    if (is.null(dat) || nrow(dat) == 0) {
      dat <- data.frame(note = "No results generated for this sheet.")
    }
    openxlsx::writeData(wb, sheet_name, dat)
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

nearest_trading_index <- function(dates, event_date) {
  if (length(dates) == 0 || is.na(event_date)) return(NA_integer_)
  # Events can fall on non-trading days, so use the nearest observed CARDI date.
  which.min(abs(as.numeric(dates - event_date)))
}

nearest_event_row <- function(target_date, event_dates) {
  if (length(event_dates) == 0 || is.na(target_date)) return(NA_integer_)
  which.min(abs(as.numeric(event_dates - target_date)))
}

trading_distance <- function(cardi_dates, date_a, date_b) {
  idx_a <- nearest_trading_index(cardi_dates, date_a)
  idx_b <- nearest_trading_index(cardi_dates, date_b)
  if (is.na(idx_a) || is.na(idx_b)) return(NA_integer_)
  as.integer(idx_a - idx_b)
}

event_window_data <- function(series_df, event_date, window_size) {
  idx <- nearest_trading_index(series_df$date, event_date)
  if (is.na(idx)) return(series_df[0, , drop = FALSE])
  # Window size is measured in trading observations, not calendar days.
  rows <- seq.int(max(1L, idx - window_size), min(nrow(series_df), idx + window_size))
  out <- series_df[rows, , drop = FALSE]
  out$event_trading_date <- series_df$date[idx]
  out$relative_trading_day <- seq_along(rows) - which(rows == idx)
  out
}

empty_df <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)[0, , drop = FALSE]
}

add_overlap_flags <- function(events, windows, date_col = "event_date") {
  out <- events
  for (window_size in windows) {
    flag <- rep(FALSE, nrow(events))
    for (i in seq_len(nrow(events))) {
      other <- seq_len(nrow(events)) != i
      # Two symmetric event windows overlap if their center dates are within 2 * window.
      flag[i] <- any(abs(as.numeric(events[[date_col]][i] - events[[date_col]][other])) <=
        2 * window_size, na.rm = TRUE)
    }
    out[[paste0("overlap_flag_w", window_size)]] <- flag
  }
  out
}

event_metadata_cols <- function(event_row, window_size, analysis_level) {
  data.frame(
    analysis_level = analysis_level,
    event_name = event_row$event_name,
    event_date = event_row$event_date,
    cluster_id = event_row$cluster_id,
    cluster_start_date = event_row$cluster_start_date,
    cluster_end_date = event_row$cluster_end_date,
    representative_event_date = event_row$representative_event_date,
    is_clustered_event = event_row$is_clustered_event,
    number_of_events_in_cluster = event_row$number_of_events_in_cluster,
    window_size = window_size,
    overlap_flag = event_row[[paste0("overlap_flag_w", window_size)]],
    stringsAsFactors = FALSE
  )
}

# ------------------------------- Data loading --------------------------------

load_cardi_data <- function(path, column_map) {
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  date_col <- intersect(c("date", "Date", "DATE"), names(raw))[1]
  if (is.na(date_col)) stop("CARDI file must contain a date column.", call. = FALSE)

  available_map <- column_map

  # The current processed file already stores CARDI_* names. Keep this direct
  # mapping first, while preserving the documented FRM fallback names.
  for (cardi_name in names(available_map)) {
    if (cardi_name %in% names(raw)) {
      # Prefer already-renamed CARDI_* columns when the processed file provides them.
      available_map[[cardi_name]] <- cardi_name
    }
  }

  if (!"CARDI_5P" %in% names(raw) &&
      !"FRM_High_Low_5" %in% names(raw) &&
      "FRM_High_Low" %in% names(raw)) {
    available_map[["CARDI_5P"]] <- "FRM_High_Low"
  }

  missing_cols <- available_map[!available_map %in% names(raw)]
  if (length(missing_cols) > 0) {
    stop("Missing CARDI source columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(date = parse_date(raw[[date_col]]), stringsAsFactors = FALSE)
  for (cardi_name in names(available_map)) {
    out[[cardi_name]] <- safe_numeric(raw[[available_map[[cardi_name]]]])
  }

  out <- out[!is.na(out$date), , drop = FALSE]
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL

  if (any(duplicated(out$date))) {
    # If duplicate trading dates exist, collapse to one daily CARDI observation.
    out <- stats::aggregate(. ~ date, data = out, FUN = function(z) mean(z, na.rm = TRUE))
    out <- out[order(out$date), , drop = FALSE]
  }

  if (any(!is.na(out[-1, "date"]) & out[-1, "date"] < out[-nrow(out), "date"])) {
    stop("CARDI dates are not sorted after loading.", call. = FALSE)
  }

  for (cardi_name in names(available_map)) {
    if (all(is.na(out[[cardi_name]]))) {
      stop(cardi_name, " is entirely missing after loading.", call. = FALSE)
    }
  }

  out
}

load_events <- function(path) {
  raw <- readxl::read_excel(path)
  date_col <- intersect(c("Date", "date", "EVENT_DATE", "event_date"), names(raw))[1]
  if (is.na(date_col)) stop("Event file must contain a Date column.", call. = FALSE)

  name_col <- intersect(c("Event", "event", "event_name", "Name", "name"), names(raw))[1]
  type_col <- intersect(c("Type", "type", "event_type"), names(raw))[1]

  out <- data.frame(
    event_date = parse_date(raw[[date_col]]),
    event_name = if (!is.na(name_col)) as.character(raw[[name_col]]) else paste0("Event_", seq_len(nrow(raw))),
    event_type = if (!is.na(type_col)) as.character(raw[[type_col]]) else NA_character_,
    stringsAsFactors = FALSE
  )

  out$event_name[is.na(out$event_name) | out$event_name == ""] <-
    paste0("Event_", which(is.na(out$event_name) | out$event_name == ""))

  out <- out[!is.na(out$event_date), , drop = FALSE]
  out <- out[order(out$event_date, out$event_name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

cluster_events <- function(events, gap_days, windows) {
  events <- events[order(events$event_date), , drop = FALSE]
  cluster_id <- integer(nrow(events))
  current_cluster <- 1L
  cluster_id[1] <- current_cluster

  if (nrow(events) > 1) {
    for (i in 2:nrow(events)) {
      gap <- as.numeric(events$event_date[i] - events$event_date[i - 1])
      # Start a new cluster only when the gap exceeds the configured threshold.
      if (!is.na(gap) && gap > gap_days) current_cluster <- current_cluster + 1L
      cluster_id[i] <- current_cluster
    }
  }

  events$cluster_id <- paste0("C", sprintf("%03d", cluster_id))

  # Build cluster metadata explicitly because aggregate() can coerce Date vectors
  # to numeric when multiple summary statistics are returned together.
  cluster_split <- split(events, events$cluster_id)
  cluster_meta <- do.call(rbind, lapply(names(cluster_split), function(id) {
    x <- cluster_split[[id]]
    data.frame(
      cluster_id = id,
      cluster_start_date = min(x$event_date, na.rm = TRUE),
      cluster_end_date = max(x$event_date, na.rm = TRUE),
      representative_event_date = min(x$event_date, na.rm = TRUE),
      number_of_events_in_cluster = nrow(x),
      stringsAsFactors = FALSE
    )
  }))

  cluster_names <- stats::aggregate(
    event_name ~ cluster_id,
    data = events,
    FUN = function(x) paste(unique(x), collapse = " | ")
  )
  cluster_types <- stats::aggregate(
    event_type ~ cluster_id,
    data = events,
    FUN = function(x) paste(unique(stats::na.omit(x)), collapse = " | ")
  )

  cluster_meta <- merge(cluster_meta, cluster_names, by = "cluster_id", all.x = TRUE)
  cluster_meta <- merge(cluster_meta, cluster_types, by = "cluster_id", all.x = TRUE)
  names(cluster_meta)[names(cluster_meta) == "event_name"] <- "event_name"
  names(cluster_meta)[names(cluster_meta) == "event_type"] <- "event_type"
  cluster_meta$event_date <- cluster_meta$representative_event_date
  cluster_meta$is_clustered_event <- cluster_meta$number_of_events_in_cluster > 1

  events <- merge(events, cluster_meta[
    c("cluster_id", "cluster_start_date", "cluster_end_date",
      "representative_event_date", "number_of_events_in_cluster")
  ], by = "cluster_id", all.x = TRUE)
  events$is_clustered_event <- events$number_of_events_in_cluster > 1
  events <- events[order(events$event_date, events$event_name), , drop = FALSE]
  cluster_meta <- cluster_meta[order(cluster_meta$representative_event_date), , drop = FALSE]

  list(
    individual = add_overlap_flags(events, windows, "event_date"),
    clusters = add_overlap_flags(cluster_meta, windows, "representative_event_date")
  )
}

# ----------------------------- Chow tests ------------------------------------

run_chow_one <- function(series_df, cardi_var, event_row, window_size, analysis_level) {
  local_df <- event_window_data(series_df[c("date", cardi_var)], event_row$event_date, window_size)
  names(local_df)[names(local_df) == cardi_var] <- "value"
  local_df <- local_df[!is.na(local_df$value), , drop = FALSE]

  meta <- event_metadata_cols(event_row, window_size, analysis_level)
  if (nrow(local_df) < 12 || length(unique(local_df$relative_trading_day < 0)) < 2) {
    return(cbind(
      data.frame(CARDI_variable = cardi_var, test_statistic = NA_real_, p_value = NA_real_,
                 n_before_event = NA_integer_, n_after_event = NA_integer_,
                 conclusion_10pct = NA_character_, conclusion_5pct = NA_character_,
                 conclusion_1pct = NA_character_, stringsAsFactors = FALSE),
      meta
    ))
  }

  local_df$time_index <- seq_len(nrow(local_df))
  local_df$post_event <- as.integer(local_df$date >= local_df$event_trading_date[1])
  n_before <- sum(local_df$date < local_df$event_trading_date[1], na.rm = TRUE)
  n_after <- sum(local_df$date >= local_df$event_trading_date[1], na.rm = TRUE)

  stat <- NA_real_
  p_value <- NA_real_

  if (n_before >= 4 && n_after >= 4) {
    restricted <- stats::lm(value ~ time_index, data = local_df)
    unrestricted <- stats::lm(value ~ time_index * post_event, data = local_df)
    # Manual Chow F statistic compares one trend before/after the known event date.
    rss_restricted <- sum(stats::residuals(restricted)^2)
    rss_unrestricted <- sum(stats::residuals(unrestricted)^2)
    q <- length(stats::coef(unrestricted)) - length(stats::coef(restricted))
    df_den <- stats::df.residual(unrestricted)
    stat <- ((rss_restricted - rss_unrestricted) / q) / (rss_unrestricted / df_den)
    p_value <- stats::pf(stat, q, df_den, lower.tail = FALSE)
  }

  cbind(
    data.frame(
      CARDI_variable = cardi_var,
      test_statistic = stat,
      p_value = p_value,
      n_before_event = n_before,
      n_after_event = n_after,
      conclusion_10pct = conclusion(p_value, 0.10),
      conclusion_5pct = conclusion(p_value, 0.05),
      conclusion_1pct = conclusion(p_value, 0.01),
      stringsAsFactors = FALSE
    ),
    meta
  )
}

run_chow_tests <- function(cardi_df, individual_events, cluster_events, windows, cardi_vars) {
  results <- list()
  k <- 1L
  for (cardi_var in cardi_vars) {
    series_df <- cardi_df[c("date", cardi_var)]
    for (window_size in windows) {
      for (i in seq_len(nrow(individual_events))) {
        results[[k]] <- run_chow_one(series_df, cardi_var, individual_events[i, ], window_size, "individual_event")
        k <- k + 1L
      }
      for (i in seq_len(nrow(cluster_events))) {
        results[[k]] <- run_chow_one(series_df, cardi_var, cluster_events[i, ], window_size, "event_cluster")
        k <- k + 1L
      }
    }
  }
  do.call(rbind, results)
}

# ----------------------- Intervention / ITS tests ----------------------------

newey_west_coeftest <- function(model, lag) {
  # Do not use summary(lm) here: event coefficients require HAC/Newey-West SEs.
  vcov_nw <- sandwich::NeweyWest(model, lag = lag, prewhite = FALSE, adjust = TRUE)
  lmtest::coeftest(model, vcov. = vcov_nw)
}

run_intervention_one <- function(series_df, cardi_var, event_row, window_size, analysis_level, lag) {
  local_df <- event_window_data(series_df[c("date", cardi_var)], event_row$event_date, window_size)
  names(local_df)[names(local_df) == cardi_var] <- "value"
  local_df <- local_df[!is.na(local_df$value), , drop = FALSE]
  meta <- event_metadata_cols(event_row, window_size, analysis_level)

  if (nrow(local_df) < 12 || length(unique(local_df$date >= local_df$event_trading_date[1])) < 2) {
    return(cbind(
      data.frame(CARDI_variable = cardi_var, coefficient_name = c("EventPulse", "PostEventStep", "PostEventTrend"),
                 estimate = NA_real_, newey_west_se = NA_real_, t_statistic = NA_real_,
                 p_value = NA_real_, significance = NA_character_, n_observations = nrow(local_df),
                 standard_error_type = "Newey-West", stringsAsFactors = FALSE),
      meta
    ))
  }

  event_trading_date <- local_df$event_trading_date[1]
  # Pulse captures the event trading day; step/trend capture persistence afterward.
  local_df$EventPulse <- as.integer(local_df$date == event_trading_date)
  local_df$PostEventStep <- as.integer(local_df$date > event_trading_date)
  local_df$PostEventTrend <- pmax(local_df$relative_trading_day, 0L)

  fit <- stats::lm(value ~ EventPulse + PostEventStep + PostEventTrend, data = local_df)
  ct <- newey_west_coeftest(fit, lag)

  coef_names <- c("EventPulse", "PostEventStep", "PostEventTrend")
  out <- data.frame(
    CARDI_variable = cardi_var,
    coefficient_name = coef_names,
    estimate = NA_real_,
    newey_west_se = NA_real_,
    t_statistic = NA_real_,
    p_value = NA_real_,
    significance = NA_character_,
    n_observations = stats::nobs(fit),
    standard_error_type = "Newey-West",
    stringsAsFactors = FALSE
  )

  for (coef_name in coef_names) {
    if (coef_name %in% rownames(ct)) {
      idx <- out$coefficient_name == coef_name
      out$estimate[idx] <- ct[coef_name, "Estimate"]
      out$newey_west_se[idx] <- ct[coef_name, "Std. Error"]
      out$t_statistic[idx] <- ct[coef_name, "t value"]
      out$p_value[idx] <- ct[coef_name, "Pr(>|t|)"]
      out$significance[idx] <- stars(out$p_value[idx])
    }
  }

  cbind(out, meta)
}

run_intervention_tests <- function(cardi_df, individual_events, cluster_events, windows, cardi_vars, lag) {
  results <- list()
  k <- 1L
  for (cardi_var in cardi_vars) {
    series_df <- cardi_df[c("date", cardi_var)]
    for (window_size in windows) {
      # Clustered events are the main specification because they reduce overlap.
      for (i in seq_len(nrow(cluster_events))) {
        results[[k]] <- run_intervention_one(series_df, cardi_var, cluster_events[i, ], window_size, "event_cluster_main", lag)
        k <- k + 1L
      }
      # Individual events are retained as robustness checks and overlap is flagged.
      for (i in seq_len(nrow(individual_events))) {
        results[[k]] <- run_intervention_one(series_df, cardi_var, individual_events[i, ], window_size, "individual_event_robustness", lag)
        k <- k + 1L
      }
    }
  }
  do.call(rbind, results)
}

# ---------------------- Reference-style event dummy tests ---------------------

plot_reference_event_series <- function(cardi_df, event_dates, output_path) {
  # Match the legacy plot style: first trading day of each year as the x-axis tick.
  plot_labels <- stats::aggregate(date ~ year, data = transform(cardi_df, year = format(date, "%Y")), min)$date

  plot_df <- cardi_df
  # Legacy naming: FRM_High_Low corresponds to the 5% CARDI series.
  plot_df$FRM_High_Low <- plot_df$CARDI_5P

  grDevices::png(output_path, width = 1000, height = 600, bg = "transparent")
  print(
    ggplot2::ggplot(plot_df, ggplot2::aes(x = date, y = FRM_High_Low)) +
      ggplot2::geom_point(color = "grey") +
      ggplot2::labs(x = "Date", y = "CARDI") +
      ggplot2::scale_x_date(
        breaks = plot_labels,
        labels = substr(as.character(plot_labels), 1, 7),
        expand = ggplot2::expansion(mult = c(0.01, 0.03))
      ) +
      ggplot2::geom_smooth(method = "loess", color = "red", span = 0.1) +
      ggplot2::geom_vline(
        xintercept = as.numeric(event_dates),
        linetype = "dashed",
        color = "blue",
        linewidth = 0.5
      ) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.key = ggplot2::element_rect(fill = "transparent"),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.title.y = ggplot2::element_text(size = 14),
        axis.text.x = ggplot2::element_text(size = 14),
        axis.text.y = ggplot2::element_text(size = 14),
        panel.border = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  )
  grDevices::dev.off()
  output_path
}

build_smoothed_frm_event_data <- function(cardi_df) {
  out <- cardi_df
  out$Id <- seq_len(nrow(out))
  out$FRM_High_Low <- out$CARDI_5P
  out$FRM_High_Low_1 <- out$CARDI_1P
  out$FRM_High_Low_10 <- out$CARDI_10P

  smooth_map <- c(
    SmoothFRMDiff = "FRM_High_Low",
    SmoothFRMDiff_1 = "FRM_High_Low_1",
    SmoothFRMDiff_10 = "FRM_High_Low_10"
  )

  for (smooth_name in names(smooth_map)) {
    source_col <- smooth_map[[smooth_name]]
    # LOESS smoothing follows the original Event_test.R reference workflow.
    fit <- stats::loess(stats::as.formula(paste(source_col, "~ Id")), data = out, span = 0.1)
    out[[smooth_name]] <- stats::predict(fit)
    out[[paste0("delta", smooth_name)]] <- c(NA_real_, diff(log(out[[smooth_name]])) * 100)
  }

  out
}

make_prior_event_dummy <- function(dates, event_dates, n_day) {
  vapply(dates, function(current_date) {
    start_date <- current_date - (n_day - 1L)
    # Dummy equals one if any event occurred in the trailing calendar-day window.
    as.integer(any(event_dates >= start_date & event_dates <= current_date, na.rm = TRUE))
  }, integer(1))
}

run_reference_event_regressions <- function(index_logit, event_dates, windows, lag, output_dir) {
  type_map <- c(
    SmoothFRMDiff = "CARDI_5P",
    SmoothFRMDiff_1 = "CARDI_1P",
    SmoothFRMDiff_10 = "CARDI_10P"
  )

  csv_files <- character()
  all_results <- list()

  for (type in names(type_map)) {
    event_relation <- data.frame(
      CARDI_variable = type_map[[type]],
      smooth_variable = type,
      event_window = windows,
      beta = NA_real_,
      T_Value = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      significance = NA_character_,
      obs = NA_integer_,
      Mean_Diff_Delta = NA_real_,
      Mean_Diff = NA_real_,
      Mean_Dummy = NA_real_,
      standard_error_type = "Newey-West",
      stringsAsFactors = FALSE
    )

    for (n_day in windows) {
      reg_df <- index_logit
      reg_df$Dummy_Event <- make_prior_event_dummy(reg_df$date, event_dates, n_day)
      if (nrow(reg_df) >= n_day) {
        # Drop early rows whose trailing event window is mechanically incomplete.
        reg_df <- reg_df[-seq_len(n_day - 1L), , drop = FALSE]
      }

      # Dependent variable mirrors the legacy code: log change in smoothed CARDI.
      reg_df$delta_FRM_High_Low <- c(NA_real_, diff(log(reg_df[[type]])) * 100)
      reg_df <- reg_df[is.finite(reg_df$delta_FRM_High_Low) & !is.na(reg_df$Dummy_Event), , drop = FALSE]

      if (nrow(reg_df) >= 10 && length(unique(reg_df$Dummy_Event)) > 1) {
        model <- stats::lm(delta_FRM_High_Low ~ Dummy_Event, data = reg_df)
        ct <- newey_west_coeftest(model, lag)
        if ("Dummy_Event" %in% rownames(ct)) {
          idx <- event_relation$event_window == n_day
          event_relation$beta[idx] <- ct["Dummy_Event", "Estimate"]
          event_relation$se[idx] <- ct["Dummy_Event", "Std. Error"]
          event_relation$T_Value[idx] <- ct["Dummy_Event", "t value"]
          event_relation$p_value[idx] <- ct["Dummy_Event", "Pr(>|t|)"]
          event_relation$significance[idx] <- stars(event_relation$p_value[idx])
          event_relation$obs[idx] <- stats::nobs(model)
        }
      }

      idx <- event_relation$event_window == n_day
      event_relation$Mean_Diff_Delta[idx] <- mean(reg_df$delta_FRM_High_Low, na.rm = TRUE)
      event_relation$Mean_Diff[idx] <- mean(reg_df$FRM_High_Low, na.rm = TRUE)
      event_relation$Mean_Dummy[idx] <- mean(reg_df$Dummy_Event, na.rm = TRUE)
    }

    csv_path <- file.path(output_dir, paste0("RegEvent_", type, ".csv"))
    utils::write.csv(event_relation, csv_path, row.names = FALSE, quote = FALSE)
    csv_files <- c(csv_files, csv_path)
    all_results[[type]] <- event_relation
  }

  list(csv_files = csv_files, results = do.call(rbind, all_results))
}

# ------------------------- Markov switching model ----------------------------

map_date_to_events <- function(target_date, cardi_dates, individual_events, cluster_events) {
  if (!inherits(target_date, "Date")) {
    target_date <- as.Date(target_date, origin = "1970-01-01")
  }

  i_event <- nearest_event_row(target_date, individual_events$event_date)
  i_cluster <- nearest_event_row(target_date, cluster_events$representative_event_date)

  data.frame(
    nearest_event_name = individual_events$event_name[i_event],
    nearest_event_date = individual_events$event_date[i_event],
    distance_to_event_calendar_days = as.integer(target_date - individual_events$event_date[i_event]),
    distance_to_event_trading_days = trading_distance(cardi_dates, target_date, individual_events$event_date[i_event]),
    nearest_cluster_id = cluster_events$cluster_id[i_cluster],
    nearest_cluster_event_name = cluster_events$event_name[i_cluster],
    nearest_cluster_date = cluster_events$representative_event_date[i_cluster],
    distance_to_cluster_calendar_days = as.integer(target_date - cluster_events$representative_event_date[i_cluster]),
    distance_to_cluster_trading_days = trading_distance(cardi_dates, target_date, cluster_events$representative_event_date[i_cluster]),
    stringsAsFactors = FALSE
  )
}

extract_mswm_probabilities <- function(msm_model, expected_n) {
  probs <- tryCatch(as.data.frame(msm_model@Fit@smoProb), error = function(e) NULL)
  if (is.null(probs)) return(NULL)
  probs[] <- lapply(probs, safe_numeric)

  # MSwM can include an initial t=0 probability row. Drop it so probabilities
  # align one-for-one with observed CARDI dates.
  if (nrow(probs) == expected_n + 1L) {
    probs <- probs[-1, , drop = FALSE]
  }

  probs
}

run_markov_one <- function(cardi_df, individual_events, cluster_events, cardi_var, thresholds) {
  if (!has_mswm) {
    return(list(
      probabilities = empty_df(),
      switches = data.frame(CARDI_variable = cardi_var, model_status = "MSwM not installed", stringsAsFactors = FALSE),
      plots = character()
    ))
  }

  series_df <- cardi_df[c("date", cardi_var)]
  names(series_df)[2] <- "value"
  series_df <- series_df[!is.na(series_df$value), , drop = FALSE]

  base_model <- stats::lm(value ~ 1, data = series_df)
  msm_model <- tryCatch(
    MSwM::msmFit(base_model, k = 2, sw = c(TRUE, TRUE), control = list(parallel = FALSE)),
    error = function(e) e
  )

  if (inherits(msm_model, "error")) {
    return(list(
      probabilities = empty_df(),
      switches = data.frame(CARDI_variable = cardi_var, model_status = msm_model$message, stringsAsFactors = FALSE),
      plots = character()
    ))
  }

  probs <- extract_mswm_probabilities(msm_model, nrow(series_df))
  if (is.null(probs) || nrow(probs) != nrow(series_df)) {
    return(list(
      probabilities = empty_df(),
      switches = data.frame(CARDI_variable = cardi_var, model_status = "Could not extract smoothed probabilities", stringsAsFactors = FALSE),
      plots = character()
    ))
  }

  regime_means <- colSums(probs * series_df$value, na.rm = TRUE) / colSums(probs, na.rm = TRUE)
  # The high-risk regime is whichever latent regime has the higher CARDI mean.
  high_regime_col <- which.max(regime_means)
  high_prob <- probs[[high_regime_col]]

  probability_table <- data.frame(
    CARDI_variable = cardi_var,
    date = series_df$date,
    CARDI_value = series_df$value,
    high_risk_probability = high_prob,
    high_risk_regime = names(probs)[high_regime_col],
    low_risk_regime = names(probs)[which.min(regime_means)],
    high_regime_mean = max(regime_means, na.rm = TRUE),
    low_regime_mean = min(regime_means, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  switch_rows <- list()
  k <- 1L
  for (threshold in thresholds) {
    # A switch date is the first day the high-risk probability crosses above threshold.
    crossed <- high_prob >= threshold & c(FALSE, head(high_prob, -1) < threshold)
    switch_dates <- series_df$date[crossed]

    if (length(switch_dates) == 0) {
      switch_rows[[k]] <- data.frame(
        CARDI_variable = cardi_var,
        threshold = threshold,
        regime_switch_date = as.Date(NA),
        nearest_event_name = NA_character_,
        nearest_event_date = as.Date(NA),
        distance_to_event_calendar_days = NA_integer_,
        distance_to_event_trading_days = NA_integer_,
        nearest_cluster_id = NA_character_,
        nearest_cluster_event_name = NA_character_,
        nearest_cluster_date = as.Date(NA),
        distance_to_cluster_calendar_days = NA_integer_,
        distance_to_cluster_trading_days = NA_integer_,
        model_status = "no threshold crossing",
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    } else {
      for (switch_date in switch_dates) {
        switch_date <- as.Date(switch_date, origin = "1970-01-01")
        switch_rows[[k]] <- cbind(
          data.frame(
            CARDI_variable = cardi_var,
            threshold = threshold,
            regime_switch_date = switch_date,
            model_status = "ok",
            stringsAsFactors = FALSE
          ),
          map_date_to_events(switch_date, series_df$date, individual_events, cluster_events)
        )
        k <- k + 1L
      }
    }
  }

  plot_df <- data.frame(
    date = rep(series_df$date, 2),
    value = c(series_df$value, high_prob),
    panel = rep(c(cardi_var, "High-risk regime probability"), each = nrow(series_df)),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "grey35", linewidth = 0.35) +
    ggplot2::geom_vline(
      xintercept = as.numeric(individual_events$event_date),
      color = "#2F6B9A",
      linetype = "dotted",
      alpha = 0.25
    ) +
    ggplot2::geom_vline(
      xintercept = as.numeric(cluster_events$representative_event_date),
      color = "#20854E",
      linetype = "dotdash",
      alpha = 0.55
    ) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "Date", y = NULL, title = paste0(cardi_var, ": Markov Switching Regime Probabilities")) +
    ggplot2::theme_classic()+
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )

  plot_path <- file.path(figure_dir, paste0("MarkovSwitching_", cardi_var, ".png"))
  ggplot2::ggsave(plot_path, p, width = 11, height = 7, dpi = 300, bg = "transparent")
  list(
    probabilities = probability_table,
    switches = do.call(rbind, switch_rows),
    plots = plot_path
  )
}

run_markov_tests <- function(cardi_df, individual_events, cluster_events, cardi_vars, thresholds) {
  all_prob <- list()
  all_switch <- list()
  all_plots <- character()

  for (cardi_var in cardi_vars) {
    res <- run_markov_one(cardi_df, individual_events, cluster_events, cardi_var, thresholds)
    all_prob[[cardi_var]] <- res$probabilities
    all_switch[[cardi_var]] <- res$switches
    all_plots <- c(all_plots, res$plots)
  }

  list(
    probabilities = do.call(rbind, all_prob),
    switches = do.call(rbind, all_switch),
    plots = all_plots
  )
}

# ---------------------------------- Main --------------------------------------

cardi_data <- load_cardi_data(cardi_file, cardi_column_map)
event_data <- load_events(event_file)
event_sets <- cluster_events(event_data, event_cluster_gap_days, event_windows)

cardi_vars <- c("CARDI_1P", "CARDI_5P", "CARDI_10P")

chow_results <- run_chow_tests(
  cardi_df = cardi_data,
  individual_events = event_sets$individual,
  cluster_events = event_sets$clusters,
  windows = event_windows,
  cardi_vars = cardi_vars
)

intervention_results <- run_intervention_tests(
  cardi_df = cardi_data,
  individual_events = event_sets$individual,
  cluster_events = event_sets$clusters,
  windows = event_windows,
  cardi_vars = cardi_vars,
  lag = nw_lag
)

reference_plot_file <- plot_reference_event_series(
  cardi_df = cardi_data,
  event_dates = event_data$event_date,
  output_path = file.path(figure_dir, "Plot_Event.png")
)

index_logit <- build_smoothed_frm_event_data(cardi_data)
frm_event_file <- file.path(table_dir, "FRM_Event.csv")
utils::write.csv(index_logit, frm_event_file, row.names = FALSE, quote = FALSE)

reference_regression_outputs <- run_reference_event_regressions(
  index_logit = index_logit,
  event_dates = event_data$event_date,
  windows = reference_event_windows,
  lag = nw_lag,
  output_dir = table_dir
)

markov_results <- run_markov_tests(
  cardi_df = cardi_data,
  individual_events = event_sets$individual,
  cluster_events = event_sets$clusters,
  cardi_vars = cardi_vars,
  thresholds = markov_thresholds
)

event_cluster_table <- event_sets$individual[order(event_sets$individual$event_date), , drop = FALSE]
cluster_table <- event_sets$clusters[order(event_sets$clusters$representative_event_date), , drop = FALSE]

saved_tables <- c(
  write_workbook(
    file.path(table_dir, "event_clusters.xlsx"),
    list(individual_events = event_cluster_table, event_clusters = cluster_table)
  ),
  write_workbook(
    file.path(table_dir, "chow_known_breakpoint_tests.xlsx"),
    list(
      all_results = chow_results,
      individual_events = chow_results[chow_results$analysis_level == "individual_event", ],
      event_clusters = chow_results[chow_results$analysis_level == "event_cluster", ]
    )
  ),
  write_workbook(
    file.path(table_dir, "intervention_newey_west_tests.xlsx"),
    list(
      all_results = intervention_results,
      cluster_main = intervention_results[intervention_results$analysis_level == "event_cluster_main", ],
      individual_robustness = intervention_results[intervention_results$analysis_level == "individual_event_robustness", ]
    )
  ),
  write_workbook(
    file.path(table_dir, "reference_event_dummy_newey_west.xlsx"),
    list(all_results = reference_regression_outputs$results)
  ),
  write_workbook(
    file.path(table_dir, "markov_switching_results.xlsx"),
    list(
      regime_probabilities = markov_results$probabilities,
      switch_event_comparison = markov_results$switches
    )
  )
)

saved_csv_files <- c(frm_event_file, reference_regression_outputs$csv_files)
saved_plot_files <- c(reference_plot_file, markov_results$plots)

cat("\nEvent structural-change workflow complete.\n")
cat("CARDI variables tested: ", paste(cardi_vars, collapse = ", "), "\n", sep = "")
cat("Event windows: +/-", paste(event_windows, collapse = ", +/-"), " trading days\n", sep = "")
cat("Reference event-dummy windows: ", paste(reference_event_windows, collapse = ", "), " calendar days\n", sep = "")
cat("Event cluster gap: ", event_cluster_gap_days, " calendar days\n", sep = "")
cat("Markov-switching thresholds: ", paste(markov_thresholds, collapse = ", "), "\n", sep = "")
cat("Intervention standard errors: Newey-West, lag = ", nw_lag, "\n", sep = "")
cat("\nSaved result tables:\n")
cat(paste0(" - ", saved_tables, collapse = "\n"), "\n", sep = "")
cat("\nSaved reference CSV files:\n")
cat(paste0(" - ", saved_csv_files, collapse = "\n"), "\n", sep = "")
cat("\nSaved plots:\n")
cat(paste0(" - ", saved_plot_files, collapse = "\n"), "\n", sep = "")
