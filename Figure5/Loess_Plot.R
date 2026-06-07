################################################################################
# Loess_Plot.R
#
# Purpose: Generate Figure 5 using the reference event-series LOESS plot only.
# Dependencies: readxl, ggplot2
################################################################################

rm(list = ls(all = TRUE))

# ----------------------------- Configuration ---------------------------------

project_root <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Figure5"
cardi_file <- file.path(project_root, "Data", "Processed", "FRM_Carbon_risk.csv")
event_file <- file.path(project_root, "Data", "raw", "Important_Carbon_Events.xlsx")
figure_dir <- file.path(project_root, "Output", "Figure")
reference_plot_file <- file.path(figure_dir, "Plot_Event.png")

cardi_column_map <- c(
  CARDI_1P = "FRM_High_Low_1",
  CARDI_5P = "FRM_High_Low_5",
  CARDI_10P = "FRM_High_Low_10"
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------ Dependencies ---------------------------------

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required. Install it with install.packages('",
      pkg, "').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

invisible(lapply(c("readxl", "ggplot2"), require_package))

# ------------------------------- Utilities -----------------------------------

parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  as.Date(as.character(x), tryFormats = c(
    "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y", "%Y%m%d"
  ))
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

load_cardi_data <- function(path, column_map) {
  if (!file.exists(path)) stop("Missing CARDI file: ", path, call. = FALSE)
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  date_col <- intersect(c("date", "Date", "DATE"), names(raw))[1]
  if (is.na(date_col)) stop("CARDI file must contain a date column.", call. = FALSE)

  available_map <- column_map
  for (cardi_name in names(available_map)) {
    if (cardi_name %in% names(raw)) available_map[[cardi_name]] <- cardi_name
  }
  if (!"CARDI_5P" %in% names(raw) &&
      !"FRM_High_Low_5" %in% names(raw) &&
      "FRM_High_Low" %in% names(raw)) {
    available_map[["CARDI_5P"]] <- "FRM_High_Low"
  }

  missing_cols <- available_map[!available_map %in% names(raw)]
  if (length(missing_cols) > 0) {
    stop("Missing CARDI source columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  out <- data.frame(date = parse_date(raw[[date_col]]), stringsAsFactors = FALSE)
  for (cardi_name in names(available_map)) {
    out[[cardi_name]] <- safe_numeric(raw[[available_map[[cardi_name]]]])
  }
  out <- out[!is.na(out$date), , drop = FALSE]
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL

  if (any(duplicated(out$date))) {
    out <- stats::aggregate(. ~ date, data = out, FUN = function(z) mean(z, na.rm = TRUE))
    out <- out[order(out$date), , drop = FALSE]
  }
  out
}

load_events <- function(path) {
  if (!file.exists(path)) stop("Missing event file: ", path, call. = FALSE)
  raw <- readxl::read_excel(path)
  date_col <- intersect(c("Date", "date", "EVENT_DATE", "event_date"), names(raw))[1]
  if (is.na(date_col)) stop("Event file must contain a Date column.", call. = FALSE)

  out <- data.frame(event_date = parse_date(raw[[date_col]]), stringsAsFactors = FALSE)
  out <- out[!is.na(out$event_date), , drop = FALSE]
  out <- out[order(out$event_date), , drop = FALSE]
  rownames(out) <- NULL
  out
}

plot_reference_event_series <- function(cardi_df, event_dates, output_path) {
  plot_labels <- stats::aggregate(
    date ~ year,
    data = transform(cardi_df, year = format(date, "%Y")),
    min
  )$date

  plot_df <- cardi_df
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
      ggplot2::geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "red",
        linewidth = 0.5
      ) +
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

# ---------------------------------- Main --------------------------------------

cardi_data <- load_cardi_data(cardi_file, cardi_column_map)
event_data <- load_events(event_file)

saved_plot_file <- plot_reference_event_series(
  cardi_df = cardi_data,
  event_dates = event_data$event_date,
  output_path = reference_plot_file
)

cat("\nFigure 5 LOESS event plot complete.\n")
cat("Saved figure:\n")
cat(" - ", saved_plot_file, "\n", sep = "")
