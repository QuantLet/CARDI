# Missing-data orchestration.
#
# Default behavior is conservative: if an existing processed prerequisite is
# missing, stop with an explicit message. Set allow_reference_generation = TRUE
# in config only when you intentionally want to run reference scripts.

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

ensure_processed_frequency_inputs <- function(config) {
  ensure_dir(config$output_dir)

  required_paths <- c(
    config$fama_files[[config$frequency]],
    first_existing_path(config$cardi_files[[config$frequency]],
                        paste(config$frequency, "CARDI file")),
    config$macro_files[[config$frequency]]
  )
  missing <- required_paths[!file.exists(required_paths)]
  if (length(missing) == 0) return(invisible(TRUE))

  if (!isTRUE(config$allow_reference_generation)) {
    stop(
      "Missing required processed input(s):\n  ",
      paste(missing, collapse = "\n  "),
      "\nReference data-generation is disabled in this modular workflow. ",
      "Run the original processing scripts manually or set ",
      "allow_reference_generation = TRUE after reviewing overwrite behavior."
    )
  }

  stop(
    "Reference generation was requested, but this workflow does not run it ",
    "automatically because the reference scripts write to project output ",
    "locations. This protects existing files from accidental overwrite."
  )
}

save_new_dataset <- function(data, csv_path, rds_path = NULL) {
  ensure_dir(dirname(csv_path))
  if (file.exists(csv_path)) {
    stop("Refusing to overwrite existing file: ", csv_path)
  }
  write.csv(data, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
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
