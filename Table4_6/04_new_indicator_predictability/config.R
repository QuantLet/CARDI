# NewIndicator predictability stage config helpers.
# This stage reads NewIndicator inputs from 00_Submit/Table4/Data/NewIndicators.

new_indicator_table4_dir <- function() {
  if (exists("TABLE4_STAGE_DIR", inherits = TRUE)) {
    return(normalizePath(file.path(get("TABLE4_STAGE_DIR", inherits = TRUE),
                                   ".."),
                         mustWork = TRUE))
  }

  cwd <- normalizePath(getwd(), mustWork = TRUE)
  if (basename(cwd) == "04_new_indicator_predictability") {
    return(normalizePath(file.path(cwd, ".."), mustWork = TRUE))
  }
  if (basename(cwd) == "Table4") {
    return(cwd)
  }

  candidate <- file.path(cwd, "00_Submit", "Table4")
  if (dir.exists(candidate)) {
    return(normalizePath(candidate, mustWork = TRUE))
  }

  stop("Could not locate 00_Submit/Table4 from: ", cwd)
}

normalize_frequency <- function(frequency) {
  frequency <- tolower(as.character(frequency)[1])
  if (frequency %in% c("m", "month", "monthly")) return("monthly")
  if (frequency %in% c("w", "week", "weekly")) return("weekly")
  stop("Unsupported frequency: ", frequency)
}
