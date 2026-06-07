# =============================================================================
# File    : functions_pure_return.R
# Purpose : Estimate "pure returns" as residuals from a pooled OLS regression
#           of firm excess returns on lagged firm controls and industry fixed
#           effects.
#
# Model (per frequency):
#   ExReturn_{i,t} = beta * X_{i,year(t)-1} + gamma_j * I(Industry=j) + e_{i,t}
#
#   Where:
#     X_{i,y-1} = [ lag_logAsset, lag_ROE, lag_BM, lag_CapExRatio ]
#                  (previous-year controls)
#     I(Industry=j) = industry fixed-effect indicators
#     e_{i,t}       = PureReturn  (the residual stored as PureReturn)
#
# Notes
#   - The regression is run on the full cross-time panel (not period-by-period).
#   - Industry fixed effects are absorbed via factor(IndustryCode) in lm().
#   - Industry groups with fewer than min_industry_count observations are
#     excluded from the FE to avoid near-singular design matrices.
#   - Rows missing ExReturn, any control, or IndustryCode are excluded from the
#     regression but retained in the output with PureReturn = NA.
#
# Author  : CARDI Research Team
# Date    : 2026-05-15
# Dependencies:
#   - functions_utils.R  (finite_complete)
#   - Base R (stats package)
# =============================================================================


#' Fit the pooled pure-return regression and extract residuals
#'
#' @description
#' Runs a single pooled OLS:
#'   ExReturn ~ lag_logAsset + lag_ROE + lag_BM + lag_CapExRatio +
#'              factor(IndustryCode)
#' and stores the residuals as PureReturn in the returned data.frame.
#'
#' Rows that were excluded from the regression (due to missing values) receive
#' PureReturn = NA.
#'
#' @param reg_data  data.frame containing at minimum: ExReturn, the lag_*
#'   control columns produced by \code{merge_lagged_controls()}, and
#'   IndustryCode.
#' @param frequency Character scalar "daily" or "monthly" (used in messages).
#' @param min_obs   Integer.  Minimum number of complete observations required
#'   to fit the model.  Default 50.
#' @param min_ind_count Integer.  Industry groups with fewer observations are
#'   excluded from the industry fixed effect.  Default 3.
#'
#' @return The input \code{reg_data} with an additional PureReturn column.
extract_pure_returns <- function(reg_data, frequency = "daily",
                                 min_obs = 50L, min_ind_count = 3L) {

  reg_data$PureReturn <- NA_real_

  # -- Determine which control variables are present -------------------------
  ctrl_vars <- intersect(
    c("lag_logAsset", "lag_ROE", "lag_BM", "lag_CapExRatio"),
    names(reg_data)
  )
  if (length(ctrl_vars) == 0) {
    warning("No lag_* control variables found for pure return regression (",
            frequency, "). Skipping.")
    return(reg_data)
  }
  if (!"IndustryCode" %in% names(reg_data)) reg_data$IndustryCode <- NA_character_

  # -- Identify complete rows ------------------------------------------------
  needed       <- c("ExReturn", ctrl_vars, "IndustryCode")
  complete_idx <- which(
    finite_complete(reg_data, c("ExReturn", ctrl_vars)) &
      !is.na(reg_data$IndustryCode) &
      nzchar(reg_data$IndustryCode)
  )

  if (length(complete_idx) < min_obs) {
    warning("Only ", length(complete_idx), " complete observations available ",
            "for the pure return regression (", frequency, "). ",
            "Minimum required: ", min_obs, ". Skipping.")
    return(reg_data)
  }

  fit_data <- reg_data[complete_idx, , drop = FALSE]

  # -- Filter out thinly represented industries ------------------------------
  ind_counts   <- table(fit_data$IndustryCode)
  valid_ind    <- names(ind_counts[ind_counts >= min_ind_count])
  valid_in_fit <- which(fit_data$IndustryCode %in% valid_ind)

  if (length(valid_in_fit) < min_obs) {
    warning("After filtering thin industries, only ", length(valid_in_fit),
            " observations remain (", frequency, "). Skipping.")
    return(reg_data)
  }

  # Map back to original row indices
  valid_rows <- complete_idx[valid_in_fit]
  fit_data   <- reg_data[valid_rows, , drop = FALSE]

  # -- Build regression formula ----------------------------------------------
  n_ind <- length(unique(fit_data$IndustryCode))
  rhs   <- if (n_ind >= 2) {
    paste(c(ctrl_vars, "factor(IndustryCode)"), collapse = " + ")
  } else {
    message("[pure_return] Only 1 industry code present; fitting without FE (",
            frequency, ").")
    paste(ctrl_vars, collapse = " + ")
  }

  form <- stats::as.formula(paste("ExReturn ~", rhs))

  # -- Fit model and extract residuals ---------------------------------------
  message("[pure_return] Fitting pooled OLS (", frequency, ") on ",
          nrow(fit_data), " observations, ",
          n_ind, " industry groups, ",
          length(ctrl_vars), " control variables.")

  fit <- tryCatch(
    stats::lm(form, data = fit_data),
    error = function(e) {
      warning("Pure return regression failed (", frequency, "): ", e$message)
      NULL
    }
  )

  if (is.null(fit)) return(reg_data)

  # Store residuals using the original row indices preserved by lm().
  # lm() names residuals by the row numbers of fit_data, which correspond
  # directly to valid_rows in reg_data.
  resid_vec <- stats::residuals(fit)
  # resid_vec is named by the row numbers of fit_data (1, 2, 3, ...)
  # We want to map these back to valid_rows in reg_data.
  reg_data$PureReturn[valid_rows] <- as.numeric(resid_vec)

  # Report fit diagnostics
  s <- summary(fit)
  message(sprintf(
    "[pure_return] Done (%s). R2=%.3f, adj.R2=%.3f, df.resid=%d",
    frequency, s$r.squared, s$adj.r.squared, s$df[2]
  ))

  reg_data
}
