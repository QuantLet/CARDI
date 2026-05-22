# Event_test.R Documentation

## Purpose

`Event_test.R` tests whether carbon-related regulatory events are associated with structural changes in the daily CARDI time series. The script focuses on three CARDI variables:

- `CARDI_1P`
- `CARDI_5P`
- `CARDI_10P`

It combines event-window structural break tests, interrupted time-series regressions, a legacy-style event dummy regression, and Markov-switching regime analysis.

## Script Location

```text
Code/Plot_Code/Event_test.R
```

## Inputs

### CARDI Daily Series

```text
Data/Processed/FRM_Carbon_risk.csv
```

Expected date column:

- `date`, `Date`, or `DATE`

Expected CARDI columns:

- `CARDI_1P`
- `CARDI_5P`
- `CARDI_10P`

The script also preserves fallback mappings from the older naming convention:

- `FRM_High_Low_1` -> `CARDI_1P`
- `FRM_High_Low_5` -> `CARDI_5P`
- `FRM_High_Low_10` -> `CARDI_10P`

If the processed file already contains `CARDI_*` columns, those columns are used directly.

### Event Data

```text
Data/raw/Important_Carbon_Events.xlsx
```

Expected columns:

- `Date`
- `Event`
- `Type`

Only the date column is strictly required. If event names are missing, the script creates generic event labels.

## Outputs

### Result Tables

All result tables are written to:

```text
Output/Event_test/
```

Main outputs:

- `event_clusters.xlsx`
- `chow_known_breakpoint_tests.xlsx`
- `intervention_newey_west_tests.xlsx`
- `reference_event_dummy_newey_west.xlsx`
- `markov_switching_results.xlsx`
- `FRM_Event.csv`
- `RegEvent_SmoothFRMDiff.csv`
- `RegEvent_SmoothFRMDiff_1.csv`
- `RegEvent_SmoothFRMDiff_10.csv`

### Figures

All figures are written to:

```text
Output/Figure/
```

Main outputs:

- `Plot_Event.png`
- `MarkovSwitching_CARDI_1P.png`
- `MarkovSwitching_CARDI_5P.png`
- `MarkovSwitching_CARDI_10P.png`

## Configuration

The main configuration block appears near the top of the script.

### Event Windows

```r
event_windows <- c(30L, 60L)
```

These are symmetric trading-day windows around known event dates for the Chow tests and intervention regressions.

For example, `30L` means the local sample contains up to 30 observed CARDI trading days before and 30 observed CARDI trading days after the event.

### Reference Event-Dummy Windows

```r
reference_event_windows <- c(7L, 30L, 60L)
```

These windows follow the older reference workflow. They are calendar-day lookback windows. A daily observation receives `Dummy_Event = 1` if at least one event occurred in the previous `n` calendar days.

### Event Clustering

```r
event_cluster_gap_days <- 30L
```

Events within 30 calendar days of each other are grouped into one cluster. This is important because several regulatory events occur close together, and treating them as independent event shocks can overstate precision.

For each cluster, the script records:

- `cluster_id`
- `cluster_start_date`
- `cluster_end_date`
- `representative_event_date`
- `number_of_events_in_cluster`

The representative date is the first event date in the cluster.

### Newey-West Lag

```r
nw_lag <- 5L
```

This lag is used in all regression-based event tests requiring Newey-West standard errors.

The script intentionally avoids `summary(lm)` standard errors for event coefficients in the intervention and reference event-dummy regressions.

### Markov-Switching Thresholds

```r
markov_thresholds <- c(0.5, 0.8)
```

The Markov-switching module identifies switch dates when the smoothed probability of the high-CARDI regime crosses above these thresholds.

## Workflow Overview

The script runs in this order:

1. Load CARDI daily series.
2. Load carbon-related event data.
3. Sort and cluster events.
4. Run Chow known-breakpoint tests.
5. Run interrupted time-series regressions with Newey-West standard errors.
6. Generate the legacy-style event plot and smoothed CARDI variables.
7. Run reference-style event dummy regressions with Newey-West standard errors.
8. Run two-regime Markov-switching models.
9. Save Excel/CSV result tables and figures.

## Data Preparation

### Date Handling

The helper `parse_date()` accepts several common formats:

- `YYYY-MM-DD`
- `YYYY/MM/DD`
- `MM/DD/YYYY`
- `DD/MM/YYYY`
- `YYYYMMDD`

This matters because CARDI data and Excel-derived event files can use different date encodings.

### Sorting and Duplicate Dates

CARDI observations are sorted by date before testing. If duplicate trading dates exist, they are collapsed to one observation using the mean value for each CARDI variable.

### Nearest Trading Date

Some events may fall on weekends or non-trading days. The function `nearest_trading_index()` maps each event date to the closest available CARDI trading date.

## Event Clustering

The function `cluster_events()` sorts all events by date and assigns cluster IDs. A new cluster starts only when the gap from the previous event exceeds `event_cluster_gap_days`.

The script keeps both:

- individual event results
- clustered event results

For local event-window tests, overlap flags are added. A test is flagged if another event window overlaps the current event window.

## Chow Known-Breakpoint Tests

### Purpose

The Chow test checks whether the CARDI series has a structural change around a known event date.

### Local Regression Design

For each CARDI variable, event, and window, the script estimates:

```text
Restricted model:
CARDI_t = alpha + beta * time_t + error_t

Unrestricted model:
CARDI_t = alpha + beta * time_t
        + gamma * post_event_t
        + delta * time_t * post_event_t
        + error_t
```

The restricted model imposes one common trend across the event window. The unrestricted model allows a different intercept and trend after the event.

### Test Statistic

The script computes the Chow F statistic manually:

```text
F = ((RSS_restricted - RSS_unrestricted) / q)
    /
    (RSS_unrestricted / df_unrestricted)
```

where `q` is the number of restrictions.

### Output

Results are saved in:

```text
Output/Event_test/chow_known_breakpoint_tests.xlsx
```

Sheets:

- `all_results`
- `individual_events`
- `event_clusters`

Important columns:

- `CARDI_variable`
- `analysis_level`
- `event_name`
- `event_date`
- `window_size`
- `test_statistic`
- `p_value`
- `n_before_event`
- `n_after_event`
- `conclusion_10pct`
- `conclusion_5pct`
- `conclusion_1pct`
- `overlap_flag`

## Interrupted Time-Series Regressions

### Purpose

The intervention model tests whether event dates are associated with:

- same-day shocks
- post-event level shifts
- post-event trend changes

### Regression Formula

For each CARDI variable, event, and window:

```text
CARDI_t = alpha
        + beta1 * EventPulse_t
        + beta2 * PostEventStep_t
        + beta3 * PostEventTrend_t
        + error_t
```

Definitions:

- `EventPulse_t = 1` on the nearest trading day to the event date.
- `PostEventStep_t = 1` after the event trading date.
- `PostEventTrend_t` is a running post-event trading-day trend.

### Standard Errors

The script uses:

```r
sandwich::NeweyWest(model, lag = nw_lag, prewhite = FALSE, adjust = TRUE)
lmtest::coeftest(model, vcov. = vcov_nw)
```

This ensures the reported standard errors for event coefficients are Newey-West standard errors, not ordinary OLS standard errors.

### Main and Robustness Specifications

The script estimates:

- clustered-event models as the main specification
- individual-event models as robustness checks

### Output

Results are saved in:

```text
Output/Event_test/intervention_newey_west_tests.xlsx
```

Sheets:

- `all_results`
- `cluster_main`
- `individual_robustness`

Important columns:

- `CARDI_variable`
- `coefficient_name`
- `estimate`
- `newey_west_se`
- `t_statistic`
- `p_value`
- `significance`
- `n_observations`
- `standard_error_type`
- `analysis_level`
- `event_name`
- `event_date`
- `cluster_id`
- `overlap_flag`

## Reference-Style Event Dummy Regressions

### Purpose

This module preserves the older `Event_test.R` logic, but updates the regression standard errors to Newey-West standard errors.

### Plot

The function `plot_reference_event_series()` creates the legacy event plot:

- grey CARDI points
- red LOESS smooth
- blue event-date vertical lines
- red horizontal reference line at `1`

Output:

```text
Output/Figure/Plot_Event.png
```

### Smoothed CARDI Variables

The function `build_smoothed_frm_event_data()` creates:

- `SmoothFRMDiff` from `CARDI_5P`
- `SmoothFRMDiff_1` from `CARDI_1P`
- `SmoothFRMDiff_10` from `CARDI_10P`

It also computes log differences:

- `deltaSmoothFRMDiff`
- `deltaSmoothFRMDiff_1`
- `deltaSmoothFRMDiff_10`

Output:

```text
Output/Event_test/FRM_Event.csv
```

### Event Dummy Construction

For each daily observation and each lookback window, the script defines:

```text
Dummy_Event_t = 1
```

if at least one event occurred between:

```text
t - (n_day - 1)
```

and:

```text
t
```

Otherwise:

```text
Dummy_Event_t = 0
```

The early rows with incomplete lookback windows are dropped.

### Regression Formula

For each smoothed CARDI variable and event window:

```text
delta_FRM_High_Low_t = alpha
                     + beta * Dummy_Event_t
                     + error_t
```

The dependent variable is:

```text
100 * diff(log(smoothed_CARDI))
```

### Standard Errors

The coefficient on `Dummy_Event` uses Newey-West standard errors.

### Outputs

CSV outputs:

```text
Output/Event_test/RegEvent_SmoothFRMDiff.csv
Output/Event_test/RegEvent_SmoothFRMDiff_1.csv
Output/Event_test/RegEvent_SmoothFRMDiff_10.csv
```

Combined Excel output:

```text
Output/Event_test/reference_event_dummy_newey_west.xlsx
```

Important columns:

- `CARDI_variable`
- `smooth_variable`
- `event_window`
- `beta`
- `T_Value`
- `se`
- `p_value`
- `significance`
- `obs`
- `Mean_Diff_Delta`
- `Mean_Diff`
- `Mean_Dummy`
- `standard_error_type`

## Markov-Switching Model

### Purpose

The Markov-switching module tests whether each CARDI series moves between low-risk and high-risk latent regimes, and whether regime switches occur near regulatory events.

### Model

For each CARDI variable, the script estimates a two-regime Markov-switching model using:

```r
MSwM::msmFit(base_model, k = 2, sw = c(TRUE, TRUE))
```

where the base model is:

```r
CARDI_t ~ 1
```

The model allows the intercept and variance to switch across two regimes.

### High-Risk Regime

The high-risk regime is identified as the regime with the higher probability-weighted mean CARDI value.

### Smoothed Probabilities

The script extracts smoothed regime probabilities from:

```r
msm_model@Fit@smoProb
```

Some versions of `MSwM` include an initial `t = 0` probability row. The script drops that extra row when needed so the probabilities align with the CARDI trading dates.

### Regime Switch Dates

For each threshold in `markov_thresholds`, the script identifies switch dates where:

```text
P(high-risk regime)_t >= threshold
```

and:

```text
P(high-risk regime)_{t-1} < threshold
```

### Event Matching

Each regime switch date is matched to:

- the nearest individual event date
- the nearest event-cluster representative date

The script reports both calendar-day and trading-day distances.

### Outputs

Results are saved in:

```text
Output/Event_test/markov_switching_results.xlsx
```

Sheets:

- `regime_probabilities`
- `switch_event_comparison`

Plots:

```text
Output/Figure/MarkovSwitching_CARDI_1P.png
Output/Figure/MarkovSwitching_CARDI_5P.png
Output/Figure/MarkovSwitching_CARDI_10P.png
```

## Important Econometric Notes

### Newey-West Requirement

The script reports Newey-West standard errors for regression-based event coefficients in:

- interrupted time-series regressions
- reference-style event dummy regressions

The helper function is:

```r
newey_west_coeftest <- function(model, lag) {
  vcov_nw <- sandwich::NeweyWest(model, lag = lag, prewhite = FALSE, adjust = TRUE)
  lmtest::coeftest(model, vcov. = vcov_nw)
}
```

### Chow Tests

The Chow test uses an F statistic based on restricted and unrestricted residual sums of squares. It is not a Newey-West regression table. The Newey-West requirement applies to regression-based event coefficients.

### Event Overlap

Many event windows overlap. The script flags overlapping windows and provides clustered-event results to reduce the risk of treating nearby events as independent shocks.

### Non-Trading Event Dates

Events are matched to the nearest available CARDI trading date for event-window tests and intervention regressions.

## Package Requirements

Required packages:

```r
readxl
openxlsx
ggplot2
sandwich
lmtest
```

Optional package:

```r
MSwM
```

If `MSwM` is missing, the Markov-switching section returns a status message instead of silently failing.

## How to Run

From the project root:

```bash
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/Plot_Code/Event_test.R
```

The script prints a concise summary of saved tables, CSV files, and plots at the end.

## Interpretation Guide

### Chow Test Results

A small p-value indicates evidence of a known structural break around the event date within the chosen local window.

### Intervention Results

Interpret coefficients as:

- `EventPulse`: same-day event shock.
- `PostEventStep`: persistent level shift after the event.
- `PostEventTrend`: change in post-event trend.

### Reference Event Dummy Results

The `beta` coefficient measures whether smoothed CARDI log changes differ on days following recent event occurrence.

### Markov Results

High-risk regime probabilities near one indicate the model classifies the day as a high-CARDI regime day. Switch-event comparison tables show whether probability threshold crossings occur near known carbon events.
