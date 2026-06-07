# Process_CARDI_Frequency.R Code Process

This document explains the workflow implemented in
`Code/Data_Process/Process_CARDI_Frequency.R`.

The script converts daily CARDI variables into monthly and weekly datasets that
can be merged with portfolio returns, Fama factors, and macro controls. It does
not estimate CARDI. It uses the already processed daily CARDI file and changes
only the frequency of the CARDI information.

## Objective

The script creates two frequency-aligned CARDI datasets:

```text
Month_CARDI
Week_CARDI
```

For each frequency, it calculates:

```text
period CARDI level = mean of daily CARDI values inside the period
period CARDI log difference = log(period CARDI_t) - log(period CARDI_{t-1})
```

The monthly and weekly outputs are designed to use the same period identifiers
as the processed Fama factor files. This keeps later merges by `FrequencyID` or
`Month` consistent.

## Input Files

The script reads:

```text
Data/Processed/FRM_Carbon_risk.csv
Data/Processed/FamaFactors/FamaFactors_Monthly.xlsx
Data/Processed/FamaFactors/FamaFactors_Weekly.xlsx
```

The daily CARDI file must contain:

```text
Date or date-like column
CARDI_5P
CARDI_1P
CARDI_10P
```

The date column is detected automatically from common names:

```text
date
Date
TradingDate
trading_date
Trddt
日期
```

In the current data, the detected date column is:

```text
date
```

The Fama reference files must contain:

```text
Date
FrequencyID
```

These files provide the final monthly and weekly date alignment.

## Output Files

All outputs are written to:

```text
Data/Processed/CARDI/
```

The script writes:

```text
Month_CARDI.csv
Month_CARDI.xlsx
Week_CARDI.csv
Week_CARDI.xlsx
CARDI_Frequency_Validation.csv
```

The `.xlsx` files are written only if the `writexl` package is installed. The
CSV files are always written.

## Step 1: Locate the Project Root

The script starts from the running script directory and searches upward until it
finds both:

```text
Data/Processed/
Code/Data_Process/
```

It then sets the working directory to the project root. This allows the script
to run from the project root or directly from the code folder while preserving
project-relative paths.

## Step 2: Check Packages and Input Files

The script requires `readxl` to read the Fama monthly and weekly reference
files. It then checks that all required input files exist:

```text
FRM_Carbon_risk.csv
FamaFactors_Monthly.xlsx
FamaFactors_Weekly.xlsx
```

If any file is missing, the script stops with a clear error before generating
partial outputs.

## Step 3: Parse Dates

The helper function `parse_date()` handles common date formats:

```text
Date objects
POSIX date-time objects
YYYY-MM-DD
YYYY/MM/DD
YYYYMMDD
```

Rows with unparseable CARDI dates are removed. The remaining daily CARDI data
are sorted chronologically before aggregation.

## Step 4: Read Fama Date References

The function `read_fama_reference()` reads each Fama factor reference file and
keeps:

```text
Date
FrequencyID
```

It removes rows where either field is missing, removes duplicated
`FrequencyID` values, and sorts by `Date`.

The reason for using the Fama references is that later analysis also uses the
processed Fama factor files. Therefore, the CARDI monthly and weekly datasets
should use the same period labels as the factor datasets.

## Step 5: Read Daily CARDI

The function `read_cardi_daily()` reads:

```text
Data/Processed/FRM_Carbon_risk.csv
```

It detects the date column, parses it into `Date`, and converts the three CARDI
columns to numeric:

```text
CARDI_5P
CARDI_1P
CARDI_10P
```

The output of this step is a daily dataset with four columns:

```text
Date
CARDI_5P
CARDI_1P
CARDI_10P
```

## Step 6: Assign Each Daily Observation to a Reference Period

The function `assign_reference_period()` assigns each daily CARDI observation to
a monthly or weekly `FrequencyID`.

For monthly data, the raw period key is:

```text
YYYY-MM
```

For weekly data, the raw period key is the ISO week:

```text
YYYY-WW
```

If the raw period key exists in the Fama reference file, the observation uses
that key directly. If it does not exist, the script assigns the daily
observation to the next available Fama reference period whose reference `Date`
is on or after the CARDI date.

This fallback is important for weekly alignment because the project's processed
weekly Fama data may not contain every calendar ISO week exactly.

## Step 7: Aggregate Daily CARDI to Monthly and Weekly Frequency

The function `aggregate_cardi()` performs the main frequency conversion.

For each target period, it calculates finite-value means:

```text
CARDI_5P_M  = mean(CARDI_5P daily values in month)
CARDI_1P_M  = mean(CARDI_1P daily values in month)
CARDI_10P_M = mean(CARDI_10P daily values in month)
```

and:

```text
CARDI_5P_W  = mean(CARDI_5P daily values in week)
CARDI_1P_W  = mean(CARDI_1P daily values in week)
CARDI_10P_W = mean(CARDI_10P daily values in week)
```

The mean is calculated by `safe_mean()`, which first keeps only finite values.
If a period has no finite daily values, the period average is set to `NA`.

The reported period averages are not modified by the log-difference handling
described below.

## Step 8: Calculate CARDI Log Differences

After sorting by period, the script calculates log differences:

```text
CARDI_5P_LogDiff_M  = log(CARDI_5P_M)  - log(CARDI_5P_M lag 1)
CARDI_1P_LogDiff_M  = log(CARDI_1P_M)  - log(CARDI_1P_M lag 1)
CARDI_10P_LogDiff_M = log(CARDI_10P_M) - log(CARDI_10P_M lag 1)
```

and:

```text
CARDI_5P_LogDiff_W  = log(CARDI_5P_W)  - log(CARDI_5P_W lag 1)
CARDI_1P_LogDiff_W  = log(CARDI_1P_W)  - log(CARDI_1P_W lag 1)
CARDI_10P_LogDiff_W = log(CARDI_10P_W) - log(CARDI_10P_W lag 1)
```

Log differences require positive values. Therefore, the script uses
`carry_forward_positive()` only inside the log-difference calculation:

```text
If the period average is finite and positive:
    use the current period average
If the period average is missing, zero, negative, or infinite:
    use the previous finite positive period average
```

This means:

```text
reported CARDI average columns are unchanged
log-difference inputs are adjusted only when needed
```

In the current output, there are no non-positive average cells, so this
fallback is present as protection rather than an active adjustment.

## Step 9: Build Final Monthly Output

The monthly output contains:

```text
Date
FrequencyID
CARDI_5P_M
CARDI_1P_M
CARDI_10P_M
CARDI_5P_LogDiff_M
CARDI_1P_LogDiff_M
CARDI_10P_LogDiff_M
```

The current validation result is:

```text
Rows: 121
Columns: 8
Date range: 2015-01-31 to 2025-01-31
Duplicate FrequencyID: 0
Missing average cells: 0
Missing log-difference cells: 3
Non-positive average cells: 0
```

The three missing log-difference cells are expected because the first month has
no previous month for the three CARDI series.

## Step 10: Build Final Weekly Output

The weekly output contains:

```text
Date
FrequencyID
CARDI_5P_W
CARDI_1P_W
CARDI_10P_W
CARDI_5P_LogDiff_W
CARDI_1P_LogDiff_W
CARDI_10P_LogDiff_W
```

The current validation result is:

```text
Rows: 515
Columns: 8
Date range: 2015-01-11 to 2025-02-02
Duplicate FrequencyID: 0
Missing average cells: 0
Missing log-difference cells: 3
Non-positive average cells: 0
```

The three missing log-difference cells are expected because the first week has
no previous week for the three CARDI series.

## Validation File

The script writes:

```text
Data/Processed/CARDI/CARDI_Frequency_Validation.csv
```

The validation file records:

```text
Dataset
Rows
Columns
StartDate
EndDate
DuplicateFrequencyID
MissingAverageCells
MissingLogDiffCells
NonPositiveAverageCells
InputRows
InputStartDate
InputEndDate
DateColumnDetected
NonPositiveHandling
```

The current input validation values are:

```text
Input rows: 2447
Input start date: 2015-01-07
Input end date: 2025-01-27
Detected date column: date
```

## Economic Interpretation

The level variables, such as `CARDI_5P_M`, measure the average daily level of
carbon risk dynamics during a month or week.

The log-difference variables, such as `CARDI_5P_LogDiff_M`, measure whether the
period-level CARDI signal increased or decreased relative to the previous
period. These variables are useful when the research question is about the
change or acceleration of carbon-risk dynamics rather than the level alone.

## How to Run

From the project root:

```text
/Library/Frameworks/R.framework/Resources/bin/Rscript \
  Code/Data_Process/Process_CARDI_Frequency.R
```

If `Rscript` is available on the shell path:

```text
Rscript Code/Data_Process/Process_CARDI_Frequency.R
```
