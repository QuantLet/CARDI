# Process Fama Factor Code Process

This document explains the workflow implemented in `Process_FamaFactor.R`.

The script creates cleaned Fama factor datasets at daily, weekly, and monthly
frequencies. It also reconstructs two market-premium style variables:

```text
RiskPremium   = MarketReturn - RiskFreeRate
MarketPremium = IndexMarketReturn - IndexRiskFreeRate
```

The two variables use different source files and are kept separately to avoid
mixing definitions.

## Output Location

All generated files are written to:

```text
Data/Processed/FamaFactors/
```

The script does not overwrite or edit raw files.

## Main Input Files

### Fama Factor Files

```text
Data/raw/Famafactor/STK_MKT_FIVEFACDAY.xlsx
Data/raw/Famafactor/STK_MKT_FIVEFACWEEK.xlsx
Data/raw/Famafactor/STK_MKT_FIVEFACMONTH.xlsx
```

These files use CSMAR-style metadata rows:

```text
row 1 = machine-readable column names
row 2 = Chinese column labels
row 3 = units/comments
row 4 onward = data
```

The script reads row 1 as column names and skips rows 2-3.

### TRD Market Return Files

```text
Data/raw/MarketReturns/TRD_Dalym.xlsx
Data/raw/MarketReturns/TRD_Weekm.xlsx
Data/raw/MarketReturns/TRD_Mont.xlsx
```

These files also use CSMAR-style metadata rows. The script keeps only:

```text
Markettype = 1
```

This is the Shanghai A-share market (`上证A股`).

Column mappings:

```text
Daily requested date column Trdt -> actual workbook column Trddt
Weekly requested TradingWeek     -> actual workbook column Trdwnt
Monthly date column              -> Trdmnt
```

### TRD Risk-Free Rate File

```text
Data/raw/MarketReturns/TRDNEW_Nrrate.xlsx
```

This file provides CSMAR risk-free rates:

```text
Clsdt
Nrrdata
Nrrdaydt
Nrrwkdt
Nrrmtdt
```

The script uses:

```text
Daily:   Nrrdaydt
Weekly:  Nrrwkdt
Monthly: Nrrmtdt
```

The raw values are labeled as percent, so they are divided by 100 before being
subtracted from decimal market returns.

### Macro Market Premium Files

```text
Data/raw/macro/000001.SH.xlsx
Data/raw/macro/macro.xlsx
```

`000001.SH.xlsx` provides the Shanghai Composite close price:

```text
收盘价(元)
```

`macro.xlsx` provides the annualized 3-month government bond yield:

```text
中债国债到期收益率:3个月
```

The macro risk-free series starts in 2006, so the main output files are filtered
to rows where the macro-based market premium can be calculated.

## Step 1: Project Root and Package Setup

The script detects the project root by searching upward until it finds:

```text
Data/raw/
Code/
```

Required R packages:

```text
readxl
writexl
```

The output directory is created if needed:

```text
Data/Processed/FamaFactors/
```

## Step 2: Robust Date and Numeric Cleaning

The helper `parse_date()` handles:

```text
Date objects
POSIX datetime objects
Excel numeric dates
YYYY-MM-DD strings
YYYY/MM/DD strings
YYYYMMDD strings
```

The helper `to_numeric()` converts numeric-looking text to numeric values and
removes commas before conversion.

These helpers are used across Fama, market return, risk-free, and macro files.

## Step 3: Read CSMAR-Style Excel Files

The helper `read_csmar_excel()` reads files with metadata rows:

```text
1. read the first three rows without column names
2. use row 1 as machine-readable column names
3. store row 2 as Chinese labels
4. store row 3 as units
5. read the actual data with skip = 3
```

This is used for:

```text
Fama factor files
TRD market return files
TRDNEW_Nrrate.xlsx
```

## Step 4: Select Fama Factor Rows and Columns

The script keeps only rows satisfying:

```text
Portfolios = 1
MarkettypeID = P9709
```

`P9709` is labeled:

```text
沪深A股和创业板
```

The script selects Fama factor columns by reading the Chinese labels and keeping
columns whose labels contain:

```text
总市值加权
```

The market factor columns from the Fama file are not kept because market
premia are reconstructed separately. The retained non-market five-factor
columns are:

```text
SMB2
HML2
RMW2
CMA2
```

## Step 5: TRD-Based RiskPremium

The script preserves the earlier TRD-based market premium calculation:

```text
RiskPremium = MarketReturn - RiskFreeRate
```

### MarketReturn

`MarketReturn` comes from `Markettype = 1` in the TRD market return files:

```text
Daily:   Dretwdtl from TRD_Dalym.xlsx
Weekly:  Wretwdtl from TRD_Weekm.xlsx
Monthly: Mretwdtl from TRD_Mont.xlsx
```

### RiskFreeRate

`RiskFreeRate` comes from `TRDNEW_Nrrate.xlsx`:

```text
Daily:   Nrrdaydt / 100
Weekly:  Nrrwkdt / 100
Monthly: Nrrmtdt / 100
```

For weekly data, the risk-free series is aligned using ISO week keys:

```text
%G-%V
```

For monthly data, the risk-free series is aligned using calendar months:

```text
%Y-%m
```

For weekly and monthly frequencies, the script uses the last available `Clsdt`
inside each week or month.

## Step 6: Index-Based MarketPremium

The script also constructs a close-price-based market premium:

```text
MarketPremium = IndexMarketReturn - IndexRiskFreeRate
```

This is based on:

```text
IndexMarketReturn: 000001.SH close-price return
IndexRiskFreeRate: converted annualized 3-month government bond yield
```

### Daily IndexMarketReturn

Daily return is:

```text
IndexMarketReturn_t =
    Close_t / Close_{t-1} - 1
```

where `Close` is `收盘价(元)` from `000001.SH.xlsx`.

### Weekly IndexMarketReturn

The script groups daily close-price returns by ISO week:

```text
%G-%V
```

Weekly return is compounded from daily returns:

```text
IndexMarketReturn_week =
    product(1 + daily_return) - 1
```

The weekly output date is the last available index trading date in that week.

### Monthly IndexMarketReturn

The script groups daily close-price returns by calendar month:

```text
%Y-%m
```

Monthly return is compounded from daily returns:

```text
IndexMarketReturn_month =
    product(1 + daily_return) - 1
```

The monthly output date is the last available index trading date in that month.

## Step 7: Convert Annualized Macro Risk-Free Rate

The macro risk-free rate is annualized and reported as a percentage. The script
first converts it to decimal:

```text
rf_annual = raw_percent / 100
```

Then it uses compound conversion.

### Daily

```text
IndexRiskFreeRate_daily =
    (1 + rf_annual)^(1 / 365) - 1
```

### Weekly

For each ISO week, the script takes the last available macro observation in that
week and converts it to a weekly return:

```text
IndexRiskFreeRate_weekly =
    (1 + rf_annual_last_week_date)^(7 / 365) - 1
```

### Monthly

For each month, the script takes the last available macro observation in that
month and converts it to a monthly return:

```text
IndexRiskFreeRate_monthly =
    (1 + rf_annual_last_month_date)^(1 / 12) - 1
```

## Step 8: Merge Logic

For each frequency, the script merges:

```text
1. filtered Fama factor rows
2. TRD Markettype = 1 market return
3. TRDNEW risk-free rate
4. index-based market premium from 000001.SH and macro.xlsx
```

The merge keys are:

```text
Daily:   YYYY-MM-DD date string
Weekly:  ISO week key, %G-%V
Monthly: calendar month key, %Y-%m
```

After the merge, the main Excel outputs are filtered to rows where both:

```text
IndexRiskFreeRate is available
MarketPremium is available
```

This is why the final output starts in 2006 instead of 1994. The macro risk-free
file begins in October 2006.

Rows excluded by this filter remain available in the validation-detail CSVs.

## Step 9: Main Output Files

The main cleaned Excel files are:

```text
Data/Processed/FamaFactors/FamaFactors_Daily.xlsx
Data/Processed/FamaFactors/FamaFactors_Weekly.xlsx
Data/Processed/FamaFactors/FamaFactors_Monthly.xlsx
```

Final columns are:

```text
Date
FrequencyID
MarkettypeID
Markettype
MarketReturn
RiskFreeRate
RiskPremium
IndexMarketReturn
IndexRiskFreeRate
MarketPremium
SMB2
HML2
RMW2
CMA2
```

Column meanings:

```text
MarketReturn       = TRD Markettype 1 return
RiskFreeRate       = TRDNEW frequency-specific risk-free rate
RiskPremium        = MarketReturn - RiskFreeRate
IndexMarketReturn  = 000001.SH close-price return
IndexRiskFreeRate  = macro 3-month government bond yield converted to frequency
MarketPremium      = IndexMarketReturn - IndexRiskFreeRate
SMB2/HML2/RMW2/CMA2 = total-market-cap weighted non-market Fama factors
```

## Step 10: Validation Outputs

The script writes one summary validation file:

```text
Data/Processed/FamaFactors/FamaFactors_Validation.csv
```

It reports:

```text
input row counts
rows before market-premium filtering
final output row counts
date ranges
missing RiskPremium counts
missing MarketPremium counts
unmatched market return rows
unmatched risk-free rows
final column names
output file paths
validation detail file paths
```

The script also writes row-level validation files:

```text
FamaFactors_Daily_ValidationDetail.csv
FamaFactors_Weekly_ValidationDetail.csv
FamaFactors_Monthly_ValidationDetail.csv
```

These keep diagnostics for rows that were excluded from the main Excel files.
They include missingness flags such as:

```text
MissingMarketReturn
MissingRiskFreeRate
MissingRiskPremium
MissingIndexMarketReturn
MissingMacroRiskFreeRate
MissingMarketPremium
```

## Step 11: Conversion Tests

The script writes a small test file:

```text
Data/Processed/FamaFactors/MarketPremium_ConversionTests.csv
```

It checks:

```text
1. weekly compound RF conversion
2. monthly compound RF conversion
3. market premium subtraction
```

The expected formulas are:

```text
weekly  = (1 + rf_annual)^(7 / 365) - 1
monthly = (1 + rf_annual)^(1 / 12) - 1
premium = market_return - converted_risk_free_return
```

The script stops if any test fails.

## Current Validation Summary

The latest successful run produced:

```text
Daily:
  rows before market-premium filter = 7858
  final output rows = 4453
  date range = 2006-10-09 to 2025-01-27
  missing MarketPremium = 0

Weekly:
  rows before market-premium filter = 1636
  final output rows = 936
  date range = 2006-10-15 to 2025-02-02
  missing MarketPremium = 0

Monthly:
  rows before market-premium filter = 388
  final output rows = 220
  date range = 2006-10-31 to 2025-01-31
  missing MarketPremium = 0
```

Weekly still has some missing `RiskPremium` values from the TRD-based source
because `Wretwdtl` is unavailable for some `Markettype = 1` weeks. This does
not affect the index-based `MarketPremium`, which is complete after filtering.

## How to Run

From the project root:

```bash
/usr/local/bin/Rscript Code/Data_Process/Process_FamaFactor.R
```

The script can also be run from another directory because it detects the project
root automatically.
