# Macro Variable Generation Code Process

This document explains the workflow implemented in
`Process_MacroVariables.R`.

The script generates two macro datasets:

```text
Month_Macro
Week_Macro
```

Both datasets use project-relative paths, align their dates to the processed
Fama factor files, and save outputs under:

```text
Data/Processed/Macro/
```

## Objective

The purpose is to create monthly and weekly macro controls for later portfolio
and regression analysis. The variables cover:

```text
1. carbon market volatility and returns
2. bank sector returns
3. real estate sector returns
4. real estate minus bank premium
5. monetary policy slope, TED, and TY_3m changes
6. market volatility
7. important carbon event dummies
```

All return variables in this script use log price differences:

```text
DailyReturn_t  = log(Price_t) - log(Price_{t-1})
PeriodReturn_t = log(EndPrice_t) - log(EndPrice_{t-1})
```

## Main Inputs

The script reads the following raw and processed files:

```text
Data/raw/CarbonMarketData/CNE_CEmissRightTrade.xlsx
Data/raw/macro/Bank_801780.SI.xlsx
Data/raw/macro/RealEstate_801180.SI.xlsx
Data/raw/macro/macro.xlsx
Data/raw/Important_Carbon_Events.xlsx
Data/raw/macro/000001.SH.xlsx
Data/Processed/FamaFactors/FamaFactors_Monthly.xlsx
Data/Processed/FamaFactors/FamaFactors_Weekly.xlsx
```

The Fama factor files provide the final target dates and frequency identifiers:

```text
Monthly reference: Date and FrequencyID from FamaFactors_Monthly.xlsx
Weekly reference:  Date and FrequencyID from FamaFactors_Weekly.xlsx
```

This makes the macro datasets easy to merge with the factor datasets.

## Main Outputs

The script writes:

```text
Data/Processed/Macro/Month_Macro.csv
Data/Processed/Macro/Month_Macro.xlsx
Data/Processed/Macro/Week_Macro.csv
Data/Processed/Macro/Week_Macro.xlsx
Data/Processed/Macro/MacroVariables_Validation.csv
```

The `.xlsx` outputs are written when the `writexl` package is available. The
CSV outputs and validation file are always written.

## Step 1: Locate Project Root

The script searches upward from the script location until it finds:

```text
Data/raw/
Data/Processed/
```

Then it sets the working directory to the project root. All paths inside the
script are project-relative, so the code can run from the project root or from
inside `Code/Data_Process`.

## Step 2: Validate Required Inputs

The `config` object stores all input and output paths. Before processing, the
script checks that every required input file exists.

If any input file is missing, the script stops with an error listing the missing
files. This prevents silent generation of incomplete macro datasets.

## Step 3: Shared Helpers

### 3.1 Date Parsing

The helper `parse_date()` handles several date formats:

```text
Date objects
POSIX date-time objects
YYYY-MM-DD strings
YYYY/MM/DD strings
YYYYMMDD strings
Excel serial dates
```

This is needed because the raw workbooks use mixed date representations. For
example, `macro.xlsx` can appear as Excel serial dates when read as text, while
other files use character dates.

### 3.2 Numeric Conversion

The helper `as_num()` converts numeric columns from text to numeric after
removing commas.

### 3.3 Frequency Keys

Monthly keys are:

```text
YYYY-MM
```

Weekly keys are ISO year-week:

```text
YYYY-WW
```

The weekly key matches the processed Fama weekly `FrequencyID` convention, such
as:

```text
2006-41
2006-42
```

### 3.4 Safe Aggregators

The script uses safe aggregation helpers:

```text
finite_sd(x) = standard deviation of finite values, NA if fewer than 2 values
safe_mean(x) = mean of finite values, NA if no finite values
```

This avoids misleading volatility values when a month or week has too few valid
daily observations.

## Step 4: Read Fama Date References

The function `read_fama_reference()` reads:

```text
Date
FrequencyID
```

from each Fama factor file. These references define the final row structure of
`Month_Macro` and `Week_Macro`.

For monthly data:

```text
Date = month-end date from FamaFactors_Monthly.xlsx
FrequencyID = YYYY-MM
```

For weekly data:

```text
Date = weekly period date from FamaFactors_Weekly.xlsx
FrequencyID = ISO year-week
```

## Step 5: Read Carbon Market Data

The function `read_carbon_daily()` reads:

```text
CNE_CEmissRightTrade.xlsx
```

It keeps:

```text
TradingDate
CityName
ClosePrice
AvgPrice
Amount
```

Only three carbon markets are kept:

```text
深圳 -> Shenzhen
广东 -> Guangdong
湖北 -> Hubei
```

### 5.1 Price Convention

The current convention is:

```text
Price = ClosePrice
```

If `ClosePrice` is missing or invalid, the script falls back to:

```text
Price = AvgPrice
```

This keeps the requested preference for `ClosePrice` while still allowing a
valid price when only `AvgPrice` is usable.

### 5.2 Multiple City-Date Observations

The raw carbon file can contain more than one trading product for the same city
and date. To preserve the earlier project convention, the script keeps the
first city-date observation after sorting by:

```text
Market
Date
original row order
```

This avoids duplicate market-date rows before calculating returns.

### 5.3 Daily Carbon Returns

Daily carbon returns are calculated within each market after sorting by date:

```text
DailyReturn_t = log(Price_t) - log(Price_{t-1})
```

The first observation in each market has missing daily return.

## Step 6: Read Sector and Market Index Prices

The function `read_price_index()` is used for:

```text
Bank_801780.SI.xlsx
RealEstate_801180.SI.xlsx
000001.SH.xlsx
```

The function identifies the date column as either:

```text
日期
```

or, for the bank workbook:

```text
...2
```

It identifies the price column as the first column whose name contains:

```text
收盘价
```

If no close-price column is available but `LogPrice` exists, the script uses:

```text
Price = exp(LogPrice)
```

This is relevant for `Bank_801780.SI.xlsx`, which includes a clean `LogPrice`
column and has been used this way by earlier project code.

Daily returns are calculated as:

```text
DailyReturn_t = log(Price_t) - log(Price_{t-1})
```

## Step 7: Read Monetary Policy Data

The function `read_monetary_daily()` reads:

```text
Data/raw/macro/macro.xlsx
```

The first four columns are renamed to:

```text
Date
TY_3m
TY_10Y
Shibor_3m
```

The script then calculates:

```text
Slope = TY_10Y - TY_3m
TED   = Shibor_3m - TY_3m
```

The daily monetary data are later aggregated to monthly and weekly frequency.

## Step 8: Read Important Carbon Events

The function `read_events()` reads:

```text
Data/raw/Important_Carbon_Events.xlsx
```

It requires:

```text
Date
Type
```

The observed event types are:

```text
China
International
Covid
```

The output event dummies are:

```text
Event_dummy
Event_Covid
Event_China
Event_International
```

The convention is:

```text
Event_dummy         = 1 if any event occurs in the period
Event_Covid         = 1 if Type is Covid
Event_China         = 1 if Type is China
Event_International = 1 if Type is International or Covid
```

Covid-related events are treated as international events. If an event falls in
a week that is missing from the Fama weekly reference, the script assigns it to
the next available Fama weekly period. For example, the 2020-01-30 Covid event
falls in missing ISO week `2020-05`, so it is assigned to Fama week `2020-06`
dated 2020-02-09.

## Step 9: Build Monthly and Weekly Carbon Variables

The function `make_market_period_panel()` creates carbon variables for each
market and frequency.

### 9.1 Carbon Volatility

Monthly carbon volatility is:

```text
CarbonVol_M_market =
    standard deviation of daily carbon returns within month
```

Weekly carbon volatility is:

```text
CarbonVol_W_market =
    standard deviation of daily carbon returns within week
```

The output columns are:

```text
CarbonVol_M_Shenzhen
CarbonVol_M_Guangdong
CarbonVol_M_Hubei
CarbonVol_W_Shenzhen
CarbonVol_W_Guangdong
CarbonVol_W_Hubei
```

### 9.2 Carbon Returns

The script first identifies the last available trading price in each
month/week. It then calculates:

```text
CarbonReturn_period =
    log(end_price_current_period) - log(end_price_previous_period)
```

This uses period-end alignment, not calendar-day assumptions.

The output columns are:

```text
CarbonReturn_M_Shenzhen
CarbonReturn_M_Guangdong
CarbonReturn_M_Hubei
CarbonReturn_W_Shenzhen
CarbonReturn_W_Guangdong
CarbonReturn_W_Hubei
```

Returns remain missing until a market has enough period-end prices. The script
does not fill carbon returns across missing markets.

## Step 10: Build Bank and Real Estate Returns

The function `make_price_return()` builds sector returns from period-end prices.

For monthly data:

```text
BankReturn_M =
    log(bank sector end-month price_t)
    - log(bank sector end-month price_{t-1})

RealEstateReturn_M =
    log(real estate end-month price_t)
    - log(real estate end-month price_{t-1})
```

For weekly data:

```text
BankReturn_W =
    log(bank sector end-week price_t)
    - log(bank sector end-week price_{t-1})

RealEstateReturn_W =
    log(real estate end-week price_t)
    - log(real estate end-week price_{t-1})
```

The real estate premium is calculated after both returns are merged:

```text
RealEstate_Premium_M = RealEstateReturn_M - BankReturn_M
RealEstate_Premium_W = RealEstateReturn_W - BankReturn_W
```

## Step 11: Build Monetary Policy Variables

The function `make_monetary_period()` creates three monetary variables.

### 11.1 Slope

Monthly:

```text
Slope_M = average daily Slope within month
```

Weekly:

```text
Slope_W = average daily Slope within week
```

### 11.2 TED

Monthly:

```text
TED_M = average daily TED within month
```

Weekly:

```text
TED_W = average daily TED within week
```

### 11.3 TY_3m Change

The script uses the last available `TY_3m` value in each period and calculates:

```text
TY3M_Change_period =
    TY_3m_end_current_period - TY_3m_end_previous_period
```

The output columns are:

```text
TY3M_Change_M
TY3M_Change_W
```

This is a level change, not a log change.

## Step 12: Build Market Volatility

The Shanghai market index is read from:

```text
Data/raw/macro/000001.SH.xlsx
```

Daily market returns are:

```text
DailyReturn_t = log(Price_t) - log(Price_{t-1})
```

Monthly market volatility is:

```text
MarketVol_M =
    standard deviation of daily market returns within month
```

Weekly market volatility is:

```text
MarketVol_W =
    standard deviation of daily market returns within week
```

## Step 13: Build Event Dummies

The function `make_event_period()` aggregates event dates into period-level
dummies.

Monthly output:

```text
Event_dummy_M
Event_Covid_M
Event_China_M
Event_International_M
```

Weekly output:

```text
Event_dummy_W
Event_Covid_W
Event_China_W
Event_International_W
```

Each dummy equals 1 when at least one corresponding event occurs in that
month/week, and 0 otherwise.

## Step 14: Assemble Final Datasets

The function `assemble_macro()` merges all component datasets into the Fama
reference date grid.

For monthly data, rows come from:

```text
FamaFactors_Monthly.xlsx
```

For weekly data, rows come from:

```text
FamaFactors_Weekly.xlsx
```

This guarantees:

```text
one row per Fama month or Fama week
stable Date
stable FrequencyID
```

The final monthly columns are:

```text
Date
FrequencyID
CarbonVol_M_Shenzhen
CarbonVol_M_Guangdong
CarbonVol_M_Hubei
CarbonReturn_M_Shenzhen
CarbonReturn_M_Guangdong
CarbonReturn_M_Hubei
BankReturn_M
RealEstateReturn_M
RealEstate_Premium_M
Slope_M
TED_M
TY3M_Change_M
MarketVol_M
Event_dummy_M
Event_Covid_M
Event_China_M
Event_International_M
```

The weekly dataset uses the same structure with `_W` suffixes.

## Step 15: Save Outputs

The script writes CSV outputs:

```text
Month_Macro.csv
Week_Macro.csv
```

If `writexl` is installed, it also writes:

```text
Month_Macro.xlsx
Week_Macro.xlsx
```

All files are saved under:

```text
Data/Processed/Macro/
```

## Step 16: Validation

The validation table is:

```text
Data/Processed/Macro/MacroVariables_Validation.csv
```

It reports:

```text
Dataset
Rows
Columns
StartDate
EndDate
DuplicateDates
MissingExpectedColumns
MissingCells
EventDummyCount
InputCarbonRows
InputBankRows
InputRealEstateRows
InputMonetaryRows
InputMarketRows
InputEventRows
ReturnConvention
CarbonPriceConvention
CarbonDuplicateConvention
EventTypeConvention
```

The script also stops with an error if either final dataset has duplicate
`Date` values.

## Current Verified Output

The current verified run produced:

```text
Month_Macro:
    rows    = 220
    columns = 19
    date range = 2006-10-31 to 2025-01-31
    duplicate dates = 0

Week_Macro:
    rows    = 936
    columns = 19
    date range = 2006-10-15 to 2025-02-02
    duplicate dates = 0
```

Event counts:

```text
Monthly any-event periods           = 19
Weekly any-event periods            = 19
Monthly Covid event periods         = 1
Weekly Covid event periods          = 1
Monthly China event periods         = 10
Weekly China event periods          = 10
Monthly International event periods = 9
Weekly International event periods  = 9
```

Some early carbon variables are missing because the carbon market data starts
after the Fama reference period starts. These missing values are retained rather
than filled, to avoid creating misleading carbon returns or volatilities.

## How to Run

From the project root:

```text
/Library/Frameworks/R.framework/Resources/bin/Rscript \
  Code/Data_Process/Process_MacroVariables.R
```

If `Rscript` is on the shell path:

```text
Rscript Code/Data_Process/Process_MacroVariables.R
```
