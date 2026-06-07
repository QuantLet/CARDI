# NewIndicator Code Explanation

## 1. Purpose

The `Code/NewIndicator` folder builds additional systemic-risk indicators for
the high-carbon and low-carbon stock pools. These indicators are intended to
complement the main CARDI/FRM analysis by measuring volatility, connectedness,
downside co-movement, tail loss, and network centrality.

The scripts are designed to run in sequence:

```text
01_Volatility_Index.R
02_DY_Index.R
03_EDC_CVaR_Index.R
04_Centrality_Index.R
05_Combine_All_Indicators.R
```

The first four scripts generate separate daily and monthly indicators. The
fifth script merges the daily outputs into one combined daily dataset and one
combined monthly dataset.

## 2. Common Project Structure

All scripts use the project root:

```r
wdir <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
setwd(wdir)
```

The main input folders are:

```text
Data/Processed/Input/HighCarbonIntens/20140704-20250127/
Data/Processed/Input/LowCarbonIntens/20140704-20250127/
```

The main output folders are:

```text
Output/NewIndicators/Daily/
Output/NewIndicators/Monthly/
```

The scripts create these output folders if they do not already exist.

## 3. Input Data

### 3.1 Price data

For high-carbon stocks:

```text
Data/Processed/Input/HighCarbonIntens/20140704-20250127/
HighCarbonIntens_Price_20250127.csv
```

For low-carbon stocks:

```text
Data/Processed/Input/LowCarbonIntens/20140704-20250127/
LowCarbonIntens_Price_20250127.csv
```

These files are stock price panels. The first column is the date, and each
remaining column is one stock.

### 3.2 Market-cap data

For high-carbon stocks:

```text
HighCarbonIntens_Mktcap_20250127.csv
```

For low-carbon stocks:

```text
LowCarbonIntens_Mktcap_20250127.csv
```

Market capitalization is used in `02_DY_Index.R` to choose the top `J` stocks
by average market cap.

### 3.3 FRM adjacency matrices

`04_Centrality_Index.R` reads precomputed FRM adjacency matrices from:

```text
Output/HighCarbonIntens/Adj_Matrices/
Output/LowCarbonIntens/Adj_Matrices/
```

These matrices must already exist before running the centrality script.

## 4. Output Data

Daily outputs:

```text
Output/NewIndicators/Daily/Volatility_All.csv
Output/NewIndicators/Daily/DY_HC.csv
Output/NewIndicators/Daily/DY_LC.csv
Output/NewIndicators/Daily/DY_All.csv
Output/NewIndicators/Daily/EDC_All.csv
Output/NewIndicators/Daily/CVaR_All.csv
Output/NewIndicators/Daily/Centrality_HC.csv
Output/NewIndicators/Daily/Centrality_LC.csv
Output/NewIndicators/Daily/Centrality_All.csv
Output/NewIndicators/Daily/All_Indicators_Daily.csv
```

Monthly outputs:

```text
Output/NewIndicators/Monthly/Volatility_Monthly.csv
Output/NewIndicators/Monthly/DY_Monthly.csv
Output/NewIndicators/Monthly/EDC_Monthly.csv
Output/NewIndicators/Monthly/CVaR_Monthly.csv
Output/NewIndicators/Monthly/Centrality_Monthly.csv
Output/NewIndicators/Monthly/All_Indicators_Monthly.csv
```

## 5. Script 01: `01_Volatility_Index.R`

### 5.1 Objective

This script calculates volatility indicators for the high-carbon and
low-carbon pools.

It creates two volatility measures:

1. Return-based volatility.
2. Volatility of fitted VaR values from the FRM pipeline.

### 5.2 Return-based volatility

The script reads the price panel and converts prices into log returns:

```r
r_t = log(P_t) - log(P_{t-1})
```

Missing, `NaN`, and infinite return values are replaced with zero:

```r
log_ret[is.na(log_ret) | is.infinite(log_ret)] <- 0
```

For each date, after the first rolling window, it calculates the rolling
standard deviation for each stock over a 63-day window:

```r
s <- 63
```

The pool-level volatility index is the cross-sectional average of stock-level
rolling volatilities:

```r
Vol_pool_t = mean_i(sd(r_i, window = 63))
```

### 5.3 VaR-volatility measure

The script first tries to read precomputed VaR-volatility files:

```text
Output/HighCarbonIntens/FitQr/Volatility_VaR5.csv
Output/LowCarbonIntens/FitQr/Volatility_VaR5.csv
```

If the CSV exists, the script calculates the cross-sectional mean across stocks
for each date.

If the CSV does not exist, the script falls back to the fitted quantile
regression RDS file:

```text
Output/{channel}/FitQr/FitQr_{channel}.rds
```

It reshapes the fitted VaR values into a wide date-by-stock matrix, removes
stocks with too many missing values, calculates rolling 63-day standard
deviations, and then averages across stocks.

### 5.4 HC/LC ratio

After calculating HC and LC volatility, the script merges both by date and
calculates:

```r
Vol_HL_Ratio = Vol_HC / Vol_LC
```

If VaR-volatility exists for both pools, it also calculates:

```r
VolVaR_HL_Ratio = VolVaR_HC / VolVaR_LC
```

### 5.5 Monthly aggregation

The script creates a `YearMonth` variable:

```r
YearMonth = substr(Date, 1, 7)
```

Then it averages each numeric column within each month.

## 6. Script 02: `02_DY_Index.R`

### 6.1 Objective

This script calculates the Diebold-Yilmaz Total Connectedness Index for the
high-carbon and low-carbon pools.

The conceptual workflow is:

1. Select top `J` stocks by average market capitalization.
2. Convert prices to log returns.
3. Convert log returns into annualized volatility-return series.
4. Run rolling VAR models.
5. Compute generalized forecast-error variance decompositions.
6. Extract the total connectedness index.
7. Save HC, LC, HC/LC ratio, and monthly averages.

### 6.2 Key parameters

The script supports environment-variable controls:

```r
DY_J
DY_NLAG
DY_NFORE
DY_WINDOW
DY_ENGINE
DY_OUTPUT_SUFFIX
```

Defaults:

```r
DY_J = 50
DY_NLAG = 1
DY_NFORE = 10
DY_WINDOW = 200
DY_ENGINE = "robust"
DY_OUTPUT_SUFFIX = ""
```

`DY_OUTPUT_SUFFIX` is useful for smoke tests. For example:

```bash
DY_J=5 DY_WINDOW=60 DY_OUTPUT_SUFFIX=_smoke \
Rscript Code/NewIndicator/02_DY_Index.R
```

This writes files such as:

```text
DY_HC_smoke.csv
DY_LC_smoke.csv
DY_All_smoke.csv
DY_Monthly_smoke.csv
```

so the normal outputs are not overwritten.

### 6.3 Stock selection

For each carbon pool, the script reads the price and market-cap panels. It
keeps only stocks that appear in both files.

It calculates average market capitalization:

```r
avg_mktcap <- colMeans(mktcap_mat, na.rm = TRUE)
```

It keeps the top `J` stocks by average market cap:

```r
top_j <- names(sort(avg_mktcap, decreasing = TRUE))[seq_len(min(J, length(avg_mktcap)))]
```

This reduces the VAR dimension and keeps the computation tractable.

### 6.4 Volatility-return transformation

After selecting stocks, the script calculates log returns:

```r
log_ret <- diff(log(price_mat))
```

Then it creates annualized volatility-return values:

```r
vol_ret <- (log_ret^2 * 0.361 * 365)^0.5 * 100
```

This follows the volatility-return transformation used in the reference code.

### 6.5 Filtering unstable series

Chinese stocks can have trading halts, stale prices, and zero returns. These
can make rolling VAR models rank-deficient.

The script removes stocks where more than one-third of the volatility-return
series is zero:

```r
zero_ratio <- colMeans(vol_ret == 0, na.rm = TRUE)
vol_ret <- vol_ret[, zero_ratio <= 1/3, drop = FALSE]
```

This reduces the chance that the rolling VAR has constant or near-constant
series.

### 6.6 Why the original package call can fail

The package `ConnectednessApproach` estimates rolling VAR models internally.
The original error was:

```text
Error in B_t[, , i] <- fit$B :
  number of items to replace is not a multiple of replacement length
```

This happens because `ConnectednessApproach:::VAR()` calls `summary(lm())`.
When a rolling window is rank-deficient, `summary(lm())` drops aliased
coefficients. Then `fit$B` becomes smaller than the fixed coefficient array
expected by `ConnectednessApproach()`.

In short:

```text
rank-deficient rolling VAR window
→ dropped coefficients
→ smaller B matrix
→ assignment into fixed B_t array fails
```

### 6.7 Robust VAR fallback

The script now uses a robust fixed-dimension VAR loop by default:

```r
DY_ENGINE = "robust"
```

The robust loop:

1. Builds the lagged VAR design matrix.
2. Uses QR regression for all equations.
3. Keeps the coefficient matrix dimensions fixed.
4. Sets aliased or non-finite coefficients to zero.
5. Constructs the residual covariance matrix.
6. Adds a small diagonal ridge term to stabilize the covariance matrix.

The key design choice is:

```r
coef_mat[!is.finite(coef_mat)] <- 0
```

This prevents rank-deficient windows from shrinking the coefficient matrix.

### 6.8 Connectedness calculation

After constructing rolling coefficient arrays `B_t` and residual covariance
arrays `Q_t`, the script calls:

```r
ConnectednessApproach:::TimeConnectedness(
  Phi = B_t,
  Sigma = Q_t,
  nfore = nfore,
  generalized = TRUE,
  corrected = FALSE
)
```

This preserves the Diebold-Yilmaz generalized connectedness calculation while
avoiding the rolling-VAR dimension error.

### 6.9 Package engine option

For comparison, the package engine can still be requested:

```bash
DY_ENGINE=package Rscript Code/NewIndicator/02_DY_Index.R
```

If the package engine fails, the script falls back to the robust engine.

### 6.10 Output

The script saves:

```text
Output/NewIndicators/Daily/DY_HC.csv
Output/NewIndicators/Daily/DY_LC.csv
Output/NewIndicators/Daily/DY_All.csv
Output/NewIndicators/Monthly/DY_Monthly.csv
```

The daily merged file contains:

```text
Date
DY_TCI_HC
DY_TCI_LC
DY_HL_Ratio
```

where:

```r
DY_HL_Ratio = DY_TCI_HC / DY_TCI_LC
```

Non-finite ratios are set to `NA`.

## 7. Script 03: `03_EDC_CVaR_Index.R`

### 7.1 Objective

This script calculates two downside-risk indicators:

1. Extreme Downside Co-movement.
2. Conditional Value-at-Risk.

Both are calculated separately for the high-carbon and low-carbon pools.

### 7.2 Log returns

The script reads stock prices and calculates log returns:

```r
log_ret <- diff(log(price_mat))
```

Missing and infinite values are replaced with zero.

### 7.3 CVaR index

The function:

```r
fun_cvar()
```

uses:

```r
cvar::ES(x, tau, dist.type = "cdf")
```

with:

```r
tau = 0.05
```

For each rolling 63-day window, the script calculates expected shortfall for
each stock and then averages across stocks:

```r
CVaR_pool_t = mean_i(ES_i,t)
```

The output is interpreted as the average tail-loss magnitude of stocks in the
pool.

### 7.4 EDC index

For each rolling 63-day window, the script:

1. Demeans each stock's return series.
2. Keeps only downside deviations by clipping positive values to zero.
3. Removes stocks with no downside movement in the window.
4. Normalizes downside vectors.
5. Computes the cosine similarity matrix.
6. Averages the upper-triangle pairwise similarities.

Mathematically, for two stocks `k` and `j`:

```text
EDC_kj = (d_k' d_j) / (||d_k|| ||d_j||)
```

where:

```text
d_k = min(r_k - mean(r_k), 0)
```

The pool EDC is the average across all stock pairs.

### 7.5 Output

The script saves:

```text
Output/NewIndicators/Daily/EDC_All.csv
Output/NewIndicators/Daily/CVaR_All.csv
Output/NewIndicators/Monthly/EDC_Monthly.csv
Output/NewIndicators/Monthly/CVaR_Monthly.csv
```

The daily files include HC, LC, and HC/LC ratio variables.

## 8. Script 04: `04_Centrality_Index.R`

### 8.1 Objective

This script calculates network centrality metrics from precomputed FRM
adjacency matrices.

It does not estimate FRM itself. It assumes the adjacency matrices already
exist.

### 8.2 Input adjacency matrices

The script reads:

```text
Output/HighCarbonIntens/Adj_Matrices/adj_matix_YYYYMMDD.csv
Output/LowCarbonIntens/Adj_Matrices/adj_matix_YYYYMMDD.csv
```

The folder also contains a `Fixed` subfolder, which the script excludes:

```r
file_list <- file_list[file_list != "Fixed"]
```

### 8.3 Removing macro variables

The adjacency matrices include stock nodes and macro-variable nodes. The code
uses:

```r
M_macro <- 9
```

to remove the final macro-variable rows and columns:

```r
M_stock <- ncol(data) - M_macro
adj_mat <- data.matrix(data[1:M_stock, 1:M_stock])
```

This keeps only the stock-to-stock network.

### 8.4 qgraph centrality

Each adjacency matrix is converted into a `qgraph` object:

```r
qgraph(adj_mat, layout = "circle", details = TRUE, vsize = c(5, 15),
       DoNotPlot = TRUE)
```

Then the script calculates:

- OutDegree;
- InDegree;
- Closeness;
- Betweenness.

For each day, the pool-level index is the average centrality across all stock
nodes.

### 8.5 Eigenvector centrality

The script converts each `qgraph` object to an `igraph` object and calculates:

```r
eigen_centrality(g, weights = E(g)$weight)$vector
```

The pool-level eigenvector index is the average across stock nodes.

### 8.6 HC/LC ratios

The script calculates:

```r
InDegree_HL_Ratio = InDegree_avg_HC / InDegree_avg_LC
```

For eigenvector centrality, the denominator can be near zero. To avoid
explosive ratios, the script sets the ratio to `NA` when:

```r
abs(Eigenvector_avg_LC) < 0.01
```

### 8.7 Output

The script saves:

```text
Output/NewIndicators/Daily/Centrality_HC.csv
Output/NewIndicators/Daily/Centrality_LC.csv
Output/NewIndicators/Daily/Centrality_All.csv
Output/NewIndicators/Monthly/Centrality_Monthly.csv
```

Monthly values are averages of daily values within each month.

## 9. Script 05: `05_Combine_All_Indicators.R`

### 9.1 Objective

This script merges all individual indicator outputs into one combined dataset.

It should be run after scripts 01 to 04.

### 9.2 Input files

The script attempts to read:

```text
Output/NewIndicators/Daily/Volatility_All.csv
Output/NewIndicators/Daily/DY_All.csv
Output/NewIndicators/Daily/EDC_All.csv
Output/NewIndicators/Daily/CVaR_All.csv
Output/NewIndicators/Daily/Centrality_All.csv
```

If a file is missing, the script warns and skips it:

```r
warning(paste("File not found, skipping:", path))
```

### 9.3 Merge logic

The loaded files are merged by `Date`:

```r
All_Daily <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE),
                    combine_list)
```

The merged dataset is sorted by date.

### 9.4 Monthly aggregation

The script creates:

```r
YearMonth = substr(Date, 1, 7)
```

Then it averages all numeric columns within each month:

```r
summarise(across(all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)))
```

### 9.5 Output

The script saves:

```text
Output/NewIndicators/Daily/All_Indicators_Daily.csv
Output/NewIndicators/Monthly/All_Indicators_Monthly.csv
```

It also prints a summary of available HC, LC, and ratio columns.

## 10. Recommended Run Commands

Run from the project root:

```bash
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/NewIndicator/01_Volatility_Index.R
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/NewIndicator/02_DY_Index.R
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/NewIndicator/03_EDC_CVaR_Index.R
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/NewIndicator/04_Centrality_Index.R
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/NewIndicator/05_Combine_All_Indicators.R
```

For a quick DY smoke test:

```bash
DY_J=5 DY_WINDOW=60 DY_OUTPUT_SUFFIX=_smoke \
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/NewIndicator/02_DY_Index.R
```

For the normal DY run:

```bash
/Library/Frameworks/R.framework/Resources/bin/Rscript Code/NewIndicator/02_DY_Index.R
```

## 11. Important Debug Notes

### 11.1 DY package error

If the original package implementation is forced with:

```bash
DY_ENGINE=package Rscript Code/NewIndicator/02_DY_Index.R
```

the user may see:

```text
Error in B_t[, , i] <- fit$B :
  number of items to replace is not a multiple of replacement length
```

This indicates that at least one rolling VAR window is rank-deficient. The
robust default engine avoids this by preserving the full coefficient matrix.

### 11.2 Long runtime

`02_DY_Index.R` is the slowest script because it estimates rolling VAR models.
The full setting uses:

```text
J = 50
window_size = 200
```

For debugging, use smaller values and an output suffix.

### 11.3 Output overwrite behavior

Most scripts write fixed output filenames. Running them again overwrites the
corresponding indicator output. `02_DY_Index.R` supports `DY_OUTPUT_SUFFIX`,
which makes it safer for smoke tests.

### 11.4 Package installation

Some scripts still install missing packages automatically. The DY script has
been changed to stop with a clear error if packages are missing, instead of
installing packages during execution.

## 12. Indicator Interpretation

### Volatility

Higher values mean larger average stock-level return volatility in the pool.

### VolVaR

Higher values mean greater instability in fitted VaR values across stocks.

### DY TCI

Higher values mean stronger volatility connectedness among stocks in the pool.

### EDC

Higher values mean stronger co-movement during downside return periods.

### CVaR

Higher values mean larger expected shortfall in the lower tail.

### Centrality

Higher average centrality means stocks in that pool are more important in the
FRM network structure.

### HC/LC ratios

The ratio variables compare high-carbon systemic-risk intensity with
low-carbon systemic-risk intensity:

```text
HL_Ratio > 1  means HC indicator is larger than LC indicator.
HL_Ratio < 1  means LC indicator is larger than HC indicator.
```

## 13. Suggested Validation Checks

After running the scripts, check:

1. Output files exist in `Output/NewIndicators/Daily` and
   `Output/NewIndicators/Monthly`.
2. Date ranges are consistent across indicators.
3. Ratio variables do not contain excessive infinite values.
4. Monthly outputs have one row per `YearMonth`.
5. `All_Indicators_Daily.csv` contains all expected indicator groups.
6. `All_Indicators_Monthly.csv` contains the same indicator families after
   monthly aggregation.

Example R checks:

```r
daily <- read.csv("Output/NewIndicators/Daily/All_Indicators_Daily.csv")
monthly <- read.csv("Output/NewIndicators/Monthly/All_Indicators_Monthly.csv")

range(daily$Date)
sum(duplicated(daily$Date))
sum(!is.finite(as.matrix(daily[sapply(daily, is.numeric)])), na.rm = TRUE)

range(monthly$YearMonth)
sum(duplicated(monthly$YearMonth))
```

