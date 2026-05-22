# New Risk Indicators — Code Overview

This folder contains R scripts to compute five systemic risk indicators for
the **High-Carbon (HC)** and **Low-Carbon (LC)** firm pools.
Each script is self-contained and saves daily + monthly outputs to
`Output/NewIndicators/`.

---

## Scripts

| # | Script | Indicator | Key Package | Runtime |
|---|--------|-----------|-------------|---------|
| 01 | `01_Volatility_Index.R` | Rolling-window cross-sectional return volatility | base R | Fast (< 1 min) |
| 02 | `02_DY_Index.R` | Diebold-Yilmaz (2012) Total Connectedness Index (TCI) | `ConnectednessApproach` | Slow (30–60 min) |
| 03 | `03_EDC_CVaR_Index.R` | Extreme Downside Co-movement (EDC) + CVaR | `cvar` | Moderate (10–30 min) |
| 04 | `04_Centrality_Index.R` | Network centrality from FRM adjacency matrices | `igraph`, `qgraph` | Moderate (5–15 min) |
| 05 | `05_Combine_All_Indicators.R` | Merge all, HC/LC ratios, monthly aggregation | `dplyr` | Fast (< 1 min) |

Run in order 01 → 02 → 03 → 04 → 05.

---

## Input Data

| Pool | Price CSV | Mktcap CSV |
|------|-----------|------------|
| HC | `Data/Processed/Input/HighCarbonIntens/20140704-20250127/HighCarbonIntens_Price_20250127.csv` | `...Mktcap...` |
| LC | `Data/Processed/Input/LowCarbonIntens/20140704-20250127/LowCarbonIntens_Price_20250127.csv` | `...Mktcap...` |

Script 04 additionally reads FRM adjacency matrices from:
- `Output/HighCarbonIntens/Adj_Matrices/`
- `Output/LowCarbonIntens/Adj_Matrices/`

---

## Output Structure

```
Output/NewIndicators/
├── Daily/
│   ├── Volatility_HC.csv / Volatility_LC.csv / Volatility_All.csv
│   ├── DY_HC.csv / DY_LC.csv / DY_All.csv
│   ├── EDC_All.csv
│   ├── CVaR_All.csv
│   ├── Centrality_HC.csv / Centrality_LC.csv / Centrality_All.csv
│   └── All_Indicators_Daily.csv    ← combined master file
└── Monthly/
    ├── Volatility_Monthly.csv
    ├── DY_Monthly.csv
    ├── EDC_Monthly.csv
    ├── CVaR_Monthly.csv
    ├── Centrality_Monthly.csv
    └── All_Indicators_Monthly.csv  ← combined master file
```

Column naming convention: `{indicator}_{HC|LC}` for pool series;
`{indicator}_HL_Ratio` = HC / LC.

---

## Methodology Summary

### 01 — Volatility Index
- Log returns: `r_t = log(P_t) - log(P_{t-1})`
- Per-stock rolling std: `σ_i = std(r[(t-62):t])`
- Pool index: `Vol_pool = mean_i(σ_i)` at each date t

### 02 — DY Index (Diebold-Yilmaz 2012)
- Annualised vol return: `v = (r² × 0.361 × 365)^0.5 × 100`
- Rolling VAR with generalised FEVD, window = 200 days
- Top 50 stocks by average market cap per pool (for computational tractability)
- Output: Total Connectedness Index (TCI)

### 03 — EDC Index
- Demean returns in window; clip positives to zero: `d_k = min(r_k - mean(r_k), 0)`
- Pairwise cosine similarity: `EDC_{kj} = (d_k · d_j) / (||d_k|| ||d_j||)`
- Pool EDC = mean over all k≠j pairs

### 03 — CVaR Index
- Rolling 63-day Expected Shortfall at τ=5% per stock (`cvar::ES`, empirical CDF)
- Pool CVaR = cross-sectional mean of individual ES values

### 04 — Centrality Index
- Reads pre-computed FRM adjacency matrices (requires FRM to have been run first)
- Metrics: OutDegree, InDegree, Closeness, Betweenness (qgraph `centrality()`),
  Eigenvector (igraph `eigen_centrality()`)
- Pool index = average across all stocks in the pool on that day
- Primary HC/LC ratios: InDegree_HL_Ratio, Eigenvector_HL_Ratio

---

## BGVAR Index — MATLAB Required

The Bayesian Graphical VAR (BGVAR) indicator from Ahelegbey, Billio & Casarin (2014)
is implemented in MATLAB only:

- Reference script: `Code/R/Ref_Bayesian_graphical_var.m`
- Required functions: `Code/R/functions/SAMPLE_BGMAR_DAG.m`, `SAMPLE_BGMIN_DAG.m`,
  `CONVERGENCE.m`, `LOG_SCORE.m`, etc.
- Metric: DAG density = `sum(DAG) / N² × 100`, rolling window = 200 days,
  nsimu = 40,000 MCMC draws, lag = 1

**To produce the BGVAR index:**
1. Open `Code/R/Ref_Bayesian_graphical_var.m` in MATLAB
2. Update `sRoot` to point to the HC or LC return file
3. Generate `Return_YYYYMMDD.csv` from price data (log returns, top-50 by mktcap)
4. Run the script; output is saved to `BGVAR_YYYYMMDD.xlsx`

No equivalent R implementation is provided because the `BGVAR` package on CRAN
implements a different model (Global VAR, not graphical SVAR).

---

## Key Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| Rolling window `s` | 63 days | ~3 months |
| DY window | 200 days | ~10 months |
| DY top-J stocks | 50 | By average market cap |
| CVaR / EDC τ | 0.05 | 5% tail |
| Macro variables | 9 | Excluded from centrality adjacency matrix |
| Data period | 2014-07-04 – 2025-01-27 | |
