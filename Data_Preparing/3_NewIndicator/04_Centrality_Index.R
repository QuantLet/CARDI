# =============================================================================
# 04_Centrality_Index.R
# Network Centrality Index for High-Carbon (HC) and Low-Carbon (LC) Pools
#
# Method: Reads pre-computed FRM adjacency matrices and computes per-day
#         average centrality metrics across all stocks in each pool.
#         Metrics: OutDegree, InDegree, Closeness, Betweenness (qgraph),
#                  Eigenvector (igraph).
#
# Prerequisites: FRM adjacency matrices must already be computed and stored in
#   Output/HighCarbonIntens/Adj_Matrices/
#   Output/LowCarbonIntens/Adj_Matrices/
#
# Inputs:
#   Output/HighCarbonIntens/Adj_Matrices/adj_matix_YYYYMMDD.csv
#   Output/LowCarbonIntens/Adj_Matrices/adj_matix_YYYYMMDD.csv
#
# Outputs:
#   Output/NewIndicators/Daily/Centrality_HC.csv
#   Output/NewIndicators/Daily/Centrality_LC.csv
#   Output/NewIndicators/Daily/Centrality_All.csv
#   Output/NewIndicators/Monthly/Centrality_Monthly.csv
# =============================================================================

rm(list = ls(all = TRUE))

libraries <- c("igraph", "qgraph", "strex", "dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) install.packages(x))
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# ---- Parameters ----
wdir    <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
M_macro <- 9    # macro variables appended to the adjacency matrix

setwd(wdir)

dir.create("Output/NewIndicators/Daily",   showWarnings = FALSE, recursive = TRUE)
dir.create("Output/NewIndicators/Monthly", showWarnings = FALSE, recursive = TRUE)

# ---- Helper: compute eigenvector centrality for a list of qgraph objects ----
compute_eigencentrality <- function(graph_list) {
  lapply(seq_along(graph_list), function(i) {
    g <- tryCatch(as.igraph(graph_list[[i]]), error = function(e) NULL)
    if (is.null(g)) return(NA_real_)
    ec <- tryCatch(
      suppressWarnings(eigen_centrality(g, weights = E(g)$weight)$vector),
      error = function(e) rep(NA_real_, vcount(g))
    )
    ec
  })
}

# ---- Helper: compute all centrality metrics for one pool ----
compute_centrality_index <- function(channel) {

  adj_dir   <- paste0("Output/", channel, "/Adj_Matrices")
  file_list <- list.files(adj_dir)
  file_list <- file_list[file_list != "Fixed"]

  if (length(file_list) == 0) {
    stop(paste("No adjacency matrix files found in", adj_dir))
  }

  # Extract dates from filenames (format: adj_matix_YYYYMMDD.csv)
  dates <- as.Date(as.character(str_first_number(file_list)), format = "%Y%m%d")
  N     <- length(file_list)

  cat("  Loading", N, "adjacency matrices for", channel, "...\n")

  # Build qgraph objects (stocks only, exclude macro columns/rows)
  allgraphs <- lapply(seq_len(N), function(i) {
    data     <- read.csv(paste0(adj_dir, "/", file_list[i]), row.names = 1,
                         check.names = FALSE)
    M_stock  <- ncol(data) - M_macro
    if (M_stock < 1) return(NULL)
    adj_mat  <- data.matrix(data[1:M_stock, 1:M_stock])
    tryCatch(
      qgraph(adj_mat, layout = "circle", details = TRUE, vsize = c(5, 15),
             DoNotPlot = TRUE),
      error = function(e) NULL
    )
  })

  valid <- !sapply(allgraphs, is.null)
  allgraphs_valid <- allgraphs[valid]
  dates_valid     <- dates[valid]
  N_valid         <- length(allgraphs_valid)

  cat("  Computing qgraph centralities...\n")
  allcentralities <- centrality(allgraphs_valid)

  cat("  Computing eigenvector centralities...\n")
  eigencentrality <- compute_eigencentrality(allgraphs_valid)

  outdegree_avg  <- sapply(seq_len(N_valid), function(i) mean(allcentralities[[i]]$OutDegree,   na.rm = TRUE))
  indegree_avg   <- sapply(seq_len(N_valid), function(i) mean(allcentralities[[i]]$InDegree,    na.rm = TRUE))
  closeness_avg  <- sapply(seq_len(N_valid), function(i) mean(allcentralities[[i]]$Closeness,   na.rm = TRUE))
  betweenness_avg <- sapply(seq_len(N_valid), function(i) mean(allcentralities[[i]]$Betweenness, na.rm = TRUE))
  eigenvector_avg <- sapply(seq_len(N_valid), function(i) mean(eigencentrality[[i]], na.rm = TRUE))

  data.frame(
    Date            = as.character(dates_valid),
    OutDegree_avg   = outdegree_avg,
    InDegree_avg    = indegree_avg,
    Closeness_avg   = closeness_avg,
    Betweenness_avg = betweenness_avg,
    Eigenvector_avg = eigenvector_avg,
    stringsAsFactors = FALSE
  )
}

# ---- Compute for HC and LC ----
cat("Computing HC Centrality Index...\n")
Cent_HC <- compute_centrality_index("HighCarbonIntens")

cat("Computing LC Centrality Index...\n")
Cent_LC <- compute_centrality_index("LowCarbonIntens")

# Rename columns with pool suffix
suffix_rename <- function(df, suffix) {
  non_date <- setdiff(colnames(df), "Date")
  colnames(df)[match(non_date, colnames(df))] <- paste0(non_date, "_", suffix)
  df
}
Cent_HC <- suffix_rename(Cent_HC, "HC")
Cent_LC <- suffix_rename(Cent_LC, "LC")

# ---- Merge and compute HC/LC ratios ----
Cent_All <- merge(Cent_HC, Cent_LC, by = "Date", all = TRUE)

# HC/LC ratios for InDegree and Eigenvector (main metrics per reference code)
Cent_All$InDegree_HL_Ratio   <- Cent_All$InDegree_avg_HC   / Cent_All$InDegree_avg_LC

# Eigenvector ratio: set to NA when denominator is near-zero (|LC| < 0.01)
# to avoid explosion from near-zero eigenvectors in directed signed graphs
eig_denom <- Cent_All$Eigenvector_avg_LC
Cent_All$Eigenvector_HL_Ratio <- ifelse(
  !is.na(eig_denom) & abs(eig_denom) >= 0.01,
  Cent_All$Eigenvector_avg_HC / eig_denom,
  NA_real_
)

# ---- Save daily outputs ----
write.csv(Cent_HC,  "Output/NewIndicators/Daily/Centrality_HC.csv",  row.names = FALSE, quote = FALSE)
write.csv(Cent_LC,  "Output/NewIndicators/Daily/Centrality_LC.csv",  row.names = FALSE, quote = FALSE)
write.csv(Cent_All, "Output/NewIndicators/Daily/Centrality_All.csv", row.names = FALSE, quote = FALSE)
cat("Saved daily centrality outputs.\n")

# ---- Monthly aggregation ----
ratio_cols <- c("InDegree_HL_Ratio", "Eigenvector_HL_Ratio")
all_cols   <- setdiff(colnames(Cent_All), c("Date", "YearMonth"))

Cent_All$YearMonth <- substr(Cent_All$Date, 1, 7)
Cent_Monthly <- Cent_All %>%
  group_by(YearMonth) %>%
  summarise(across(all_of(all_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

write.csv(Cent_Monthly, "Output/NewIndicators/Monthly/Centrality_Monthly.csv",
          row.names = FALSE, quote = FALSE)
cat("Saved monthly centrality outputs.\n")
cat("Centrality Index: DONE\n")
