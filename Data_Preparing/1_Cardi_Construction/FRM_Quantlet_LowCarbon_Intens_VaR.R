# update quantile fitted value 

## 0. Preparation

rm(list = ls(all = TRUE))

#----------------------------------------START UPDATE----------------------------------------

wdir = "/32_CARDI/00_Submit/Data_Preparing/1_Cardi_Construction"

setwd(wdir)


#TODO choose between quantile and expectile in the header
source("FRM_Statistics_Algorithm.R")

library(ggplot2)
library(data.table)
library(igraph)
require(timeDate)
library(stringr)
library(graphics)
library(magick)
library(scales)
library(tidyr)
library(dplyr)
library(zoo)
library(readxl)

options(digits=6)

#Choose between "Americas", "Europe", "Crypto", "SP500", "ER", "Asia", "EM"
channel = "LowCarbonIntens"

#Data source
date_end_source = 20250127
#Index output, varying companies
date_start = 20141015
date_end = date_end_source
#Network output, fixed companies
date_start_fixed = 20241108
date_end_fixed = date_end_source
#Note: fixed companies are needed to produce network gif and for analysis
#Note: allow min of s days in between date_start_source 
#and date_start, date_start_fixed

quantiles = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55, 0.50, 0.25)

#Estimation window size (63)
s = 63 
#Tail risk level (0.05)
tau = 0.05
if (channel == "ER") tau = 1 - tau
#Number of iterations (25)
I = 25   
#CoStress top and bottom L (5)
L = 5

stock_main = "X601857.SH"
date_start_source = 20140704
# J = 15
J = 50

#-----------------------------------------END UPDATE-----------------------------------------

# change data generate part
input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
dir.create(input_path)

dir.create(paste0("Output/", channel))
dir.create(paste0("Output/", channel, "/Adj_Matrices"))
dir.create(paste0("Output/", channel, "/residual"))
dir.create(paste0("Output/", channel, "/FitQr"))

dir.create(paste0("Output/", channel, "/Network"))
dir.create(paste0("Output/", channel, "/Boxplot"))
dir.create(paste0("Output/", channel, "/Lambda"))
dir.create(paste0("Output/", channel, "/Lambda/Quantiles"))
dir.create(paste0("Output/", channel, "/Top"))
dir.create(paste0("Output/", channel, "/Adj_Matrices/Fixed"))
dir.create(paste0("Output/", channel, "/Macro"))
if (tau == 0.05 & s == 63) output_path = paste0("Output/", channel) else 
  output_path = paste0("Output/", channel, "/Sensitivity/tau=", 100*tau, "/s=", s)

## 1. Data Preprocess

#Note: requires additional preprocessing for i.a. Crypto channel


mktcap = read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                  date_end_source, ".csv"), header = TRUE) %>% as.matrix()
stock_prices = read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                        date_end_source, ".csv"), header = TRUE)
macro = read.csv(file = paste0(input_path, "/", channel, "_Macro_", 
                 date_end_source, ".csv"), header = TRUE)

if (!all(sort(colnames(mktcap)) == sort(colnames(stock_prices)))) 
  stop("columns do not match")

M_stock = ncol(mktcap)-1
M_macro = ncol(macro)-1
M = M_stock+M_macro

colnames(mktcap)[1] = "ticker"
colnames(stock_prices)[1] = "ticker"
colnames(macro)[1] = "ticker"

#Can potentially cause LHS==0 in the regression
#but almost certainly it will be excluded wrt mktcap 
if (channel =="EM") stock_prices = na.locf(stock_prices, na.rm = FALSE)
#If missing market caps are kept NA, the column will be excluded 
#from top J  => do not interpolate in mktcap
mktcap[is.na(mktcap)] = 0

#Load the stock prices and macro-prudential data matrix
#Macros on days when stock is not traded are excluded
all_prices = merge(stock_prices, macro, by = "ticker", all.x = TRUE)
#Fill up macros on the missing days
all_prices[, (M_stock+2):(M+1)] = all_prices[, (M_stock+2):(M+1)] %>% na.locf()

#TODO: exceptions that break crypto algorithm and result in large lambda
#all_prices = all_prices[-c(377,603,716,895,896),]

ticker_str = all_prices$ticker[-1]
if (channel == "SP500") ticker_str = ticker_str %>% 
  as.Date(format = "%d.%m.%Y") %>% sort()
ticker = as.numeric(gsub("-", "", ticker_str))

N = length(ticker_str)

#Calculate the daily return and differences matrix of all selected financial 
#companies and macro-prudential variables; use exponential function for selected
#macro-prudential variables that are expressed in first order differences

all_prices[, -1] = sapply(all_prices[, -1], as.numeric)

all_return = diff(log(as.matrix(all_prices[, c(2:(M_stock+1))])))
all_return = cbind(all_return, as.matrix(all_prices[-1, c((M_stock+2):ncol(all_prices))]))
all_return[is.na(all_return)] = 0
all_return[is.infinite(all_return)] = 0
stock_return = all_return[, 1:M_stock]
macro_return = all_return[, (M_stock+1):M]

#Sorting the market capitalization data
FRM_sort = function(data) {sort(as.numeric(data), decreasing = TRUE, index.return = TRUE)}
#Determining the index number of each company
#according to decreasing market capitalization
mktcap_index = matrix(0, N, M_stock)
mktcap_sort = apply(mktcap[-1, -1], 1, FRM_sort)
for (t in 1:N) mktcap_index[t,] = mktcap_sort[[t]]$ix
mktcap_index = cbind(ticker, mktcap_index)


## 2. Estimation

#Row index corresponding to date_start and date_end
N0 = which(ticker == date_start)
N1 = which(ticker == date_end)

N0_fixed = which(ticker == date_start_fixed)
N1_fixed = which(ticker == date_end_fixed)

N_upd = N1-N0+1
N_fixed = N1_fixed-N0_fixed+1

## 2.1 Varying companies or coins

FRM_individ = vector(mode = "list")
FitQr_individ = vector(mode = "list")
residual_individ = vector(mode = "list")


J_dynamic = matrix(0, 1, N_upd)

J = min(J, ncol(mktcap_index)-1)
# for (t in N0:N1) { 
for (t in N0:100) {
  #Biggest companies at each time point
  biggest_index = as.matrix(mktcap_index[t, 2:(J+1)])
  data = cbind(stock_return[(t-s+1):t, biggest_index], 
               macro_return[(t-s):(t-1),])
  #J_dynamic needed for data available for less than J stocks:
  #relevant for crypto channel before 2014
  data = data[, colSums(data != 0) > 0]
  M_t = ncol(data)
  J_t = M_t - M_macro
  J_dynamic[t-N0+1] = J_t
  #Initialize adjacency matrix
  adj_matix = matrix(0, M_t, M_t) 
  est_lambda_t = vector()
  est_residual_t = vector()
  est_FitQr_t = vector()
  #FRM quantile regression
  for (k in 1:M_t) { 
    est = FRM_Quantile_Regression(as.matrix(data), k, tau, I)
    est_lambda = abs(data.matrix(est$lambda[which(est$Cgacv == min(est$Cgacv))]))
    
    loc_beta = which(est$Cgacv == min(est$Cgacv))
    est_beta = t(as.matrix(est$beta[loc_beta,]))
    adj_matix[k, -k] = est_beta
    est_lambda_t = c(est_lambda_t, est_lambda)
    
    # beta0 = 
    est_beta_0 = est$beta0[loc_beta]
    fitted <- est_beta_0 + as.numeric(est_beta %*% as.matrix(data[nrow(data), -k]))
    residual_final <- data[nrow(data), k] - fitted
    
    # keep residual and fitted value
    est_residual_t = c(est_residual_t, residual_final)
    est_FitQr_t = c(est_FitQr_t, fitted)
  }
  #List of vectors of different size with different column names

  est_residual_t = t(data.frame(est_residual_t[1:J_t]))
  colnames(est_residual_t) = colnames(data)[1:J_t]
  residual_individ[[t-N0+1]] = est_residual_t
  
  est_FitQr_t = t(data.frame(est_FitQr_t[1:J_t]))
  colnames(est_FitQr_t) = colnames(data)[1:J_t]
  FitQr_individ[[t-N0+1]] = est_FitQr_t
}

names(FitQr_individ) = ticker_str[N0:N1]
names(residual_individ) = ticker_str[N0:N1]
saveRDS(FitQr_individ, paste0(output_path, "/FitQr/FitQr_", channel, ".rds"))
saveRDS(residual_individ, paste0(output_path, "/residual/residual_", channel, ".rds"))


