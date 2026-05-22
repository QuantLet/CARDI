
rm(list = ls())

wdir = - "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
setwd(wdir)

# root = "/Users/rutingwang/Library/Mobile Documents/com~apple~CloudDocs/Documents/macbook/PcBack/18.Bond_Project/"

# libraries = c("purrr","stringr","rfPermute","haven","sandwich","lmtest","nnet","rms","grf","varImp", "sandwich", "lmtest", "Hmisc", "ggplot2", "readxl", "plm", "psych", "sjmisc","xlsx","RODBC","stringr")
libraries = c("quantreg","purrr","stringr","haven","sandwich","lmtest","nnet","rms","grf","varImp", "sandwich", "lmtest", "Hmisc", "ggplot2", "readxl", "plm", "psych", "sjmisc","xlsx","RODBC","stringr")

lapply(libraries, function(x) if((!x %in% installed.packages())){
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

date = 20260519

# Summary for CARDI construction
# stock returns macro 
Chanlist = c("HighCarbonIntens","LowCarbonIntens")
date_start_source = 20140704
date_end_source = 20250127

# Statistics
for (channel in Chanlist){
  input_path = paste0("Data/Processed/Input/", channel, "/", date_start_source, "-", date_end_source)
  
  mktcap = read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                  date_end_source, ".csv"), header = TRUE) 
  mktcap[ , -1] <- lapply(mktcap[ , -1], as.numeric)
  
  mktcap = as.matrix(mktcap[ -1, -1])
  mktcap[is.na(mktcap)] = 0
  stock_prices = read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                                        date_end_source, ".csv"), header = TRUE)
  macro = read.csv(file = paste0(input_path, "/", channel, "_Macro_", 
                                 date_end_source, ".csv"), header = TRUE)
  
  
  M_stock = ncol(mktcap)
  M_macro = ncol(macro)-1
  M = M_stock+M_macro
  
  colnames(stock_prices)[1] = "ticker"
  colnames(macro)[1] = "ticker"
  
  #Load the stock prices and macro-prudential data matrix
  #Macros on days when stock is not traded are excluded
  all_prices = merge(stock_prices, macro, by = "ticker", all.x = TRUE)
  #Fill up macros on the missing days
  all_prices[, (M_stock+2):(M+1)] = all_prices[, (M_stock+2):(M+1)] %>% na.locf()
  
  ticker_str = all_prices$ticker[-1]
  ticker = as.numeric(gsub("-", "", ticker_str))
  N = length(ticker_str)
  
  all_prices[, -1] = sapply(all_prices[, -1], as.numeric)
  
  all_return = diff(log(as.matrix(all_prices[, c(2:(M_stock+1))])))
  all_return = cbind(all_return, as.matrix(all_prices[-1, c((M_stock+2):ncol(all_prices))]))
  all_return[is.na(all_return)] = 0
  all_return[is.infinite(all_return)] = 0
  stock_return = all_return[, 1:M_stock]
  macro_return = all_return[, (M_stock+1):M]
  
  Temp_out = data.frame(
    Var = c("return", "MarketValue"),
    Obs = c(nrow(stock_return)*ncol(stock_return), nrow(stock_return)*ncol(stock_return)),
    Mean = c(mean(as.numeric(all_return), na.rm = TRUE), mean(mktcap, na.rm = TRUE)),
    Std = c(sd(as.numeric(all_return), na.rm = TRUE), sd(mktcap, na.rm = TRUE)),
    Min = c(min(as.numeric(all_return), na.rm = TRUE), min(mktcap, na.rm = TRUE)),
    Max = c(max(as.numeric(all_return), na.rm = TRUE), max(mktcap, na.rm = TRUE)),
    Median = c(median(as.numeric(all_return), na.rm = TRUE), median(mktcap, na.rm = TRUE)),
    P25 = c(quantile(as.numeric(all_return), 0.25, na.rm = TRUE), quantile(mktcap, 0.25, na.rm = TRUE)),
    P75 = c(quantile(as.numeric(all_return), 0.75, na.rm = TRUE), quantile(mktcap, 0.75, na.rm = TRUE))
  )
  
  macro_stats_list <- list()
  
  # 遍历 macro_return 的每一列
  for (i in 1:ncol(macro_return)) {
    x <- macro_return[, i]
    var_name <- colnames(macro_return)[i]
    
    temp <- data.frame(
      Var = var_name,
      Obs = sum(!is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      Std = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      P25 = quantile(x, 0.25, na.rm = TRUE),
      P75 = quantile(x, 0.75, na.rm = TRUE)
    )
    
    macro_stats_list[[i]] <- temp
  }
  
  # 合并为一个数据框
  macro_stats_df <- do.call(rbind, macro_stats_list)
  sum_out = rbind(Temp_out,macro_stats_df)
  write.csv(sum_out, file = paste0('Output/Statistic_Summary/Summary_',channel,'.csv'), 
            row.names = FALSE,
            fileEncoding = "GB18030")
  
}

# Monthly LC Premium PureRC Shock RC, average CARDI_1P, 5P, 10P, Other Indicators, factors
file = "new_indicator_analysis_dataset_monthly.csv"

# Firm volatility and financial data 
file ="/Output/FirmReturns/HC_Firm_Monthly.rds"
HC_Month = readRDS(file)

# CARDI Original
# daily CARDI, SCARDI
SCARDI = "/Output/Event_test/FRM_Event.csv"


