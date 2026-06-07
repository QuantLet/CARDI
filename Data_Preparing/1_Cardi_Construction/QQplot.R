rm(list = ls())

wdir = "CARDI/Data_Preparing/1_Cardi_Construction"
setwd(wdir)

#Data source
date_end_source = 20250127
date_start_source = 20140704

# channel = c("HighCarbon_CAPCO")
# channel = c("HighCarbonIntens")
channel = c("LowCarbonIntens")

J = 50
input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
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

ticker_str = all_prices$ticker[-1]
if (channel == "SP500") ticker_str = ticker_str %>% 
  as.Date(format = "%d.%m.%Y") %>% sort()
ticker = as.numeric(gsub("-", "", ticker_str))

N = length(ticker_str)

all_prices[, -1] = sapply(all_prices[, -1], as.numeric)

all_return = diff(log(as.matrix(all_prices[, c(2:(M_stock+1))])))
all_return = cbind(all_return, as.matrix(all_prices[-1, c((M_stock+2):ncol(all_prices))]))
all_return[is.na(all_return)] = 0
all_return[is.infinite(all_return)] = 0
stock_return = all_return[, 1:M_stock]
macro_return = all_return[, (M_stock+1):M]

N0_fixed = 2

#Make companies constant, select the biggest companies 
#Sorting the market capitalization data

FRM_sort = function(data) {sort(as.numeric(data), decreasing = TRUE, index.return = TRUE)}
#Determining the index number of each company
#according to decreasing market capitalization
mktcap_index = matrix(0, N, M_stock)
mktcap_sort = apply(mktcap[-1, -1], 1, FRM_sort)
for (t in 1:N) mktcap_index[t,] = mktcap_sort[[t]]$ix
mktcap_index = cbind(ticker, mktcap_index)

biggest_index_fixed = as.matrix(mktcap_index[N0_fixed, 2:(J+1)])

data = stock_return[, biggest_index_fixed]
data = as.vector(data)

# data = data[data>-0.1 & data<0.1 ]
data = data.frame(y = (data - mean(data)) /sd(data))

# png(paste0("Output/QQ_HighCarbon.png"), width = 900, height = 900, bg = "transparent")

png(paste0("Output/QQ_",channel,".png"), width = 900, height = 900, bg = "transparent")

# 绘制 QQ Plot
qq_plot <- ggplot(data,aes(sample=y)) +
  stat_qq() + 
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1.2) +  
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
  ) +
  scale_x_continuous(limits = c(-4, 4))+
                     scale_y_continuous(limits = c(-4, 4)) + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),  # 透明面板背景
    plot.background = element_rect(fill = "transparent", colour = NA),  # 透明绘图区域背景
    legend.box.background = element_rect(fill = "transparent", colour = NA),  # 透明图例框背景
    legend.background = element_rect(fill = "transparent", colour = NA),  # 透明图例背景
    legend.key = element_rect(fill = "transparent"),  # 透明图例键背景
    axis.line = element_line(colour = "black"),  # 坐标轴线颜色
    axis.title.x = element_text(size = 26),  # X 轴标题字体大小
    axis.title.y = element_text(size = 26),  # Y 轴标题字体大小
    axis.text.x = element_text(size = 26),  # X 轴刻度字体大小
    axis.text.y = element_text(size = 26),  # Y 轴刻度字体大小
    panel.border = element_blank(),  # 移除面板边框
    panel.grid = element_blank()  # 移除网格线
  )
qq_plot
dev.off()
