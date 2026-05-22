

rm(list = ls(all = TRUE))

libraries = c("patchwork","readxl","SHAPforxgboost","shapr","xgboost","readxl","iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo","xts", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","Hmisc","vars","aTSA","quantreg","rapport","sjmisc","haven","foreign","e1071")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

SCRIPT_DIR <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI"
setwd(SCRIPT_DIR)

# Relative path - looks inside SCRIPT_DIR
CARDI <- read_excel("Data/Processed/CARDI/Month_CARDI.xlsx")
LCPremium = readRDS("Code/CARDI_Test/output/monthly/portfolio_premiums_monthly.rds")
colnames(CARDI)[2] = "Period"
CARDI$Date = NULL

Premia_df <- CARDI %>% 
  left_join(LCPremium[,c("Date","Period","LC_HC_Premium")], by = "Period") 
Premia_df$LC_HC_Premium = Premia_df$LC_HC_Premium*100


png(paste0("Output/Figure/Intro_CarbonPremia_Month.png"), width = 900, height = 600, bg = "transparent")


plot(Premia_df$Date, Premia_df$LC_HC_Premium,
     type = "l",
     col = "black",
     lwd = 2,
     xlab = "Date",
     ylab = "Low-Carbon Premium (%)",
     xaxt = "n",
     cex.lab = 1.5,
     cex.axis = 1.3,
     cex.main = 1.8
)

# # === 阴影部分 ===
# 正区间（Return > 0）
polygon(c(Premia_df$Date, rev(Premia_df$Date)),
        c(pmax(Premia_df$LC_HC_Premium, 0), rep(0, length(Premia_df$LC_HC_Premium))),
        col = rgb(0.8, 0.95, 0.8, 0.6), border = NA)

# 负区间（Return < 0）
polygon(c(Premia_df$Date, rev(Premia_df$Date)),
        c(pmin(Premia_df$LC_HC_Premium, 0), rep(0, length(Premia_df$LC_HC_Premium))),
        col = rgb(1, 0.8, 0.8, 0.6), border = NA)

# 重新绘制线条（确保在线上方）
lines(Premia_df$Date, Premia_df$LC_HC_Premium, col = "black", lwd = 2)
# 添加红色虚线
abline(h = 0, col = "red", lwd = 2, lty = 2)
# 手动绘制x轴：显示每一年
axis.Date(side = 1,
          at = seq(min(Premia_df$Date), max(Premia_df$Date), by = "year"),
          format = "%Y",
          cex.axis = 1.3)

dev.off()

# Colorful Plot 


# 使用 patchwork 包在同一文件中但作为两个独立子图
# 确保所有 theme 设置都包括透明背景
p1 <- ggplot(Premia_df, aes(x = Date)) +
  geom_line(aes(y = CARDI_1P_M, color = "CarbonRisk_tau_1"), linewidth = 1.2) +
  geom_line(aes(y = CARDI_5P_M, color = "CarbonRisk_tau_5"), linewidth = 1.2) +
  geom_line(aes(y = CARDI_10P_M, color = "CarbonRisk_tau_10"), linewidth = 1.2)  +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    name = "Legend",
    values = c(
      "CarbonRisk_tau_1" = "#1F77B4",  # 深蓝
      "CarbonRisk_tau_5" = "#FF7F0E",  # 橙
      "CarbonRisk_tau_10" = "#2CA02C"  # 绿
    )
  )+
  labs(y = "CARDI", x = NULL) +
  theme(
    # 设置所有可能的背景为透明
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    # 其他设置
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

p2 <- ggplot(Premia_df, aes(x = Date)) +
  geom_line(aes(y = LC_HC_Premium, color = "Carbon Premia"), linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Low-Carbon Premium (%)", x = "Time") +
  theme(
    # 设置所有可能的背景为透明
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    # 其他设置
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

# 组合图形时也设置透明背景
combined_plot <- p1 / p2 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "cm"),
        # 组合图形的背景透明
        plot.background = element_rect(fill = "transparent", color = NA))

# 使用 ggsave 保存
ggsave("Output/Figure/CARDI_and_Premia.png",
       plot = combined_plot,
       width = 9, height = 10,
       bg = "transparent",
       dpi = 300)
