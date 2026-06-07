

rm(list = ls(all = TRUE))

project_root <- "/Users/ruting/Documents/macbook/PcBack/32_CARDI/00_Submit/Figure3"
setwd(project_root)

required_packages <- c("readxl", "dplyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing required package: ", pkg, call. = FALSE)
  }
}
library(dplyr)

input_dir <- file.path(project_root, "Input")
output_dir <- file.path(project_root, "Output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

stock_info_file <- file.path(input_dir, "StockInfor_2025.xlsx")
carbon_emission_file <- file.path(input_dir, "CNE_CemissDerive.xlsx")


Stockinfor <- readxl::read_excel(stock_info_file) %>% data.frame()
Stockinfor = Stockinfor[c(1:(nrow(Stockinfor)-2)),]
colnames(Stockinfor) = c('ID','Shortname_CN','ShortName_EN','ListDate','ListLocation','Name_EN',
                         'Ownership','Prefeture','Prefeture_Code','Province','City','NEIC_B','NEIC_M','NEIC_S',
                         'NEIC_B_Code','NEIC_M_Code','NEIC_S_Code')


# read emission 
CarbonEmission <- readxl::read_excel(carbon_emission_file) %>% data.frame()

CarbonEmission = CarbonEmission[-c(1,2),]
CarbonEmission_keep = CarbonEmission[,c('EndDate','Symbol','CEmission','CemissionIntensity','NEIndustryCode','NEIndustryName')]
CarbonEmission_keep[,c('CEmission','CemissionIntensity')] = lapply(CarbonEmission_keep[,c('CEmission','CemissionIntensity')], as.numeric)
CarbonEmission_keep$Year = as.numeric(substr(CarbonEmission_keep$EndDate,1,4))
Carbon_Rank = data.frame(Symbol = unique(CarbonEmission_keep$Symbol), ID = NA, CarbonIntensity_Mean = NA, 
                         CarbonEmi_Mean = NA, Start = NA, End = NA)

for (iR in c(1: length(Carbon_Rank$ID))){
  iCode = Carbon_Rank$Symbol[iR]
  Temp = CarbonEmission_keep[CarbonEmission_keep$Symbol == iCode, c('EndDate','Symbol','Year','CEmission','CemissionIntensity')]
  if (as.numeric(substr(iCode,1,1) == 6)){
    ID = paste0(iCode,'.SH')
  }else if (substr(iCode,1,1) == '0' | substr(iCode,1,1) == '3'){
    ID = paste0(iCode,'.SZ')
  }else if (substr(iCode,1,1) == '8'){
    ID = paste0(iCode,'.BJ')
  }
  Carbon_Rank$ID[iR] = ID
  Carbon_Rank[iR, c('CarbonIntensity_Mean','CarbonEmi_Mean')] = colMeans(Temp[,c('CemissionIntensity','CEmission')],na.rm = TRUE)
  Carbon_Rank[iR,c('Start','End')] = c(min(Temp$Year),max(Temp$Year))
}

Carbon_Rank = Carbon_Rank[Carbon_Rank$Start<= 2010 & Carbon_Rank$End >= 2021 & (!is.na(Carbon_Rank$CarbonIntensity_Mean)),]
Unique_Indu = unique(CarbonEmission_keep[,c('Symbol','NEIndustryCode','NEIndustryName')]) %>%data.frame()

duplicated_symbols <- duplicated(Unique_Indu$Symbol)

Unique_Indu<- Unique_Indu[!duplicated_symbols, ]

# Top_Carbon Low_Carbon
Carbon_Rank = merge(Carbon_Rank,Unique_Indu, all.x = TRUE)

saveRDS(Carbon_Rank, file = file.path(output_dir, "Carbon_Rank.rds")) 


quantiles <- quantile(Carbon_Rank$CarbonIntensity_Mean, probs = c(0.3, 0.7))

intensity_groups <- list(
  Low_Intensity = Carbon_Rank$ID[Carbon_Rank$CarbonIntensity_Mean < quantiles[1]],
  Medium_Intensity = Carbon_Rank$ID[Carbon_Rank$CarbonIntensity_Mean >= quantiles[1] 
                                    & Carbon_Rank$CarbonIntensity_Mean <= quantiles[2]],
  High_Intensity = Carbon_Rank$ID[Carbon_Rank$CarbonIntensity_Mean > quantiles[2]]
)

intensity_groups_Indus <- list(
  Low_Intensity = Carbon_Rank[Carbon_Rank$CarbonIntensity_Mean < quantiles[1], c('NEIndustryCode','NEIndustryName')],
  Medium_Intensity = Carbon_Rank[Carbon_Rank$CarbonIntensity_Mean >= quantiles[1] 
                                 & Carbon_Rank$CarbonIntensity_Mean <= quantiles[2],c('NEIndustryCode','NEIndustryName')],
  High_Intensity = Carbon_Rank[Carbon_Rank$CarbonIntensity_Mean > quantiles[2],c('NEIndustryCode','NEIndustryName')]
)


High_Intensity_Indus <- intensity_groups_Indus[[3]] %>%
  group_by(NEIndustryCode, NEIndustryName) %>%
  summarise(num = n(), .groups = 'drop') %>%
  arrange(desc(num))

Low_Intensity_Indus <- intensity_groups_Indus[[1]] %>%
  group_by(NEIndustryCode, NEIndustryName) %>%
  summarise(num = n(), .groups = 'drop') %>%
  arrange(desc(num))

Median_Intensity_Indus <- intensity_groups_Indus[[2]] %>%
  group_by(NEIndustryCode, NEIndustryName) %>%
  summarise(num = n(), .groups = 'drop') %>%
  arrange(desc(num))

write.csv(High_Intensity_Indus, 
          file = file.path(output_dir, "HighCarbonIndustry.csv"), 
          row.names = FALSE,
          fileEncoding = "GB18030")

write.csv(Low_Intensity_Indus, file = file.path(output_dir, "LowCarbonIndustry.csv"),
          row.names = FALSE,
          fileEncoding = "GB18030")

write.csv(Median_Intensity_Indus, file = file.path(output_dir, "MedianCarbonIndustry.csv"), 
          row.names = FALSE,
          fileEncoding = "GB18030")




Type = c("High_CarbonIntensity","Low_CarbonIntensity","Medium_Intensity")

for (iType in Type){
  if (iType == "High_CarbonIntensity"){
    Stocklist = intensity_groups$High_Intensity
    
  }else if(iType == "Low_CarbonIntensity"){
    Stocklist = intensity_groups$Low_Intensity
  }else{
    Stocklist = intensity_groups$Medium_Intensity
  }
  dir.create(file.path(input_dir, paste0("StockPrice_", iType)),
             recursive = TRUE, showWarnings = FALSE)
  write.csv(Stocklist,
            file = file.path(input_dir, paste0("Stocklist_", iType, ".csv")),
            row.names = FALSE)
 
}



