rm(list = ls())

# 安裝和載入所需的套件
# install.packages("micEconAids")

library(micEconAids)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(xtable)

# 設定工作目錄
user <- Sys.info()["user"]

if (user == "brianhjli"){
  setwd("/Users/brianhjli/Dropbox/113-1/Research Methodology/DSE-5")
} else if (user == "QQ"){
  setwd("C:/Users/QQ/Dropbox/DSE-5")
} else if (user == "hayashijikyou"){
  setwd("/Users/hayashijikyou/Library/CloudStorage/Dropbox/DSE-5")
} else if (user == "11097"){
  setwd("C:/Users/11097/Dropbox/DSE-5")
} else {
  warning("使用者名稱為空，請檢查檔案路徑！")
}


# # 讀取資料
# # data <- fread("data/fullData.csv")
# data <- fread("data/DSE-5.csv")

# # 確認資料框中有年份欄位
# if (!"year" %in% names(data)) {
#   stop("資料中缺少年份 (year) 欄位")
# }


# # 設定數量欄位
# quantityCols <- c("FruitVegetableJuice_volumn", "CarbonatedBeverage_volumn",
#                   "SportsDrink_volumn", "CoffeeDrink_volumn", "TeaDrink_volumn")
# # 設定價格欄位
# priceCols <- c("FruitVegetableJuice_price", "CarbonatedBeverage_price", 
#                "SportsDrink_price", "CoffeeDrink_price", "TeaDrink_price")
# # 設定價值欄位
# valueCols <- c("FruitVegetableJuice_value", "CarbonatedBeverage_value",
#                "SportsDrink_value", "CoffeeDrink_value", "TeaDrink_value")
# # 設定需求數量（需求份額）欄位
# shareCols <- c("FruitVegetableJuice_share", "CarbonatedBeverage_share", 
#                "SportsDrink_share", "CoffeeDrink_share", "TeaDrink_share")
# # 設定總所得欄位
# incomeCols <- c("total_value", "AvgHouseholdDisposableIncome", "LowestQuintileIncome", "SecondLowestQuintileIncome",
#                 "MiddleQuintileIncome", "SecondHighestQuintileIncome", "HighestQuintileIncome")

# #計算total volue
# data$total_value <- rowSums(data[, ..valueCols])
# # 保留需要的欄位

# # # 確保只對數值型變數計算相關性
# # numeric_cols <- sapply(fullData, is.numeric)
# # # 計算數值型變數的相關性矩陣
# # correlation_matrix <- cor(fullData[, numeric_cols], use = "complete.obs")

# #計算份額
# fullData <- as.data.table(data)
# fullData[, FruitVegetableJuice_share := FruitVegetableJuice_value/get(incomeCols[1])]
# fullData[, CarbonatedBeverage_share := CarbonatedBeverage_value/get(incomeCols[1])]
# fullData[, SportsDrink_share := SportsDrink_value/get(incomeCols[1])]
# fullData[, CoffeeDrink_share := CoffeeDrink_value/get(incomeCols[1])]
# fullData[, TeaDrink_share := TeaDrink_value/get(incomeCols[1])]

# required_cols <- c(priceCols, shareCols, incomeCols[1])
# fullData <- subset(fullData, select = required_cols)

# # 檢查數據中是否有 NA 並處理
# if (sum(is.na(fullData)) > 0) {
#   fullData <- na.omit(fullData)
#   message("數據中含有 NA，已移除缺失值。")
# }

# # # 確認需要的欄位是否存在
# # missing_cols <- setdiff(required_cols, names(fullData))
# # if (length(missing_cols) > 0) {
# #   stop(paste("以下欄位缺失:", paste(missing_cols, collapse = ", ")))
# # }

# # 進行 AIDS 模型估計
# aids_model <- aidsEst(
#   priceNames = priceCols,
#   shareNames = shareCols, 
#   totExpName = incomeCols[1], 
#   data = fullData, priceIndex = "T", method = "IL")
# summary(aids_model)
# aids_elasticities <- aidsElas(
#   coef = aids_model$coef,  # Pass coefficients manually
#   shares = aids_model$wMeans,  # Average budget shares
#   prices = aids_model$pMeans   # Average prices
# )

# # write.csv(as.data.frame(aids_model$coef$stat), "outcome/output_aids_stat.csv", quote = FALSE)
# # write.csv(as.data.frame(aids_elasticities$exp), "outcome/output_aids_exp.csv", quote = FALSE)
# # write.csv(as.data.frame(aids_elasticities$marshall), "outcome/output_aids_marshall.csv", quote = FALSE)
# # write.csv(as.data.frame(aids_elasticities$hicks), "outcome/output_aids_hicks.csv", quote = FALSE)
# # saveRDS(aids_model, file = "outcome/aids_model.rds")
# # saveRDS(aids_elasticities, file = "outcome/aids_elasticities.rds")


# # 進行 LAAIDS 模型估計
# laaids_model <- aidsEst(
#   priceNames = priceCols,
#   shareNames = shareCols, 
#   totExpName = incomeCols[1], 
#   data = fullData, priceIndex = "S", method = "LA")
# summary(laaids_model)
# laaids_elasticities <- aidsElas(
#   coef = laaids_model$coef,  # Pass coefficients manually
#   shares = laaids_model$wMeans,  # Average budget shares
#   prices = laaids_model$pMeans   # Average prices
# )

# # write.csv(as.data.frame(laaids_model$coef$stat), "outcome/output_laaids.csv", quote = FALSE)
# # write.csv(as.data.frame(laaids_elasticities$exp), "outcome/output_laaids_exp.csv", quote = FALSE)
# # write.csv(as.data.frame(laaids_elasticities$marshall), "outcome/output_laaids_marshall.csv", quote = FALSE)
# # write.csv(as.data.frame(laaids_elasticities$hicks), "outcome/output_laaids_hicks.csv", quote = FALSE)
# # saveRDS(laaids_model, file = "outcome/laaids_model.rds")
# # saveRDS(laaids_elasticities, file = "outcome/laaids_elasticities.rds")

# 讀取 AIDS 和 LAAIDS 模型
aids_model <- readRDS("outcome/aids_model.rds")
laaids_model <- readRDS("outcome/laaids_model.rds")

# 計算對數似然值
logLik_AIDS <- logLik(aids_model)
logLik_LAAIDS <- logLik(laaids_model)

# 計算檢定統計量
LR_stat <- -2 * (logLik_AIDS - logLik_LAAIDS)

# 計算自由度差異並檢測顯著性
K_aids <- length(aids_model$coef)  # AIDS模型的参数个数
K_laaids <- length(laaids_model$coef)  # LAAIDS模型的参数个数
df_diff <- K_aids - K_laaids
p_value <- 1 - pchisq(LR_stat, df_diff)

# 輸出結果
cat("LR statistic:", LR_stat, "\nP-value:", p_value, "\n")
