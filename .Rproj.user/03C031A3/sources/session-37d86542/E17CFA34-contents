rm(list = ls())


# 安裝和載入所需的套件
# install.packages("micEconAids")
install.packages("urca")
library(urca)
library(micEconAids)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)


# 設定工作目錄
user <- Sys.info()["user"]
if (user == "brianhjli"){
  setwd("/Users/brianhjli/Dropbox/113-1/Research Methodology/DSE-5")
} else if (user == "QQ"){
  setwd("C:/Users/QQ/Dropbox/DSE-5")
} else if (user == "hayashijikyou"){
  setwd("/Users/hayashijikyou/Library/CloudStorage/Dropbox/DSE-5")
} else {
  warning("使用者名稱為空，請檢查檔案路徑！")
}


####################
# 檢視是否有時間序列特性

#單根檢驗 (ADF 檢驗)對價格變數和消費份額進行單根檢驗，以確定它們是否為平穩時間序列

# 假設 priceCols 是價格變數的列名
for (price in priceCols) {
  adf_test <- ur.df(fullData[[price]], type="drift", selectlags = "AIC")
  print(summary(adf_test))
}

# 假設 quantityCols 是消費份額變數的列名
for (share in quantityCols) {
  adf_test <- ur.df(fullData[[share]], type="drift", selectlags = "AIC")
  print(summary(adf_test))
}



# 創建一個列表來存儲檢驗結果
test_results <- list()

# 對多個變數執行 ADF 檢驗並存儲結果
for (i in 1:length(priceCols)) {
  # 執行 ADF 檢驗
  adf_test <- ur.df(fullData[[priceCols[i]]], type="drift", selectlags = "AIC")
  
  # 檢查對象結構（僅檢查用）
  # str(adf_test@testreg)
  # print(summary(adf_test)) # 可選擇性檢查 ADF 檢驗的結果
  
  # 提取必要的統計量和臨界值
  test_results[[i]] <- data.frame(
    Variable = priceCols[i],
    Test_Statistic = adf_test@teststat[1],  # 檢驗統計量
    Critical_Value_1pct = adf_test@cval[1, "1pct"],  # 1% 水準的臨界值
    Critical_Value_5pct = adf_test@cval[1, "5pct"],  # 5% 水準的臨界值
    Critical_Value_10pct = adf_test@cval[1, "10pct"] # 10% 水準的臨界值
  )
}

# 將所有結果合併到一個數據框中
test_results_df <- do.call(rbind, test_results)

# 顯示結果表格
print(test_results_df)

# 可選擇將結果保存為 CSV 文件
write.csv(test_results_df, "ADF_test_results.csv")



data <- read.csv("path/to/fullData.csv")
