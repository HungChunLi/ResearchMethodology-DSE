rm(list = ls())


# 安裝和載入所需的套件
# install.packages("micEconAids")
library(micEconAids)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(lmtest)


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


# 讀取資料
data <- fread("data/fullData.csv")
data <- as.data.table(lapply(data, function(col) as.numeric(as.character(col))))
data <- subset(data, year >= 1991)
data <- subset(data, year < 2024)
data <- data[!is.na(data$month), ]  # 刪除 month 為 NA 的行

# 將年和月合併成一個日期變數，將日期設置為該月份的第一天
data$date <- as.Date(paste(data$year, data$month, "01", sep = "-"), format = "%Y-%m-%d")

# 執行 Durbin-Watson 測試
# 使用 date 作為時間變數
dw_fvj <- dwtest(FruitVegetableJuice_price ~ date, data = data)
dw_cb <- dwtest(CarbonatedBeverage_price ~ date, data = data)
dw_sd <- dwtest(SportsDrink_price ~ date, data = data)
dw_cd <- dwtest(CoffeeDrink_price ~ date, data = data)
dw_td <- dwtest(TeaDrink_price ~ date, data = data)

# 顯示結果
dw_fvj
dw_cb
dw_sd
dw_cd
dw_td
