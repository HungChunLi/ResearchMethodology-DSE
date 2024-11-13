rm(list = ls())

# 安裝和載入所需的套件
# install.packages("micEconAids")
install.packages("tsibble")
library(micEconAids)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(readr)
library(tsibble)
library(lubridate)
library(forecast)

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


# 讀取 CSV 檔案
data <- read_csv("C:/Users/QQ/Dropbox/DSE-5/data/fullData.csv")


data$date <- ymd(paste(data$year, data$month, "01", sep = "-"))
problems(data)

# 確保 year 和 month 是數字
data$year <- as.integer(data$year)
data$month <- as.integer(data$month)

data$date <- ymd(paste(data$year, data$month, "01", sep = "-"))

data_with_issues <- data %>%
  filter(is.na(year) | is.na(month) | month > 12 | month < 1)

data$date <- ymd(paste(data$year, data$month, "01", sep = "-"))

# 篩選出不合理的年份或月份
invalid_data <- data %>%
  filter(is.na(year) | is.na(month) | month > 12 | month < 1)

# 查看不合理的數據
print(invalid_data)


# 移除不合理的行
data <- data %>%
  filter(!is.na(year) & !is.na(month) & month <= 12 & month >= 1)

# 創建日期欄位
data$date <- ymd(paste(data$year, data$month, "01", sep = "-"))

# 將資料轉換為時間序列
ts_data <- ts(data$FruitVegetableJuice_volumn, start = c(1982, 1), frequency = 12)

# 繪製飲料銷售量的時間序列圖
plot.ts(ts_data, main = "FruitVegetableJuice Volumn 時間序列", ylab = "銷售量", xlab = "時間")

# 其他變數可以用類似方式進行
# 如需要分析 CarbonatedBeverage_volumn，可以替換 ts_data 變數


ts_data <- ts(data$FruitVegetableJuice_volumn, start = c(1982, 1), frequency = 12)


# 分解時間序列
decomp <- decompose(ts_data)
plot(decomp)

# 檢查時間序列中是否有缺失值
sum(is.na(ts_data))

# 移除含有 NA 的行
ts_data_clean <- na.omit(ts_data)



# 重新進行 ADF 檢驗
adf.test(ts_data_clean)


# 建立 ARIMA 模型
fit <- auto.arima(ts_data)

# 預測未來 12 個月的數據
forecast_data <- forecast(fit, h = 12)
plot(forecast_data)
