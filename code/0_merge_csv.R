rm(list = ls())

library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
# library(psych)

user <- Sys.info()["user"]

if (user == "brianhjli"){
  setwd("/Users/brianhjli/Dropbox/113-1/Research Methodology/DSE-5")
} else if (user == "QQ"){
  setwd("C:/Users/QQ/Dropbox/DSE-5")
} else if (user == "hayashijikyou"){
  setwd("/Users/hayashijikyou/Library/CloudStorage/Dropbox/DSE-5")
} else if (user == "11097"){
  setwd("C:/Users/11097/Dropbox/DSE-5")
}

sale_data <- fread("data/銷售量及銷售值.csv") 
# income_data <- fread("data/國民所得統計.csv", fileEncoding = "Big5") 
# house_income_data <- fread("data/家庭收支統計.csv") 
# price_data <- fread("data/物價統計.csv") 
# salary_data <- fread("data/薪資收入月資料.csv") 


# sale_data
colnames(sale_data) <- as.character(sale_data[2,])
sale_data <- sale_data[-1,]
sale_data <- sale_data[-1,]
sale_data <- sale_data[,1:12]

setnames(sale_data, 1, "year")
setnames(sale_data, 2, "month")
sale_data[, year := as.numeric(gsub("年", "", year))]
sale_data[, month := as.numeric(gsub("月", "", month))]
sale_data[sale_data == "-"] <- NA
sale_data[sale_data == ""] <- NA
sale_data[, 1:2 := lapply(.SD, nafill, type = "locf"), .SDcols = 1:2]
# sale_data <- sale_data[year > 1990]
sale_data[, 3:12] <- lapply(sale_data[, 3:12], function(x) as.numeric(gsub(",", "", x)))

setnames(sale_data, 
         c("(0920010)果蔬汁 (千公升)", "(0920910)碳酸飲料 (千公升)",  "(0920930)運動飲料 (千公升)", 
           "(0920940)咖啡飲料 (千公升)", "(0920950)茶類飲料 (千公升)", "(0920010)果蔬汁", 
           "(0920910)碳酸飲料", "(0920930)運動飲料", "(0920940)咖啡飲料", "(0920950)茶類飲料"),
         c("FruitVegetableJuice_volumn", "CarbonatedBeverage_volumn", "SportsDrink_volumn", 
           "CoffeeDrink_volumn", "TeaDrink_volumn", "FruitVegetableJuice_value", 
           "CarbonatedBeverage_value", "SportsDrink_value", "CoffeeDrink_value", "TeaDrink_value"))


# income_data

# house_income_data
# colnames(house_income_data) <- as.character(house_income_data[2,])
# house_income_data <- house_income_data[-1,]
# house_income_data <- house_income_data[-1,]
# # house_income_data <- house_income_data[,1:12]
# 
# colnames(house_income_data) <- c("year", "DisposableIncome", "AvgHouseholdDisposableIncome", 
#                   "LowestQuintileIncome", "SecondLowestQuintileIncome", 
#                   "MiddleQuintileIncome", "SecondHighestQuintileIncome", 
#                   "HighestQuintileIncome")
# house_income_data[, year := as.numeric(gsub("年", "", year))]
# 
# house_income_data[, year := year+1911]
# 
# house_income_data[, ] <- lapply(house_income_data[,], function(x) as.numeric(gsub(",", "", x)))


# # Price_data
# price_data[, c("year", "month") := tstrsplit(統計期, "年")]
# price_data[, year := as.numeric(year)]
# price_data[, month := as.numeric(gsub("月", "", month))]
# setnames(price_data, c("總指數", "15.非酒精性飲料及材料"), c("TotalIndex", "SoftDrink") )
# price_data <- price_data[, c("year", "month", "TotalIndex", "SoftDrink")]
# price_data[, year := year+1911]

# # salary_data
# salary_data[, c("year", "month") := tstrsplit(統計期, "年 ")]
# salary_data[, year := as.numeric(year)]
# salary_data[, month := as.numeric(gsub("月", "", month))]
# setnames(salary_data, c("總薪資"), c("TotalSalary"))
# salary_data <- salary_data[, c("year", "month", "TotalSalary")]
# salary_data[, year := year+1911]

# 將多個 data.table 進行 FULL JOIN
# list_of_dts <- list(sale_data, price_data, salary_data)
list_of_dts <- list(sale_data)
full_join_all <- Reduce(function(x, y) merge(x, y, by = c("year", "month"), all = TRUE), list_of_dts)
# full_join_all <- merge(full_join_all, house_income_data, by = "year", all = TRUE)
# 查看結果
print(full_join_all)

# 計算新增單位價格並合併
full_join_all[, FruitVegetableJuice_price := FruitVegetableJuice_value/FruitVegetableJuice_volumn] 
full_join_all[, CarbonatedBeverage_price := CarbonatedBeverage_value/CarbonatedBeverage_volumn] 
full_join_all[, SportsDrink_price := SportsDrink_value/SportsDrink_volumn] 
full_join_all[, CoffeeDrink_price := CoffeeDrink_value/CoffeeDrink_volumn] 
full_join_all[, TeaDrink_price := TeaDrink_value/TeaDrink_volumn] 
print(full_join_all)


full_join_all <- as.data.table(lapply(full_join_all, function(col) as.numeric(as.character(col))))
full_join_all <- subset(full_join_all, year >= 1991)
full_join_all <- subset(full_join_all, year < 2024)
full_join_all <- full_join_all[!is.na(full_join_all$month), ]  # 刪除 month 為 NA 的行

# write.csv(full_join_all, "data/fullData.csv", row.names = FALSE)
write.csv(full_join_all, "data/DSE-5.csv", row.names = FALSE)