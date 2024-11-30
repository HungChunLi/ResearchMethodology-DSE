rm(list = ls())
# # 安裝必要的package
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("micEconAids")
# install.packages("xtable")
# install.packages("knitr")
# install.packages("writexl")
# install.packages("readr")
# install.packages("tidyr")
# install.packages("kableExtra")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("gridExtra")
# install.packages("grid")

# 載入必要的package
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(micEconAids)
library(xtable)
library(knitr)
library(writexl)
library(readr)
library(tidyr)
library(kableExtra)
library(tidyverse)
library(stringr)
library(tseries)
library(forecast)
library(gridExtra)
library(grid)

# 資料前處理

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

# 處理sale_data
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
sale_data[, 3:12] <- lapply(sale_data[, 3:12], function(x) as.numeric(gsub(",", "", x)))

setnames(sale_data, 
         c("(0920010)果蔬汁 (千公升)", "(0920910)碳酸飲料 (千公升)",  "(0920930)運動飲料 (千公升)", 
           "(0920940)咖啡飲料 (千公升)", "(0920950)茶類飲料 (千公升)", "(0920010)果蔬汁", 
           "(0920910)碳酸飲料", "(0920930)運動飲料", "(0920940)咖啡飲料", "(0920950)茶類飲料"),
         c("FruitVegetableJuice_volumn", "CarbonatedBeverage_volumn", "SportsDrink_volumn", 
           "CoffeeDrink_volumn", "TeaDrink_volumn", "FruitVegetableJuice_value", 
           "CarbonatedBeverage_value", "SportsDrink_value", "CoffeeDrink_value", "TeaDrink_value"))

list_of_dts <- list(sale_data)
full_join_all <- Reduce(function(x, y) merge(x, y, by = c("year", "month"), all = TRUE), list_of_dts)
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

# 保存為csv資料
write.csv(full_join_all, "data/DSE-5.csv", row.names = FALSE)

# AIDS和LAAIDS模型分析

# 讀取資料
data <- fread("data/DSE-5.csv")

# 確認資料框中有年份欄位
if (!"year" %in% names(data)) {
  stop("資料中缺少年份 (year) 欄位")
}

# 設定數量欄位
quantityCols <- c("FruitVegetableJuice_volumn", "CarbonatedBeverage_volumn",
                  "SportsDrink_volumn", "CoffeeDrink_volumn", "TeaDrink_volumn")
# 設定價格欄位
priceCols <- c("FruitVegetableJuice_price", "CarbonatedBeverage_price", 
               "SportsDrink_price", "CoffeeDrink_price", "TeaDrink_price")
# 設定價值欄位
valueCols <- c("FruitVegetableJuice_value", "CarbonatedBeverage_value",
               "SportsDrink_value", "CoffeeDrink_value", "TeaDrink_value")
# 設定需求數量（需求份額）欄位
shareCols <- c("FruitVegetableJuice_share", "CarbonatedBeverage_share", 
               "SportsDrink_share", "CoffeeDrink_share", "TeaDrink_share")
# # 設定總所得欄位
incomeCols <- c("total_value", "AvgHouseholdDisposableIncome", "LowestQuintileIncome", "SecondLowestQuintileIncome",
                "MiddleQuintileIncome", "SecondHighestQuintileIncome", "HighestQuintileIncome")

#計算total volue
data$total_value <- rowSums(data[, ..valueCols])

#計算份額
fullData <- as.data.table(data)
fullData[, FruitVegetableJuice_share := FruitVegetableJuice_value/get(incomeCols[1])]
fullData[, CarbonatedBeverage_share := CarbonatedBeverage_value/get(incomeCols[1])]
fullData[, SportsDrink_share := SportsDrink_value/get(incomeCols[1])]
fullData[, CoffeeDrink_share := CoffeeDrink_value/get(incomeCols[1])]
fullData[, TeaDrink_share := TeaDrink_value/get(incomeCols[1])]

required_cols <- c(priceCols, shareCols, incomeCols[1])
fullData <- subset(fullData, select = required_cols)

# 檢查數據中是否有 NA 並處理
if (sum(is.na(fullData)) > 0) {
  fullData <- na.omit(fullData)
  message("數據中含有 NA，已移除缺失值。")
}

# 進行 AIDS 模型估計
aids_model <- aidsEst(
  priceNames = priceCols,
  shareNames = shareCols, 
  totExpName = incomeCols[1], 
  data = fullData, priceIndex = "T", method = "IL")
summary(aids_model)
aids_elasticities <- aidsElas(
  coef = aids_model$coef,  # Pass coefficients manually
  shares = aids_model$wMeans,  # Average budget shares
  prices = aids_model$pMeans   # Average prices
)

write.csv(as.data.frame(aids_model$coef$stat), "outcome/output_aids_stat.csv", quote = FALSE)
write.csv(as.data.frame(aids_elasticities$exp), "outcome/output_aids_exp.csv", quote = FALSE)
write.csv(as.data.frame(aids_elasticities$marshall), "outcome/output_aids_marshall.csv", quote = FALSE)
write.csv(as.data.frame(aids_elasticities$hicks), "outcome/output_aids_hicks.csv", quote = FALSE)
saveRDS(aids_model, file = "outcome/aids_model.rds")
saveRDS(aids_elasticities, file = "outcome/aids_elasticities.rds")


# 進行 LAAIDS 模型估計
laaids_model <- aidsEst(
  priceNames = priceCols,
  shareNames = shareCols, 
  totExpName = incomeCols[1], 
  data = fullData, priceIndex = "S", method = "LA")
summary(laaids_model)
laaids_elasticities <- aidsElas(
  coef = laaids_model$coef,  # Pass coefficients manually
  shares = laaids_model$wMeans,  # Average budget shares
  prices = laaids_model$pMeans   # Average prices
)

write.csv(as.data.frame(laaids_model$coef$stat), "outcome/output_laaids.csv", quote = FALSE)
write.csv(as.data.frame(laaids_elasticities$exp), "outcome/output_laaids_exp.csv", quote = FALSE)
write.csv(as.data.frame(laaids_elasticities$marshall), "outcome/output_laaids_marshall.csv", quote = FALSE)
write.csv(as.data.frame(laaids_elasticities$hicks), "outcome/output_laaids_hicks.csv", quote = FALSE)
saveRDS(laaids_model, file = "outcome/laaids_model.rds")
saveRDS(laaids_elasticities, file = "outcome/laaids_elasticities.rds")


# homo_and_sym_test

# 讀取 AIDS 和 LAAIDS 模型
aids_model <- readRDS("outcome/aids_model.rds")
laaids_model <- readRDS("outcome/laaids_model.rds")

# 定義函數來檢查同質性和對稱性
check_homogeneity_symmetry <- function(model, model_name) {
  # 提取 gamma 矩陣
  gamma_matrix <- coef(model)$gamma
  
  # 確保 gamma_matrix 是矩陣格式
  if (!is.matrix(gamma_matrix)) {
    stop(paste(model_name, "gamma_matrix is not a matrix."))
  }

  # **同質性檢查**
  row_sums <- rowSums(gamma_matrix)
  homogeneity_deviation <- row_sums  # 每行總和的實際值
  homogeneity_pass <- abs(homogeneity_deviation) < 1e-6  # 判斷每行是否符合條件
  
  # **對稱性檢查**
  symmetry_deviation <- gamma_matrix - t(gamma_matrix)  # 對稱性偏差
  symmetry_pass_matrix <- abs(symmetry_deviation) < 1e-6  # 判斷每個元素是否符合條件
  
  # **整合檢查 (Homogeneity * Symmetry)**
  combined_pass <- homogeneity_pass & apply(symmetry_pass_matrix, 1, all)  # 行方向判斷
  
  # **整理表格**
  homogeneity_table <- data.frame(
    Row = seq_len(nrow(gamma_matrix)),  # 行數
    `Row Sum` = round(homogeneity_deviation, 10),  # 每行總和
    `Homogeneity Check` = homogeneity_pass,  # 同質性判斷
    `Homogeneity * Symmetry Check` = combined_pass  # 同質性與對稱性綜合判斷
  )
  
  symmetry_table <- as.data.frame(as.table(round(symmetry_deviation, 10)))  # 對稱偏差矩陣展平
  colnames(symmetry_table) <- c("Row", "Column", "Deviation")  # 命名列
  symmetry_table$`Symmetry Check` <- abs(symmetry_table$Deviation) < 1e-6  # 判斷結果
  
  # 返回結果
  list(
    model_name = model_name,
    homogeneity_table = homogeneity_table,
    symmetry_table = symmetry_table
  )
}

# 同時檢查 AIDS 和 LAAIDS 模型
aids_results <- check_homogeneity_symmetry(aids_model, "AIDS Model")
laaids_results <- check_homogeneity_symmetry(laaids_model, "LAAIDS Model")

# **將結果寫入 Excel 文件**
output_data <- list(
  "AIDS_Homogeneity" = aids_results$homogeneity_table,
  "AIDS_Symmetry" = aids_results$symmetry_table,
  "LAAIDS_Homogeneity" = laaids_results$homogeneity_table,
  "LAAIDS_Symmetry" = laaids_results$symmetry_table
)

write_xlsx(output_data, path = "Homogeneity_Symmetry_Combined_Results.xlsx")

# 輸出結果
cat("\nResults have been saved to Homogeneity_Symmetry_Combined_Results.xlsx\n")

# likelihood ratio test

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


# 敘述統計
data <- read.csv("data/DSE-5.csv")
filtered_data <- data %>% select(-year, -month)

# 計算敘述統計
descriptive_stats <- filtered_data %>%
  summarise(
    Count = sapply(filtered_data, function(x) sum(!is.na(x))),
    Mean = sapply(filtered_data, function(x) mean(x, na.rm = TRUE)),
    Std_Dev = sapply(filtered_data, function(x) sd(x, na.rm = TRUE)),
    Min = sapply(filtered_data, function(x) min(x, na.rm = TRUE)),
    `25th_Percentile` = sapply(filtered_data, function(x) quantile(x, 0.25, na.rm = TRUE)),
    Median = sapply(filtered_data, function(x) median(x, na.rm = TRUE)),
    `75th_Percentile` = sapply(filtered_data, function(x) quantile(x, 0.75, na.rm = TRUE)),
    Max = sapply(filtered_data, function(x) max(x, na.rm = TRUE))
  ) %>%
  as.data.frame()
#  %>% t()
# 轉置數據框使其易於閱讀
descriptive_stats_t <- t(descriptive_stats)
colnames(descriptive_stats_t) <- names(filtered_data)
# 輸出結果
print(as.data.frame(descriptive_stats_t))

# 整理時間資料
data <- data %>%
  mutate(time = as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d"))

# 定義中文名稱映射
rename_mapping <- c(
  "FruitVegetableJuice" = "果蔬汁",
  "CarbonatedBeverage" = "碳酸飲料",
  "SportsDrink" = "運動飲料",
  "CoffeeDrink" = "咖啡",
  "TeaDrink" = "茶"
)

# 定義圖表標題和單位
plot_groups <- list(
  volume = list(
    title = "銷售量 (千公升)",
    variables = grep("volume", colnames(data), value = TRUE)
  ),
  value = list(
    title = "銷售值 (千元)",
    variables = grep("value", colnames(data), value = TRUE)
  ),
  price = list(
    title = "價格 (元)",
    variables = grep("price", colnames(data), value = TRUE)
  )
)

volumn_list <- c("time", "FruitVegetableJuice_volumn", "CarbonatedBeverage_volumn", "SportsDrink_volumn", "CoffeeDrink_volumn", "TeaDrink_volumn")
df_volumn <- data[volumn_list]

# Exclude 'time' column
df_long <- pivot_longer(
  df_volumn,
  cols = -time,  # Pivot all columns except 'time'
  names_to = "category",
  values_to = "value"
) %>% mutate(
  # category = str_replace(category, paste0("_", group), ""),
  category = str_replace(category, "_volumn", ""),
  category = recode(category, !!!rename_mapping) # 使用 recode 替代 str_replace_all
)

volumn <- ggplot(df_long, aes(x = time, y = value, color = category)) +
  geom_line(size = 1) +
  labs(title = NULL, x = "時間", y = "值") +
  theme_replace() +
  theme(
    legend.title = element_blank(),          # 去掉圖例標題
    # legend.text = element_text(size = 8),    # 縮小圖例字體
    legend.position = "bottom",              # 將圖例移到橫軸下方
    # legend.background = element_rect(fill = "white", color = "black", size = 0.5) # 添加圖例背景
  )

ggsave(
  filename = paste0("outcome/chart1.png"),
  plot = volumn,   # 對應的圖表
  width = 8,               # 圖片寬度 (英寸)
  height = 6,              # 圖片高度 (英寸)
  dpi = 300                # 圖片解析度 (dpi)
)



price_list <- c("time", "FruitVegetableJuice_price", "CarbonatedBeverage_price", "SportsDrink_price", "CoffeeDrink_price", "TeaDrink_price")
df_price <- data[price_list]

# Exclude 'time' column
df_long <- pivot_longer(
  df_price,
  cols = -time,  # Pivot all columns except 'time'
  names_to = "category",
  values_to = "value"
) %>% mutate(
  # category = str_replace(category, paste0("_", group), ""),
  category = str_replace(category, "_price", ""),
  category = recode(category, !!!rename_mapping) # 使用 recode 替代 str_replace_all
)

price <- ggplot(df_long, aes(x = time, y = value, color = category)) +
  geom_line(size = 1) +
  labs(title = NULL, x = "時間", y = "值") +
  theme_replace() +
  theme(
    legend.title = element_blank(),          # 去掉圖例標題
    # legend.text = element_text(size = 8),    # 縮小圖例字體
    legend.position = "bottom",              # 將圖例移到橫軸下方
    # legend.background = element_rect(fill = "white", color = "black", size = 0.5) # 添加圖例背景
  )

ggsave(
  filename = paste0("outcome/chart2.png"),
  plot = price,   # 對應的圖表
  width = 8,               # 圖片寬度 (英寸)
  height = 6,              # 圖片高度 (英寸)
  dpi = 300                # 圖片解析度 (dpi)
)



value_list <- c("time", "FruitVegetableJuice_value", "CarbonatedBeverage_value", "SportsDrink_value", "CoffeeDrink_value", "TeaDrink_value")
df_value <- data[value_list]

# Exclude 'time' column
df_long <- pivot_longer(
  df_value,
  cols = -time,  # Pivot all columns except 'time'
  names_to = "category",
  values_to = "value"
) %>% mutate(
  # category = str_replace(category, paste0("_", group), ""),
  category = str_replace(category, "_value", ""),
  category = recode(category, !!!rename_mapping) # 使用 recode 替代 str_replace_all
)

value <- ggplot(df_long, aes(x = time, y = value, color = category)) +
  geom_line(size = 1) +
  labs(title = NULL, x = "時間", y = "值") +
  theme_replace() +
  theme(
    legend.title = element_blank(),          # 去掉圖例標題
    # legend.text = element_text(size = 8),    # 縮小圖例字體
    legend.position = "bottom",              # 將圖例移到橫軸下方
    # legend.background = element_rect(fill = "white", color = "black", size = 0.5) # 添加圖例背景
  )

ggsave(
  filename = paste0("outcome/chart3.png"),
  plot = value,   # 對應的圖表
  width = 8,               # 圖片寬度 (英寸)
  height = 6,              # 圖片高度 (英寸)
  dpi = 300                # 圖片解析度 (dpi)
)

#ADF檢定

data <- read_csv("data/DSE-5.csv")

variables <- names(data)[3:ncol(data)]

# 初始化一個列表來存放成功執行 ADF 檢定的結果
adf_results <- list()

# 進行 ADF 檢定並檢查是否有結果
for (var in variables) {
  # 去除缺失值，並嘗試進行 ADF 檢定
  x <- na.omit(data[[var]])
  if (length(x) > 0) {  # 確認變數有非空的數據
    test_result <- tryCatch(adf.test(x, alternative = "stationary"), error = function(e) NULL)
    
    # 如果檢定成功，儲存結果
    if (!is.null(test_result)) {
      adf_results[[var]] <- list(statistic = test_result$statistic, p_value = test_result$p.value)
    } else {
      adf_results[[var]] <- list(statistic = NA, p_value = NA)
    }
  } else {
    adf_results[[var]] <- list(statistic = NA, p_value = NA)
  }
}

# 將結果轉換為 data.frame
adf_summary <- data.frame(
  Variable = names(adf_results),
  Statistic = sapply(adf_results, function(res) res$statistic),
  P_Value = sapply(adf_results, function(res) res$p_value),
  Stationary = sapply(adf_results, function(res) ifelse(res$p_value < 0.05, "Yes", "No"))
)

# 查看結果表格
print(adf_summary)


#ACF檢定

# 讀取模型並提取殘差
aids_model <- readRDS("outcome/aids_model.rds")
laaids_model <- readRDS("outcome/laaids_model.rds")
resid_aids <- aids_model$qResid
resid_laaids <- laaids_model$qResid

# 定義模型變數分類
categories <- list("AIDS" = resid_aids, "LAAIDS" = resid_laaids)

# 初始化圖表清單
plots <- list()

# 對每個模型生成ACF圖表
for (category_name in names(categories)) {
  category_data <- categories[[category_name]]  # 獲取當前模型的數據
  category_plots <- list()  # 存放該模型的變量圖
  
  for (var_name in names(category_data)) {  
    ts_var <- ts(category_data[[var_name]], start = c(1991, 1), frequency = 12)
    
    # 使用 forecast 套件生成 ACF 圖表
    p <- ggAcf(ts_var) +
      ggtitle(var_name) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)
      )
    
    # 将圖表加入對應模型的清单
    category_plots[[var_name]] <- p
  }
  
  # 合併每個模型的所有圖表
  plots[[category_name]] <- do.call(grid.arrange, c(category_plots, ncol = 2, top = category_name))
}

# 保存每個模型的圖表
for (category_name in names(plots)) {
  ggsave(
    filename = paste0("outcome/", category_name, "_plot.png"),  # 生成完整文件路径
    plot = plots[[category_name]],                              # 指定要保存的图
    width = 10, height = 8                                      # 图片大小
  )
}
