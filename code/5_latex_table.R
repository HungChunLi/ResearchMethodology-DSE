rm(list = ls())

# 安裝和載入所需的套件
# install.packages("micEconAids")

library(micEconAids)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(xtable)
library(tidyr)


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

# 自定義格式：將 Estimate 和 Std. Error 放在一起，用星號表示顯著性
format_table <- function(data) {
  # 添加顯著性星號
  data$Significance <- cut(data[, "Pr(>|t|)"], 
                           breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                           labels = c("***", "**", "*", ".", ""))
  
  # 合併 Estimate 和 Std. Error
  data$Combined <- sprintf("%.3f%s", 
                           data[, "Estimate"], 
                           data$Significance)
  data$StdErr <- sprintf("(%.3f)", data[, "Std. Error"])
  
  # 格式化表格
  data <- data[, c("Combined", "StdErr")]
  data$Variable <- rownames(data)

  pivot_data <- data %>% pivot_longer(
      cols = c("Combined", "StdErr"),  # Columns to pivot
      names_to = "Measure",            # New column for the measure name
      values_to = "Value"              # New column for the values
    ) %>% mutate(
      Variable = if_else(Measure == "StdErr", "", Variable)
    ) %>% select(-Measure)

  # 返回格式化表格，只保留感興趣的列
  return(pivot_data)
}


aids_model <- readRDS(file = "outcome/aids_model.rds")
aids_elasticities <- readRDS(file = "outcome/aids_elasticities.rds")
laaids_model <- readRDS(file = "outcome/laaids_model.rds")
laaids_elasticities <- readRDS(file = "outcome/laaids_elasticities.rds")


price_old <- c("FruitVegetableJuice_price", "CarbonatedBeverage_price", "SportsDrink_price", "CoffeeDrink_price", "TeaDrink_price")
price_new <- c("果蔬汁價格", "碳酸飲料價格", "運動飲料價格", "咖啡飲料價格", "茶飲料價格")
share_old <- c("FruitVegetableJuice_share", "CarbonatedBeverage_share", "SportsDrink_share", "CoffeeDrink_share", "TeaDrink_share")
share_new <- c("果蔬汁份額", "碳酸飲料份額", "運動飲料份額", "咖啡飲料份額", "茶飲料份額")


aids_coef <- as.data.frame(aids_model$coef$stat)
# setnames(aids_coef, old = share_old, new = share_new)
# rownames(aids_coef) <- "支出彈性"
print(aids_coef)
xt <- xtable(aids_coef)
align(xt) <- c("c", "c", "c", "c", "c")
print(xt, file = "report/tables/aids_coef.tex", floating = FALSE)

laaids_coef <- as.data.frame(laaids_model$coef$stat)
# setnames(laaids_coef, old = share_old, new = share_new)
# rownames(laaids_coef) <- "支出彈性"
print(laaids_coef)
xt <- xtable(laaids_coef)
align(xt) <- c("c", "c", "c", "c", "c")
print(xt, file = "report/tables/laaids_coef.tex", floating = FALSE)

formatted_aids_coef <- format_table(aids_coef)
formatted_laaids_coef <- format_table(laaids_coef)
combined_df <- cbind(formatted_aids_coef, formatted_laaids_coef)
colnames(combined_df) <- c("變數", "(1)", "X", "(2)")
combined_df <- combined_df %>% select(c("變數", "(1)", "(2)"))
combined_df_alpha <- combined_df[1:10, ]
combined_df_beta <- combined_df[11:20, ]
combined_df_gamma <- combined_df[21:70, ]
xt <- xtable(combined_df_alpha)
align(xt) <- c("l", "l", "l", "l")
print(xt, file = "report/tables/combined_coef_alpha.tex", floating = FALSE, include.rownames = FALSE)
xt <- xtable(combined_df_beta)
align(xt) <- c("l", "l", "l", "l")
print(xt, file = "report/tables/combined_coef_beta.tex", floating = FALSE, include.rownames = FALSE)
xt <- xtable(combined_df_gamma)
align(xt) <- c("l", "l", "l", "l")
print(xt, file = "report/tables/combined_coef_gamma.tex", floating = FALSE, include.rownames = FALSE)

aids_exp <- aids_elasticities$exp %>% t() %>% as.data.frame()
setnames(aids_exp, new = share_new)
rownames(aids_exp) <- "支出彈性"
print(aids_exp)
xt <- xtable(aids_exp)
align(xt) <- c("c", "c", "c", "c", "c", "c")
print(xt, file = "report/tables/aids_exp.tex", floating = FALSE)

laaids_exp <- laaids_elasticities$exp %>% t() %>% as.data.frame()
setnames(laaids_exp, new = share_new)
rownames(laaids_exp) <- "支出彈性"
print(laaids_exp)
xt <- xtable(laaids_exp)
align(xt) <- c("c", "c", "c", "c", "c", "c")
print(xt, file = "report/tables/laaids_exp.tex", floating = FALSE)


aids_marshall <- as.data.frame(aids_elasticities$marshall)
setnames(aids_marshall, old = price_old, new = price_new)
rownames(aids_marshall) <- share_new
print(aids_marshall)
xt <- xtable(aids_marshall)
align(xt) <- c("c", "c", "c", "c", "c", "c")
print(xt, file = "report/tables/aids_marshall.tex", floating = FALSE)

laaids_marshall <- as.data.frame(laaids_elasticities$marshall)
setnames(laaids_marshall, old = price_old, new = price_new)
rownames(laaids_marshall) <- share_new
print(laaids_marshall)
xt <- xtable(laaids_marshall)
align(xt) <- c("c", "c", "c", "c", "c", "c")
print(xt, file = "report/tables/laaids_marshall.tex", floating = FALSE)


aids_hicks <- as.data.frame(aids_elasticities$hicks)
setnames(aids_hicks, old = price_old, new = price_new)
rownames(aids_hicks) <- share_new
print(aids_hicks)
xt <- xtable(aids_hicks)
align(xt) <- c("c", "c", "c", "c", "c", "c")
print(xt, file = "report/tables/aids_hicks.tex", floating = FALSE)

laaids_hicks <- as.data.frame(laaids_elasticities$hicks)
setnames(laaids_hicks, old = price_old, new = price_new)
rownames(laaids_hicks) <- share_new
print(laaids_hicks)
xt <- xtable(laaids_hicks)
align(xt) <- c("c", "c", "c", "c", "c", "c")
print(xt, file = "report/tables/laaids_hicks.tex", floating = FALSE)