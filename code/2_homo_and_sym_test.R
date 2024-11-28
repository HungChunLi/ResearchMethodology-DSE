rm(list = ls())


# 安裝和載入所需的套件
# install.packages("micEconAids")
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

library(knitr)
library(writexl)  # 用於導出 Excel 文件

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

# 輸出到終端
cat("\nResults have been saved to Homogeneity_Symmetry_Combined_Results.xlsx\n")
