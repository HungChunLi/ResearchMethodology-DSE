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


# read files
aids_model <- readRDS("outcome/aids_model.rds")
laaids_model <- readRDS("outcome/laaids_model.rds")


# homogeneity and symmetry

# 提取 AIDS 模型的 gamma 矩阵
gamma_matrix <- coef(aids_model)$gamma  # 或者使用 laaids_model

# 确保 gamma_matrix 是一个矩阵
if (!is.matrix(gamma_matrix)) {
  stop("gamma_matrix is not a matrix.")
}

# 检查同质性（每行的价格弹性之和是否为 0）
homogeneity_check <- all.equal(rowSums(gamma_matrix), rep(0, nrow(gamma_matrix)), check.attributes = FALSE)

# 输出同质性检查结果
cat("Homogeneity check result:", homogeneity_check, "\n")

# 检查對稱性（價格彈性矩陣是否对称）
symmetry_check <- isSymmetric(gamma_matrix, tol = 1e-10, check.attributes = FALSE)

# 输出对称性检查结果
cat("Symmetry check result:", symmetry_check, "\n")
