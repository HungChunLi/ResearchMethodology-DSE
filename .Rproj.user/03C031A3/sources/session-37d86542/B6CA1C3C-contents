# 安裝和載入所需的套件
# install.packages("micEconAids")

library(micEconAids)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)

gamma_matrix <- matrix(c(
  # 45932, -58530, -33387, 75073, -29087, 
  # 0.12928, -0.51902, -0.25260, 0.37423, 0.26811, 
  -6876.8, -5973.9, 2711.7, 3853.2, 6285.8, 
  -5973.9, -36348, -2408.1, 9330.4, 35399, 
  2711.7, -2408.1, -11501, 6126.5, 5070.5, 
  3853.2, 9330.4, 6126.5, -7007.1, -12303, 
  6285.8, 35399, 5070.5, -12303, -34452
), nrow=5, byrow=TRUE)

# 將矩陣轉換為數據框
gamma_df <- as.data.frame(gamma_matrix)

# 添加行名和列名
colnames(gamma_df) <- paste0("Index_", 1:ncol(gamma_df))
gamma_df$Type <- paste0("Type_", 1:nrow(gamma_df))
p