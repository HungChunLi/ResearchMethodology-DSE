install.packages("readr")

library(readr)

data <- read_csv("C:/Users/QQ/Dropbox/DSE-5/data/fullData.csv")

install.packages("tseries")
library(tseries)

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



















