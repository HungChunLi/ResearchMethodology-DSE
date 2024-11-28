user <- Sys.info()["user"]

if (user == "brianhjli") {
  setwd("/Users/brianhjli/Dropbox/113-1/Research Methodology/DSE-5")
} else if (user == "QQ") {
  setwd("C:/Users/QQ/Dropbox/DSE-5")
} else if (user == "hayashijikyou") {
  setwd("/Users/hayashijikyou/Library/CloudStorage/Dropbox/DSE-5")
} else if (user == "11097"){
  setwd("C:/Users/11097/Dropbox/DSE-5")
} else {
  warning("使用者名稱為空，請檢查檔案路徑！")
}

#ADF檢定

install.packages("readr")
install.packages("tseries")
library(readr)
library(tseries)

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
library(forecast)
library(ggplot2)
library(gridExtra)
library(grid)

# 設定工作路徑
output_path <- "C:/Users/11097/Dropbox/DSE-5/孝儒的部分/"

# 讀取模型並提取殘差
aids_model <- readRDS("outcome/aids_model.rds")
laaids_model <- readRDS("outcome/laaids_model.rds")
resid_aids <- aids_model$qResid
resid_laaids <- laaids_model$qResid

# # # 繪製 AIDS 模型的殘差 ACF
# # acf(resid_aids, main = "ACF of AIDS Model Residuals")
# # 
# # # 繪製 LAAIDS 模型的殘差 ACF
# # acf(resid_laaids, main = "ACF of LAAIDS Model Residuals")
# 
# 
# # 定義變數分類
# # categories <- list(
# #   Price = c("FruitVegetableJuice_price", "CarbonatedBeverage_price",
# #             "SportsDrink_price", "CoffeeDrink_price", "TeaDrink_price"),
# #   Volume = c("FruitVegetableJuice_volumn", "CarbonatedBeverage_volumn",
# #              "SportsDrink_volumn", "CoffeeDrink_volumn", "TeaDrink_volumn"),
# #   Value = c("FruitVegetableJuice_value", "CarbonatedBeverage_value",
# #             "SportsDrink_value", "CoffeeDrink_value", "TeaDrink_value")
# # )
# 
# # categories <- list(
# #   resid_aids = c("qResid1", "qResid2", "qResid3", "qResid4", "qResid5"),
# #   resid_laaids = c("qResid1", "qResid2", "qResid3", "qResid4", "qResid5")
# # )
# categories <- list("aids" = resid_aids, "laaids" = resid_laaids)
# 
# # 初始化圖表清單
# plots <- list()
# 
# # 對每個分類生成 ACF 圖
# for (category in names(categories)) {
#   category_plots <- list()  # 存放此分類中的變數圖
#   
#   for (var in categories[[category]]) {
#     # 將變數轉換為時間序列
#     ts_var <- ts(data[[var]], start = c(1991, 1), frequency = 12)
#     
#     # 使用 forecast 套件生成 ACF 圖
#     p <- ggAcf(ts_var) +
#       ggtitle(var) +
#       theme_minimal() +
#       theme(
#         plot.title = element_text(size = 10, hjust = 0.5),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8)
#       )
#     
#     # 將圖加入此分類的清單
#     category_plots[[var]] <- p
#   }
#   
#   # 合併此分類的所有圖
#   plots[[category]] <- do.call(grid.arrange, c(category_plots, ncol = 2, top = category))
# }
# 
# # 保存每個分類的圖表
# for (category in names(plots)) {
#   ggsave(
#     filename = paste0(output_path, category, "_plot.png"),  # 生成完整文件路徑
#     plot = plots[[category]],                              # 指定要保存的圖
#     width = 10, height = 8                                 # 圖片大小
#   )
# }

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
    filename = paste0(output_path, category_name, "_plot.png"),  # 生成完整文件路径
    plot = plots[[category_name]],                              # 指定要保存的图
    width = 10, height = 8                                      # 图片大小
  )
}
