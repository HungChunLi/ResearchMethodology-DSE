rm(list = ls())

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("kableExtra")
install.packages("tidyr")  # 如果尚未安裝
install.packages("stringr")


# 載入套件
library(readr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(ggplot2)
library(tidyverse)
library(stringr)


user <- Sys.info()["user"]
if (user == "brianhjli") {
  setwd("/Users/brianhjli/Dropbox/113-1/Research Methodology/DSE-5")
} else if (user == "QQ") {
  setwd("C:/Users/QQ/Dropbox/DSE-5")
} else if (user == "hayashijikyou") {
  setwd("/Users/hayashijikyou/Library/CloudStorage/Dropbox/DSE-5")
} else {
  warning("使用者名稱未匹配，請檢查檔案路徑！")
}


# data <- read.csv("data/fullData.csv")
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




# # 初始化空的 plots 列表
# plots <- list()

# # 繪製圖表
# for (group in names(plot_groups)) {
#   group_data <- data %>%
#     select(time, all_of(plot_groups[[group]]$variables)) %>%
#     pivot_longer(-time, names_to = "type", values_to = "value") %>%
#     mutate(
#       type = str_replace(type, paste0("_", group), ""),
#       type = str_replace(type, "volume", ""),
#       type = recode(type, !!!rename_mapping) # 使用 recode 替代 str_replace_all
#     )
  
#   # 繪製圖表
#   p <- ggplot(group_data, aes(x = time, y = value, color = type)) +
#     geom_line() +
#     labs(
#       title = plot_groups[[group]]$title,
#       x = NULL, # 移除橫軸標籤
#       y = NULL  # 移除縱軸標籤
#     ) +
#     scale_y_continuous(labels = scales::label_number(big.mark = ",")) + # 格式化 Y 軸數字
#     theme_minimal() +
#     theme(
#       legend.title = element_blank(),          # 去掉圖例標題
#       legend.text = element_text(size = 8),    # 縮小圖例字體
#       legend.position = "bottom",              # 將圖例移到橫軸下方
#       legend.background = element_rect(fill = "white", color = "black", size = 0.5) # 添加圖例背景
#     )

#   # 存入列表
#   plots[[group]] <- p
# }

# print(plots$volume) # 預覽銷售量圖表
# print(plots$value) # 預覽銷售值圖表
# print(plots$price) # 預覽價格圖表

# # 定義輸出路徑
# output_dir1 <- "/outcome" # 替換為你的儲存路徑
# output_dir2 <- "/report/figures" # 替換為你的儲存路徑
# # # 確保資料夾存在
# # if (!dir.exists(output_dir)) {
# #   dir.create(output_dir, recursive = TRUE) # 遞迴建立資料夾
# # }

# # 儲存圖表
# for (group in names(plots)) {

#   # 使用 ggsave 儲存
#   ggsave(
#     filename = paste0(output_dir1, "/", group, "_chart.png"),
#     plot = plots[[group]],   # 對應的圖表
#     width = 8,               # 圖片寬度 (英寸)
#     height = 6,              # 圖片高度 (英寸)
#     dpi = 300                # 圖片解析度 (dpi)
#   )
#   ggsave(
#     filename = paste0(output_dir2, "/", group, "_chart.png"),
#     plot = plots[[group]],   # 對應的圖表
#     width = 8,               # 圖片寬度 (英寸)
#     height = 6,              # 圖片高度 (英寸)
#     dpi = 300                # 圖片解析度 (dpi)
#   )

#   # 儲存訊息
#   # message("已儲存圖表至: ", file_name)
# }