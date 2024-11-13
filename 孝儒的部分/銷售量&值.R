
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")

library(readxl)
library(dplyr)
library(psych)

# 讀取Excel檔案中的資料
data <- read_excel("C:/Users/QQ/Desktop/R存檔/銷售量和銷售值民國.xlsx", sheet = 1) 

data_clean <- data %>%
  select(`銷售量_果蔬汁 (千公升)`, `銷售量_碳酸飲料 (千公升)`, `銷售量_運動飲料 (千公升)`, 
         `銷售量_咖啡飲料 (千公升)`, `銷售量_茶類飲料 (千公升)`, 
         `銷售值 (千元)_果蔬汁`, `銷售值 (千元)_碳酸飲料`, `銷售值 (千元)_運動飲料`, 
         `銷售值 (千元)_咖啡飲料`, `銷售值 (千元)_茶類飲料`)

describe(data_clean)

summary_stats <- describe(data_clean)

View(summary_stats)

install.packages("writexl")
library(writexl)

summary_stats <- describe(data_clean)
summary_stats_df <- as.data.frame(summary_stats)
summary_stats_df <- tibble::rownames_to_column(summary_stats_df, var = "`銷售量_果蔬汁 (千公升)`, `銷售量_碳酸飲料 (千公升)`, `銷售量_運動飲料 (千公升)`, 
         `銷售量_咖啡飲料 (千公升)`, `銷售量_茶類飲料 (千公升)`, 
         `銷售值 (千元)_果蔬汁`, `銷售值 (千元)_碳酸飲料`, `銷售值 (千元)_運動飲料`, 
         `銷售值 (千元)_咖啡飲料`, `銷售值 (千元)_茶類飲料`")

write_xlsx(summary_stats_df, "C:/Users/QQ/Desktop/R存檔/敘述統計結果.xlsx")


install.packages("ggplot2")
library(ggplot2)

data$Date <- seq(as.Date("1991-01-01"), by = "month", length.out = nrow(data))

library(ggplot2)
library(tidyr)

# 轉換為長格式
sales_long <- pivot_longer(data, cols = c(`銷售量_果蔬汁 (千公升)`, `銷售量_碳酸飲料 (千公升)`, `銷售量_運動飲料 (千公升)`, 
                                          `銷售量_咖啡飲料 (千公升)`, `銷售量_茶類飲料 (千公升)`), 
                           names_to = "飲料類型", values_to = "銷售量_千公升")

# 使用 ggplot 正確繪製圖表
P<-ggplot(sales_long, aes(x = Date, y = `銷售量_千公升`, color = `飲料類型`)) +
  geom_line(linewidth = 1) +  # 用 linewidth 替代 size
  labs(title = "隨時間變化的銷售量", 
       x = "Date", 
       y = "銷售量 (千公升)", 
       color = "飲料類型") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Users/QQ/Desktop/R存檔/銷售量.jpg", plot = P, device = "jpeg", width = 10, height = 6, dpi = 300)


# 轉換為長格式
sales_long2 <- pivot_longer(data, cols = c(`銷售值 (千元)_果蔬汁`, `銷售值 (千元)_碳酸飲料`, 
                                           `銷售值 (千元)_運動飲料`, `銷售值 (千元)_咖啡飲料`, 
                                           `銷售值 (千元)_茶類飲料`), 
                            names_to = "飲料類型", values_to = "銷售值_千元")

# 使用 ggplot 正確繪製圖表，確保縱軸刻度完整顯示
P2 <- ggplot(sales_long2, aes(x = Date, y = `銷售值_千元`, color = `飲料類型`)) +
  geom_line(linewidth = 1) +  # 用 linewidth 替代 size
  labs(title = "隨時間變化的銷售值", 
       x = "Date", 
       y = "銷售值 (千元)", 
       color = "飲料類型") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # 使用 scales::comma 來確保縱軸顯示完整數字

# 保存圖表為 JPG 檔案
ggsave("C:/Users/QQ/Desktop/R存檔/銷售值.jpg", plot = P2, device = "jpeg", width = 10, height = 6, dpi = 300)


