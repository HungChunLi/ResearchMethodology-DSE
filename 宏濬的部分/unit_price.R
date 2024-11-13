library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
# library(psych)

user <- "hung-chun"

if (user == "hung-chun"){
    setwd("/Users/brianhjli/Dropbox/113-1/Research Methodology/DSE-5")
} else if (user == "xiao-ru"){
    setwd("C:/Users/QQ/Desktop/R存檔")
} else if (user == "li-hong"){
    setwd("")
} else if (user == ""){
    setwd("")
}

data <- fread("data/銷售量及銷售值.csv") 

colnames(data) <- as.character(data[2,])
data <- data[-1,]
data <- data[-1,]
data <- data[,1:12]

setnames(data, 1, "year")
setnames(data, 2, "month")
data[data == ""] <- NA
data[, year := as.numeric(gsub("年", "", year))]
data[, month := as.numeric(gsub("月", "", month))]
data[, 1:2 := lapply(.SD, nafill, type = "locf"), .SDcols = 1:2]
data <- data[year > 1990]

data[, (names(data)) := lapply(.SD, function(x) as.numeric(gsub(",", "", x)))]
data[, 果蔬汁單位價格 := .SD[[8]] / .SD[[3]]]
data[, 碳酸飲料單位價格 := .SD[[9]] / .SD[[4]]]
data[, 運動飲料單位價格 := .SD[[10]] / .SD[[5]]]
data[, 咖啡飲料單位價格 := .SD[[11]] / .SD[[6]]]
data[, 茶類飲料單位價格 := .SD[[12]] / .SD[[7]]]


# price <- fread("data/物價統計.csv", encoding = "UTF-8") 
# price <- price[V4 := NULL]
# price[, c("year", "month") := tstrsplit(統計期, "年|月", fixed = TRUE)]

select_data <- data[, c("year", "month", "果蔬汁單位價格", "碳酸飲料單位價格", "運動飲料單位價格", "咖啡飲料單位價格", "茶類飲料單位價格")]
select_data_long <- melt(select_data, id.vars = c("year", "month"), 
                 measure.vars = c("果蔬汁單位價格", "碳酸飲料單位價格", "運動飲料單位價格", "咖啡飲料單位價格", "茶類飲料單位價格"),
                 variable.name = "Drink_Type", 
                 value.name = "Unit_Price")
select_data_long[, YearMonth := as.Date(paste(year, month, "1", sep = "-"))]
                 
p <- ggplot(select_data_long, aes(x = YearMonth, y = Unit_Price, color = Drink_Type)) +
  geom_line(size = 1) +  # Add lines
#   geom_point(size = ) +  # Add points at each month
  labs(title = "Prices of Drinks Since 1991",
       x = "Year",
       y = "Unit Price",
       color = "Drink Type") +
  theme_classic() +  # Use a minimal theme
  scale_x_date(
    date_labels = "%Y/%m",  # Format for x-axis labels
    date_breaks = "5 year",  # Set breaks for each month
    # breaks = as.Date(c("1991-01-01", "1991-03-01", "1991-05-01"))  # Specify which breaks to label
  )   # Set x-axis breaks for each month

ggsave("宏濬的部分/drinks_prices_plot.jpg", plot = p, width = 10, height = 6, units = "in", dpi = 300)