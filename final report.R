install.packages("tidyverse")

library(tidyverse)

data <- read.csv("112_ab111_C.csv")

# 確認資料讀取正確
glimpse(data)

# 先整體描述資料有多少欄位，多少筆資料。再進行逐欄描述。
data_info <- list()

# 計算資料框的欄位數量和資料筆數
data_info$num_columns <- ncol(data)
data_info$num_rows <- nrow(data)

# 逐欄描述
column_info <- list()

for (col in colnames(data)) {
  # 描述缺失值情況
  missing_values <- sum(is.na(data[[col]]))
  missing_percentage <- (missing_values / nrow(data)) * 100
  
  # 判斷欄位型別
  if (is.numeric(data[[col]])) {
    # 描述數值型欄位
    column_stats <- list(
      missing_values = missing_values,
      missing_percentage = missing_percentage,
      range = max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE),
      mean = mean(data[[col]], na.rm = TRUE),
      max_value = max(data[[col]], na.rm = TRUE),
      min_value = min(data[[col]], na.rm = TRUE),
      quartiles = quantile(data[[col]], na.rm = TRUE),
      median = median(data[[col]], na.rm = TRUE)
    )
  } else {
    # 描述非數值型間斷資料欄位
    if (length(unique(data[[col]])) < 10) {
      # 低於10類的情況
      class_counts <- table(data[[col]])
      class_proportions <- prop.table(class_counts)
      column_stats <- list(
        missing_values = missing_values,
        missing_percentage = missing_percentage,
        class_counts = as.numeric(class_counts),
        class_proportions = as.numeric(class_proportions)
      )
    } else {
      # 大於或等於10類的情況，只列出類別總數量
      total_categories <- length(unique(data[[col]]))
      column_stats <- list(
        missing_values = missing_values,
        missing_percentage = missing_percentage,
        total_categories = total_categories
      )
    }
  }
  column_info[[col]] <- column_stats
}

# 將資料描述整合到data_info中
data_info$column_info <- column_info

# 檢視資料描述結果
data_info

write.csv(data_info, "112_ab111_C.csv")



library(dplyr)

# 定義要計算總和的洲別
continents <- c("亞洲", "大洋洲", "非洲", "歐洲", "美洲")

# 計算各洲別的學位生_正式修讀學位外國生的總和
continent_sums <- data %>%
  filter(洲別 %in% continents) %>%
  group_by(洲別) %>%
  summarise(總數 = sum(學位生_正式修讀學位外國生, na.rm = TRUE))

# 顯示結果
glimpse(continent_sums)

# 計算所有學位生_正式修讀學位外國生的總數
total_foreign_degree_students <- sum(data$學位生_正式修讀學位外國生, na.rm = TRUE)

# 篩選出洲別為亞洲的資料
asia_data <- filter(data, 洲別 == "亞洲")

# 計算亞洲的學位生_正式修讀學位外國生的總數
asia_foreign_degree_students <- sum(asia_data$學位生_正式修讀學位外國生, na.rm = TRUE)

# 計算比例
percentage <- (asia_foreign_degree_students / total_foreign_degree_students) * 100

# 顯示結果
percentage

