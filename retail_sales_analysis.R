
# ============================================================
# PROJECT: RETAIL SALES ANALYSIS USING R
# Description: Analysis of retail sales, profit and products
# ============================================================


# 0. SET PROJECT WORKING DIRECTORY

project_dir <- "C:/Users/dell/Downloads/Retail_Sales_Analysis_R_Project/Retail_Sales_Analysis_R_Project"

setwd(project_dir)

cat("Working Directory:", getwd(), "\n")


# 1. INSTALL AND LOAD REQUIRED PACKAGES

packages <- c(
  "ggplot2", "dplyr", "lubridate",
  "readr", "scales"
)

new_packages <- packages[
  !packages %in% installed.packages()[, "Package"]
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(scales)


# 2. CREATE OUTPUT FOLDER

if (!dir.exists("outputs")) {
  dir.create("outputs")
}


# 3. IMPORT DATASET

dataset_path <- file.path("data", "retail_sales.csv")

# Check whether the dataset exists
if (!file.exists(dataset_path)) {
  stop(
    paste(
      "Dataset not found at:",
      file.path(project_dir, dataset_path),
      "\nPlease check your project folder and data folder."
    )
  )
}

# Import CSV file
sales_data <- read_csv(
  dataset_path,
  show_col_types = FALSE
)

cat("\n========== DATASET INFORMATION ==========\n")

print(head(sales_data))
str(sales_data)
summary(sales_data)

cat("\nDataset Dimensions:\n")
print(dim(sales_data))


# 4. DATA PREPROCESSING

sales_data$Order_Date <- as.Date(sales_data$Order_Date)

# Remove duplicate records
sales_data <- distinct(sales_data)

# Remove rows with missing important values
sales_data <- sales_data %>%
  filter(
    !is.na(Order_ID),
    !is.na(Order_Date),
    !is.na(Sales),
    !is.na(Profit)
  )

cat("\n========== CLEANED DATASET ==========\n")
print(head(sales_data))

cat("\nMissing Values:\n")
print(colSums(is.na(sales_data)))


# 5. KEY PERFORMANCE INDICATORS (KPIs)

total_sales <- sum(sales_data$Sales, na.rm = TRUE)

total_profit <- sum(sales_data$Profit, na.rm = TRUE)

total_orders <- n_distinct(sales_data$Order_ID)

total_quantity <- sum(sales_data$Quantity, na.rm = TRUE)

average_order_value <- ifelse(
  total_orders > 0,
  total_sales / total_orders,
  0
)

profit_margin <- ifelse(
  total_sales != 0,
  (total_profit / total_sales) * 100,
  0
)

cat("\n========== RETAIL SALES KPI REPORT ==========\n")

cat("Total Sales: ", round(total_sales, 2), "\n")
cat("Total Profit: ", round(total_profit, 2), "\n")
cat("Total Orders: ", total_orders, "\n")
cat("Total Quantity Sold: ", total_quantity, "\n")

cat(
  "Average Order Value: ",
  round(average_order_value, 2), "\n"
)

cat(
  "Profit Margin: ",
  round(profit_margin, 2), "%\n"
)


# 6. CATEGORY-WISE SALES ANALYSIS

category_sales <- sales_data %>%
  group_by(Category) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n========== CATEGORY-WISE SALES ==========\n")
print(category_sales)

category_plot <- ggplot(
  category_sales,
  aes(x = Category, y = Total_Sales, fill = Category)
) +
  geom_col() +
  labs(
    title = "Category-wise Sales Analysis",
    x = "Product Category",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(category_plot)

ggsave(
  "outputs/category_sales.png",
  plot = category_plot,
  width = 8,
  height = 5
)


# 7. REGION-WISE SALES ANALYSIS

region_sales <- sales_data %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n========== REGION-WISE SALES ==========\n")
print(region_sales)

region_plot <- ggplot(
  region_sales,
  aes(x = Region, y = Total_Sales, fill = Region)
) +
  geom_col() +
  labs(
    title = "Region-wise Sales Analysis",
    x = "Region",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(region_plot)

ggsave(
  "outputs/region_sales.png",
  plot = region_plot,
  width = 8,
  height = 5
)


# 8. MONTHLY SALES TREND ANALYSIS

monthly_sales <- sales_data %>%
  mutate(Month = floor_date(Order_Date, "month")) %>%
  group_by(Month) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n========== MONTHLY SALES ==========\n")
print(monthly_sales)

monthly_plot <- ggplot(
  monthly_sales,
  aes(x = Month, y = Total_Sales)
) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(size = 2, color = "red") +
  labs(
    title = "Monthly Sales Trend",
    x = "Month",
    y = "Total Sales"
  ) +
  theme_minimal()

print(monthly_plot)

ggsave(
  "outputs/monthly_sales.png",
  plot = monthly_plot,
  width = 8,
  height = 5
)


# 9. TOP 10 PRODUCTS ANALYSIS

top_products <- sales_data %>%
  group_by(Product) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10)

cat("\n========== TOP 10 PRODUCTS ==========\n")
print(top_products)

top_products_plot <- ggplot(
  top_products,
  aes(
    x = reorder(Product, Total_Sales),
    y = Total_Sales
  )
) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 Products by Sales",
    x = "Product",
    y = "Total Sales"
  ) +
  theme_minimal()

print(top_products_plot)

ggsave(
  "outputs/top_10_products.png",
  plot = top_products_plot,
  width = 8,
  height = 5
)


# 10. DISCOUNT VERSUS PROFIT ANALYSIS

discount_plot <- ggplot(
  sales_data,
  aes(x = Discount, y = Profit)
) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  labs(
    title = "Discount versus Profit Analysis",
    x = "Discount",
    y = "Profit"
  ) +
  theme_minimal()

print(discount_plot)

ggsave(
  "outputs/discount_vs_profit.png",
  plot = discount_plot,
  width = 8,
  height = 5
)


# 11. CORRELATION ANALYSIS

correlation <- cor(
  sales_data$Sales,
  sales_data$Profit,
  use = "complete.obs"
)

cat("\n========== CORRELATION ANALYSIS ==========\n")

cat(
  "Correlation between Sales and Profit: ",
  round(correlation, 4),
  "\n"
)


# 12. LINEAR REGRESSION ANALYSIS

model <- lm(
  Profit ~ Sales,
  data = sales_data
)

cat("\n========== LINEAR REGRESSION RESULTS ==========\n")

print(summary(model))

regression_plot <- ggplot(
  sales_data,
  aes(x = Sales, y = Profit)
) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(
    method = "lm",
    color = "red"
  ) +
  labs(
    title = "Linear Regression: Sales vs Profit",
    x = "Sales",
    y = "Profit"
  ) +
  theme_minimal()

print(regression_plot)

ggsave(
  "outputs/regression_plot.png",
  plot = regression_plot,
  width = 8,
  height = 5
)


# 13. EXPORT ANALYSIS RESULTS

write_csv(
  category_sales,
  "outputs/category_sales.csv"
)

write_csv(
  region_sales,
  "outputs/region_sales.csv"
)

write_csv(
  monthly_sales,
  "outputs/monthly_sales.csv"
)

write_csv(
  top_products,
  "outputs/top_10_products.csv"
)


# 14. EXPORT KPI RESULTS

kpi_results <- data.frame(
  Metric = c(
    "Total Sales",
    "Total Profit",
    "Total Orders",
    "Total Quantity",
    "Average Order Value",
    "Profit Margin"
  ),
  Value = c(
    total_sales,
    total_profit,
    total_orders,
    total_quantity,
    average_order_value,
    profit_margin
  )
)

write_csv(
  kpi_results,
  "outputs/kpi_results.csv"
)


# 15. EXPORT REGRESSION SUMMARY

capture.output(
  summary(model),
  file = "outputs/regression_summary.txt"
)


# 16. FINAL OUTPUT

cat("\n============================================\n")
cat("RETAIL SALES ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("============================================\n")

cat("\nGenerated Files:\n")
cat("1. Category Sales Chart\n")
cat("2. Region Sales Chart\n")
cat("3. Monthly Sales Chart\n")
cat("4. Top 10 Products Chart\n")
cat("5. Discount vs Profit Chart\n")
cat("6. Regression Chart\n")
cat("7. KPI Results CSV\n")
cat("8. Analysis Summary CSV Files\n")
cat("9. Regression Summary Text File\n")

cat("\nAll results are saved in the outputs folder.\n")
cat("\nProject Location:\n")
cat(getwd(), "\n")
