# ============================================================
# Lab | Data Visualization in R
# Dataset: Superstore
# ============================================================

# ------------------------------------------------------------
# 1. Load dataset and libraries
# ------------------------------------------------------------

# Install packages if not already installed
# install.packages(c("tidyverse", "VIM", "ggplot2", "dplyr", "readr", "naniar"))

library(tidyverse)   # includes ggplot2, dplyr, readr, tidyr
library(VIM)         # for missing data visualization
library(naniar)      # optional, for additional missing data plots

# Load the Superstore dataset
# Adjust the path if your CSV is in a different location
superstore <- read_csv("superstore.csv")

# Quick look at the data
head(superstore)
str(superstore)
summary(superstore)

# ------------------------------------------------------------
# 2. Boxplots: Sales and Profit
# ------------------------------------------------------------

# Boxplot for Sales
ggplot(superstore, aes(y = Sales)) +
  geom_boxplot(fill = "steelblue", outlier.color = "red") +
  labs(title = "Boxplot of Sales", y = "Sales") +
  theme_minimal()

# Boxplot for Profit
ggplot(superstore, aes(y = Profit)) +
  geom_boxplot(fill = "darkgreen", outlier.color = "red") +
  labs(title = "Boxplot of Profit", y = "Profit") +
  theme_minimal()

# Summary statistics for both variables
summary(superstore$Sales)
summary(superstore$Profit)

# ------------------------------------------------------------
# 3. Bar Plot: Top 10 Orders by Sales
# ------------------------------------------------------------

top10_sales <- superstore %>%
  arrange(desc(Sales)) %>%
  slice_head(n = 10)

ggplot(top10_sales, aes(x = reorder(`Order ID`, Sales), y = Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Orders by Sales",
       x = "Order ID", y = "Sales") +
  theme_minimal()

# ------------------------------------------------------------
# 4. Heatmap: Missing Data Pattern
# ------------------------------------------------------------

# Visualize missing data pattern using VIM
missing_pattern <- aggr(
  superstore,
  col = c('navyblue', 'red'),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(superstore),
  cex.axis = .7,
  gap = 3,
  ylab = c("Missing data", "Pattern")
)

# Optional: naniar heatmap of missingness
gg_miss_var(superstore)

# ------------------------------------------------------------
# 5. Histogram: Distribution of Profit
# ------------------------------------------------------------

ggplot(superstore, aes(x = Profit)) +
  geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
  labs(title = "Histogram of Profit", x = "Profit", y = "Frequency") +
  theme_minimal()

# ------------------------------------------------------------
# 6. Bar Plots for Aggregated Data
# ------------------------------------------------------------

# Total Sales by Category
sales_by_category <- superstore %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))

ggplot(sales_by_category, aes(x = Category, y = Total_Sales, fill = Category)) +
  geom_col() +
  labs(title = "Total Sales by Category", x = "Category", y = "Total Sales") +
  theme_minimal()

# Total Profit by Category
profit_by_category <- superstore %>%
  group_by(Category) %>%
  summarise(Total_Profit = sum(Profit, na.rm = TRUE))

ggplot(profit_by_category, aes(x = Category, y = Total_Profit, fill = Category)) +
  geom_col() +
  labs(title = "Total Profit by Category", x = "Category", y = "Total Profit") +
  theme_minimal()