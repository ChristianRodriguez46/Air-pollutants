# =============================================================================
# Integrated Analysis of PM2.5 Exposure and Socioeconomic Disparities
# =============================================================================
# This script reads and processes PM2.5 data, median income, and population 
# data. It then calculates county‐level summaries, aggregates the data by 
# income groups, and performs additional analyses including time-series 
# comparisons, coefficient of divergence (COD), Pearson's correlations, ANOVA, 
# and the Concentration Index (CI) for PM2.5 exposure.
# =============================================================================

# -----------------------------------------------------------------------------
# Section 0: Load Required Libraries
# -----------------------------------------------------------------------------
library(dplyr)       # Data manipulation
library(tidyr)       # Data tidying and reshaping
library(lubridate)   # Date and time manipulation
library(ggplot2)     # Plotting
library(reshape2)    # Reshaping data (e.g., melt)

# -----------------------------------------------------------------------------
# Section 1: Read and Combine PM2.5 Data Files
# -----------------------------------------------------------------------------
file_names <- c('ad_viz_plotval_data.csv',
                'ad_viz_plotval_data (1).csv',
                'ad_viz_plotval_data (2).csv',
                'ad_viz_plotval_data (3).csv',
                'ad_viz_plotval_data (4).csv')

# Read each CSV into a list and combine into one data frame
pm25_data_list <- lapply(file_names, function(file) {
  read.csv(file, stringsAsFactors = FALSE)
})
pm25_data <- bind_rows(pm25_data_list)

# -----------------------------------------------------------------------------
# Section 2: Select and Rename Relevant Columns
# -----------------------------------------------------------------------------
pm25_selected <- pm25_data %>%
  select(Date, 'Daily.Mean.PM2.5.Concentration', County) %>%
  rename(pm2.5 = 'Daily.Mean.PM2.5.Concentration')

# -----------------------------------------------------------------------------
# Section 3: Convert Date Column and Filter by Date Range (2018-2022)
# -----------------------------------------------------------------------------
pm25_selected$Date <- as.Date(pm25_selected$Date, format = "%m/%d/%Y")
pm25_filtered_date <- pm25_selected %>%
  filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2022-12-31")) %>%
  group_by(County, Date) %>%
  summarize(pm2.5 = mean(pm2.5, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# Section 4: Calculate Missing Data Percentages and Remove Counties
# -----------------------------------------------------------------------------
missing_values <- pm25_filtered_date %>%
  group_by(County) %>%
  summarise(
    Total_Observations = n(),
    Missing_PM25 = sum(is.na(pm2.5)),
    Percent_Missing = (Missing_PM25 / Total_Observations) * 100
  ) %>%
  arrange(desc(Percent_Missing))
print(missing_values)

# Define a threshold (e.g., 75% of 1826 expected observations over 5 years)
threshold <- 1826 * 0.75
counties_below_threshold <- missing_values %>%
  filter(Total_Observations < threshold) %>%
  select(County)
counties_to_remove <- counties_below_threshold$County

pm25_filtered_date <- pm25_filtered_date %>%
  filter(!County %in% counties_to_remove)
head(pm25_filtered_date)

# -----------------------------------------------------------------------------
# Section 5: Create Time Variables and Compute Aggregated PM2.5 Averages
# -----------------------------------------------------------------------------
pm25_filtered_date <- pm25_filtered_date %>%
  mutate(
    Week = isoweek(Date),
    SemiMonth = paste(year(Date), month(Date), ifelse(day(Date) <= 15, '1', '2'), sep = '-'),
    Month = month(Date),
    Year = year(Date)
  )

# Compute various aggregations
weekly_means_filtered <- pm25_filtered_date %>%
  group_by(County, Year, Week) %>%
  summarise(Weekly_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')

semimonthly_means_filtered <- pm25_filtered_date %>%
  group_by(County, SemiMonth) %>%
  summarise(SemiMonthly_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')

monthly_means <- pm25_filtered_date %>%
  group_by(County, Year, Month) %>%
  summarise(Monthly_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')

overall_means_filtered <- pm25_filtered_date %>%
  group_by(County) %>%
  summarise(Overall_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')

# -----------------------------------------------------------------------------
# Section 6: Read and Process Median Income Data
# -----------------------------------------------------------------------------
median_income <- read.csv('calimedianincome.csv', stringsAsFactors = FALSE) %>%
  mutate(County = trimws(County))
AMI <- 91905  # California Area Median Income

median_income <- median_income %>%
  mutate(AMI_Multiple = Median_Income / AMI)

# Decide on the number of income groups (quartiles)
num_groups <- 4
quantile_probs <- seq(0, 1, length.out = num_groups + 1)
income_quantiles <- quantile(median_income$AMI_Multiple, probs = quantile_probs, na.rm = TRUE)
income_group_labels <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "Upper Income")

median_income <- median_income %>%
  mutate(
    Income_Group = case_when(
      AMI_Multiple <= income_quantiles[2] ~ income_group_labels[1],
      AMI_Multiple > income_quantiles[2] & AMI_Multiple <= income_quantiles[3] ~ income_group_labels[2],
      AMI_Multiple > income_quantiles[3] & AMI_Multiple <= income_quantiles[4] ~ income_group_labels[3],
      AMI_Multiple > income_quantiles[4] ~ income_group_labels[4],
      TRUE ~ NA_character_
    )
  ) %>%
  select(-AMI_Multiple)

median_income_summary <- median_income %>%
  group_by(Income_Group) %>%
  summarise(
    Median_of_Median_Income = median(Median_Income, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Count))
print(median_income_summary)

# -----------------------------------------------------------------------------
# Section 7: Read and Process Population Data
# -----------------------------------------------------------------------------
population_files <- list(
  under_18 = 'calipopn(under 18).csv',
  age_18_39 = 'calipopn(18-39).csv',
  age_40_over = 'calipopn(40-over).csv'
)

pop_under_18 <- read.csv(population_files$under_18, stringsAsFactors = FALSE)
names(pop_under_18)[names(pop_under_18) == "?..County"] <- "County"

pop_18_39 <- read.csv(population_files$age_18_39, stringsAsFactors = FALSE)
names(pop_18_39)[names(pop_18_39) == "?..County"] <- "County"

pop_40_over <- read.csv(population_files$age_40_over, stringsAsFactors = FALSE)
names(pop_40_over)[names(pop_40_over) == "?..County"] <- "County"

clean_population_data <- function(df, age_group_col) {
  df %>%
    mutate(
      County = gsub(" County$", "", County),
      County = trimws(County)
    ) %>%
    select(County, !!sym(age_group_col))
}

pop_under_18 <- clean_population_data(pop_under_18, "People..Age.Under.18.") %>%
  rename(People_Under_18 = `People..Age.Under.18.`)
pop_18_39 <- clean_population_data(pop_18_39, "People..Age.18.39.") %>%
  rename(People_18_39 = `People..Age.18.39.`)
pop_40_over <- clean_population_data(pop_40_over, "People..Age.40.And.Over.") %>%
  rename(People_40_Over = `People..Age.40.And.Over.`)

population_data <- pop_under_18 %>%
  left_join(pop_18_39, by = "County") %>%
  left_join(pop_40_over, by = "County") %>%
  mutate(population = People_Under_18 + People_18_39 + People_40_Over) %>%
  select(County, population)
print(head(population_data))

# -----------------------------------------------------------------------------
# Section 8: Clean County Names for Consistency
# -----------------------------------------------------------------------------
pm25_filtered_date$County <- trimws(pm25_filtered_date$County)
median_income$County <- gsub(" County$", "", median_income$County) %>% trimws()

# Check for mismatches
county_mismatches_filtered_mi <- setdiff(unique(pm25_filtered_date$County), unique(median_income$County))
if(length(county_mismatches_filtered_mi) > 0) {
  print("Counties in PM2.5 data not found in median income data:")
  print(county_mismatches_filtered_mi)
} else {
  print("All counties in PM2.5 data are present in median income data.")
}

county_mismatches_filtered_pop <- setdiff(unique(pm25_filtered_date$County), unique(population_data$County))
if(length(county_mismatches_filtered_pop) > 0) {
  print("Counties in PM2.5 data not found in population data:")
  print(county_mismatches_filtered_pop)
} else {
  print("All counties in PM2.5 data are present in population data.")
}

# -----------------------------------------------------------------------------
# Section 9: Merge Data for Disparity Analysis
# -----------------------------------------------------------------------------
analysis_data_filtered <- overall_means_filtered %>%
  left_join(median_income, by = 'County') %>%
  left_join(population_data, by = 'County')

if(sum(is.na(analysis_data_filtered$Income_Group)) > 0) {
  warning("Some counties have missing Income_Group after merging.")
} else {
  print("All counties have associated Income_Group.")
}

# -----------------------------------------------------------------------------
# Section 10: Calculate PM2.5 Statistics by Income Group (Population-Weighted)
# -----------------------------------------------------------------------------
pm25_by_income_group_filtered <- analysis_data_filtered %>%
  group_by(Income_Group) %>%
  summarise(
    Weighted_Mean_PM25 = sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    Total_Population = sum(population, na.rm = TRUE)
  ) %>%
  arrange(desc(Income_Group))
print(pm25_by_income_group_filtered)

# -----------------------------------------------------------------------------
# Section 11: Graph Disparities by Income Group
# -----------------------------------------------------------------------------
# Bar Plot: Weighted Average PM2.5 by Income Group
ggplot(pm25_by_income_group_filtered, aes(x = Income_Group, y = Weighted_Mean_PM25, fill = Income_Group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Disparity in PM2.5 Concentrations by Income Group",
    x = "Income Group",
    y = "Weighted Average PM2.5 (μg/m³)",
    caption = "Data from 2018-2022 PM2.5 Measurements"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

# Scatter Plot: Weighted Mean PM2.5 vs Total Population
ggplot(pm25_by_income_group_filtered, aes(x = Total_Population, y = Weighted_Mean_PM25, color = Income_Group)) +
  geom_point(size = 5) +
  geom_text(aes(label = Income_Group), vjust = -1, hjust = 0.5, show.legend = FALSE) +
  labs(
    title = "Weighted Mean PM2.5 vs Total Population by Income Group",
    x = "Total Population",
    y = "Weighted Mean PM2.5 (μg/m³)",
    color = "Income Group",
    caption = "Data from 2018-2022 PM2.5 Measurements"
  ) +
  theme_minimal()

# Normalized Side-by-Side Bar Plot for Comparison
pm25_normalized <- pm25_by_income_group_filtered %>%
  mutate(
    Norm_PM25 = Weighted_Mean_PM25 / max(Weighted_Mean_PM25),
    Norm_Population = Total_Population / max(Total_Population)
  ) %>%
  select(Income_Group, Norm_PM25, Norm_Population) %>%
  pivot_longer(cols = c(Norm_PM25, Norm_Population),
               names_to = "Metric",
               values_to = "Normalized_Value")

ggplot(pm25_normalized, aes(x = Income_Group, y = Normalized_Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Normalized PM2.5 and Population by Income Group",
    x = "Income Group",
    y = "Normalized Value",
    fill = "Metric",
    caption = "Data from 2018-2022 PM2.5 Measurements"
  ) +
  scale_fill_manual(values = c("Norm_PM25" = "steelblue", "Norm_Population" = "skyblue")) +
  theme_minimal()

# Pie Chart: Proportion of Total Population by Income Group
pm25_by_income_group_filtered <- pm25_by_income_group_filtered %>%
  mutate(Percentage = Total_Population / sum(Total_Population) * 100)

ggplot(pm25_by_income_group_filtered, aes(x = "", y = Total_Population, fill = Income_Group)) +
  geom_bar(width = 1, stat = "identity", color = "gray") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  labs(
    title = "Proportion of Total Population by Income Group",
    fill = "Income Group",
    caption = paste("Weighted Mean PM2.5:",
                    paste(pm25_by_income_group_filtered$Income_Group, 
                          ":", 
                          round(pm25_by_income_group_filtered$Weighted_Mean_PM25, 1),
                          collapse = ", "))
  ) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# -----------------------------------------------------------------------------
# Section 12: Compute Time-Series PM2.5 Averages by Income Group
# -----------------------------------------------------------------------------
# Merge PM2.5 data with income group information
pm25_with_income <- pm25_filtered_date %>%
  left_join(median_income %>% select(County, Income_Group), by = "County") %>%
  filter(!is.na(Income_Group))

# Daily Aggregation
daily_income_avg <- pm25_with_income %>%
  group_by(Income_Group, Date) %>%
  summarize(Daily_Avg_PM25 = round(mean(pm2.5, na.rm = TRUE), 2), .groups = "drop")

# Weekly Aggregation
weekly_income_avg <- daily_income_avg %>%
  mutate(Year = year(Date), Week = isoweek(Date)) %>%
  group_by(Income_Group, Year, Week) %>%
  summarize(Weekly_Avg_PM25 = round(mean(Daily_Avg_PM25, na.rm = TRUE), 2), .groups = "drop")

# Biweekly Aggregation
daily_income_avg <- daily_income_avg %>%
  mutate(Biweek = as.integer((yday(Date) - 1) %/% 14) + 1,
         Year = year(Date))
biweekly_income_avg <- daily_income_avg %>%
  group_by(Income_Group, Year, Biweek) %>%
  summarize(Biweekly_Avg_PM25 = round(mean(Daily_Avg_PM25, na.rm = TRUE), 2), .groups = "drop")

# Monthly Aggregation
monthly_income_avg <- daily_income_avg %>%
  mutate(Month = month(Date)) %>%
  group_by(Income_Group, Year, Month) %>%
  summarize(Monthly_Avg_PM25 = round(mean(Daily_Avg_PM25, na.rm = TRUE), 2), .groups = "drop")

# Display a sample of the daily time series
print(head(daily_income_avg))

# -----------------------------------------------------------------------------
# Section 13: Compute Coefficient of Divergence (COD)
# -----------------------------------------------------------------------------
# Create a summary data frame (one mean value per income group)
income_summary <- pm25_by_income_group_filtered %>%
  select(Income_Group, Weighted_Mean_PM25) %>%
  rename(mean_pm25 = Weighted_Mean_PM25)
print(income_summary)

# Function to calculate the pairwise COD matrix
calculate_cod_matrix <- function(data) {
  n <- nrow(data)
  cod_matrix <- matrix(0, n, n)
  colnames(cod_matrix) <- data$Income_Group
  rownames(cod_matrix) <- data$Income_Group
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      diff <- abs(data$mean_pm25[i] - data$mean_pm25[j])
      sum_ab <- data$mean_pm25[i] + data$mean_pm25[j]
      cod_value <- ifelse(sum_ab == 0, NA, diff / sum_ab)
      cod_matrix[i, j] <- cod_value
      cod_matrix[j, i] <- cod_value
    }
  }
  return(cod_matrix)
}

cod_matrix <- calculate_cod_matrix(income_summary)
print(cod_matrix)

# Reshape for heatmap plotting
cod_df <- melt(cod_matrix)
colnames(cod_df) <- c("Income_Group1", "Income_Group2", "COD")
head(cod_df)

ggplot(cod_df, aes(x = Income_Group1, y = Income_Group2, fill = COD)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey80") +
  labs(
    title = "Coefficient of Divergence Matrix by Income Group",
    x = "Income Group",
    y = "Income Group",
    fill = "COD"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------------------------------
# Section 14: Compute Pearson's Correlation Matrix and Heatmap
# -----------------------------------------------------------------------------
# Pivot daily data into wide format (one column per income group)
daily_wide <- daily_income_avg %>%
  pivot_wider(names_from = Income_Group, values_from = Daily_Avg_PM25)

income_group_cols <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "Upper Income")

cor_matrix <- cor(daily_wide %>% dplyr::select(all_of(income_group_cols)), use = "complete.obs")
print(cor_matrix)

# Reshape and plot correlation matrix heatmap
cor_df <- melt(cor_matrix)
colnames(cor_df) <- c("Income_Group1", "Income_Group2", "Correlation")
head(cor_df)

ggplot(cor_df, aes(x = Income_Group1, y = Income_Group2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name = "Pearson\nCorrelation") +
  labs(
    title = "Pearson's Correlation Matrix among Income Groups",
    x = "Income Group",
    y = "Income Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# -----------------------------------------------------------------------------
# Section 15: Perform ANOVA on PM2.5 by Income Group
# -----------------------------------------------------------------------------
# Daily ANOVA
anova_daily <- aov(Daily_Avg_PM25 ~ Income_Group, data = daily_income_avg)
print(summary(anova_daily))

# anova_daily <- aov(Daily_Avg_CPM25 ~ Income_Group, data = pivot_longer(daily_income_avg, 
#                                                                       cols = -Date, 
#                                                                       names_to = "Income_Group", 
#                                                                       values_to = "Daily_Avg_PM25"))
print("ANOVA Results for Daily PM2.5 by Income Group:")
print(summary(anova_daily))

# Weekly ANOVA
anova_weekly <- aov(Weekly_Avg_PM25 ~ Income_Group, data = weekly_income_avg)
print("ANOVA Results for Weekly PM2.5 by Income Group:")
print(summary(anova_weekly))

# Biweekly ANOVA
anova_biweekly <- aov(Biweekly_Avg_PM25 ~ Income_Group, data = biweekly_income_avg)
print("ANOVA Results for Biweekly PM2.5 by Income Group:")
print(summary(anova_biweekly))

# Monthly ANOVA
anova_monthly <- aov(Monthly_Avg_PM25 ~ Income_Group, data = monthly_income_avg)
print("ANOVA Results for Monthly PM2.5 by Income Group:")
print(summary(anova_monthly))

# -----------------------------------------------------------------------------
# Section 16: Compute Concentration Index (CI) for PM2.5
# -----------------------------------------------------------------------------
# Merge analysis data to include population and median income
analysis_data_filtered <- analysis_data_filtered %>%
  arrange(Median_Income) %>%  # Order counties from poorest to richest
  mutate(
    rank = rank(Median_Income, ties.method = "first"),
    total_count = n(),
    fractional_rank = (rank - 0.5) / total_count
  )

# Compute population-weighted mean PM2.5
mu_pm25 <- with(analysis_data_filtered, 
                sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE))

# Calculate CI: (2/μ) * cov(Overall_Mean, fractional_rank)
CI_PM25 <- 2 * cov(analysis_data_filtered$Overall_Mean,
                   analysis_data_filtered$fractional_rank,
                   use = "complete.obs") / mu_pm25

cat("Population-weighted Mean PM2.5 (mu):", mu_pm25, "\n")
cat("Concentration Index (CI) for PM2.5:", CI_PM25, "\n")
# A negative CI indicates higher PM2.5 exposure among poorer counties.

# -----------------------------------------------------------------------------
# Section 17: Compute Population-Weighted PM2.5 Averages by Income Group (Time Series)
# -----------------------------------------------------------------------------
# Merge PM2.5 data with income and population data
pm25_compute <- pm25_filtered_date %>%
  left_join(median_income, by = "County") %>%
  left_join(population_data, by = "County") %>%
  filter(!is.na(Income_Group) & !is.na(population))

# Function to compute population-weighted PM2.5 average for a given time variable
compute_weighted_pm25 <- function(df, time_var) {
  df %>%
    group_by(!!sym(time_var), Income_Group) %>%
    summarise(
      Weighted_Avg_PM25 = round(sum(pm2.5 * population, na.rm = TRUE) / sum(population, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Income_Group, values_from = Weighted_Avg_PM25) %>%
    mutate(Date = as.character(!!sym(time_var))) %>%
    select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`) %>%
    filter(!is.na(Date))
}

pm25_weighted_daily <- compute_weighted_pm25(pm25_compute, "Date")
pm25_weighted_weekly <- compute_weighted_pm25(pm25_compute, "Week")
pm25_weighted_monthly <- compute_weighted_pm25(pm25_compute, "Month")

# For bi-weekly averages, define a biweekly period then compute
pm25_weighted_biweekly <- pm25_compute %>%
  mutate(BiWeek = floor((Week - 1) / 2) + 1) %>%
  compute_weighted_pm25("BiWeek")

# Display a sample of the weighted time-series data
print(head(pm25_weighted_daily))
print(head(pm25_weighted_weekly))
print(head(pm25_weighted_biweekly))
print(head(pm25_weighted_monthly))

# =============================================================================
# End of Integrated Script
# =============================================================================
