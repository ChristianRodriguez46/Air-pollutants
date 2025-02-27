# =============================================================================
# Integrated Analysis of NO2 Exposure and Socioeconomic Disparities
# =============================================================================
# This script reads and processes NO2 data, median income, and population 
# data. It then calculates county‐level summaries, aggregates the data by 
# income groups, and performs additional analyses including time-series 
# comparisons, coefficient of divergence (COD), Pearson's correlations, ANOVA, 
# and the Concentration Index (CI) for NO2 exposure.
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
# Section 1: Read and Combine NO2 Data Files
# -----------------------------------------------------------------------------
file_names <- c('NO2-2018.csv',
                'NO2-2019.csv',
                'NO2-2020.csv',
                'NO2-2021.csv',
                'NO2-2022.csv')

# Read each CSV into a list and combine into one data frame
NO2_data_list <- lapply(file_names, function(file) {
  read.csv(file, stringsAsFactors = FALSE)
})
NO2_data <- bind_rows(NO2_data_list)

# -----------------------------------------------------------------------------
# Section 2: Select and Rename Relevant Columns
# -----------------------------------------------------------------------------
NO2_selected <- NO2_data %>%
  select(Date, 'Daily.Max.1.hour.NO2.Concentration', County) %>%
  rename(NO2 = 'Daily.Max.1.hour.NO2.Concentration')

# -----------------------------------------------------------------------------
# Section 3: Convert Date Column and Filter by Date Range (2018-2022)
# -----------------------------------------------------------------------------
NO2_selected$Date <- as.Date(NO2_selected$Date, format = "%m/%d/%Y")
NO2_filtered_date <- NO2_selected %>%
  filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2022-12-31")) %>%
  group_by(County, Date) %>%
  summarize(NO2 = mean(NO2, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# Section 4: Calculate Missing Data Percentages and Remove Counties
# -----------------------------------------------------------------------------
missing_values <- NO2_filtered_date %>%
  group_by(County) %>%
  summarise(
    Total_Observations = n(),
    Missing_NO2 = sum(is.na(NO2)),
    Percent_Missing = (Missing_NO2 / Total_Observations) * 100
  ) %>%
  arrange(desc(Percent_Missing))
print(missing_values)

# Define a threshold (e.g., 75% of 1826 expected observations over 5 years)
threshold <- 1826 * 0.75
counties_below_threshold <- missing_values %>%
  filter(Total_Observations < threshold) %>%
  select(County)
counties_to_remove <- counties_below_threshold$County

NO2_filtered_date <- NO2_filtered_date %>%
  filter(!County %in% counties_to_remove)
head(NO2_filtered_date)

# Additionally, to remain consistent with PM2.5, remove these counties:
# "El Dorado", "Humboldt", "Lake", "Napa", and "Shasta"
NO2_filtered_date <- NO2_filtered_date %>%
  filter(!County %in% c("El Dorado", "Humboldt", "Lake", "Napa", "Shasta"))
head(NO2_filtered_date)

# -----------------------------------------------------------------------------
# Section 5: Create Time Variables and Compute Aggregated NO2 Averages
# -----------------------------------------------------------------------------
NO2_filtered_date <- NO2_filtered_date %>%
  mutate(
    Week = isoweek(Date),
    SemiMonth = paste(year(Date), month(Date), ifelse(day(Date) <= 15, '1', '2'), sep = '-'),
    Month = month(Date),
    Year = year(Date)
  )

# Compute various aggregations
weekly_means_filtered <- NO2_filtered_date %>%
  group_by(County, Year, Week) %>%
  summarise(Weekly_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

semimonthly_means_filtered <- NO2_filtered_date %>%
  group_by(County, SemiMonth) %>%
  summarise(SemiMonthly_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

monthly_means <- NO2_filtered_date %>%
  group_by(County, Year, Month) %>%
  summarise(Monthly_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

overall_means_filtered <- NO2_filtered_date %>%
  group_by(County) %>%
  summarise(Overall_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

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
NO2_filtered_date$County <- trimws(NO2_filtered_date$County)
median_income$County <- gsub(" County$", "", median_income$County) %>% trimws()

# Check for mismatches
county_mismatches_filtered_mi <- setdiff(unique(NO2_filtered_date$County), unique(median_income$County))
if(length(county_mismatches_filtered_mi) > 0) {
  print("Counties in NO2 data not found in median income data:")
  print(county_mismatches_filtered_mi)
} else {
  print("All counties in NO2 data are present in median income data.")
}

county_mismatches_filtered_pop <- setdiff(unique(NO2_filtered_date$County), unique(population_data$County))
if(length(county_mismatches_filtered_pop) > 0) {
  print("Counties in NO2 data not found in population data:")
  print(county_mismatches_filtered_pop)
} else {
  print("All counties in NO2 data are present in population data.")
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
# Section 10: Calculate NO2 Statistics by Income Group (Population-Weighted)
# -----------------------------------------------------------------------------
NO2_by_income_group_filtered <- analysis_data_filtered %>%
  group_by(Income_Group) %>%
  summarise(
    Weighted_Mean_NO2 = sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    Total_Population = sum(population, na.rm = TRUE)
  ) %>%
  arrange(desc(Income_Group))
print(NO2_by_income_group_filtered)

# -----------------------------------------------------------------------------
# Section 11: Graph Disparities by Income Group
# -----------------------------------------------------------------------------
# Bar Plot: Weighted Average NO2 by Income Group
ggplot(NO2_by_income_group_filtered, aes(x = Income_Group, y = Weighted_Mean_NO2, fill = Income_Group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Disparity in NO2 Concentrations by Income Group",
    x = "Income Group",
    y = "Weighted Average NO2 (μg/m³)",
    caption = "Data from 2018-2022 NO2 Measurements"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

# Scatter Plot: Weighted Mean NO2 vs Total Population
ggplot(NO2_by_income_group_filtered, aes(x = Total_Population, y = Weighted_Mean_NO2, color = Income_Group)) +
  geom_point(size = 5) +
  geom_text(aes(label = Income_Group), vjust = -1, hjust = 0.5, show.legend = FALSE) +
  labs(
    title = "Weighted Mean NO2 vs Total Population by Income Group",
    x = "Total Population",
    y = "Weighted Mean NO2 (μg/m³)",
    color = "Income Group",
    caption = "Data from 2018-2022 NO2 Measurements"
  ) +
  theme_minimal()

# Normalized Side-by-Side Bar Plot for Comparison
NO2_normalized <- NO2_by_income_group_filtered %>%
  mutate(
    Norm_NO2 = Weighted_Mean_NO2 / max(Weighted_Mean_NO2),
    Norm_Population = Total_Population / max(Total_Population)
  ) %>%
  select(Income_Group, Norm_NO2, Norm_Population) %>%
  pivot_longer(cols = c(Norm_NO2, Norm_Population),
               names_to = "Metric",
               values_to = "Normalized_Value")

ggplot(NO2_normalized, aes(x = Income_Group, y = Normalized_Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Normalized NO2 and Population by Income Group",
    x = "Income Group",
    y = "Normalized Value",
    fill = "Metric",
    caption = "Data from 2018-2022 NO2 Measurements"
  ) +
  scale_fill_manual(values = c("Norm_NO2" = "steelblue", "Norm_Population" = "skyblue")) +
  theme_minimal()

# Pie Chart: Proportion of Total Population by Income Group
NO2_by_income_group_filtered <- NO2_by_income_group_filtered %>%
  mutate(Percentage = Total_Population / sum(Total_Population) * 100)

ggplot(NO2_by_income_group_filtered, aes(x = "", y = Total_Population, fill = Income_Group)) +
  geom_bar(width = 1, stat = "identity", color = "gray") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  labs(
    title = "Proportion of Total Population by Income Group",
    fill = "Income Group",
    caption = paste("Weighted Mean NO2:",
                    paste(NO2_by_income_group_filtered$Income_Group, 
                          ":", 
                          round(NO2_by_income_group_filtered$Weighted_Mean_NO2, 1),
                          collapse = ", "))
  ) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# -----------------------------------------------------------------------------
# Section 12: Compute Time-Series NO2 Averages by Income Group
# -----------------------------------------------------------------------------
# Merge NO2 data with income group information
NO2_with_income <- NO2_filtered_date %>%
  left_join(median_income %>% select(County, Income_Group), by = "County") %>%
  filter(!is.na(Income_Group))

# Daily Aggregation
daily_income_avg <- NO2_with_income %>%
  group_by(Income_Group, Date) %>%
  summarize(Daily_Avg_NO2 = round(mean(NO2, na.rm = TRUE), 2), .groups = "drop")

# Weekly Aggregation
weekly_income_avg <- daily_income_avg %>%
  mutate(Year = year(Date), Week = isoweek(Date)) %>%
  group_by(Income_Group, Year, Week) %>%
  summarize(Weekly_Avg_NO2 = round(mean(Daily_Avg_NO2, na.rm = TRUE), 2), .groups = "drop")

# Biweekly Aggregation
daily_income_avg <- daily_income_avg %>%
  mutate(Biweek = as.integer((yday(Date) - 1) %/% 14) + 1,
         Year = year(Date))
biweekly_income_avg <- daily_income_avg %>%
  group_by(Income_Group, Year, Biweek) %>%
  summarize(Biweekly_Avg_NO2 = round(mean(Daily_Avg_NO2, na.rm = TRUE), 2), .groups = "drop")

# Monthly Aggregation
monthly_income_avg <- daily_income_avg %>%
  mutate(Month = month(Date)) %>%
  group_by(Income_Group, Year, Month) %>%
  summarize(Monthly_Avg_NO2 = round(mean(Daily_Avg_NO2, na.rm = TRUE), 2), .groups = "drop")

# Display a sample of the daily time series
print(head(daily_income_avg))

# -----------------------------------------------------------------------------
# Section 13: Compute Coefficient of Divergence (COD)
# -----------------------------------------------------------------------------
# Create a summary data frame (one mean value per income group)
income_summary <- NO2_by_income_group_filtered %>%
  select(Income_Group, Weighted_Mean_NO2) %>%
  rename(mean_NO2 = Weighted_Mean_NO2)
print(income_summary)

# Function to calculate the pairwise COD matrix
calculate_cod_matrix <- function(data) {
  n <- nrow(data)
  cod_matrix <- matrix(0, n, n)
  colnames(cod_matrix) <- data$Income_Group
  rownames(cod_matrix) <- data$Income_Group
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      diff <- abs(data$mean_NO2[i] - data$mean_NO2[j])
      sum_ab <- data$mean_NO2[i] + data$mean_NO2[j]
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
  pivot_wider(names_from = Income_Group, values_from = Daily_Avg_NO2)

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
# Section 15: Perform ANOVA on NO2 by Income Group
# -----------------------------------------------------------------------------
# Daily ANOVA
anova_daily <- aov(Daily_Avg_NO2 ~ Income_Group, data = daily_income_avg)
print(summary(anova_daily))

# anova_daily <- aov(Daily_Avg_CNO2 ~ Income_Group, data = pivot_longer(daily_income_avg, 
#                                                                       cols = -Date, 
#                                                                       names_to = "Income_Group", 
#                                                                       values_to = "Daily_Avg_NO2"))
print("ANOVA Results for Daily NO2 by Income Group:")
print(summary(anova_daily))

# Weekly ANOVA
anova_weekly <- aov(Weekly_Avg_NO2 ~ Income_Group, data = weekly_income_avg)
print("ANOVA Results for Weekly NO2 by Income Group:")
print(summary(anova_weekly))

# Biweekly ANOVA
anova_biweekly <- aov(Biweekly_Avg_NO2 ~ Income_Group, data = biweekly_income_avg)
print("ANOVA Results for Biweekly NO2 by Income Group:")
print(summary(anova_biweekly))

# Monthly ANOVA
anova_monthly <- aov(Monthly_Avg_NO2 ~ Income_Group, data = monthly_income_avg)
print("ANOVA Results for Monthly NO2 by Income Group:")
print(summary(anova_monthly))

# -----------------------------------------------------------------------------
# Section 16: Compute Concentration Index (CI) for NO2
# -----------------------------------------------------------------------------
# Merge analysis data to include population and median income
analysis_data_filtered <- analysis_data_filtered %>%
  arrange(Median_Income) %>%  # Order counties from poorest to richest
  mutate(
    rank = rank(Median_Income, ties.method = "first"),
    total_count = n(),
    fractional_rank = (rank - 0.5) / total_count
  )

# Compute population-weighted mean NO2
mu_NO2 <- with(analysis_data_filtered, 
                sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE))

# Calculate CI: (2/μ) * cov(Overall_Mean, fractional_rank)
CI_NO2 <- 2 * cov(analysis_data_filtered$Overall_Mean,
                   analysis_data_filtered$fractional_rank,
                   use = "complete.obs") / mu_NO2

cat("Population-weighted Mean NO2 (mu):", mu_NO2, "\n")
cat("Concentration Index (CI) for NO2:", CI_NO2, "\n")
# A negative CI indicates higher NO2 exposure among poorer counties.

# -----------------------------------------------------------------------------
# Section 17: Compute Population-Weighted NO2 Averages by Income Group (Time Series)
# -----------------------------------------------------------------------------
# Merge NO2 data with income and population data
NO2_compute <- NO2_filtered_date %>%
  left_join(median_income, by = "County") %>%
  left_join(population_data, by = "County") %>%
  filter(!is.na(Income_Group) & !is.na(population))

# Function to compute population-weighted NO2 average for a given time variable
compute_weighted_NO2 <- function(df, time_var) {
  df %>%
    group_by(!!sym(time_var), Income_Group) %>%
    summarise(
      Weighted_Avg_NO2 = round(sum(NO2 * population, na.rm = TRUE) / sum(population, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Income_Group, values_from = Weighted_Avg_NO2) %>%
    mutate(Date = as.character(!!sym(time_var))) %>%
    select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`) %>%
    filter(!is.na(Date))
}

NO2_weighted_daily <- compute_weighted_NO2(NO2_compute, "Date")
NO2_weighted_weekly <- compute_weighted_NO2(NO2_compute, "Week")
NO2_weighted_monthly <- compute_weighted_NO2(NO2_compute, "Month")

# For bi-weekly averages, define a biweekly period then compute
NO2_weighted_biweekly <- NO2_compute %>%
  mutate(BiWeek = floor((Week - 1) / 2) + 1) %>%
  compute_weighted_NO2("BiWeek")

# Display a sample of the weighted time-series data
print(head(NO2_weighted_daily))
print(head(NO2_weighted_weekly))
print(head(NO2_weighted_biweekly))
print(head(NO2_weighted_monthly))

# =============================================================================
# Methodology:
# =============================================================================
# In this analysis, NO2 exposure disparities across counties are assessed by 
# integrating air quality measurements with socioeconomic and population data.
#
# The approach is as follows:
#
# 1. NO2 data are read from multiple CSV files, combined, and cleaned by selecting 
#    relevant columns and converting date strings into Date objects.
#
# 2. Data are filtered to include records from 2018 to 2022. Missing data percentages 
#    per county are calculated, and counties with insufficient observations (less than 
#    75% of 1826 expected days) are removed. Additionally, to remain consistent with 
#    the PM2.5 analysis, the counties "El Dorado", "Humboldt", "Lake", "Napa", and 
#    "Shasta" were removed.
#
# 3. Time variables (Week, SemiMonth, Month, Year) are created, and NO2 concentrations 
#    are aggregated at various temporal scales (weekly, semi-monthly, monthly, and overall).
#
# 4. Median income data are processed to assign counties into income groups based on 
#    quartiles, and population data are merged to obtain county-level totals.
#
# 5. The NO2 data are merged with the socioeconomic data to compute population-weighted 
#    NO2 averages by income group. Additional analyses include:
#    - Graphical comparisons of NO2 exposure and population distribution across income groups.
#    - Time-series analyses to capture temporal variations in NO2 exposure by income group.
#    - Calculation of the Coefficient of Divergence (COD) to assess pairwise disparities.
#    - Pearson's correlation analysis among income groups.
#    - ANOVA tests to evaluate statistical differences in NO2 exposure across income groups.
#    - Calculation of the Concentration Index (CI) to quantify the inequality in NO2 exposure.
#
# This comprehensive framework provides insights into both the spatial and temporal 
# disparities in NO2 exposure in relation to socioeconomic status.
#
# =============================================================================

# calc avg exposure for each income group
# 
# concentration index (read more)
#   - sort counties by income (done)
#   -compute fractional rank()