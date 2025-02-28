# =============================================================================
# Integrated Analysis of AQI Exposure and Socioeconomic Disparities
# =============================================================================
# This script reads and processes AQI data, median income, and population 
# data. It then calculates county‐level summaries, aggregates the data by 
# income groups, and performs additional analyses including time-series 
# comparisons, coefficient of divergence (COD), Pearson's correlations, ANOVA, 
# and the Concentration Index (CI) for AQI exposure.
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
# Section 1: Read and Combine AQI Data Files
# -----------------------------------------------------------------------------
file_names <- c('AQI-2018.csv',
                'AQI-2019.csv',
                'AQI-2020.csv',
                'AQI-2021.csv',
                'AQI-2022.csv')

# Read each CSV into a list and combine into one data frame
AQI_data_list <- lapply(file_names, function(file) {
  read.csv(file, stringsAsFactors = FALSE)
})
AQI_data <- bind_rows(AQI_data_list)

# -----------------------------------------------------------------------------
# Section 2: Select and Rename Relevant Columns
# -----------------------------------------------------------------------------
AQI_selected <- AQI_data %>%
  select(Date, 'AQI', County) %>%
  rename(AQI = 'AQI')

# -----------------------------------------------------------------------------
# Section 3: Convert Date Column and Filter by Date Range (2018-2022)
# -----------------------------------------------------------------------------
AQI_selected$Date <- as.Date(AQI_selected$Date, format = "%m/%d/%Y")
AQI_filtered_date <- AQI_selected %>%
  filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2022-12-31")) %>%
  group_by(County, Date) %>%
  summarize(AQI = mean(AQI, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# Section 4: Calculate Missing Data Percentages and Remove Counties
# -----------------------------------------------------------------------------
missing_values <- AQI_filtered_date %>%
  group_by(County) %>%
  summarise(
    Total_Observations = n(),
    Missing_AQI = sum(is.na(AQI)),
    Percent_Missing = (Missing_AQI / Total_Observations) * 100
  ) %>%
  arrange(desc(Percent_Missing))
print(missing_values)

# Define a threshold (e.g., 75% of 1826 expected observations over 5 years)
threshold <- 1826 * 0.75
counties_below_threshold <- missing_values %>%
  filter(Total_Observations < threshold) %>%
  select(County)
counties_to_remove <- counties_below_threshold$County

AQI_filtered_date <- AQI_filtered_date %>%
  filter(!County %in% counties_to_remove)
head(AQI_filtered_date)

# Additionally, to remain consistent with PM2.5, remove these counties:
# "El Dorado", "Humboldt", "Lake", "Napa", and "Shasta"
AQI_filtered_date <- AQI_filtered_date %>%
  filter(!County %in% c("El Dorado", "Humboldt", "Lake", "Napa", "Shasta"))
head(AQI_filtered_date)

# -----------------------------------------------------------------------------
# Section 5: Create Time Variables and Compute Aggregated AQI Averages
# -----------------------------------------------------------------------------
AQI_filtered_date <- AQI_filtered_date %>%
  mutate(
    Week = isoweek(Date),
    SemiMonth = paste(year(Date), month(Date), ifelse(day(Date) <= 15, '1', '2'), sep = '-'),
    Month = month(Date),
    Year = year(Date)
  )

# Compute various aggregations
weekly_means_filtered <- AQI_filtered_date %>%
  group_by(County, Year, Week) %>%
  summarise(Weekly_Mean = mean(AQI, na.rm = TRUE), .groups = 'drop')

semimonthly_means_filtered <- AQI_filtered_date %>%
  group_by(County, SemiMonth) %>%
  summarise(SemiMonthly_Mean = mean(AQI, na.rm = TRUE), .groups = 'drop')

monthly_means <- AQI_filtered_date %>%
  group_by(County, Year, Month) %>%
  summarise(Monthly_Mean = mean(AQI, na.rm = TRUE), .groups = 'drop')

overall_means_filtered <- AQI_filtered_date %>%
  group_by(County) %>%
  summarise(Overall_Mean = mean(AQI, na.rm = TRUE), .groups = 'drop')

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
AQI_filtered_date$County <- trimws(AQI_filtered_date$County)
median_income$County <- gsub(" County$", "", median_income$County) %>% trimws()

# Check for mismatches
county_mismatches_filtered_mi <- setdiff(unique(AQI_filtered_date$County), unique(median_income$County))
if(length(county_mismatches_filtered_mi) > 0) {
  print("Counties in AQI data not found in median income data:")
  print(county_mismatches_filtered_mi)
} else {
  print("All counties in AQI data are present in median income data.")
}

county_mismatches_filtered_pop <- setdiff(unique(AQI_filtered_date$County), unique(population_data$County))
if(length(county_mismatches_filtered_pop) > 0) {
  print("Counties in AQI data not found in population data:")
  print(county_mismatches_filtered_pop)
} else {
  print("All counties in AQI data are present in population data.")
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
# Section 10: Calculate AQI Statistics by Income Group (Population-Weighted)
# -----------------------------------------------------------------------------
AQI_by_income_group_filtered <- analysis_data_filtered %>%
  group_by(Income_Group) %>%
  summarise(
    Weighted_Mean_AQI = sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    Total_Population = sum(population, na.rm = TRUE)
  ) %>%
  arrange(desc(Income_Group))
print(AQI_by_income_group_filtered)

# -----------------------------------------------------------------------------
# Section 11: Graph Disparities by Income Group
# -----------------------------------------------------------------------------
# Bar Plot: Weighted Average AQI by Income Group
ggplot(AQI_by_income_group_filtered, aes(x = Income_Group, y = Weighted_Mean_AQI, fill = Income_Group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Disparity in AQI Concentrations by Income Group",
    x = "Income Group",
    y = "Weighted Average AQI (μg/m³)",
    caption = "Data from 2018-2022 AQI Measurements"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

# Scatter Plot: Weighted Mean AQI vs Total Population
ggplot(AQI_by_income_group_filtered, aes(x = Total_Population, y = Weighted_Mean_AQI, color = Income_Group)) +
  geom_point(size = 5) +
  geom_text(aes(label = Income_Group), vjust = -1, hjust = 0.5, show.legend = FALSE) +
  labs(
    title = "Weighted Mean AQI vs Total Population by Income Group",
    x = "Total Population",
    y = "Weighted Mean AQI (μg/m³)",
    color = "Income Group",
    caption = "Data from 2018-2022 AQI Measurements"
  ) +
  theme_minimal()

# Normalized Side-by-Side Bar Plot for Comparison
AQI_normalized <- AQI_by_income_group_filtered %>%
  mutate(
    Norm_AQI = Weighted_Mean_AQI / max(Weighted_Mean_AQI),
    Norm_Population = Total_Population / max(Total_Population)
  ) %>%
  select(Income_Group, Norm_AQI, Norm_Population) %>%
  pivot_longer(cols = c(Norm_AQI, Norm_Population),
               names_to = "Metric",
               values_to = "Normalized_Value")

ggplot(AQI_normalized, aes(x = Income_Group, y = Normalized_Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Normalized AQI and Population by Income Group",
    x = "Income Group",
    y = "Normalized Value",
    fill = "Metric",
    caption = "Data from 2018-2022 AQI Measurements"
  ) +
  scale_fill_manual(values = c("Norm_AQI" = "steelblue", "Norm_Population" = "skyblue")) +
  theme_minimal()

# Pie Chart: Proportion of Total Population by Income Group
AQI_by_income_group_filtered <- AQI_by_income_group_filtered %>%
  mutate(Percentage = Total_Population / sum(Total_Population) * 100)

ggplot(AQI_by_income_group_filtered, aes(x = "", y = Total_Population, fill = Income_Group)) +
  geom_bar(width = 1, stat = "identity", color = "gray") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  labs(
    title = "Proportion of Total Population by Income Group",
    fill = "Income Group",
    caption = paste("Weighted Mean AQI:",
                    paste(AQI_by_income_group_filtered$Income_Group, 
                          ":", 
                          round(AQI_by_income_group_filtered$Weighted_Mean_AQI, 1),
                          collapse = ", "))
  ) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# -----------------------------------------------------------------------------
# Section 12: Compute Time-Series AQI Averages by Income Group
# -----------------------------------------------------------------------------
# Merge AQI data with income group information
AQI_with_income <- AQI_filtered_date %>%
  left_join(median_income %>% select(County, Income_Group), by = "County") %>%
  filter(!is.na(Income_Group))

# Daily Aggregation
daily_income_avg <- AQI_with_income %>%
  group_by(Income_Group, Date) %>%
  summarize(Daily_Avg_AQI = round(mean(AQI, na.rm = TRUE), 2), .groups = "drop")

# Weekly Aggregation
weekly_income_avg <- daily_income_avg %>%
  mutate(Year = year(Date), Week = isoweek(Date)) %>%
  group_by(Income_Group, Year, Week) %>%
  summarize(Weekly_Avg_AQI = round(mean(Daily_Avg_AQI, na.rm = TRUE), 2), .groups = "drop")

# Biweekly Aggregation
daily_income_avg <- daily_income_avg %>%
  mutate(Biweek = as.integer((yday(Date) - 1) %/% 14) + 1,
         Year = year(Date))
biweekly_income_avg <- daily_income_avg %>%
  group_by(Income_Group, Year, Biweek) %>%
  summarize(Biweekly_Avg_AQI = round(mean(Daily_Avg_AQI, na.rm = TRUE), 2), .groups = "drop")

# Monthly Aggregation
monthly_income_avg <- daily_income_avg %>%
  mutate(Month = month(Date)) %>%
  group_by(Income_Group, Year, Month) %>%
  summarize(Monthly_Avg_AQI = round(mean(Daily_Avg_AQI, na.rm = TRUE), 2), .groups = "drop")

# Display a sample of the daily time series
print(head(daily_income_avg))

# -----------------------------------------------------------------------------
# Section 13: Compute Coefficient of Divergence (COD)
# -----------------------------------------------------------------------------
# Create a summary data frame (one mean value per income group)
income_summary <- AQI_by_income_group_filtered %>%
  select(Income_Group, Weighted_Mean_AQI) %>%
  rename(mean_AQI = Weighted_Mean_AQI)
print(income_summary)

# Function to calculate the pairwise COD matrix
calculate_cod_matrix <- function(data) {
  n <- nrow(data)
  cod_matrix <- matrix(0, n, n)
  colnames(cod_matrix) <- data$Income_Group
  rownames(cod_matrix) <- data$Income_Group
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      diff <- abs(data$mean_AQI[i] - data$mean_AQI[j])
      sum_ab <- data$mean_AQI[i] + data$mean_AQI[j]
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
  pivot_wider(names_from = Income_Group, values_from = Daily_Avg_AQI)

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
# Section 15: Perform ANOVA on AQI by Income Group
# -----------------------------------------------------------------------------
# Daily ANOVA
anova_daily <- aov(Daily_Avg_AQI ~ Income_Group, data = daily_income_avg)
print(summary(anova_daily))

# anova_daily <- aov(Daily_Avg_CAQI ~ Income_Group, data = pivot_longer(daily_income_avg, 
#                                                                       cols = -Date, 
#                                                                       names_to = "Income_Group", 
#                                                                       values_to = "Daily_Avg_AQI"))
print("ANOVA Results for Daily AQI by Income Group:")
print(summary(anova_daily))

# Weekly ANOVA
anova_weekly <- aov(Weekly_Avg_AQI ~ Income_Group, data = weekly_income_avg)
print("ANOVA Results for Weekly AQI by Income Group:")
print(summary(anova_weekly))

# Biweekly ANOVA
anova_biweekly <- aov(Biweekly_Avg_AQI ~ Income_Group, data = biweekly_income_avg)
print("ANOVA Results for Biweekly AQI by Income Group:")
print(summary(anova_biweekly))

# Monthly ANOVA
anova_monthly <- aov(Monthly_Avg_AQI ~ Income_Group, data = monthly_income_avg)
print("ANOVA Results for Monthly AQI by Income Group:")
print(summary(anova_monthly))

# -----------------------------------------------------------------------------
# Section 16: Compute Concentration Index (CI) for AQI
# -----------------------------------------------------------------------------
# Merge analysis data to include population and median income
analysis_data_filtered <- analysis_data_filtered %>%
  arrange(Median_Income) %>%  # Order counties from poorest to richest
  mutate(
    rank = rank(Median_Income, ties.method = "first"),
    total_count = n(),
    fractional_rank = (rank - 0.5) / total_count
  )

# Compute population-weighted mean AQI
mu_AQI <- with(analysis_data_filtered, 
               sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE))

# Calculate CI: (2/μ) * cov(Overall_Mean, fractional_rank)
CI_AQI <- 2 * cov(analysis_data_filtered$Overall_Mean,
                  analysis_data_filtered$fractional_rank,
                  use = "complete.obs") / mu_AQI

cat("Population-weighted Mean AQI (mu):", mu_AQI, "\n")
cat("Concentration Index (CI) for AQI:", CI_AQI, "\n")
# A negative CI indicates higher AQI exposure among poorer counties.

# -----------------------------------------------------------------------------
# Section 17: Compute Population-Weighted AQI Averages by Income Group (Time Series)
# -----------------------------------------------------------------------------
# Merge AQI data with income and population data
AQI_compute <- AQI_filtered_date %>%
  left_join(median_income, by = "County") %>%
  left_join(population_data, by = "County") %>%
  filter(!is.na(Income_Group) & !is.na(population))

# Function to compute population-weighted AQI average for a given time variable
compute_weighted_AQI <- function(df, time_var) {
  df %>%
    group_by(!!sym(time_var), Income_Group) %>%
    summarise(
      Weighted_Avg_AQI = round(sum(AQI * population, na.rm = TRUE) / sum(population, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Income_Group, values_from = Weighted_Avg_AQI) %>%
    mutate(Date = as.character(!!sym(time_var))) %>%
    select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`) %>%
    filter(!is.na(Date))
}

AQI_weighted_daily <- compute_weighted_AQI(AQI_compute, "Date")
AQI_weighted_weekly <- compute_weighted_AQI(AQI_compute, "Week")
AQI_weighted_monthly <- compute_weighted_AQI(AQI_compute, "Month")

# For bi-weekly averages, define a biweekly period then compute
AQI_weighted_biweekly <- AQI_compute %>%
  mutate(BiWeek = floor((Week - 1) / 2) + 1) %>%
  compute_weighted_AQI("BiWeek")

# Display a sample of the weighted time-series data
print(head(AQI_weighted_daily))
print(head(AQI_weighted_weekly))
print(head(AQI_weighted_biweekly))
print(head(AQI_weighted_monthly))

# =============================================================================
# Methodology:
# =============================================================================
# In this analysis, AQI exposure disparities across counties are assessed by 
# integrating air quality measurements with socioeconomic and population data.
#
# The approach is as follows:
#
# 1. AQI data are read from multiple CSV files, combined, and cleaned by selecting 
#    relevant columns and converting date strings into Date objects.
#
# 2. Data are filtered to include records from 2018 to 2022. Missing data percentages 
#    per county are calculated, and counties with insufficient observations (less than 
#    75% of 1826 expected days) are removed. Additionally, to remain consistent with 
#    the PM2.5 analysis, the counties "El Dorado", "Humboldt", "Lake", "Napa", and 
#    "Shasta" were removed.
#
# 3. Time variables (Week, SemiMonth, Month, Year) are created, and AQI concentrations 
#    are aggregated at various temporal scales (weekly, semi-monthly, monthly, and overall).
#
# 4. Median income data are processed to assign counties into income groups based on 
#    quartiles, and population data are merged to obtain county-level totals.
#
# 5. The AQI data are merged with the socioeconomic data to compute population-weighted 
#    AQI averages by income group. Additional analyses include:
#    - Graphical comparisons of AQI exposure and population distribution across income groups.
#    - Time-series analyses to capture temporal variations in AQI exposure by income group.
#    - Calculation of the Coefficient of Divergence (COD) to assess pairwise disparities.
#    - Pearson's correlation analysis among income groups.
#    - ANOVA tests to evaluate statistical differences in AQI exposure across income groups.
#    - Calculation of the Concentration Index (CI) to quantify the inequality in AQI exposure.
#
# This comprehensive framework provides insights into both the spatial and temporal 
# disparities in AQI exposure in relation to socioeconomic status.
#
# =============================================================================