# Load necessary libraries
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(lubridate)   # For date and time manipulation
# -------------------------------------------
# Step 1: Read and Combine Data Files
# -------------------------------------------

# Define a vector of PM2.5 data file names to be read
file_names <- c('ad_viz_plotval_data.csv',
                'ad_viz_plotval_data (1).csv',
                'ad_viz_plotval_data (2).csv',
                'ad_viz_plotval_data (3).csv',
                'ad_viz_plotval_data (4).csv')

# Use lapply to read each CSV file into a list of data frames
pm25_data_list <- lapply(file_names, function(file) {
  read.csv(file, stringsAsFactors = FALSE)
})

# Combine all data frames in the list into one data frame using bind_rows
pm25_data <- bind_rows(pm25_data_list)

# -------------------------------------------
# Step 2: Select and Rename Relevant Columns
# -------------------------------------------

# Create a new data frame with only specified variables
# Select 'Date', 'Daily.Mean.PM2.5.Concentration', and 'County' columns
pm25_selected <- pm25_data %>%
  select(Date, 'Daily.Mean.PM2.5.Concentration', 'County')

# Rename columns for clarity
pm25_selected <- pm25_selected %>%
  rename(pm2.5 = 'Daily.Mean.PM2.5.Concentration',
         County = 'County')

# -------------------------------------------
# Step 3: Convert Date Column to Date Type and Filter Date Range
# -------------------------------------------

# Convert the 'Date' column to Date type using the correct format (mm/dd/yyyy)
pm25_selected$Date <- as.Date(pm25_selected$Date, format = "%m/%d/%Y")

# Filter the data to include only records from 2018-01-01 to 2022-12-31
pm25_filtered_date <- pm25_selected %>%
  filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2022-12-31"))

# Summarize PM2.5 by County and Date, calculating the mean
pm25_filtered_date <- pm25_filtered_date %>%
  group_by(County, Date) %>%
  summarize(pm2.5 = mean(pm2.5, na.rm = TRUE), .groups = "drop")


# -------------------------------------------
# Step 4: Calculate Missing Data Percentages per County (2018-2022)
# -------------------------------------------

# Calculate missing values percentages per county within the specified date range
missing_values <- pm25_filtered_date %>%
  group_by(County) %>%                                       # Group data by 'County'
  summarise(
    Total_Observations = n(),                                # Total number of observations per county in 2018-2022
    Missing_PM25 = sum(is.na(pm2.5)),                       # Number of missing 'pm2.5' values
    Percent_Missing = (Missing_PM25 / Total_Observations) * 100  # Percentage of missing 'pm2.5' values
  ) %>%
  arrange(desc(Percent_Missing))                            # Arrange counties by descending order of missing percentage

# Display the missing values percentages
print(missing_values)


#Reason why we got rid of the counties


# Define the threshold
threshold <- 1826 * 0.75

# Filter counties with Total_Observations less than 80% of 1826
counties_below_threshold <- missing_values %>%
  filter(Total_Observations < threshold) %>%
  select(County)

# Display the resulting counties
counties_below_threshold


# Extract the list of counties to be removed
counties_to_remove <- counties_below_threshold$County

# Filter out rows with these counties from pm25_filtered_date
pm25_filtered_date <- pm25_filtered_date %>%
  filter(!County %in% counties_to_remove)

# Display the updated data
head(pm25_filtered_date)


# Add new columns for Week, SemiMonth, and Year based on the 'Date' column
pm25_filtered_date <- pm25_filtered_date %>%
  mutate(
    Week = isoweek(Date),  # ISO week number
    SemiMonth = paste(year(Date), month(Date), ifelse(day(Date) <= 15, '1', '2'), sep = '-'),
    Month = month(Date),
    Year = year(Date)      # Year
  )

# Calculate weekly means for each county
weekly_means_filtered <- pm25_filtered_date %>%
  group_by(County, Year, Week) %>%                           # Group by County, Year, and Week
  summarise(Weekly_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')  # Compute mean pm2.5

# Calculate semi-monthly means for each county
semimonthly_means_filtered <- pm25_filtered_date %>%
  group_by(County, SemiMonth) %>%                            # Group by County and SemiMonth
  summarise(SemiMonthly_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')  # Compute mean pm2.5

# Calculate annual means for each county
annual_means_filtered <- pm25_filtered_date %>%
  group_by(County, Year) %>%                                 # Group by County and Year
  summarise(Annual_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')       # Compute mean pm2.5


# Calculate monthly means for each county
monthly_means <- pm25_filtered_date %>%
  group_by(County, Year, Month) %>%                     # Group by County, Year, and Month
  summarise(Monthly_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')  # Compute mean pm2.5

# Calculate overall means for each county
overall_means_filtered <- pm25_filtered_date %>%
  group_by(County) %>%                                       # Group by County
  summarise(Overall_Mean = mean(pm2.5, na.rm = TRUE), .groups = 'drop')      # Compute mean pm2.5

# -------------------------------------------
# Step 8: Read and Process Median Income Data
# -------------------------------------------

# Read in the 'calimedianincome.csv' file which has 'County', 'Code', and 'Median_Income'
median_income <- read.csv('calimedianincome.csv', stringsAsFactors = FALSE)

# Define the Area Median Income (AMI) for California
AMI <- 91905

# # Categorize counties into income groups based on AMI
# median_income <- median_income %>%
#   mutate(
#     Income_Group = case_when(
#       Median_Income >= 0 & Median_Income <= 0.15 * AMI ~ 'g1',    # Acutely low income
#       Median_Income > 0.15 * AMI & Median_Income <= 0.3 * AMI ~ 'g2',  # Extremely low income
#       Median_Income > 0.3 * AMI & Median_Income <= 0.5 * AMI ~ 'g3',   # Very low income
#       Median_Income > 0.5 * AMI & Median_Income <= 0.8 * AMI ~ 'g4',   # Lower income
#       Median_Income > 0.8 * AMI & Median_Income <= 1.2 * AMI ~ 'g5',   # Moderate income
#       Median_Income > 1.2 * AMI ~ 'g6',                                # High income
#       TRUE ~ NA_character_  # Assign NA if none of the conditions match
#     )
#   )


# Clean county names: trim whitespace and convert to uppercase for consistency
median_income <- median_income %>%
  mutate(
    County = trimws(County)
  )

# Calculate AMI multiples
median_income <- median_income %>%
  mutate(
    AMI_Multiple = Median_Income / AMI
  )

# Inspect the updated data frame
head(median_income)

# Decide on the number of income groups (e.g., 4 for quartiles)
num_groups <- 4  # Adjust as needed (e.g., 5 for quintiles)

# Calculate quantiles for AMI_Multiple
quantile_probs <- seq(0, 1, length.out = num_groups + 1)  # e.g., 0, 0.25, 0.5, 0.75, 1 for quartiles
income_quantiles <- quantile(median_income$AMI_Multiple, probs = quantile_probs, na.rm = TRUE)

print(income_quantiles)

# Define income group labels based on quantiles
income_group_labels <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "Upper Income")

# Assign income groups using the quantiles
median_income <- median_income %>%
  mutate(
    Income_Group = case_when(
      AMI_Multiple <= income_quantiles[2] ~ income_group_labels[1],                    # 0% - 25%
      AMI_Multiple > income_quantiles[2] & AMI_Multiple <= income_quantiles[3] ~ income_group_labels[2],  # 25% - 50%
      AMI_Multiple > income_quantiles[3] & AMI_Multiple <= income_quantiles[4] ~ income_group_labels[3],  # 50% - 75%
      AMI_Multiple > income_quantiles[4] ~ income_group_labels[4],                     # 75% - 100%
      TRUE ~ NA_character_  # Assign NA if none of the conditions match
    )
  ) %>%
  select(-AMI_Multiple)

# Summarize the number of counties in each income group
median_income_summary <- median_income %>%
  group_by(Income_Group) %>%
  summarise(
    Median_of_Median_Income = median(Median_Income, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Count))  # Sort by descending count

# Display the summary
print(median_income_summary)

# -------------------------------------------
# Step 9: Read and Process Population Data
# -------------------------------------------

# Define the file paths for the three age-specific population CSV files
population_files <- list(
  under_18 = 'calipopn(under 18).csv',
  age_18_39 = 'calipopn(18-39).csv',
  age_40_over = 'calipopn(40-over).csv'
)

pop_under_18 <- read.csv(population_files$under_18, stringsAsFactors = FALSE)
names(pop_under_18)[names(pop_under_18) == "?..County"] <- "County"     # Rename the column from "?..County" to "County"

pop_18_39 <- read.csv(population_files$age_18_39, stringsAsFactors = FALSE)
names(pop_18_39)[names(pop_18_39) == "?..County"] <- "County"     # Rename the column from "?..County" to "County"

pop_40_over <- read.csv(population_files$age_40_over, stringsAsFactors = FALSE)
names(pop_40_over)[names(pop_40_over) == "?..County"] <- "County"     # Rename the column from "?..County" to "County"

# Function to clean and select relevant columns from each population data frame
clean_population_data <- function(df, age_group_col) {
  df <- df %>%
    mutate(
      County = gsub(" County$", "", County),  # Remove " County" suffix
      County = trimws(County)                 # Trim leading and trailing whitespace
    ) %>%
    select(County, !!sym(age_group_col))      # Select 'County' and the age-specific population column
  return(df)
}

# Clean each population data frame and rename the population columns for clarity
pop_under_18 <- clean_population_data(pop_under_18, "People..Age.Under.18.") %>%
  rename(People_Under_18 = `People..Age.Under.18.`)

pop_18_39 <- clean_population_data(pop_18_39, "People..Age.18.39.") %>%
  rename(People_18_39 = `People..Age.18.39.`)

pop_40_over <- clean_population_data(pop_40_over, "People..Age.40.And.Over.") %>%
  rename(People_40_Over = `People..Age.40.And.Over.`)

# Merge the three population data frames by 'County'
population_data <- pop_under_18 %>%
  left_join(pop_18_39, by = "County") %>%       # Join with 18-39 age group
  left_join(pop_40_over, by = "County") %>%     # Join with 40+ age group
  mutate(
    population = People_Under_18 + People_18_39 + People_40_Over  # Sum across age groups to get total population
  ) %>%
  select(County, population)                     # Select only 'County' and the total 'population' columns

# Optional: Verify the total population matches the provided California total
total_population <- sum(population_data$population[2], na.rm = TRUE)
expected_total <- 8774570 + 12393699 + 18187835  # 39,356,104

if (total_population != expected_total) {
  warning(paste("Total population (", total_population, ") does not match expected total (", expected_total, ")."))
} else {
  print("Total population matches the expected total.")
}

# Display the first few rows of the consolidated population data
print(head(population_data))

# -------------------------------------------
# Step 10: Clean and Ensure Consistency in County Names
# -------------------------------------------

# Convert county names trim whitespace for consistency in PM2.5 data
pm25_filtered_date$County <- trimws(pm25_filtered_date$County)

# Remove the word "COUNTY" from median_income$County
median_income$County <- gsub(" County$", "", median_income$County)

# Convert county names to uppercase and trim whitespace in median_income
median_income$County <- trimws(median_income$County)

# Note: Population data counties have already been cleaned in Step 9

# Identify counties in pm25_selected_filtered that are not in median_income
county_mismatches_filtered_mi <- setdiff(unique(pm25_filtered_date$County), unique(median_income$County))

# Print any mismatches found in median_income
if(length(county_mismatches_filtered_mi) > 0) {
  print("Counties in PM2.5 data not found in median income data:")
  print(county_mismatches_filtered_mi)
} else {
  print("All counties in PM2.5 data are present in median income data.")
}

# Identify counties in pm25_selected_filtered that are not in population_data
county_mismatches_filtered_pop <- setdiff(unique(pm25_filtered_date$County), unique(population_data$County))

# Print any mismatches found in population_data
if(length(county_mismatches_filtered_pop) > 0) {
  print("Counties in PM2.5 data not found in population data:")
  print(county_mismatches_filtered_pop)
} else {
  print("All counties in PM2.5 data are present in population data.")
}

# -------------------------------------------
# Step 11: Merge Data for Disparity Analysis
# -------------------------------------------

# Merge overall PM2.5 means with median income data and population data
analysis_data_filtered <- overall_means_filtered %>%
  left_join(median_income, by = 'County') %>%   # Join on 'County'
  left_join(population_data, by = 'County')     # Add population data

# Check for any NAs after merging
missing_income_groups <- sum(is.na(analysis_data_filtered$Income_Group))
if(missing_income_groups > 0) {
  warning(paste(missing_income_groups, "counties have missing Income_Group after merging."))
} else {
  print("All counties have associated Income_Group.")
}

# -------------------------------------------
# Step 12: Calculate PM2.5 Statistics by Income Group using Population-Weighted Average
# -------------------------------------------

# Calculate weighted average PM2.5 concentration for each income group
pm25_by_income_group_filtered <- analysis_data_filtered %>%
  group_by(Income_Group) %>%                    # Group by income group
  summarise(
    Weighted_Mean_PM25 = sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE),  # Weighted average PM2.5 concentration
    Total_Population = sum(population, na.rm = TRUE)  # Total population in the income group
  ) %>%
  arrange(desc(Income_Group))                          # Arrange by income group

# Print the results to observe disparities
print(pm25_by_income_group_filtered)



# -------------------------------------------
# Step 13:graph PM2.5 disparities by Income Group using Population-Weighted Average
# -------------------------------------------


# Load ggplot2 for plotting
library(ggplot2)

# Create a bar plot showing the weighted average PM2.5 concentration by income group
ggplot(pm25_by_income_group_filtered, aes(x = Income_Group, y = Weighted_Mean_PM25, fill = Income_Group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Disparity in PM2.5 Concentrations by Income Group",
    x = "Income Group",
    y = "Weighted Average PM2.5 Concentration",
    caption = "Data from 2018-2022 PM2.5 Measurements"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")  # Use a color palette for better aesthetics

# Create a scatter plot of Weighted Mean PM2.5 vs Total Population
ggplot(pm25_by_income_group_filtered, aes(x = Total_Population, y = Weighted_Mean_PM25, color = Income_Group)) +
  geom_point(size = 5) +
  geom_text(aes(label = Income_Group), vjust = -1, hjust = 0.5, show.legend = FALSE) +
  labs(
    title = "Weighted Mean PM2.5 vs Total Population by Income Group",
    x = "Total Population",
    y = "Weighted Mean PM2.5 Concentration (μg/m³)",
    color = "Income Group",
    caption = "Data from 2018-2022 PM2.5 Measurements"
  ) +
  theme_minimal()


# Normalize the data for comparison
pm25_normalized <- pm25_by_income_group_filtered %>%
  mutate(
    Norm_PM25 = Weighted_Mean_PM25 / max(Weighted_Mean_PM25),
    Norm_Population = Total_Population / max(Total_Population)
  ) %>%
  select(Income_Group, Norm_PM25, Norm_Population) %>%
  pivot_longer(cols = c(Norm_PM25, Norm_Population),
               names_to = "Metric",
               values_to = "Normalized_Value")

# Create a side-by-side bar plot
ggplot(pm25_normalized, aes(x = Income_Group, y = Normalized_Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Normalized PM2.5 Concentration and Population by Income Group",
    x = "Income Group",
    y = "Normalized Value",
    fill = "Metric",
    caption = "Data from 2018-2022 PM2.5 Measurements"
  ) +
  scale_fill_manual(values = c("Norm_PM25" = "steelblue", "Norm_Population" = "skyblue")) +
  theme_minimal()


# Calculate the percentage of total population for each income group
pm25_by_income_group_filtered <- pm25_by_income_group_filtered %>%
  mutate(Percentage = Total_Population / sum(Total_Population) * 100)

# Create a pie chart with percentage labels
ggplot(pm25_by_income_group_filtered, aes(x = "", y = Total_Population, fill = Income_Group)) +
  geom_bar(width = 1, stat = "identity", color = "gray") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 4) +
  labs(
    title = "Proportion of Total Population by Income Group",
    fill = "Income Group",
    caption = paste(
      "Weighted Mean PM2.5 Concentrations\n",
      paste(pm25_by_income_group_filtered$Income_Group,
            ":",
            round(pm25_by_income_group_filtered$Weighted_Mean_PM25, 1),
            collapse = ", ")
    )
  ) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
# -------------------------------------------
# End of Script
# -------------------------------------------


# -------------------------------------------
# Step: Compute Time-Series PM2.5 Averages by Income Group
# -------------------------------------------

# Merge the PM2.5 dataset with median income categories
pm25_with_income <- pm25_filtered_date %>%
  left_join(median_income, by = "County")  # Merge income data to associate counties with income groups

# Filter out any rows where Income_Group is missing
pm25_with_income <- pm25_with_income %>%
  filter(!is.na(Income_Group))

# -------------------------------------------
# Newly Added: Compute Time-Series PM2.5 Averages by Income Group
# -------------------------------------------

# NOTE: PM2.5 averages are now rounded to 2 decimal places

# Compute daily averages by income group
pm25_daily <- pm25_with_income %>%
  group_by(Date, Income_Group) %>%
  summarise(Daily_Avg_PM25 = round(mean(pm2.5, na.rm = TRUE), 2), .groups = "drop") %>%
  pivot_wider(names_from = Income_Group, values_from = Daily_Avg_PM25) %>%
  mutate(Date = format(Date, "%Y-%m-%d")) %>%
  select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`)

# Compute weekly averages by income group
pm25_weekly <- pm25_with_income %>%
  group_by(Year, Week, Income_Group) %>%
  summarise(Weekly_Avg_PM25 = round(mean(pm2.5, na.rm = TRUE), 2), .groups = "drop") %>%
  mutate(Date = format(as.Date(paste(Year, Week, 1, sep = "-"), format = "%Y-%W-%u"), "%Y-%m-%d")) %>%
  pivot_wider(names_from = Income_Group, values_from = Weekly_Avg_PM25) %>%
  select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`) %>%
  filter(!is.na(Date))


# Compute bi-weekly averages by income group
pm25_biweekly <- pm25_with_income %>%
  mutate(BiWeek = floor((Week - 1) / 2) + 1) %>%  # Define biweekly periods
  group_by(Year, BiWeek, Income_Group) %>%
  summarise(BiWeekly_Avg_PM25 = round(mean(pm2.5, na.rm = TRUE), 2), .groups = "drop") %>%
  mutate(Date = format(as.Date(paste(Year, BiWeek * 2 - 1, 1, sep = "-"), format = "%Y-%W-%u"), "%Y-%m-%d")) %>%
  pivot_wider(names_from = Income_Group, values_from = BiWeekly_Avg_PM25) %>%
  select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`) %>%
  filter(!is.na(Date))

# Compute monthly averages by income group
pm25_monthly <- pm25_with_income %>%
  group_by(Year, Month, Income_Group) %>%
  summarise(Monthly_Avg_PM25 = round(mean(pm2.5, na.rm = TRUE), 2), .groups = "drop") %>%
  mutate(Date = format(as.Date(paste(Year, Month, "01", sep = "-")), "%Y-%m-%d")) %>%
  pivot_wider(names_from = Income_Group, values_from = Monthly_Avg_PM25) %>%
  select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`)

# Display results
print(head(pm25_daily))   # Daily time series
print(head(pm25_weekly))  # Weekly time series
print(head(pm25_biweekly)) # Bi-weekly time series
print(head(pm25_monthly))  # Monthly time series



# -------------------------------------------
# Compute Coefficient of Divergence
# -------------------------------------------

compute_cod <- function(df) {
  df_long <- pivot_longer(df, cols = -Date, names_to = "Income_Group", values_to = "PM25")
  mean_vals <- df_long %>% group_by(Income_Group) %>% summarise(mean_pm25 = mean(PM25, na.rm = TRUE))
  cod_values <- df_long %>% left_join(mean_vals, by = "Income_Group") %>%
    group_by(Date) %>%
    summarise(COD = sqrt(sum(((PM25 - mean_pm25) / (PM25 + mean_pm25))^2, na.rm = TRUE) / length(PM25)))
  return(cod_values)
}

pm25_cod <- compute_cod(pm25_daily)
print(head(pm25_cod))

# -------------------------------------------
# Compute Pearson's Correlation Coefficient
# -------------------------------------------

cor_matrix <- cor(pm25_daily[,-1], use = "complete.obs", method = "pearson")
print(cor_matrix)

# -------------------------------------------
# Perform ANOVA on PM2.5 across Income Groups
# -------------------------------------------

anova_result <- aov(Daily_Avg_PM25 ~ Income_Group, data = pivot_longer(pm25_daily, cols = -Date, names_to = "Income_Group", values_to = "Daily_Avg_PM25"))
summary(anova_result)









# -------------------------------------------
# Step: Compute Time-Series PM2.5 Averages by Income Group
# -------------------------------------------

# Merge the PM2.5 dataset with median income categories and population data
pm25_compute <- pm25_filtered_date %>%
  left_join(median_income, by = "County") %>%  # Merge income data to associate counties with income groups
  left_join(population_data, by = "County")  # Merge population data for weighting

# Filter out any rows where Income_Group or population is missing
pm25_compute <- pm25_compute %>%
  filter(!is.na(Income_Group) & !is.na(population))

# -------------------------------------------
# Newly Added: Compute Population-Weighted PM2.5 Averages by Income Group
# -------------------------------------------

# -------------------------------------------
# NOTE: Function Explanation
# -------------------------------------------
# The function `compute_weighted_pm25` calculates the population-weighted PM2.5 average for a given time period.
# It groups the data by the specified time variable (e.g., Date, Week, Month) and income group.
# The PM2.5 values are weighted by the population of each county to better represent the overall exposure.
# The result is a dataset where each row represents a specific time period,
# and each column corresponds to an income group's weighted PM2.5 average.

# Function to compute population-weighted PM2.5 averages for a given time period
compute_weighted_pm25 <- function(df, time_var) {
  df %>%
    group_by(!!sym(time_var), Income_Group) %>%  # Group by the specified time variable and income group
    summarise(
      # Compute population-weighted PM2.5 average, rounding to 2 decimal places
      Weighted_Avg_PM25 = round(sum(pm2.5 * population, na.rm = TRUE) / sum(population, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Income_Group, values_from = Weighted_Avg_PM25) %>%  # Reshape the data to have income groups as columns
    mutate(Date = as.character(!!sym(time_var))) %>%  # Convert the time variable to a date format for consistency
    select(Date, `Low Income`, `Lower Middle Income`, `Upper Middle Income`, `Upper Income`) %>%  # Select relevant columns
    filter(!is.na(Date))  # Remove rows with missing Date values
}

# Compute population-weighted daily PM2.5 averages
pm25_weighted_daily <- compute_weighted_pm25(pm25_compute, "Date")

# Compute population-weighted weekly PM2.5 averages
pm25_weighted_weekly <- compute_weighted_pm25(pm25_compute, "Week")

# Compute population-weighted bi-weekly PM2.5 averages
pm25_weighted_biweekly <- pm25_compute %>%
  mutate(BiWeek = floor((Week - 1) / 2) + 1) %>%  # Define biweekly periods
  compute_weighted_pm25("BiWeek")

# Compute population-weighted monthly PM2.5 averages
pm25_weighted_monthly <- compute_weighted_pm25(pm25_compute, "Month")

# -------------------------------------------
# Display results
# -------------------------------------------

print(head(pm25_weighted_daily))   # Daily population-weighted PM2.5
print(head(pm25_weighted_weekly))  # Weekly population-weighted PM2.5
print(head(pm25_weighted_biweekly)) # Bi-weekly population-weighted PM2.5
print(head(pm25_weighted_monthly))  # Monthly population-weighted PM2.5

#Empirical Data
#explain dataset and what we used and where we got it
#what we clean up

#Analysis
#how we grouped the income
# -cod
# -R^2
# anova
# -Concentration Index
# 
# group county % below poverty

# DOING Ozone and NO2