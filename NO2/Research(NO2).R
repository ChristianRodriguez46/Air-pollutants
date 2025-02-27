# Load necessary libraries
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(lubridate)   # For date and time manipulation

# -------------------------------------------
# Step 1: Read and Combine Data Files
# -------------------------------------------

# Define a vector of NO2 data file names to be read
file_names <- c('ad_viz_plotval_data(NO2).csv',
                'ad_viz_plotval_data(NO2) (1).csv',
                'ad_viz_plotval_data(NO2) (2).csv',
                'ad_viz_plotval_data(NO2) (3).csv',
                'ad_viz_plotval_data(NO2) (4).csv')

# Use lapply to read each CSV file into a list of data frames
NO2_data_list <- lapply(file_names, function(file) {
  read.csv(file, stringsAsFactors = FALSE)
})

# Combine all data frames in the list into one data frame using bind_rows
NO2_data <- bind_rows(NO2_data_list)

# -------------------------------------------
# Step 2: Select and Rename Relevant Columns
# -------------------------------------------

# Create a new data frame with only specified variables
# Select 'Date', 'Daily.Max.1.hour.NO2.Concentration', and 'County' columns
NO2_selected <- NO2_data %>%
  select(Date, 'Daily.Max.1.hour.NO2.Concentration', 'County')

# Rename columns for clarity
NO2_selected <- NO2_selected %>%
  rename(NO2 = 'Daily.Max.1.hour.NO2.Concentration')

# -------------------------------------------
# Step 3: Convert Date Column to Date Type and Filter Date Range
# -------------------------------------------

# Convert the 'Date' column to Date type using the correct format (mm/dd/yyyy)
NO2_selected$Date <- as.Date(NO2_selected$Date, format = "%m/%d/%Y")

# Filter the data to include only records from 2018-01-01 to 2022-12-31
NO2_filtered_date <- NO2_selected %>%
  filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2022-12-31"))

# Summarize NO2 by County and Date, calculating the mean
NO2_filtered_date <- NO2_filtered_date %>%
  group_by(County, Date) %>%
  summarize(NO2 = mean(NO2, na.rm = TRUE), .groups = "drop")

# -------------------------------------------
# Step 4: Calculate Missing Data Percentages per County (2018-2022)
# -------------------------------------------

# Calculate missing values percentages per county within the specified date range
missing_values <- NO2_filtered_date %>%
  group_by(County) %>%
  summarise(
    Total_Observations = n(),
    Missing_NO2 = sum(is.na(NO2)),
    Percent_Missing = (Missing_NO2 / Total_Observations) * 100
  ) %>%
  arrange(desc(Percent_Missing))

# Display the missing values percentages
print(missing_values)

# -------------------------------------------
# Step 5: Remove Counties with Insufficient Data
# -------------------------------------------

# Define the threshold (e.g., 75% of 1826 days)
threshold <- 1826 * 0.75

# Filter counties with Total_Observations less than the threshold
counties_below_threshold <- missing_values %>%
  filter(Total_Observations < threshold) %>%
  select(County)

# Extract the list of counties to be removed
counties_to_remove <- counties_below_threshold$County

# Filter out rows with these counties from NO2_filtered_date
NO2_filtered_date <- NO2_filtered_date %>%
  filter(!County %in% counties_to_remove)

# -------------------------------------------
# Step 6: Add Time-related Columns
# -------------------------------------------

# Add new columns for Week, SemiMonth, Month, and Year based on the 'Date' column
NO2_filtered_date <- NO2_filtered_date %>%
  mutate(
    Week = isoweek(Date),
    SemiMonth = paste(year(Date), month(Date), ifelse(day(Date) <= 15, '1', '2'), sep = '-'),
    Month = month(Date),
    Year = year(Date)
  )

# -------------------------------------------
# Step 7: Calculate Aggregated Means
# -------------------------------------------

# Calculate weekly means for each county
weekly_means_filtered <- NO2_filtered_date %>%
  group_by(County, Year, Week) %>%
  summarise(Weekly_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

# Calculate semi-monthly means for each county
semimonthly_means_filtered <- NO2_filtered_date %>%
  group_by(County, SemiMonth) %>%
  summarise(SemiMonthly_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

# Calculate monthly means for each county
monthly_means <- NO2_filtered_date %>%
  group_by(County, Year, Month) %>%
  summarise(Monthly_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

# Calculate annual means for each county
annual_means_filtered <- NO2_filtered_date %>%
  group_by(County, Year) %>%
  summarise(Annual_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

# Calculate overall means for each county
overall_means_filtered <- NO2_filtered_date %>%
  group_by(County) %>%
  summarise(Overall_Mean = mean(NO2, na.rm = TRUE), .groups = 'drop')

# -------------------------------------------
# Step 8: Read and Process Median Income Data
# -------------------------------------------

# Read in the 'calimedianincome.csv' file which has 'County', 'Code', and 'Median_Income'
median_income <- read.csv('calimedianincome.csv', stringsAsFactors = FALSE)

# Define the Area Median Income (AMI) for California
AMI <- 91905

# Clean county names: trim whitespace and remove " County" suffix
median_income <- median_income %>%
  mutate(
    County = gsub(" County$", "", trimws(County))
  )

# Calculate AMI multiples
median_income <- median_income %>%
  mutate(
    AMI_Multiple = Median_Income / AMI
  )

# Decide on the number of income groups (e.g., 4 for quartiles)
num_groups <- 4  # Adjust as needed

# Calculate quantiles for AMI_Multiple
quantile_probs <- seq(0, 1, length.out = num_groups + 1)
income_quantiles <- quantile(median_income$AMI_Multiple, probs = quantile_probs, na.rm = TRUE)

# Define income group labels based on quantiles
income_group_labels <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "Upper Income")

# Assign income groups using the quantiles
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

# -------------------------------------------
# Step 9: Read and Process Population Data
# -------------------------------------------

# Define the file paths for the three age-specific population CSV files
population_files <- list(
  under_18 = 'calipopn(under 18).csv',
  age_18_39 = 'calipopn(18-39).csv',
  age_40_over = 'calipopn(40-over).csv'
)

# Read and clean each population data frame
clean_population_data <- function(file, age_col_name) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  names(df)[names(df) == "?..County"] <- "County"
  df <- df %>%
    mutate(
      County = gsub(" County$", "", trimws(County))
    ) %>%
    select(County, !!sym(age_col_name))
  return(df)
}

pop_under_18 <- clean_population_data(population_files$under_18, "People..Age.Under.18.") %>%
  rename(People_Under_18 = People..Age.Under.18.)

pop_18_39 <- clean_population_data(population_files$age_18_39, "People..Age.18.39.") %>%
  rename(People_18_39 = People..Age.18.39.)

pop_40_over <- clean_population_data(population_files$age_40_over, "People..Age.40.And.Over.") %>%
  rename(People_40_Over = People..Age.40.And.Over.)

# Merge the population data frames
population_data <- pop_under_18 %>%
  left_join(pop_18_39, by = "County") %>%
  left_join(pop_40_over, by = "County") %>%
  mutate(
    population = People_Under_18 + People_18_39 + People_40_Over
  ) %>%
  select(County, population)

# -------------------------------------------
# Step 10: Clean and Ensure Consistency in County Names
# -------------------------------------------

# Clean county names in NO2 data
NO2_filtered_date$County <- trimws(NO2_filtered_date$County)

# Ensure consistency in median income data (already cleaned in Step 8)

# Identify any mismatches between NO2 data and median income data
county_mismatches_income <- setdiff(unique(NO2_filtered_date$County), unique(median_income$County))
if(length(county_mismatches_income) > 0) {
  print("Counties in NO2 data not found in median income data:")
  print(county_mismatches_income)
} else {
  print("All counties in NO2 data are present in median income data.")
}

# Identify any mismatches between NO2 data and population data
county_mismatches_population <- setdiff(unique(NO2_filtered_date$County), unique(population_data$County))
if(length(county_mismatches_population) > 0) {
  print("Counties in NO2 data not found in population data:")
  print(county_mismatches_population)
} else {
  print("All counties in NO2 data are present in population data.")
}

# -------------------------------------------
# Step 11: Merge Data for Disparity Analysis
# -------------------------------------------

# Merge overall NO2 means with median income data and population data
analysis_data_filtered <- overall_means_filtered %>%
  left_join(median_income, by = 'County') %>%
  left_join(population_data, by = 'County')

# Check for any NAs after merging
missing_income_groups <- sum(is.na(analysis_data_filtered$Income_Group))
if(missing_income_groups > 0) {
  warning(paste(missing_income_groups, "counties have missing Income_Group after merging."))
} else {
  print("All counties have associated Income_Group.")
}

# -------------------------------------------
# Step 12: Calculate NO2 Statistics by Income Group using Population-Weighted Average
# -------------------------------------------

# Calculate weighted average NO2 concentration for each income group
NO2_by_income_group_filtered <- analysis_data_filtered %>%
  group_by(Income_Group) %>%
  summarise(
    Weighted_Mean_NO2 = sum(Overall_Mean * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    Total_Population = sum(population, na.rm = TRUE)
  ) %>%
  arrange(desc(Income_Group))

# Print the results to observe disparities
print(NO2_by_income_group_filtered)

# -------------------------------------------
# End of Script
# -------------------------------------------
