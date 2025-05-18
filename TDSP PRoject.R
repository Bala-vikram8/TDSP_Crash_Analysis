# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(randomForest)
library(caret)

# Get the working directory
getwd()

# Read the data
data <- read.csv("Motor_Vehicle_Collisions.csv", stringsAsFactors = FALSE)

# Clean and transform the data
crash_data <- data %>%
  mutate(
    `CRASH DATE` = mdy(`CRASH.DATE`),        # Convert crash date to Date format
    `CRASH TIME` = hm(`CRASH.TIME`),         # Convert crash time to Time format
    Hour = hour(`CRASH TIME`),               # Extract the hour of the crash
    Weekday = wday(`CRASH DATE`, label = TRUE),  # Extract the day of the week
    Month = month(`CRASH DATE`, label = TRUE),  # Extract the month
    Year = year(`CRASH DATE`),  # Extract the year
    BOROUGH = ifelse(is.na(BOROUGH), "UNKNOWN", BOROUGH) # Handle missing boroughs
  )

# Inspect the cleaned and transformed data
head(crash_data)
str(crash_data)

# Aggregate crash data by borough
borough_crashes <- crash_data %>%
  group_by(BOROUGH) %>%
  summarise(across(starts_with("NUMBER"), sum, na.rm = TRUE))

# Visualizations of injuries by borough
ggplot(borough_crashes, aes(x = BOROUGH, y = NUMBER.OF.PERSONS.INJURED, fill = "steelblue")) +
  geom_col(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Total Injuries by Borough", x = "Borough", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(borough_crashes, aes(x = BOROUGH, y = NUMBER.OF.PEDESTRIANS.INJURED, fill = "darkorange")) +
  geom_col(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Pedestrian Injuries by Borough", x = "Borough", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(borough_crashes, aes(x = BOROUGH, y = NUMBER.OF.CYCLIST.INJURED, fill = "seagreen")) +
  geom_col(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Cyclist Injuries by Borough", x = "Borough", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(borough_crashes, aes(x = BOROUGH, y = NUMBER.OF.MOTORIST.INJURED, fill = "purple")) +
  geom_col(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Motorist Injuries by Borough", x = "Borough", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate and visualize injury rates per borough
borough_rates <- borough_crashes %>%
  mutate(
    INJURY_RATE = NUMBER.OF.PERSONS.INJURED / sum(NUMBER.OF.PERSONS.INJURED)
  )

ggplot(borough_rates, aes(x = BOROUGH, y = INJURY_RATE, fill = "steelblue")) +
  geom_col(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Injury Rates by Borough", x = "Borough", y = "Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the structure of the dataset
str(crash_data)

# Display the first few rows of the dataset
head(crash_data)

# List all unique years in the dataset
unique_years <- unique(crash_data$Year)
unique_years

# Remove rows with any missing values in the data_year before modeling
data_year <- na.omit(data_year)

# Impute missing values using the mice package
library(mice)
imputed_data <- mice(data_year, m=5, method='pmm', seed=500)
completed_data <- complete(imputed_data)

library(randomForest)
library(caret)
library(mice)  # for imputation
library(dplyr)

# Initialize a list to store models for each year
models_by_year <- list()

# Assuming crash_data is already loaded and initial transformations are correct
# Ensure to correct any necessary transformations here before proceeding
crash_data <- crash_data %>%
  mutate(
    InjuryPresent = as.factor(ifelse(NUMBER.OF.PERSONS.INJURED > 0, 1, 0)),
    Year = as.numeric(Year)  # Ensuring Year is numeric if not already
  )

print(names(crash_data))

library(randomForest)
library(caret)
library(dplyr)

# Assuming crash_data is already loaded and correct
models_by_year <- list()

for (year in unique(crash_data$Year)) {
  data_year <- filter(crash_data, Year == year)
  data_year <- na.omit(data_year)  # Removing rows with missing values
  
  set.seed(123)
  training_index <- createDataPartition(data_year$InjuryPresent, p = 0.75, list = FALSE)
  training_set <- data_year[training_index, ]
  testing_set <- data_year[-training_index, ]
  
  if (nrow(training_set) == 0) {
    print(paste("Skipping year due to insufficient data:", year))
    next
  }
  
  # Adjust formula to correctly reference variables, especially those with spaces or special characters
  rf_model <- randomForest(InjuryPresent ~ . - `NUMBER.OF.PERSONS.INJURED` - Year - `CRASH DATE` - `CRASH TIME`, data = training_set, ntree = 500)
  models_by_year[[as.character(year)]] <- rf_model
  
  print(paste("Random Forest model for year", year))
  print(summary(rf_model))
}

for (year in unique(crash_data$Year)) {
  model <- models_by_year[[as.character(year)]]
  print(paste("Performance metrics for year", year))
  print(model$confusion)  # Confusion matrix
  print(model$err.rate)   # Error rates
}

for (year in unique(crash_data$Year)) {
  model <- models_by_year[[as.character(year)]]
  importance_data <- importance(model)
  print(paste("Variable importance for year", year))
  print(importance_data)
}

library(ggplot2)

# Loop through each year, creating and printing the variable importance plots
for (year in unique(crash_data$Year)) {
  model <- models_by_year[[as.character(year)]]
  importance_data <- as.data.frame(importance(model))
  importance_data$Variable <- rownames(importance_data)
  
  # Create the ggplot object
  p <- ggplot(importance_data, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Variable Importance for year", year), x = "Variables", y = "Importance") +
    theme_minimal()
  
  # Explicitly print the plot
  print(p)
}

for (year in unique(crash_data$Year)) {
  model <- models_by_year[[as.character(year)]]
  importance_data <- as.data.frame(importance(model))
  importance_data$Variable <- rownames(importance_data)
  
  if (nrow(importance_data) > 0) {
    p <- ggplot(importance_data, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Variable Importance for year", year), x = "Variables", y = "Importance") +
      theme_minimal()
    
    # Save the plot to a file
    plot_filename <- paste0("Variable_Importance_", year, ".png")
    ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
    print(paste("Plot saved as:", plot_filename))
  } else {
    print(paste("No data available for year", year))
  }
}