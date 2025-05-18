# Example script for quantile mapping infill of ECCC climate station data
rm(list=ls())
library(tidyr)
library(dplyr)
library(weathercan)
library(ggplot2)
library(plotly)
library(lubridate)
options(scipen = 999)
# increase the number of columns to display
options(dplyr.width = Inf)

# Load the quantile mapping infill functions
source("quantile_mapping_infill.R")
source("data_analyses_functions.R")

# Set the working directory to the app folder
setwd("C:/GitHub/ShinyWeatherCan/app")

# Load the Stewart station data as an example
data <- readRDS("stewart_data.rds")

# Get the Stewart station ID
stewart_id <- unique(data$station_id)

# Example 1: Find nearby stations to Stewart
nearby_stations <- find_nearby_stations(
  target_station_id = stewart_id,
  max_distance_km = 150, 
  min_overlap_years = 5
)

# Display the nearby stations
print(nearby_stations %>% select(station_id, station_name, distance_km, overlap_years))

# Example 2: Create a version of the Stewart data with some artificially missing values
# to test the infill process
set.seed(123)
test_data <- data

# Introduce artificial gaps - remove 10% of the data
n_rows <- nrow(test_data)
rows_to_remove <- sample(1:n_rows, size = floor(0.1 * n_rows))
test_variable <- "mean_temp"
original_values <- test_data[[test_variable]][rows_to_remove]
test_data[[test_variable]][rows_to_remove] <- NA

# Example 3: Infill the missing data using quantile mapping
infilled_data <- infill_station_data(
  target_station_id = stewart_id,
  variable = test_variable,
  start_date = min(test_data$date),
  end_date = max(test_data$date),
  max_distance_km = 150,
  min_overlap_years = 5,
  by_month = TRUE
)

# Example 4: Evaluate the infill quality
# Compare the artificially removed values with the infilled values
validation_df <- data.frame(
  date = test_data$date[rows_to_remove],
  original = original_values,
  infilled = infilled_data$target_value[match(test_data$date[rows_to_remove], infilled_data$date)]
)

# Calculate validation metrics
rmse <- sqrt(mean((validation_df$original - validation_df$infilled)^2, na.rm = TRUE))
mae <- mean(abs(validation_df$original - validation_df$infilled), na.rm = TRUE)
r_squared <- cor(validation_df$original, validation_df$infilled, use = "pairwise.complete.obs")^2

cat("Validation Metrics:\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")
cat("R²:", round(r_squared, 2), "\n")

# Example 5: Plot the original vs infilled values
p <- ggplot(validation_df, aes(x = original, y = infilled)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Validation of Infilled Values",
    subtitle = paste("RMSE =", round(rmse, 2), 
                     ", MAE =", round(mae, 2),
                     ", R² =", round(r_squared, 2)),
    x = "Original Values",
    y = "Infilled Values"
  ) +
  theme_minimal()

print(p)

# Example 6: Plot the time series with infilled values
p2 <- plot_infilled_data(infilled_data, test_variable, "Stewart Station")
print(p2)

# Example 7: Summarize the data before and after infilling
# Original data summary
original_monthly <- summarize_monthly(test_data, cols = c(test_variable), min_days = 25)

# Reconstructed data for summarization
reconstructed_data <- test_data
# Update the test variable with infilled values
reconstructed_data[[test_variable]] <- infilled_data$target_value[match(reconstructed_data$date, infilled_data$date)]
infilled_monthly <- summarize_monthly(reconstructed_data, cols = c(test_variable), min_days = 25)

# Compare monthly summaries
comparison <- original_monthly %>%
  select(station_id, year, month, contains(test_variable)) %>%
  left_join(
    infilled_monthly %>%
      select(station_id, year, month, contains(test_variable)) %>%
      rename_with(~paste0(., "_infilled"), contains(test_variable)),
    by = c("station_id", "year", "month")
  )

print(head(comparison))


