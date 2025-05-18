# Test script for the quantile mapping infill functionality

library(tidyr)
library(dplyr)
library(weathercan)
library(ggplot2)
library(plotly)
library(lubridate)

# Set working directory to the app folder
setwd("C:/GitHub/ShinyWeatherCan/app")

# Load the functions
source("quantile_mapping_infill.R")
source("data_analyses_functions.R")

# Load the Stewart station data as an example
data <- readRDS("stewart_data.rds")

# Get the Stewart station ID
stewart_id <- unique(data$station_id)

# Example 1: Find nearby stations to Stewart
cat("Finding nearby stations to Stewart...\n")
nearby_stations <- find_nearby_stations(
  target_station_id = stewart_id,
  max_distance_km = 150, 
  min_overlap_years = 5
)

# Display the nearby stations
print(nearby_stations %>% select(station_id, station_name, distance_km, overlap_years))

# Example 2: Create artificial test data with missing values
cat("\nCreating test data with artificial gaps...\n")
set.seed(123)
test_data <- data

# Introduce artificial gaps - remove 10% of the data
n_rows <- nrow(test_data)
rows_to_remove <- sample(1:n_rows, size = floor(0.1 * n_rows))
test_variable <- "mean_temp"
original_values <- test_data[[test_variable]][rows_to_remove]
test_data[[test_variable]][rows_to_remove] <- NA

# Example 3: Infill the missing data using quantile mapping
cat("\nInfilling missing data using quantile mapping...\n")
infilled_data <- tryCatch({
  infill_station_data(
    target_station_id = stewart_id,
    variable = test_variable,
    start_date = min(test_data$date),
    end_date = max(test_data$date),
    max_distance_km = 150,
    min_overlap_years = 5,
    by_month = TRUE
  )
}, error = function(e) {
  cat("Error in infill_station_data:", conditionMessage(e), "\n")
  return(NULL)
})

if (!is.null(infilled_data)) {
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
  
  cat("\nValidation Metrics:\n")
  cat("RMSE:", round(rmse, 2), "\n")
  cat("MAE:", round(mae, 2), "\n")
  cat("R²:", round(r_squared, 2), "\n")
  
  # Create a PDF output of validation plots
  pdf("quantile_mapping_validation.pdf", width = 10, height = 8)
  
  # Plot 1: Original vs Infilled values
  p1 <- ggplot(validation_df, aes(x = original, y = infilled)) +
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
  
  print(p1)
  
  # Plot 2: Time series with infilled values
  p2 <- plot_infilled_data(infilled_data, test_variable, "Stewart Station")
  print(p2)
  
  # Plot 3: Monthly summary
  # Create monthly summaries
  original_monthly <- summarize_monthly(data, cols = c(test_variable), min_days = 25)
  
  # Reconstructed data for summarization
  reconstructed_data <- test_data
  # Update the test variable with infilled values
  reconstructed_data[[test_variable]] <- infilled_data$target_value[match(reconstructed_data$date, infilled_data$date)]
  
  infilled_monthly <- summarize_monthly(reconstructed_data, cols = c(test_variable), min_days = 25)
  
  # Join the monthly summaries for comparison
  monthly_comparison <- original_monthly %>%
    select(station_id, year, month, !!paste0(test_variable, "_mean")) %>%
    left_join(
      infilled_monthly %>%
        select(station_id, year, month, !!paste0(test_variable, "_mean")) %>%
        rename(!!paste0(test_variable, "_mean_infilled") := !!paste0(test_variable, "_mean")),
      by = c("station_id", "year", "month")
    ) %>%
    mutate(date = as.Date(paste(year, month, "15", sep = "-")))
  
  # Create a long format for plotting
  plot_data <- monthly_comparison %>%
    select(date, 
           original = !!paste0(test_variable, "_mean"), 
           infilled = !!paste0(test_variable, "_mean_infilled")) %>%
    pivot_longer(cols = c(original, infilled),
                 names_to = "series", 
                 values_to = "value")
  
  p3 <- ggplot(plot_data, aes(x = date, y = value, color = series)) +
    geom_line() +
    geom_point(size = 1) +
    scale_color_manual(values = c("original" = "black", "infilled" = "red"),
                       labels = c("Original", "Infilled"),
                       name = "Data Series") +
    labs(
      title = paste("Monthly Mean", test_variable, "Comparison"),
      subtitle = "Original vs. Infilled Data",
      x = "Date",
      y = paste("Monthly Mean", test_variable)
    ) +
    theme_minimal()
  
  print(p3)
  
  dev.off()
  cat("\nPlots saved to quantile_mapping_validation.pdf\n")
}

cat("\nTest completed.\n")
