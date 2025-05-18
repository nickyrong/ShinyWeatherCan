# Functions for infilling missing data in climate station records using quantile mapping
# from nearby ECCC stations
library(tidyr)
library(dplyr)
library(weathercan)
library(ggplot2)
library(purrr)

#------------------------------------------------------------------------------
# Function to find nearby stations based on distance and data overlap
#------------------------------------------------------------------------------
find_nearby_stations <- function(target_station_id, max_distance_km = 100, 
                                min_overlap_years = 10, variables = NULL) {
  # Get target station info
  all_stations <- stations()
  target_station <- all_stations %>% 
    filter(station_id == target_station_id) %>%
    select(station_id, station_name, lat, lon, interval, start, end)
  
  if(nrow(target_station) == 0) {
    stop(paste("Station ID", target_station_id, "not found in the database"))
  }
  
  # Calculate distance to all other stations with the same interval
  nearby <- all_stations %>%
    filter(interval == target_station$interval[1]) %>%
    # Skip if it's the same station
    filter(station_id != target_station_id) %>%
    # Calculate distance using haversine formula (approximate)
    mutate(distance_km = weathercan:::haversine(
      lat1 = target_station$lat[1], 
      lon1 = target_station$lon[1],
      lat2 = lat, 
      lon2 = lon
    )) %>%
    # Filter by distance
    filter(distance_km <= max_distance_km) %>%
    # Calculate temporal overlap
    mutate(
      # Convert start/end to Date type if they're not already
      start = as.Date(start),
      end = as.Date(end),
      target_start = target_station$start[1],
      target_end = target_station$end[1],
      # Calculate overlap
      overlap_start = pmax(start, target_start),
      overlap_end = pmin(end, target_end),
      overlap_days = as.numeric(overlap_end - overlap_start),
      overlap_years = overlap_days / 365.25
    ) %>%
    # Filter by minimum overlap
    filter(overlap_years >= min_overlap_years) %>%
    # Sort by distance
    arrange(distance_km)
  
  return(nearby)
}

#------------------------------------------------------------------------------
# Function to rank stations by their suitability for infilling
#------------------------------------------------------------------------------
rank_stations_for_infill <- function(target_station_id, nearby_stations = NULL, 
                                   max_distance_km = 100, min_overlap_years = 5,
                                   variable = "mean_temp", test_period = 365) {
  # If nearby_stations is not provided, find them
  if(is.null(nearby_stations)) {
    nearby_stations <- find_nearby_stations(
      target_station_id = target_station_id,
      max_distance_km = max_distance_km,
      min_overlap_years = min_overlap_years
    )
  }
  
  if(nrow(nearby_stations) == 0) {
    return(data.frame())
  }
  
  # Get target station data
  target_data <- tryCatch({
    weather_dl(station_ids = target_station_id, interval = "day")
  }, error = function(e) {
    warning(paste("Error downloading data for station", target_station_id, ":", e$message))
    return(NULL)
  })
  
  if(is.null(target_data) || nrow(target_data) == 0) {
    return(nearby_stations %>% 
             mutate(correlation = NA, rmse = NA, 
                    mae = NA, rank_score = NA) %>%
             arrange(distance_km))
  }
  
  # Ensure the variable exists in the target data
  if(!variable %in% names(target_data)) {
    warning(paste("Variable", variable, "not found in target station data"))
    return(nearby_stations %>% 
             mutate(correlation = NA, rmse = NA, 
                    mae = NA, rank_score = NA) %>%
             arrange(distance_km))
  }
  
  # Limit test period to last N days to make computation faster
  if(!is.null(test_period) && test_period > 0) {
    max_date <- max(target_data$date, na.rm = TRUE)
    min_test_date <- max_date - test_period
    target_data <- target_data %>% filter(date >= min_test_date)
  }
  
  # Initialize results dataframe
  results <- nearby_stations %>%
    mutate(correlation = NA_real_,
           rmse = NA_real_,
           mae = NA_real_,
           rank_score = NA_real_)
  
  # For each nearby station, calculate correlation and error metrics
  for(i in 1:nrow(nearby_stations)) {
    station_id <- nearby_stations$station_id[i]
    
    # Download data for the station
    nearby_data <- tryCatch({
      weather_dl(station_ids = station_id, 
                interval = "day",
                start = min(target_data$date),
                end = max(target_data$date))
    }, error = function(e) {
      warning(paste("Error downloading data for station", station_id, ":", e$message))
      return(NULL)
    })
    
    if(is.null(nearby_data) || nrow(nearby_data) == 0 || !variable %in% names(nearby_data)) {
      next
    }
    
    # Merge data and calculate metrics
    combined <- target_data %>%
      select(date, target_value = !!variable) %>%
      inner_join(nearby_data %>%
                  select(date, nearby_value = !!variable),
                by = "date") %>%
      filter(!is.na(target_value) & !is.na(nearby_value))
    
    if(nrow(combined) < 30) {  # Need minimum data for reliable metrics
      next
    }
    
    # Calculate metrics
    results$correlation[i] <- cor(combined$target_value, combined$nearby_value, 
                                 use = "pairwise.complete.obs")
    results$rmse[i] <- sqrt(mean((combined$target_value - combined$nearby_value)^2))
    results$mae[i] <- mean(abs(combined$target_value - combined$nearby_value))
    
    # Calculate a rank score (weighted combination of metrics)
    # Lower distance, higher correlation, lower error = better score
    results$rank_score[i] <- (0.4 * results$correlation[i]) + 
      (0.3 * (1 - results$distance_km / max(results$distance_km, na.rm = TRUE))) +
      (0.15 * (1 - results$rmse[i] / max(results$rmse[i], na.rm = TRUE))) +
      (0.15 * (1 - results$mae[i] / max(results$mae[i], na.rm = TRUE)))
  }
  
  # Sort by rank score (descending)
  results <- results %>%
    arrange(desc(rank_score))
  
  return(results)
}

#------------------------------------------------------------------------------
# Function to download data from target and nearby stations
#------------------------------------------------------------------------------
get_station_data <- function(station_ids, start_date = NULL, end_date = NULL, 
                           interval = "day", variables = NULL) {
  # Download data
  data <- weather_dl(station_ids = station_ids, 
                    start = start_date,
                    end = end_date,
                    interval = interval)
  
  # Filter variables if specified
  if(!is.null(variables)) {
    var_cols <- names(data)[names(data) %in% variables]
    data <- data %>% select(station_id, date, all_of(var_cols))
  }
  
  return(data)
}

#------------------------------------------------------------------------------
# Function to prepare data for quantile mapping
#------------------------------------------------------------------------------
prepare_qmap_data <- function(target_data, nearby_data, variable) {
  # Ensure target_data and nearby_data have the same columns
  common_cols <- c("station_id", "date", variable)
  
  target_data <- target_data %>% 
    select(all_of(common_cols)) %>%
    rename(target_value = all_of(variable))
  
  nearby_data <- nearby_data %>%
    select(all_of(common_cols)) %>%
    rename(nearby_value = all_of(variable))
  
  # Join the datasets
  combined <- target_data %>%
    left_join(nearby_data, by = "date", suffix = c("_target", "_nearby"))
  
  return(combined)
}

#------------------------------------------------------------------------------
# Function to perform quantile mapping
#------------------------------------------------------------------------------
perform_quantile_mapping <- function(combined_data, variable, 
                                   by_month = TRUE, min_overlap = 30) {
  # If by_month is TRUE, perform quantile mapping separately for each month
  if(by_month) {
    # Add month column
    combined_data <- combined_data %>%
      mutate(month = lubridate::month(date))
    
    # Split data by month
    by_month_data <- split(combined_data, combined_data$month)
    
    # Apply quantile mapping to each month
    mapped_data <- map_dfr(by_month_data, function(month_data) {
      # Skip months with too few overlapping points
      overlap_count <- sum(!is.na(month_data$target_value) & !is.na(month_data$nearby_value))
      if(overlap_count < min_overlap) {
        return(month_data)
      }
      
      # Compute empirical CDFs for both stations using only overlapping period
      overlap_data <- month_data %>%
        filter(!is.na(target_value) & !is.na(nearby_value))
      
      # Define the empirical CDF functions
      target_ecdf <- ecdf(overlap_data$target_value)
      nearby_ecdf <- ecdf(overlap_data$nearby_value)
      
      # For missing values in target, estimate using quantile mapping
      missing_idx <- which(is.na(month_data$target_value) & !is.na(month_data$nearby_value))
      
      if(length(missing_idx) > 0) {
        # Get quantile in nearby data
        nearby_quantiles <- nearby_ecdf(month_data$nearby_value[missing_idx])
        
        # Map to target distribution (using inverse CDF)
        target_values <- quantile(overlap_data$target_value, probs = nearby_quantiles, type = 8)
        
        # Replace missing values
        month_data$target_value[missing_idx] <- target_values
        month_data$infilled[missing_idx] <- TRUE
      }
      
      return(month_data)
    })
    
    return(mapped_data)
  } else {
    # Perform quantile mapping without monthly differentiation
    # Compute empirical CDFs for both stations using only overlapping period
    overlap_data <- combined_data %>%
      filter(!is.na(target_value) & !is.na(nearby_value))
    
    # Check if there's enough overlapping data
    if(nrow(overlap_data) < min_overlap) {
      combined_data$infilled <- FALSE
      return(combined_data)
    }
    
    # Define the empirical CDF functions
    target_ecdf <- ecdf(overlap_data$target_value)
    nearby_ecdf <- ecdf(overlap_data$nearby_value)
    
    # For missing values in target, estimate using quantile mapping
    missing_idx <- which(is.na(combined_data$target_value) & !is.na(combined_data$nearby_value))
    
    combined_data$infilled <- FALSE
    
    if(length(missing_idx) > 0) {
      # Get quantile in nearby data
      nearby_quantiles <- nearby_ecdf(combined_data$nearby_value[missing_idx])
      
      # Map to target distribution (using inverse CDF)
      target_values <- quantile(overlap_data$target_value, probs = nearby_quantiles, type = 8)
      
      # Replace missing values
      combined_data$target_value[missing_idx] <- target_values
      combined_data$infilled[missing_idx] <- TRUE
    }
    
    return(combined_data)
  }
}

#------------------------------------------------------------------------------
# Main function to infill data using quantile mapping
#------------------------------------------------------------------------------
infill_station_data <- function(target_station_id, variable, 
                               start_date = NULL, end_date = NULL,
                               max_distance_km = 100, min_overlap_years = 10,
                               by_month = TRUE, min_overlap = 30) {
  
  # Find nearby stations
  nearby_stations <- find_nearby_stations(
    target_station_id = target_station_id,
    max_distance_km = max_distance_km,
    min_overlap_years = min_overlap_years
  )
  
  if(nrow(nearby_stations) == 0) {
    stop("No nearby stations found that meet the criteria")
  }
  
  # Download target station data
  target_data <- get_station_data(
    station_ids = target_station_id,
    start_date = start_date,
    end_date = end_date,
    variables = variable
  )
  
  # Initialize results list
  infilled_results <- list()
  
  # Try each nearby station in order of distance
  for(i in 1:nrow(nearby_stations)) {
    nearby_id <- nearby_stations$station_id[i]
    
    # Download nearby station data
    nearby_data <- get_station_data(
      station_ids = nearby_id,
      start_date = start_date,
      end_date = end_date,
      variables = variable
    )
    
    # Prepare data for quantile mapping
    combined_data <- prepare_qmap_data(target_data, nearby_data, variable)
    
    # Perform quantile mapping
    mapped_data <- perform_quantile_mapping(
      combined_data,
      variable = variable,
      by_month = by_month,
      min_overlap = min_overlap
    )
    
    # Store results
    infilled_results[[i]] <- mapped_data %>%
      mutate(
        donor_station_id = nearby_id,
        donor_station_name = nearby_stations$station_name[i],
        donor_distance_km = nearby_stations$distance_km[i]
      )
    
    # Update target data with infilled values for the next iteration
    target_data <- target_data %>%
      select(-all_of(variable)) %>%
      left_join(
        mapped_data %>% 
          select(date, target_value, infilled) %>%
          rename(!!variable := target_value),
        by = "date"
      )
  }
  
  # Combine results
  all_infilled <- bind_rows(infilled_results)
  
  # Return unique rows (last infill takes precedence)
  final_infilled <- all_infilled %>%
    arrange(date, desc(infilled)) %>%
    distinct(date, .keep_all = TRUE)
  
  return(final_infilled)
}

#------------------------------------------------------------------------------
# Function to visualize the infilled data
#------------------------------------------------------------------------------
plot_infilled_data <- function(infilled_data, variable, station_name = NULL) {
  # Create a plot
  p <- ggplot(infilled_data, aes(x = date, y = target_value)) +
    geom_line(aes(color = "Original")) +
    geom_point(data = subset(infilled_data, infilled), 
               aes(color = "Infilled"), size = 1) +
    scale_color_manual(values = c("Original" = "black", "Infilled" = "red"),
                       name = "Data Type") +
    labs(
      title = paste("Time Series with Infilled Values for", 
                    ifelse(is.null(station_name), 
                           paste("Station ID:", infilled_data$station_id_target[1]), 
                           station_name)),
      subtitle = paste("Variable:", variable),
      x = "Date",
      y = variable
    ) +
    theme_minimal()
  
  return(p)
}

#------------------------------------------------------------------------------
# Function to create a monthly heatmap of data availability and infilling
#------------------------------------------------------------------------------
plot_infill_heatmap <- function(infilled_data, variable, station_name = NULL) {
  # Process the data for heatmap display
  # Add year and month columns
  infilled_data <- infilled_data %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date, label = TRUE, abbr = TRUE))
  
  # Summarize by year and month
  heatmap_data <- infilled_data %>%
    group_by(year, month) %>%
    summarise(
      total_days = n(),
      missing_days = sum(is.na(target_value) & !infilled),
      infilled_days = sum(infilled),
      original_days = sum(!is.na(target_value) & !infilled),
      pct_available = (original_days + infilled_days) / total_days * 100,
      pct_infilled = infilled_days / total_days * 100,
      .groups = "drop"
    ) %>%
    # Set factor levels to ensure proper ordering
    mutate(month = factor(month, levels = month.abb))
  
  # Create heatmap
  p <- ggplot(heatmap_data, aes(x = month, y = year, fill = pct_available)) +
    geom_tile(color = "white") +
    # Add text showing percentage of infilled data
    geom_text(aes(label = sprintf("%.0f%%", pct_infilled)), 
              color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue",
                        name = "% Data\nAvailable") +
    labs(
      title = paste("Monthly Data Availability with Infilled Values for",
                    ifelse(is.null(station_name), 
                           paste("Station ID:", infilled_data$station_id_target[1]), 
                           station_name)),
      subtitle = paste("Variable:", variable, "- Text shows % of data that was infilled"),
      x = "Month",
      y = "Year"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0))
  
  return(p)
}

#------------------------------------------------------------------------------
# Function to evaluate infill quality
#------------------------------------------------------------------------------
evaluate_infill_quality <- function(original_data, infilled_data, variable) {
  # Create a validation dataset by removing some data points
  set.seed(123)  # For reproducibility
  validation_idx <- sample(which(!is.na(original_data[[variable]])), 
                           size = floor(0.2 * sum(!is.na(original_data[[variable]])))) 
  
  validation_data <- original_data
  validation_values <- validation_data[[variable]][validation_idx]
  validation_data[[variable]][validation_idx] <- NA
  
  # Apply infilling to validation data
  validated_infill <- infill_station_data(
    target_station_id = unique(original_data$station_id),
    variable = variable,
    start_date = min(original_data$date),
    end_date = max(original_data$date)
  )
  
  # Calculate metrics
  infilled_values <- validated_infill$target_value[validation_idx]
  
  metrics <- list(
    RMSE = sqrt(mean((validation_values - infilled_values)^2, na.rm = TRUE)),
    MAE = mean(abs(validation_values - infilled_values), na.rm = TRUE),
    R_squared = cor(validation_values, infilled_values, use = "pairwise.complete.obs")^2
  )
  
  # Create validation plot
  validation_df <- data.frame(
    Observed = validation_values,
    Infilled = infilled_values
  )
  
  p <- ggplot(validation_df, aes(x = Observed, y = Infilled)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = "Validation of Infilled Values",
      subtitle = paste("RMSE =", round(metrics$RMSE, 2), 
                       ", MAE =", round(metrics$MAE, 2),
                       ", R² =", round(metrics$R_squared, 2)),
      x = "Observed Values",
      y = "Infilled Values"
    ) +
    theme_minimal()
  
  return(list(metrics = metrics, plot = p))
}
