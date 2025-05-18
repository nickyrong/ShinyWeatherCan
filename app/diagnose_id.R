# Diagnostic script to troubleshoot specific ID problems
# Run this script independently to diagnose ID matching issues

# Load required libraries
library(dplyr)

# Function to find exact matches and close matches
find_station_id <- function(target_id, data_path = "./database/stations.rds") {
  # Load the stations data
  stations <- readRDS(data_path)
  
  # Extract all climate IDs
  climate_ids <- stations$stn$climate_id
  
  # Convert target to different formats for comparison
  target_char <- as.character(target_id)
  target_num <- as.numeric(target_id)
  
  # Check for exact matches in different formats
  exact_match_char <- which(as.character(climate_ids) == target_char)
  exact_match_num <- which(climate_ids == target_num)
  
  # Check for pattern matches (contains)
  pattern_match <- grep(target_char, as.character(climate_ids), fixed = TRUE)
  
  # Print diagnostic info
  cat("Target ID:", target_id, "\n")
  cat("Target as character:", target_char, "/ Target as numeric:", target_num, "\n")
  cat("Climate IDs class:", class(climate_ids), "\n\n")
  
  cat("Exact character matches:", length(exact_match_char), "\n")
  if(length(exact_match_char) > 0) {
    matches <- stations$stn[exact_match_char, ]
    print(matches[, c("station_id", "station_name", "climate_id")])
  }
  
  cat("\nExact numeric matches:", length(exact_match_num), "\n")
  if(length(exact_match_num) > 0) {
    matches <- stations$stn[exact_match_num, ]
    print(matches[, c("station_id", "station_name", "climate_id")])
  }
  
  cat("\nPattern matches:", length(pattern_match), "\n")
  if(length(pattern_match) > 0) {
    matches <- stations$stn[pattern_match, ]
    print(head(matches[, c("station_id", "station_name", "climate_id")], 5))
  }
  
  # Check data type issues
  cat("\nChecking for potential data type issues:\n")
  sample_ids <- head(climate_ids[!is.na(climate_ids)], 5)
  cat("Sample climate_ids:", paste(sample_ids, collapse=", "), "\n")
  cat("Sample types:", paste(sapply(sample_ids, class), collapse=", "), "\n")
  
  # Check if the ID might be in a different format in the database
  potential_formats <- c(
    target_id,                                # Original
    as.character(target_id),                  # As character
    as.numeric(target_id),                    # As numeric
    sprintf("%07d", as.numeric(target_id)),   # With leading zeros to 7 digits
    format(as.numeric(target_id), scientific=FALSE) # No scientific notation
  )
  
  cat("\nChecking potential alternative formats:\n")
  for(fmt in potential_formats) {
    cat(fmt, "- Class:", class(fmt), "- Matches:", 
        sum(as.character(climate_ids) == as.character(fmt), na.rm=TRUE), "\n")
  }
  
  # Try brute force match
  all_ids <- as.character(climate_ids[!is.na(climate_ids)])
  all_ids_numeric <- as.numeric(all_ids)
  target_numeric <- as.numeric(target_id)
  
  numeric_match <- which(all_ids_numeric == target_numeric)
  if(length(numeric_match) > 0) {
    cat("\nFound match using numeric conversion:", all_ids[numeric_match], "\n")
    matches <- stations$stn[which(as.numeric(as.character(climate_ids)) == target_numeric), ]
    print(matches[, c("station_id", "station_name", "climate_id")])
  } else {
    cat("\nNo match found using numeric conversion\n")
  }
  
  # Find closest matches
  if(is.numeric(climate_ids)) {
    diffs <- abs(climate_ids - target_numeric)
    closest <- head(order(diffs), 5)
    cat("\nClosest matches by numeric difference:\n")
    print(stations$stn[closest, c("station_id", "station_name", "climate_id")])
  }
  
  # Check if the ID might exist in a different column
  other_cols <- c("station_id", "WMO_id", "TC_id")
  for(col in other_cols) {
    if(col %in% names(stations$stn)) {
      matches <- which(as.character(stations$stn[[col]]) == target_char)
      if(length(matches) > 0) {
        cat("\nFound in column", col, ":\n")
        print(stations$stn[matches, c("station_id", "station_name", col)])
      }
    }
  }
}

# Test the problematic ID
find_station_id("1100015")

# Test a known working ID for comparison
find_station_id("1047672")
