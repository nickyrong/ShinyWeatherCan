# Test script to check what station IDs are being read from the database

# Load required libraries
library(dplyr)
library(readr)

# Determine meta database location
if(file.exists("./database/stations.rds")) {
  db_location <- "./database/stations.rds" # when running from app directory
} else if (file.exists("../database/stations.rds")) {
  db_location <- "../database/stations.rds" # when running from root directory
} else {
  stop("Could not find stations.rds database")
}

# Load the station metadata
station_meta <- readRDS(db_location)

# Check structure
str(station_meta)

# Extract all Climate IDs and save to CSV for easy inspection
climate_ids <- station_meta$stn$climate_id[!is.na(station_meta$stn$climate_id)]

# Create a dataframe with all Climate IDs and their info
climate_id_info <- station_meta$stn %>%
  filter(!is.na(climate_id)) %>%
  select(station_id, station_name, climate_id, prov) %>%
  distinct()

# Save to CSV
write.csv(climate_id_info, "climate_id_list.csv", row.names = FALSE)

# Check if specific IDs exist in the database
test_ids <- c("1071092", "1101564", "1047672", "1100015")

cat("\nChecking for specific test IDs:\n")
for(id in test_ids) {
  exists <- id %in% climate_ids
  matches <- climate_id_info %>% filter(as.character(climate_id) == as.character(id))
  if(nrow(matches) > 0) {
    cat(id, "FOUND -", matches$station_name, "\n")
  } else {
    cat(id, "NOT FOUND\n")
  }
  
  # Test if coercion helps with finding the ID
  class_id <- class(climate_ids)
  class_test <- class(id)
  
  cat("  Class of climate_ids:", class_id, "\n")
  cat("  Class of test id:", class_test, "\n")
  
  # Test with explicit coercion
  matches_coerced <- climate_id_info %>% 
    filter(as.character(climate_id) == as.character(id))
  
  if(nrow(matches_coerced) > 0) {
    cat("  Found using explicit character coercion\n")
  } else {
    cat("  Not found even with explicit character coercion\n")
  }
  
  # Print a few values from the database for comparison
  sample_ids <- head(climate_ids, 5)
  cat("  Sample climate_ids from database:", paste(sample_ids, collapse=", "), "\n\n")
}

# Check if any IDs start with '1'
start_with_1 <- climate_ids[grepl("^1", climate_ids)]
cat("\nNumber of IDs starting with '1':", length(start_with_1), "\n")
if(length(start_with_1) > 0) {
  cat("Sample IDs starting with '1':", paste(head(start_with_1, 10), collapse=", "), "\n")
}

# Check for potential data type issues
cat("\nChecking for potential data type issues:\n")
cat("Range of climate_id values:\n")
range_ids <- range(as.numeric(climate_ids), na.rm = TRUE)
cat("Min:", range_ids[1], "Max:", range_ids[2], "\n")

# Check if leading zeros might be missing
leading_zeros <- climate_ids[nchar(climate_ids) < 7]
if(length(leading_zeros) > 0) {
  cat("\nIDs with fewer than 7 digits (potential missing leading zeros):", 
      paste(head(leading_zeros, 10), collapse=", "), "\n")
}
