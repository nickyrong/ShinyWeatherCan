# Station Dataset Summarized
# Uses user input from the resolution dropdown
Dataset_Summarize <- function(station_number, summary_reso, wy_month) {
  
  TS <- read_dailyflow(station_number = station_number) %>%
    mutate(Year = year(Date) , Month = month(Date), Day = day(Date)) %>%
    select(Date, Year, Month, Day, Flow, Symbol = SYM) %>% 
    rename(Flow_cms = Flow)
  
  codes <- as.factor(TS$Symbol)
  codes <- match(codes, SYMs)
  codes <- SYMnames[codes]
  
  # Calculate Water Year
  TS <- TS %>% 
    # calculate water year
    mutate(WaterYear = hydro_yr(Date, hyrstart = wy_month),
           Flag = codes) %>% 
    
    # re-arrange column order to have WaterYear beside Year
    dplyr::relocate(WaterYear, .after = Year) %>%
    # Remove Symbol (using Flag instead)
    select(-Symbol) 
  
  if(summary_reso == "Daily"){
    TS$Flow_cms <- round(TS$Flow_cms, digits = 3)
  }
  
  else if(summary_reso == "Monthly"){
    TS <- TS %>% 
      group_by(Year, Month) %>%
      summarise(Average_Flow_cms = mean(Flow_cms, na.rm = TRUE), 
                Count = sum(is.na(Flow_cms)==FALSE),
                WaterYear = max(WaterYear, na.rm = TRUE),
                .groups = "drop") %>% 
      dplyr::relocate(WaterYear, .after = Year)
    
    TS$Average_Flow_cms <- round(TS$Average_Flow_cms, digits = 3)
  }
  
  # Make sure to group by water year
  else if(summary_reso == "Yearly"){
    TS <- TS %>% 
      group_by(WaterYear) %>%
      summarise(Average_Flow_cms = mean(Flow_cms, na.rm = TRUE), 
                Max_Daily_cms = max(Flow_cms, na.rm = TRUE),
                Min_Daily_cms = min(Flow_cms, na.rm = TRUE), 
                Count = sum(is.na(Flow_cms) == FALSE),
                .groups = "drop") %>%
      round(digits = 3)
  }
  
  return(TS)
} # EOF for flow period summary

# Helper functions for handling NA values with a threshold
#------------------------------------------------------------------------------
# These functions return NA if the number of non-NA values is less than the threshold
complete_sum = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, sum(x, na.rm = TRUE), NA_real_)}
complete_max = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, max(x, na.rm = TRUE), NA_real_)}
complete_min = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, min(x, na.rm = TRUE), NA_real_)}
complete_mean = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, mean(x, na.rm = TRUE), NA_real_)}
complete_count = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, sum(!is.na(x)), NA_real_)}

# Monthly summarization function for ECCC weather data
#------------------------------------------------------------------------------
summarize_monthly <- function(df, cols, min_days = 28){
    df_summarized <- df |>
      dplyr::group_by(station_id, year, month) |>
      # ignore NA depends on threshold
      dplyr::summarise(across(all_of(cols), 
                              list(mean = ~complete_mean(., Threshold = min_days), 
                                   min = ~complete_min(., Threshold = min_days), 
                                   max = ~complete_max(., Threshold = min_days), 
                                   sum = ~complete_sum(., Threshold = min_days), 
                                   count = ~complete_count(., Threshold = 0)), 
                              .names = "{.col}_{.fn}"), .groups = "drop") |>
      arrange(station_id, year, month)

    return(df_summarized)
}

# Yearly summarization function for ECCC weather data
#------------------------------------------------------------------------------
summarize_yearly <- function(df, cols, min_months = 12){
    df_summarized <- df |>
      dplyr::group_by(station_id, year) |>
      # ignore NA depends on threshold
      dplyr::summarise(across(all_of(cols), 
                              list(mean = ~complete_mean(., Threshold = min_months), 
                                   min = ~complete_min(., Threshold = min_months), 
                                   max = ~complete_max(., Threshold = min_months), 
                                   sum = ~complete_sum(., Threshold = min_months), 
                                   count = ~complete_count(., Threshold = 0)), 
                              .names = "{.col}_{.fn}"), .groups = "drop") |>
      arrange(station_id, year)

    return(df_summarized)
}
