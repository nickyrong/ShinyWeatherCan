
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
