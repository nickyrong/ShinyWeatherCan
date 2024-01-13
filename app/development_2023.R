# script for development use only
rm(list=ls())
library(tidyr)
library(dplyr)
library(weathercan)

#! for testing only, set the working directory to the app folder
setwd("C:/GitHub/ShinyWeatherCan/app")

#stewart_id <- stations() %>% dplyr::filter(climate_id == '1067742' & interval == "day") %>% "$"(station_id)
#stewart_daily <- weather_dl(station_ids = stewart_id, interval = "day", verbose = TRUE)
#saveRDS(stewart_daily, "stewart_data.rds")

data <- readRDS("stewart_data.rds")

# remove columns with "_flag" in the column name
vec_vars <- data |> select(-starts_with("station"), 
                            -ends_with("id"), 
                            -contains("_flag"),
                            -prov, -lat, -lon, -lat, -elev, 
                            -date, -year, -month, -day, -qual) |> 
                            names()


#? why is the year and month columns not numeric?
data <- data %>% 
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date)
  ) %>% arrange(station_id, date)


# * write a function to summarize multiple columns in "data" with multiple functions (means, standard deviations, min, max, count of non-NA values, etc.)
# * the function should return a data frame with the summarized data
# * the function should be able to summarize all columns in "data" except for "station_id", "year", and "month" that are numeric
data_monthlysummarized <- function(df, cols, min_threshold = 0.9){
    df_summarized <- df %>% 
      dplyr::group_by(station_id, year, month) %>% 
      dplyr::summarise_at(vars(cols), list(mean = mean, sd = sd, min = min, max = max, count = ~sum(!is.na(.)))) %>%
      mutate(howmanydays = as.numeric(lubridate::days_in_month(base::as.Date(paste(year, month, "01", sep = "-"))))) %>%
      # filter out each "_count" columns if larger than the min_threshold
      dplyr::filter_at(vars(ends_with("count")), all_vars(. >= min_threshold*howmanydays)) %>%
      arrange(station_id, year, month)

    return(df_summarized)
}

#! testing only
#? R return warning Using an eaxternal vector in selections was deprecated in tidyselect 1.1.0. Please use `all_of()` or `any_of()` instead.
# how to change the code to avoid this warning?

data_summarized <- data_monthlysummarized(df = data,
 cols = c("max_temp", "min_temp", "mean_temp", "total_rain", "total_snow", "total_precip"))

# R update all packages
update.packages(ask = FALSE, checkBuilt = TRUE)

