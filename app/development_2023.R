# script for development use only
rm(list=ls())
library(tidyr)
library(dplyr)
library(weathercan)

#stewart_id <- stations() %>% dplyr::filter(climate_id == '1067742' & interval == "day") %>% "$"(station_id)
#stewart_daily <- weather_dl(station_ids = stewart_id, interval = "day", verbose = TRUE)
#saveRDS(stewart_daily, "stewart_data.rds")

data <- readRDS("stewart_data.rds")
