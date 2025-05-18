#' Monthly Bias Correction Using QDM Method
#'
#' @details
#' Quantile Delta Mapping (QDM) bias correct the distribution of the modelled data to the observed data
#' It requires some overlapping data between observed and modeled (calibration period). QDM basically forces the
#' modeled data to have the same distribution as the observed data (by correcting j-th quantile of the longer modeled data
#' with the same amount of correction done to the j-th quantile of the modeled data in the calibration period).
#' This function automatically divides the time series into months and do the QDM bias correction for each month's values.
#'
#'@param obs_data observed data dataframe must have at the 2 columns: a date/datetime column and a data column (column names must be consistent with "timecolname" and "obscolname")
#'@param mod_data modeled data dataframe must have the 2 columns: a date/datetime column and a data column (column names must be consistent with "timecolname" and "modcolname")
#'@param timecolname single value text vector, name for the datetime column
#'@param obscolname single value text vector, name for the observed data column
#'@param modcolname single value text vector, name for the modeledd data column
#'@param non_negative boolean option of TRUE or FALSE for whether the data are in ratio (e.g. precip) so the value cannot be negative

#' @return tibble with two columns (datetime and a bias corrected data column) with same dimension as the mod_data
#' @export
#'
#' @examples
#' \dontrun{
#' monthly_correction_QDM(obs_data = site_daily,
#'                        mod_data = reanalysis_daily,
#'                        timecolname = "Date",
#'                        obscolname = "AirTemp_Avg_degC",
#'                        modcolname = "Tavg",
#'                        non_negative = FALSE,
#'                        manual_trace = 0.05)
#' }





monthly_correction_QDM <- function(obs_data, mod_data, timecolname, obscolname, modcolname, non_negative, manual_trace = 0.05, hourly = FALSE) {

  library(tidyr)
  library(dplyr)
  library(MBC) # QDM method

  data <-  obs_data %>% 
    select(!!as.name(timecolname), !!as.name(obscolname)) %>% 
    full_join(mod_data %>% select(!!as.name(timecolname), !!as.name(modcolname)), by = timecolname) %>% 
    arrange(!!as.name(timecolname))
  
  data_corrected <- data %>% mutate(!!modcolname := NA_real_)
  
  for(mon in 1:12) {
    
    message("Month: ", mon)
    
    
    concurrent <- data %>% 
      drop_na(!!obscolname, !!modcolname) %>%
      filter(lubridate::month(!!as.name(timecolname)) == mon)

    concurrent_datetime <- concurrent %>% select(!!as.name(timecolname)) %>% {if(hourly) as.vector(.) else .} 
    
    mod_month <-  data[,c(timecolname, modcolname)] %>%
      drop_na(!!modcolname) %>%
      filter(lubridate::month(!!as.name(timecolname)) == mon) %>%
      filter(!(!!as.name(timecolname) %in% concurrent_datetime))
    mod_datetime <- mod_month %>% select(!!as.name(timecolname))
    
    # Univariate quantile delta mapping
    v3_list <- list(rcm.c = concurrent[, obscolname] %>% data.matrix(),
                    gcm.c = concurrent[, modcolname] %>% data.matrix(),
                    gcm.p = mod_month[, modcolname] %>% data.matrix()
    )
    
    # use ratio = TRUE if this is for precip
    qdm.fit <- suppressWarnings(MBC::QDM(o.c = v3_list$rcm.c,
                        m.c = v3_list$gcm.c,
                        m.p = v3_list$gcm.p,
                        ratio = non_negative,
                        trace = manual_trace))
      
      
    data_corrected <- rows_update(data_corrected, tibble(concurrent_datetime, !!modcolname := qdm.fit$mhat.c), by = timecolname)
    data_corrected <- rows_update(data_corrected, tibble(mod_datetime, !!modcolname := as.numeric(qdm.fit$mhat.p)), by = timecolname)
    
    
  }

  return(data_corrected)
}
