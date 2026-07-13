#' Download the most up-to-date ECCC station inventory and save as CSV
#' # Functions adapted from Professor Dan Moore, UBC Geography Department
#' 
#' @description
#' This function downloads the most up-to-date inventory of Environment and
#' Climate Change Canada (ECCC) weather stations and saves it as a CSV file
#' on the local hard drive. If the file already exists in the specified
#' directory, it is read directly without re-downloading.
#'
#' @param data_dir Character. Path to the local directory in which to save
#'   (or read) the inventory CSV. Default: file.path(getwd(), "data").
#' @param filename Character. Name of the CSV file for the station inventory.
#'   Default: "ec_station_inventory_en.csv". The file is expected to contain 
#'   specific column names, including "Station.Name", "Longitude..Decimal.Degrees.", 
#'   and "Latitude..Decimal.Degrees.".
#' @param return_sf Logical. If TRUE, return the inventory as an sf object;
#'   otherwise return as a plain data.frame. Default: TRUE.
#'
#' @details
#' The function reads the English‐language version of the station inventory.
#' It first checks whether the file exists in \code{data_dir}. If not,
#' it downloads the inventory from ECCC, writes it to disk, and then reads it
#' back into R. Returning an sf object requires the \pkg{sf} package.
#'
#' @return
#' A \code{data.frame} or an \code{sf} object depending on the value of \code{return_sf}.
#' If \code{return_sf = FALSE}, the function returns a \code{data.frame} (via \pkg{data.table}).
#' If \code{return_sf = TRUE}, the function returns an \code{sf} object with geometry columns
#' (longitude/latitude) and CRS EPSG:4326.
#'
#' @seealso
#' Metadata for the inventory:
#' https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Station_Inventory_ID_Disclaimer_Metadata_EN.txt
#' @format A data frame with the following columns:
#' \describe{
#'   \item{Name}{Official Climate Station Name - the official designation of the weather station}
#'   \item{Climate ID}{7-digit official Climate ID number assigned by the Meteorological Service of Canada. 
#'                     First digit identifies the province, second and third digits identify the climatological district. 
#'                     Serves as a permanent, unique identifier for climatological purposes}
#'   \item{Station ID}{Internal database numbering system with no external meaning. Subject to change without notice}
#'   \item{WMO ID}{5-digit international identifier assigned by the World Meteorological Organization 
#'                 for stations transmitting real-time observations in international formats}
#'   \item{TC ID}{Transport Canada identifier for meteorological reports from airport observing sites 
#'                transmitted in real-time aviation formats}
#'   \item{Latitude}{Latitude coordinates in degrees, minutes, seconds (generally for instrument site). 
#'                   Accurate to nearest second or 0.003 degrees. All Canadian locations are north of equator}
#'   \item{Longitude}{Longitude coordinates in degrees, minutes, seconds (generally for instrument site). 
#'                    Accurate to nearest second or 0.003 degrees. Negative values indicate degrees west of Greenwich Meridian}
#'   \item{Latitude (Decimal Degrees)}{Latitude expressed in decimal degree format}
#'   \item{Longitude (Decimal Degrees)}{Longitude expressed in decimal degree format}
#'   \item{Elevation (m)}{Elevation above mean sea level in metres, given to nearest metre. 
#'                        Generally represents height of ground where instruments are exposed}
#'   \item{First Year}{Year when observations began at the station}
#'   \item{Last Year}{Year of most recent observation at the station}
#'   \item{HLY First Year}{Year when hourly observations began}
#'   \item{HLY Last Year}{Year of most recent hourly observation}
#'   \item{DLY First Year}{Year when daily observations began}
#'   \item{DLY Last Year}{Year of most recent daily observation}
#'   \item{MLY First Year}{Year when monthly observations began}
#'   \item{MLY Last Year}{Year of most recent monthly observation}
#' }
#' @import data.table
#' @importFrom sf st_as_sf st_set_crs
#' @export
#' 
get_ec_inv <- function(data_dir = file.path(getwd()), return_sf = TRUE,
                       filename = "ec_station_inventory_en.csv") {
if (!dir.exists(data_dir)) 
    stop(paste(data_dir, " does not exist. Please create the directory or specify an existing one."))
  fp_inv <- file.path(data_dir, filename)
if (file.exists(fp_inv) && difftime(Sys.time(), file.info(fp_inv)$mtime, units = "hours") < 24) {
    ec_inv <- fread(fp_inv)
  } else {
    url_inv <- "https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv"
    ec_inv <- tryCatch({
        data <- fread(url_inv, skip = 3)
        if (!"Name" %in% names(data)) {
            stop("Downloaded data does not contain expected columns.")
        }
        data
    }, error = function(e) {
        stop("Failed to download or process the station inventory: ", e$message)
    })
    if (inherits(ec_inv, "try-error")) {
      stop("download of eccc station inventory failed")
    }
    write.csv(ec_inv, fp_inv, row.names = FALSE)
  }
  if (return_sf) {
    cols <- names(ec_inv)
    lat_candidates <- grep("latitude.*decimal", cols, ignore.case = TRUE, value = TRUE)
    if (length(lat_candidates) == 0) lat_candidates <- grep("^lat", cols, ignore.case = TRUE, value = TRUE)
    lon_candidates <- grep("longitude.*decimal", cols, ignore.case = TRUE, value = TRUE)
    if (length(lon_candidates) == 0) lon_candidates <- grep("^lon", cols, ignore.case = TRUE, value = TRUE)
    
    if (length(lat_candidates) == 0 || length(lon_candidates) == 0) {
      stop("Latitude and/or Longitude columns could not be identified. Ensure the inventory contains appropriate column names.")
    }
    
    lat_col <- lat_candidates[1]
    lon_col <- lon_candidates[1]
    
    if (is.na(lat_col) || is.na(lon_col)) {
      stop("Latitude/Longitude columns not found after identification.")
    }
    
    ec_inv <- st_as_sf(ec_inv, coords = c(lon_col, lat_col), crs = 4326)
  }
  return(ec_inv)
}
# end of the get_ec_inv function



#' Download climate observations from Environment and Climate Change Canada via GeoMet-OGC-API
#' Functions adapted from Professor Dan Moore, UBC Geography Department
#' 
#' ECCC has developed an API for bulk data access, called GeoMet-OGC-API, which 
#' supersedes an earlier approach based on a Cygwin script. The API allows access 
#' to a range of data sets of interest to hydrologists and water resource analysts, 
#' including WSC hydrometric data, observed climate data, and CMIP5 and CMIP6 
#' climate projections. This function is focused on downloading observations from 
#' climate stations.
#'
#' @param station_id Character. Station identifier corresponding to the chosen id_type.
#' @param res Character. Temporal resolution; one of "hourly", "daily", or "monthly".
#'   Default: "daily".
#' @param limit Integer. Maximum number of observations to download. Default: 1000000.
#' @param time_out Numeric. The value of the 'timeout' option in seconds. Default: 200.
#' @param id_type Character. Type of station identifier. One of:
#'   \itemize{
#'     \item "CLIMATE_IDENTIFIER" (ECCC inventory column "Climate.ID") - default
#'     \item "WMO_IDENTIFIER" (ECCC inventory column "WMO.ID")
#'     \item "STN_ID" (ECCC inventory column "Station.ID")
#'     \item "TC_IDENTIFIER" (ECCC inventory column "TC.ID")
#'   }
#' @param dt1 Character. Date-time for start of download period in ISO8601 format 
#'   ("YYYY-MM-DDThh:mm:ssZ"). Use ".." to indicate the beginning of station record. 
#'   Default: "1840-01-01T00:00:00Z".
#' @param dt2 Character. Date-time for end of download period in ISO8601 format, 
#'   or ".." for the end of the available period of record. Default: "..".
#'
#' @details
#' To specify a period extending from the beginning of the station record up to 
#' and including a specified date, set \code{dt1 = ".."} and \code{dt2} equal to 
#' the end of the download period. Similarly, to download data from a specified 
#' date to the end of the available record, set \code{dt1} to the start date and 
#' let \code{dt2} default to "..".
#'
#' @return A \code{data.table} with meteorological observations. Columns vary by
#'   resolution but typically include LOCAL_DATE, temperature, precipitation, etc.
#'
#' @examples
#' \dontrun{
#' # Daily data for station 51442 from 2020 to latest available
#' daily_data <- get_ec_met("51442", res = "daily",
#'                         dt1 = "2020-01-01T00:00:00Z")
#'
#' # Monthly data from beginning of record to end of 2019
#' monthly_data <- get_ec_met("71845", res = "monthly",
#'                           dt1 = "..",
#'                           dt2 = "2019-12-31T23:59:59Z")
#' }
#'
#' @seealso \url{https://api.weather.gc.ca/collections?lang=en}
#' @import data.table
#' @importFrom curl new_pool curl_fetch_multi multi_run
#' @export
#' 
get_ec_met <- function(station_id, res = "daily", limit = 1000000,
                       time_out = 200,
                       id_type = "CLIMATE_IDENTIFIER", 
                       dt1 = "1840-01-01T00:00:00Z",
                       dt2 = "..") {
  options(timeout = time_out)
  if (class(dt1) != "character" | class(dt2) != "character") 
    stop("dt1 and/or dt2 not character variables")
  if (!(res %in% c("hourly", "daily", "monthly"))) 
    stop("invalid value for argument res")
  dt_format <- "%Y-%m-%dT%H:%M:%S"
  if (dt1 == ".." & dt2 == "..") stop("both dt1 and dt2 are ..")
  dt1a <- strptime(dt1, format = dt_format) 
  dt2a <- strptime(dt2, format = dt_format)
  if (is.na(dt1a) & is.na(dt2a)) {
    stop("both dt1 and dt2 are invalid")
  }
  dotdot <- 0
  if (is.na(dt1a) & !is.na(dt2a)) {
    if (dt1 != "..") stop("dt1 invalid") else dotdot = 1
  } 
  if (is.na(dt2a) & !is.na(dt1a)) {
    if (dt2 != "..") stop("dt2 invalid") else dotdot = 2
  } 
  if (dotdot == 0 & dt1a > dt2a) 
    stop("check dt1 and dt2: dt1 must be less than dt2")
  
  # First, determine total number of records available with a small test request
  page_limit <- 500
  
  # Get first page to check data availability
  url_first <- paste0("https://api.weather.gc.ca/collections/", 
                     "climate-", res, "/items?",
                     "f=csv",
                     "&limit=1",
                     "&offset=0",
                     "&sortby=LOCAL_DATE",
                     "&datetime=", dt1, "/", dt2,
                     "&", id_type, "=", station_id)
  
  first_page <- try(fread(url_first))
  if (inherits(first_page, "try-error")) {
    stop("access to meteorological data failed")
  }
  
  if (nrow(first_page) == 0) {
    stop("No data available for the specified parameters")
  }
  
  # Estimate total pages needed (conservative approach)
  max_pages <- min(ceiling(limit / page_limit), 100) # Cap at 100 pages for safety
  
  # Generate all URLs for async downloading
  urls <- sapply(0:(max_pages-1), function(page_num) {
    offset <- page_num * page_limit
    paste0("https://api.weather.gc.ca/collections/", 
           "climate-", res, "/items?",
           "f=csv",
           "&limit=", page_limit,
           "&offset=", offset,
           "&sortby=LOCAL_DATE",
           "&datetime=", dt1, "/", dt2,
           "&", id_type, "=", station_id)
  })
  
  cat("Starting async download for", length(urls), "potential pages...\n")
  
  # Storage for downloaded data
  downloaded_data <- vector("list", length(urls))
  names(downloaded_data) <- urls
  
  # Create curl pool for async requests
  pool <- curl::new_pool(total_con = 50L, host_con = 10L)
  
  # Function to handle successful downloads
  save_download <- function(req) {
    tryCatch({
      # Parse CSV content directly from memory
      content_text <- rawToChar(req$content)
      if (nchar(content_text) > 0) {
        data <- fread(text = content_text)
        if (nrow(data) > 0) {
          downloaded_data[[req$url]] <<- data
        }
      }
    }, error = function(e) {
      # Silently handle parsing errors for empty/invalid pages
    })
  }
  
  # Function to handle failed downloads
  fail_download <- function(req) {
    # Silently handle failed requests - common for pages beyond available data
  }
  
  # Setup async calls
  invisible(
    lapply(urls, function(url) {
      curl::curl_fetch_multi(url, 
                            done = save_download, 
                            fail = fail_download, 
                            pool = pool)
    })
  )
  
  # Execute all requests simultaneously
  result <- curl::multi_run(pool = pool)
  
  cat("Async download completed:", result$success, "successful,", result$error, "failed\n")
  
  # Filter out NULL results and combine data
  all_data <- downloaded_data[!sapply(downloaded_data, is.null)]
  
  if (length(all_data) == 0) {
    stop("No data retrieved")
  }
  
  # Combine all data
  ec_data <- rbindlist(all_data, fill = TRUE)
  
  # Remove duplicates that might occur due to overlapping pages
  if ("LOCAL_DATE" %in% names(ec_data)) {
    ec_data <- unique(ec_data, by = "LOCAL_DATE")
  }
  
  # Sort by date
  if ("LOCAL_DATE" %in% names(ec_data)) {
    ec_data <- ec_data[order(LOCAL_DATE)]
  }
  
  # Apply user limit if necessary
  if (nrow(ec_data) > limit) {
    ec_data <- ec_data[1:limit]
  }
  
  cat("Total records downloaded:", nrow(ec_data), "\n")
  
  return(ec_data)
}
# end of the get_ec_met function


# A function that will wrangle the data from get_ec_met and make them ready for plotting

#  # warning about WebGL does not plot in RStudio despite app launched in webbrowser
# warning about too many ID for 8 color paletes (will auto generate more colors anyway)
# surpress them is not that easy...must go around with plotly_build()
# https://www.rdocumentation.org/packages/plotly/versions/3.6.0/topics/plotly_build
# https://github.com/ropensci/plotly/issues/1202

suppressPlotlyWarnings <- function(p) {
  suppressWarnings(plotly_build(p))
}

#' Plot climate time series
#'
#' Returns a plotly figure for climate data time series for a selected station.
#'
#' @details
#' Plotly interactive plotting of Environment and Climate Change Canada (ECCC) 
#' climate data. Creates time series plots for temperature, precipitation, and 
#' other meteorological variables from weather station observations.
#'
#' @param df Tibble of the processed climate data from get_ec_met().
#' @param station_name Name of the weather station to be selected for plotting
#' @param event_reg Boolean about whether to register and parse user selection to a variable (register if T)
#'                  the registered events are saved to a reactive variable (e.g. "plotly_selected") defined below
#' @param smooth_period Integer to be used for smoothing (optional)
#' @param df_smooth Feed in pre-calculated data frame (smoothing was calculated in data processing)
#'
#' @return A plotly figure of the climate time series for a weather station
#' @export
#'
#' @examples
#' \dontrun{
#' basic_ts_plotting(df = climate_data, station_name = "VANCOUVER INTL A")
#' }
#' 

basic_ts_plotting <- function(df, station_name, event_reg = FALSE, smooth_period = NULL, df_smooth = NULL) {
  plotting_area <- df %>% filter(Location == station_name)

  # directly read in the cache tables for the smoothing periods
  if (!is.null(smooth_period)) {
    df_smooth <- df_smooth %>%
      mutate(Datetime = base::as.Date(Datetime)) %>%
      filter(Location == areaname)
  }



  fig <- subplot(

    suppressPlotlyWarnings(plot_ly(
      data = plotting_area, x = ~Datetime, y = ~cum_HDist_m, color = ~ factor(ID),
      type = "scattergl", mode = "markers", height = 800,
      customdata = ~ID,
      legendgroup = ~ID,
      showlegend = TRUE
    ) %>%
      # only plot smoothed line if period is specified
      {
        if (!is.null(smooth_period)) {
          add_lines(
            p = ., # need to specify plot_ly opject in conditional pipe
            data = df_smooth,
            x = ~Datetime,
            y = ~cum_HDist_m,
            line = list(dash = "solid", width = 1.5, color = ~ factor(ID)), name = " "
          )
        } else {
          .
        }
      }) %>%
      layout(yaxis = list(
        title = "Horizontal\n Displacement (m)"
      )),
    suppressPlotlyWarnings(plot_ly(
      data = plotting_area, x = ~Datetime, y = ~cum_VDist_m, color = ~ factor(ID),
      type = "scattergl", mode = "markers", height = 800,
      customdata = ~ID,
      legendgroup = ~ID,
      showlegend = FALSE
    ) %>%
      # only plot smoothed line if period is specified
      {
        if (!is.null(smooth_period)) {
          add_lines(
            p = ., # need to specify plot_ly opject in conditional pipe
            data = df_smooth,
            x = ~Datetime,
            y = ~cum_VDist_m,
            line = list(dash = "solid", width = 1.5, color = ~ factor(ID)), name = " "
          )
        } else {
          .
        }
      }) %>%
      layout(yaxis = list(
        title = "Vertical\n Displacement (m)",
        fixedrange = FALSE
      )),
    nrows = 2, shareX = TRUE, titleY = TRUE
  ) %>% {
    if (event_reg == TRUE) event_register(., "plotly_selected") else .
  }


  # Add range slider
  fig <- fig %>% layout(
    xaxis = list(
      rangeslider = list(
        visible = TRUE,
        thickness = 0.05
      ),
      type = "date",
      title = "Datetime"
    ),
    # re-enable the zooming of y-axis after adding the slider
    yaxis = list(fixedrange = FALSE)
  )

  fig
}


#' Customized Data Table Setting
#'
#' Changing setting like buttons & X/Y scrolling etc
#'
#' @details
#' Customization of the DT::datatable before rendering
#'
#'@param .df data frame to be plotted as table
#'@param bts boolean variable about whether to use buttons (colum hiding etc, T), default F
#'@param single boolean variable about whether selection should be single (T), otherwise default (F) allows multiple
#'@param escape whether to escape HTML entities in the table: T means to escape the whole table, and F means not to escape it; 
#'
#' @return DT::datable object
#' @export
#'
#' @examples
#' \dontrun{
#' df %>% customized_datatable()
#' }

customized_datatable <- function(.df, bts = FALSE, single = FALSE, escape = TRUE){
  
  if(single == TRUE) {S = 'single'} else {S = c("multiple", "single", "none")}
  if(escape == TRUE) {E = TRUE} else {E = FALSE}
  
  .df %>%
  DT::datatable(
    selection = S,
    escape = E,
    extensions = c('FixedColumns', 'Scroller') %>%
      {if (bts == TRUE) c(., 'Buttons') else .},
    options = list(
      
      # Options for extension "Buttons"
      dom = 'Bfrtip',
      
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      
      # Options for extension "FixedColumns"
      scrollX = TRUE,
      fixedColumns = TRUE,
      
      # Options for extension "Scroller"
      deferRender = TRUE,
      scrollY = 600,
      scroller = TRUE
      
    ) %>% 
      {if (bts == TRUE) append(., list(buttons = list(I('colvis')))) else .} 
    
  ) # End of DT::datatable
}


# Function to remove non-plottable columns
remove_non_plottable <- function(.data) {
  # Base data cleaning - remove completely empty columns
  clean_data <- .data |>
    select(where(~ !all(is.na(.))))
  
    # For plotting - more aggressive filtering to remove time components and identifiers
    plot_data <- clean_data |>
      # Remove non-numerical columns but keep "LOCAL_DATE" and "LOCAL_TIME"
      select(any_of(c("LOCAL_DATE", "LOCAL_TIME")), where(is.numeric)) |>
      # Remove time components, coordinates, and identifiers that aren't useful for plotting
      select(-any_of(c("x", "y", "LOCAL_MONTH", "LOCAL_DAY", "LOCAL_YEAR", "LOCAL_HOUR", 
                  "UTC_MONTH", "UTC_YEAR", "UTC_DAY", "CLIMATE_IDENTIFIER", 
                  "LONGITUDE", "LATITUDE"))) |>
      # Clean up NA values
      mutate(across(where(is.character), ~ ifelse(.x %in% c("NA", "<NA>", ""), NA_character_, .x)),
             across(where(is.numeric), ~ ifelse(is.na(.x) | .x == "", NA_real_, .x)),
             across(where(is.logical), ~ ifelse(.x %in% c("NA", "<NA>", ""), NA, .x)))


  return(plot_data)
}



# Helper functions for handling NA values with a threshold
#------------------------------------------------------------------------------
# These functions return NA if the number of non-NA values is less than the threshold
complete_sum = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, sum(x, na.rm = TRUE), NA_real_)}
complete_max = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, max(x, na.rm = TRUE), NA_real_)}
complete_min = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, min(x, na.rm = TRUE), NA_real_)}
complete_mean = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, mean(x, na.rm = TRUE), NA_real_)}
complete_median = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, median(x, na.rm = TRUE), NA_real_)}
complete_sd = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, stats::sd(x, na.rm = TRUE), NA_real_)}
complete_skewness = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=Threshold, moments::skewness(x, na.rm = TRUE), NA_real_)}
complete_count = function(x, Threshold) {base::ifelse(sum(!is.na(x))>=0, sum(!is.na(x)), NA_real_)}

# Monthly summarization function for ECCC weather data
#------------------------------------------------------------------------------
summarize_monthly <- function(df, cols, stats, min_days = 28){

    df_summarized <- df |>
      mutate(year = lubridate::year(LOCAL_DATE),
             month = lubridate::month(LOCAL_DATE)) |>
      dplyr::group_by(year, month) |>
      # ignore NA depends on threshold
      dplyr::summarise(across(all_of(cols), 
              setNames(lapply(stats, function(stat) {
                threshold <- min_days  # Capture min_days in closure
                switch(stat,
                  "mean" = ~complete_mean(., Threshold = threshold),
                  "min" = ~complete_min(., Threshold = threshold),
                  "max" = ~complete_max(., Threshold = threshold),
                  "sum" = ~complete_sum(., Threshold = threshold),
                  "count" = ~complete_count(., Threshold = threshold),
                  "median" = ~complete_median(., Threshold = threshold),
                  "sd" = ~complete_sd(., Threshold = threshold),
                  "skewness" = ~complete_skewness(., Threshold = threshold))
              }), stats),
              .names = "{.col}_{.fn}"), .groups = "drop") |>
      arrange(year, month)

    return(df_summarized)
}

# Yearly summarization function for ECCC weather data
#------------------------------------------------------------------------------
summarize_yearly <- function(df, cols, stats, min_months = 360){
  df_summarized <- df |>
    mutate(year = lubridate::year(LOCAL_DATE)) |>
    dplyr::group_by(year) |>
    # ignore NA depends on threshold
    dplyr::summarise(across(all_of(cols), 
        setNames(lapply(stats, function(stat) {
        threshold <- min_months  # Capture min_months in closure
        switch(stat,
          "mean" = ~complete_mean(., Threshold = threshold),
          "min" = ~complete_min(., Threshold = threshold),
          "max" = ~complete_max(., Threshold = threshold),
          "sum" = ~complete_sum(., Threshold = threshold),
          "count" = ~complete_count(., Threshold = threshold),
          "median" = ~complete_median(., Threshold = threshold),
          "sd" = ~complete_sd(., Threshold = threshold),
          "skewness" = ~complete_skewness(., Threshold = threshold))
        }), stats),
        .names = "{.col}_{.fn}"), .groups = "drop") |>
    arrange(year)

  return(df_summarized)
}

