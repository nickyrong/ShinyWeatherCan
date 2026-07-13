rm(list = ls())

library(tidyr)
library(dplyr)
library(data.table) # for fread()
library(sf) # additional package required by weathercan
library(DT) # datatable()
library(plotly) # interactive available data plotting
library(moments) # for skewness and kurtosis in the summary stat tab
library(stringr) # for str_to_title function
library(lubridate) # for date functions used in summarization

# load the functions
source("./functions_basic.R")
source("./functions_era5.R")         # ERA5-Land ARCO access (reticulate)
source("./functions_qaqc.R")         # QA/QC of ECCC daily data (WMO/GHCN)
source("./functions_bias_correct.R") # QDM bias-correction + monthly CDF review plot
source("./functions_infill.R")       # infill_station() assembly driver

function(input, output, session) {
  
  # End the app loading spinner----
  waiter_hide()
  # Here set up any elements that require spinner
  #spin_map <- Waiter$new("MapPlot", html = spin_3k(), color = "black")
  #spin_datatable <- Waiter$new("datatable", html = spin_3k(), color = "black")
  #spin_plot <- Waiter$new("pctmiss_plotly", html = spin_3k(), color = "black")
  
  #* Data are stored in reactive in Shiny -------------------------------
  # Reactive values to store selected stations
  primary_station <- reactiveVal(NULL)
  secondary_station <- reactiveVal(NULL)
  tertiary_station <- reactiveVal(NULL)
  # Reactive values to store downloaded data
  primary_data <- reactiveVal(NULL)
  secondary_data <- reactiveVal(NULL)
  tertiary_data <- reactiveVal(NULL)

  # Reactive value to store removed data points
  removed_points <- reactiveVal(data.frame())
  
  # Reactive expression to combine all downloaded data
  all_downloaded_data <- reactive({
    list(
      primary = primary_data(),
      secondary = secondary_data(),
      tertiary = tertiary_data()
    )
  })

  # Clear downloaded data when data interval changes
  observeEvent(input$global_data_interval, {
    # Clear all downloaded data when interval changes
    primary_data(NULL)
    secondary_data(NULL)
    tertiary_data(NULL)
    
    # Show notification to user
    showNotification("Data interval changed. All downloaded data has been cleared.", 
                    type = "message", duration = 3)
  }, ignoreInit = TRUE)  # ignoreInit = TRUE prevents clearing data on app startup
  
  #* SideBar UI-------------------------------
  
  # Display primary station info
  output$primary_station_info <- renderText({
    if(is.null(primary_station())) {
      return("No primary station selected")
    }
    station <- primary_station()
    paste0("Climate ID: ", station$climate_id, "\n",
           "Station: ", station$name, "\n",
           "Hourly Record: ", station$hly_first, " to ", station$hly_last, "\n",
           "Daily Record: ", station$dly_first, " to ", station$dly_last, "\n",
           "Monthly Record: ", station$mly_first, " to ", station$mly_last)
  })
  
  # Display secondary station info
  output$secondary_station_info <- renderText({
    if(is.null(secondary_station())) {
      return("No secondary station selected")
    }
    station <- secondary_station()
    paste0("Climate ID: ", station$climate_id, "\n",
           "Station: ", station$name, "\n",
           "Hourly Record: ", station$hly_first, " to ", station$hly_last, "\n",
           "Daily Record: ", station$dly_first, " to ", station$dly_last, "\n",
           "Monthly Record: ", station$mly_first, " to ", station$mly_last)
  })
  
  # Display tertiary station info
  output$tertiary_station_info <- renderText({
    if(is.null(tertiary_station())) {
      return("No tertiary station selected")
    }
    station <- tertiary_station()
    paste0("Climate ID: ", station$climate_id, "\n",
           "Station: ", station$name, "\n",
           "Hourly Record: ", station$hly_first, " to ", station$hly_last, "\n",
           "Daily Record: ", station$dly_first, " to ", station$dly_last, "\n",
           "Monthly Record: ", station$mly_first, " to ", station$mly_last)
  })
  
  # Show/hide station panels
  output$has_primary_station <- reactive({
    !is.null(primary_station())
  })
  outputOptions(output, "has_primary_station", suspendWhenHidden = FALSE)
  
  output$has_secondary_station <- reactive({
    !is.null(secondary_station())
  })
  outputOptions(output, "has_secondary_station", suspendWhenHidden = FALSE)
  
  output$has_tertiary_station <- reactive({
    !is.null(tertiary_station())
  })
  outputOptions(output, "has_tertiary_station", suspendWhenHidden = FALSE)
  
  # Show/hide data download info message
  output$has_primary_data <- reactive({
    !is.null(primary_data()) && nrow(primary_data()) > 0
  })
  outputOptions(output, "has_primary_data", suspendWhenHidden = FALSE)
  
  output$has_secondary_data <- reactive({
    !is.null(secondary_data()) && nrow(secondary_data()) > 0
  })
  outputOptions(output, "has_secondary_data", suspendWhenHidden = FALSE)
  
  output$has_tertiary_data <- reactive({
    !is.null(tertiary_data()) && nrow(tertiary_data()) > 0
  })
  outputOptions(output, "has_tertiary_data", suspendWhenHidden = FALSE)
  
  # Clear selection buttons
  observeEvent(input$clear_primary, {
    primary_station(NULL)
    primary_data(NULL)  # Also clear downloaded data
    # This will automatically invalidate all_downloaded_data() and combined_station_data()
  })
  
  observeEvent(input$clear_secondary, {
    secondary_station(NULL)
    secondary_data(NULL)  # Also clear downloaded data
    # This will automatically invalidate all_downloaded_data() and combined_station_data()
  })
  
  observeEvent(input$clear_tertiary, {
    tertiary_station(NULL)
    tertiary_data(NULL)  # Also clear downloaded data
    # This will automatically invalidate all_downloaded_data() and combined_station_data()
  })

  # Handle primary station selection from map popup
  observeEvent(input$map_primary_select, {
    climate_id <- input$map_primary_select
    station_data <- map_plot_data() |> 
      filter(`Climate ID` == climate_id) |> 
      slice(1)
    
    if(nrow(station_data) > 0) {
      primary_station(list(
        climate_id = station_data$`Climate ID`,
        name = station_data$Name,
        hly_first = station_data$`HLY First Year`,
        hly_last = station_data$`HLY Last Year`,
        dly_first = station_data$`DLY First Year`,
        dly_last = station_data$`DLY Last Year`,
        mly_first = station_data$`MLY First Year`,
        mly_last = station_data$`MLY Last Year`
      ))
    }
  })
  
  # Handle secondary station selection from map popup
  observeEvent(input$map_secondary_select, {
    climate_id <- input$map_secondary_select
    station_data <- map_plot_data() |> 
      filter(`Climate ID` == climate_id) |> 
      slice(1)
    
    if(nrow(station_data) > 0) {
      secondary_station(list(
        climate_id = station_data$`Climate ID`,
        name = station_data$Name,
        hly_first = station_data$`HLY First Year`,
        hly_last = station_data$`HLY Last Year`,
        dly_first = station_data$`DLY First Year`,
        dly_last = station_data$`DLY Last Year`,
        mly_first = station_data$`MLY First Year`,
        mly_last = station_data$`MLY Last Year`
      ))
    }
  })
  
  # Handle tertiary station selection from map popup
  observeEvent(input$map_tertiary_select, {
    climate_id <- input$map_tertiary_select
    station_data <- map_plot_data() |> 
      filter(`Climate ID` == climate_id) |> 
      slice(1)
    
    if(nrow(station_data) > 0) {
      tertiary_station(list(
        climate_id = station_data$`Climate ID`,
        name = station_data$Name,
        hly_first = station_data$`HLY First Year`,
        hly_last = station_data$`HLY Last Year`,
        dly_first = station_data$`DLY First Year`,
        dly_last = station_data$`DLY Last Year`,
        mly_first = station_data$`MLY First Year`,
        mly_last = station_data$`MLY Last Year`
      ))
    }
  })

  #* Direct Climate ID entry -------------------------------
  # Full station inventory (all provinces) for direct Climate ID lookup.
  # Kept separate from map_plot_data() so a typed ID can match a station in
  # any province, not just the one currently selected.
  full_station_inventory <- reactive({
    get_ec_inv(return_sf = FALSE)
  })

  # Helper: look up a single station by Climate ID across the full inventory.
  # Returns a station list (same shape as the map-selection path) or NULL.
  lookup_station_by_id <- function(climate_id) {
    if (is.null(climate_id)) return(NULL)
    id_clean <- trimws(as.character(climate_id))
    if (id_clean == "") return(NULL)

    match_row <- full_station_inventory() |>
      filter(trimws(as.character(`Climate ID`)) == id_clean) |>
      slice(1)

    if (nrow(match_row) == 0) return(NULL)

    list(
      climate_id = match_row$`Climate ID`,
      name       = match_row$Name,
      hly_first  = match_row$`HLY First Year`,
      hly_last   = match_row$`HLY Last Year`,
      dly_first  = match_row$`DLY First Year`,
      dly_last   = match_row$`DLY Last Year`,
      mly_first  = match_row$`MLY First Year`,
      mly_last   = match_row$`MLY Last Year`
    )
  }

  # Set primary station from a typed Climate ID
  observeEvent(input$set_primary_id, {
    if (is.null(input$climate_id_primary) || trimws(input$climate_id_primary) == "") {
      showNotification("Please enter a Climate ID for the primary station.", type = "warning", duration = 3)
      return()
    }
    station <- lookup_station_by_id(input$climate_id_primary)
    if (is.null(station)) {
      showNotification(paste0("Climate ID '", trimws(input$climate_id_primary),
                              "' was not found in the ECCC station inventory."),
                       type = "error", duration = 5)
    } else {
      primary_station(station)
      primary_data(NULL)  # clear any previously downloaded data for this slot
      showNotification(paste0("Primary station set: ", station$name,
                              " (Climate ID: ", station$climate_id, ")."),
                       type = "message", duration = 3)
    }
  })

  # Set secondary station from a typed Climate ID
  observeEvent(input$set_secondary_id, {
    if (is.null(input$climate_id_secondary) || trimws(input$climate_id_secondary) == "") {
      showNotification("Please enter a Climate ID for the secondary station.", type = "warning", duration = 3)
      return()
    }
    station <- lookup_station_by_id(input$climate_id_secondary)
    if (is.null(station)) {
      showNotification(paste0("Climate ID '", trimws(input$climate_id_secondary),
                              "' was not found in the ECCC station inventory."),
                       type = "error", duration = 5)
    } else {
      secondary_station(station)
      secondary_data(NULL)  # clear any previously downloaded data for this slot
      showNotification(paste0("Secondary station set: ", station$name,
                              " (Climate ID: ", station$climate_id, ")."),
                       type = "message", duration = 3)
    }
  })

  # Set tertiary station from a typed Climate ID
  observeEvent(input$set_tertiary_id, {
    if (is.null(input$climate_id_tertiary) || trimws(input$climate_id_tertiary) == "") {
      showNotification("Please enter a Climate ID for the tertiary station.", type = "warning", duration = 3)
      return()
    }
    station <- lookup_station_by_id(input$climate_id_tertiary)
    if (is.null(station)) {
      showNotification(paste0("Climate ID '", trimws(input$climate_id_tertiary),
                              "' was not found in the ECCC station inventory."),
                       type = "error", duration = 5)
    } else {
      tertiary_station(station)
      tertiary_data(NULL)  # clear any previously downloaded data for this slot
      showNotification(paste0("Tertiary station set: ", station$name,
                              " (Climate ID: ", station$climate_id, ")."),
                       type = "message", duration = 3)
    }
  })

  # Data frame required for the map plotting
  map_plot_data <- reactive({
    get_ec_inv(return_sf = FALSE) |>
      rename(lat_deg = 'Latitude (Decimal Degrees)', lon_deg = 'Longitude (Decimal Degrees)') |>
      # filter by province/territory
      filter(Province == input$province_selector) |>
      # calculate how many years of data available
      mutate( DLY_length = `DLY Last Year` - `DLY First Year`) |>
      # format the data frame for pop up with Select buttons
      mutate(text = paste(sep = "<br/>", 
          paste("<b>", Name, "</b>"), 
          paste0("Climate ID : ", `Climate ID`),
          paste0("Station ID: ", `Station ID`, " | WMO ID: ", `WMO ID`, " | TC ID: ", `TC ID`),
          paste0("Elevation (m) : ", `Elevation (m)`),
          paste0("Hourly Record: ", `HLY First Year`, " to ", `HLY Last Year`, " (", `HLY Last Year`-`HLY First Year`, " Yrs)"),
          paste0("Daily Record: ", `DLY First Year`, " to ", `DLY Last Year`, " (", `DLY Last Year`-`DLY First Year`, " Yrs)"),
          paste0("Monthly Record: ", `MLY First Year`, " to ", `MLY Last Year`, " (", `MLY Last Year`-`MLY First Year`, " Yrs)"),
          paste0("<button onclick='Shiny.setInputValue(\"map_primary_select\", \"", `Climate ID`, "\", {priority: \"event\"})' class='btn btn-primary btn-sm' style='background-color: magenta; color: white;'>Primary</button> ",
             "<button onclick='Shiny.setInputValue(\"map_secondary_select\", \"", `Climate ID`, "\", {priority: \"event\"})' class='btn btn-success btn-sm'>Secondary</button> ",
             "<button onclick='Shiny.setInputValue(\"map_tertiary_select\", \"", `Climate ID`, "\", {priority: \"event\"})' class='btn btn-primary btn-sm' style='background-color: darkblue; color: white;'>Tertiary</button>")
          )
      )
  })

  
  # Leaflet map rendering
  output$MapPlot <- renderLeaflet({

    # Create color categories based on daily record length
    map_data_with_colors <- map_plot_data() |>
      mutate(
        color_category = case_when(
          DLY_length < 10 ~ "red",
          DLY_length >= 10 & DLY_length < 20 ~ "orange", 
          DLY_length >= 20 & DLY_length < 40 ~ "yellow",
          DLY_length >= 40 & DLY_length < 60 ~ "green",
          DLY_length >= 60 ~ "blue",
          TRUE ~ "gray"
        )
      )

    # Create awesome icons for the markers, text indicate last year of daily record
    icons <- awesomeIcons(
      icon = "stats",
      iconColor = "white",
      library = "fa",
      markerColor = map_data_with_colors$color_category,
      text = map_data_with_colors$`DLY Last Year`
    )

    # change the zoom based on each province/territory
    # Create a lookup table for province view extents
    view_extent <- data.frame(
      province = c("BRITISH COLUMBIA", "ALBERTA", "SASKATCHEWAN", "MANITOBA", 
      "ONTARIO", "QUEBEC", "NEW BRUNSWICK", "NOVA SCOTIA", 
      "PRINCE EDWARD ISLAND", "NEWFOUNDLAND", "NORTHWEST TERRITORIES",
      "NUNAVUT", "YUKON TERRITORY"),
      lng = c(-126.545, -114.995, -105.675, -95.485, 
        -84.75, -68.43, -66.415, -62.995, 
        -63.19, -60.205, -117.706, -95.000, -135.000),
      lat = c(54.15, 54.495, 54.495, 54.495, 
        49.26, 53.79, 46.335, 45.225, 
        46.505, 53.49, 64.825, 70.000, 64.000),
      zoom = c(5, 5, 5, 5, 5, 4, 7, 7, 8, 5, 4, 3, 5)
    ) |> 
      filter(province == input$province_selector)
    

    map_data_with_colors |>
      drop_na(lon_deg, lat_deg) |>
      filter(between(lon_deg, -142, -51), # East to west boundary of Canada
             between(lat_deg, 41, 84) # South to north boundary of Canada
      ) |>
      leaflet() |>
      addTiles() |>
      # default zoom to BC
      setView(lng = view_extent$lng, lat = view_extent$lat, zoom = view_extent$zoom) |>
      addAwesomeMarkers(~lon_deg, ~lat_deg, icon = icons, popup = ~text, clusterOptions = markerClusterOptions(maxClusterRadius = 10)) |>
      # Add legend
      addLegend(
        position = "bottomright",
        colors = c("red", "orange", "yellow", "green", "blue"),
        labels = c("< 10 years", "10-20 years", "20-40 years", "40-60 years", "60+ years"),
        title = paste("Daily Record Length",
                 "<small>Number on marker = <br/>
                 Last year of daily record</small>", 
               sep = "<br/>"),
        opacity = 1
      ) |>
      addMeasure(
        position = "topright",
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "meters",
        primaryAreaUnit = "hectares",
        secondaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      )

  }) # End of Leaflet map rendering
  
  
  #* Data Download Tab --------------------------------
  
  # Render station download controls dynamically
  output$station_download_controls <- renderUI({
    controls <- list()
    
    # Primary Station Controls
    if(!is.null(primary_station())) {
      station <- primary_station()
      has_data <- switch(input$global_data_interval,
        "hourly" = !is.na(station$hly_first) && !is.na(station$hly_last),
        "daily" = !is.na(station$dly_first) && !is.na(station$dly_last),
        "monthly" = !is.na(station$mly_first) && !is.na(station$mly_last)
      )
      
      if(has_data) {
        date_range <- get_date_range(station, input$global_data_interval)
        
        controls <- append(controls, list(
          div(style = "border: 2px solid magenta; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
            h5("Primary Station Data Download", style = "color: magenta; margin-top: 0;"),
            fluidRow(
              column(6,
                dateRangeInput("primary_date_range", "Date Range:",
                              start = date_range[1],
                              end = date_range[2],
                              min = date_range[1],
                              max = date_range[2])
              ),
              column(6,
                br(),
                actionButton("download_primary_data", "Retrieve Data from ECCC", 
                            class = "btn-primary", 
                            style = "background-color: magenta; border-color: magenta;")
              )
            )
          )
        ))
      } else {
        controls <- append(controls, list(
          div(style = "border: 2px solid magenta; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
            h5("Primary Station Data Download", style = "color: magenta; margin-top: 0;"),
            p(paste("No", input$global_data_interval, "interval data available for this station."), 
              style = "color: #888; font-style: italic;")
          )
        ))
      }
    }
    
    # Secondary Station Controls
    if(!is.null(secondary_station())) {
      station <- secondary_station()
      has_data <- switch(input$global_data_interval,
        "hourly" = !is.na(station$hly_first) && !is.na(station$hly_last),
        "daily" = !is.na(station$dly_first) && !is.na(station$dly_last),
        "monthly" = !is.na(station$mly_first) && !is.na(station$mly_last)
      )
      
      if(has_data) {
        date_range <- get_date_range(station, input$global_data_interval)
        
        controls <- append(controls, list(
          div(style = "border: 2px solid #006400; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
            h5("Secondary Station Data Download", style = "color: #006400; margin-top: 0;"),
            fluidRow(
              column(6,
                dateRangeInput("secondary_date_range", "Date Range:",
                              start = date_range[1],
                              end = date_range[2],
                              min = date_range[1],
                              max = date_range[2])
              ),
              column(6,
                br(),
                actionButton("download_secondary_data", "Retrieve Data from ECCC", class = "btn-success")
              )
            )
          )
        ))
      } else {
        controls <- append(controls, list(
          div(style = "border: 2px solid #006400; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
            h5("Secondary Station Data Download", style = "color: #006400; margin-top: 0;"),
            p(paste("No", input$global_data_interval, "interval data available for this station."), 
              style = "color: #888; font-style: italic;")
          )
        ))
      }
    }
    
    # Tertiary Station Controls
    if(!is.null(tertiary_station())) {
      station <- tertiary_station()
      has_data <- switch(input$global_data_interval,
        "hourly" = !is.na(station$hly_first) && !is.na(station$hly_last),
        "daily" = !is.na(station$dly_first) && !is.na(station$dly_last),
        "monthly" = !is.na(station$mly_first) && !is.na(station$mly_last)
      )
      
      if(has_data) {
        date_range <- get_date_range(station, input$global_data_interval)
        
        controls <- append(controls, list(
          div(style = "border: 2px solid blue; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
            h5("Tertiary Station Data Download", style = "color: darkblue; margin-top: 0;"),
            fluidRow(
              column(6,
                dateRangeInput("tertiary_date_range", "Date Range:",
                              start = date_range[1],
                              end = date_range[2],
                              min = date_range[1],
                              max = date_range[2])
              ),
              column(6,
                br(),
                actionButton("download_tertiary_data", "Retrieve Data from ECCC", 
                            class = "btn-primary", 
                            style = "background-color: darkblue; color: white;")
              )
            )
          )
        ))
      } else {
        controls <- append(controls, list(
          div(style = "border: 2px solid blue; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
            h5("Tertiary Station Data Download", style = "color: darkblue; margin-top: 0;"),
            p(paste("No", input$global_data_interval, "interval data available for this station."), 
              style = "color: #888; font-style: italic;")
          )
        ))
      }
    }
    
    if(length(controls) == 0) {
      return(p("Please select stations from the map to enable data download."))
    }
    
    return(controls)
  })
  
  # Helper function to get date range based on station and interval
  get_date_range <- function(station, interval) {
    # Handle NA values by providing reasonable defaults
    tryCatch({
      switch(interval,
        "hourly" = {
          if(is.na(station$hly_first) || is.na(station$hly_last)) {
            c(as.Date("1900-01-01"), as.Date("2023-12-31"))  # Default range
          } else {
            c(as.Date(paste0(station$hly_first, "-01-01")), 
              as.Date(paste0(station$hly_last, "-12-31")))
          }
        },
        "daily" = {
          if(is.na(station$dly_first) || is.na(station$dly_last)) {
            c(as.Date("1900-01-01"), as.Date("2023-12-31"))  # Default range
          } else {
            c(as.Date(paste0(station$dly_first, "-01-01")), 
              as.Date(paste0(station$dly_last, "-12-31")))
          }
        },
        "monthly" = {
          if(is.na(station$mly_first) || is.na(station$mly_last)) {
            c(as.Date("1900-01-01"), as.Date("2023-12-31"))  # Default range
          } else {
            c(as.Date(paste0(station$mly_first, "-01-01")), 
              as.Date(paste0(station$mly_last, "-12-31")))
          }
        }
      )
    }, error = function(e) {
      # Fallback to default range if any error occurs
      c(as.Date("1900-01-01"), as.Date("2023-12-31"))
    })
  }
  
  # Render the download preview table
  # Download event handlers
  observeEvent(input$download_primary_data, {
    req(primary_station(), input$primary_date_range, input$global_data_interval)
    
    station <- primary_station()
    date_range <- input$primary_date_range
    
    # Validate date range - don't allow future dates
    if(date_range[2] > Sys.Date()) {
      showNotification("End date cannot be in the future. Adjusting to today's date.", type = "warning", duration = 3)
      date_range[2] <- Sys.Date()
    }
    
    # Convert date range to ISO8601 format for API
    dt1 <- paste0(format(date_range[1], "%Y-%m-%d"), "T00:00:00Z")
    dt2 <- paste0(format(date_range[2], "%Y-%m-%d"), "T23:59:59Z")
    
    # Show loading message
    showNotification("Downloading primary station data...", type = "default", duration = NULL, id = "download_primary")
    
    tryCatch({
      data <- get_ec_met(
        station_id = station$climate_id,
        res = input$global_data_interval,
        dt1 = dt1,
        dt2 = dt2
      )
      
      # Check if data is empty
      if(is.null(data) || nrow(data) == 0) {
        showNotification("No data available for the selected date range and station.", type = "warning", duration = 5)
      } else {
        primary_data(data)
        showNotification(paste("Primary station data downloaded successfully!", nrow(data), "records retrieved."), type = "message", duration = 3)
      }
    }, error = function(e) {
      showNotification(paste("Error downloading primary data:", e$message), type = "error", duration = 5)
    })
    
    removeNotification("download_primary")
  })
  
  observeEvent(input$download_secondary_data, {
    req(secondary_station(), input$secondary_date_range, input$global_data_interval)
    
    station <- secondary_station()
    date_range <- input$secondary_date_range
    
    # Validate date range
    if(date_range[2] > Sys.Date()) {
      showNotification("End date cannot be in the future. Adjusting to today's date.", type = "warning", duration = 3)
      date_range[2] <- Sys.Date()
    }
    
    dt1 <- paste0(format(date_range[1], "%Y-%m-%d"), "T00:00:00Z")
    dt2 <- paste0(format(date_range[2], "%Y-%m-%d"), "T23:59:59Z")
    
    showNotification("Downloading secondary station data...", type = "default", duration = NULL, id = "download_secondary")
    
    tryCatch({
      data <- get_ec_met(
        station_id = station$climate_id,
        res = input$global_data_interval,
        dt1 = dt1,
        dt2 = dt2
      )
      
      if(is.null(data) || nrow(data) == 0) {
        showNotification("No data available for the selected date range and station.", type = "warning", duration = 5)
      } else {
        secondary_data(data)
        showNotification(paste("Secondary station data downloaded successfully!", nrow(data), "records retrieved."), type = "message", duration = 3)
      }
    }, error = function(e) {
      showNotification(paste("Error downloading secondary data:", e$message), type = "error", duration = 5)
    })
    
    removeNotification("download_secondary")
  })
  
  observeEvent(input$download_tertiary_data, {
    req(tertiary_station(), input$tertiary_date_range, input$global_data_interval)
    
    station <- tertiary_station()
    date_range <- input$tertiary_date_range
    
    # Validate date range
    if(date_range[2] > Sys.Date()) {
      showNotification("End date cannot be in the future. Adjusting to today's date.", type = "warning", duration = 3)
      date_range[2] <- Sys.Date()
    }
    
    dt1 <- paste0(format(date_range[1], "%Y-%m-%d"), "T00:00:00Z")
    dt2 <- paste0(format(date_range[2], "%Y-%m-%d"), "T23:59:59Z")
    
    showNotification("Downloading tertiary station data...", type = "default", duration = NULL, id = "download_tertiary")
    
    tryCatch({
      data <- get_ec_met(
        station_id = station$climate_id,
        res = input$global_data_interval,
        dt1 = dt1,
        dt2 = dt2
      )
      
      if(is.null(data) || nrow(data) == 0) {
        showNotification("No data available for the selected date range and station.", type = "warning", duration = 5)
      } else {
        tertiary_data(data)
        showNotification(paste("Tertiary station data downloaded successfully!", nrow(data), "records retrieved."), type = "message", duration = 3)
      }
    }, error = function(e) {
      showNotification(paste("Error downloading tertiary data:", e$message), type = "error", duration = 5)
    })
    
    removeNotification("download_tertiary")
  })

  # Data preview table
  output$download_preview <- DT::renderDataTable({
    # Get all downloaded data
    downloaded_data <- all_downloaded_data()
    data_list <- list()
    
    if(!is.null(downloaded_data$primary)) {
      data_list[["Primary"]] <- downloaded_data$primary |> 
        mutate(Selection = "Primary", .before = 1) |>
        mutate(across(everything(), as.character))  # Convert all to character for consistent binding
    }
    if(!is.null(downloaded_data$secondary)) {
      data_list[["Secondary"]] <- downloaded_data$secondary |> 
        mutate(Selection = "Secondary", .before = 1) |>
        mutate(across(everything(), as.character))
    }
    if(!is.null(downloaded_data$tertiary)) {
      data_list[["Tertiary"]] <- downloaded_data$tertiary |> 
        mutate(Selection = "Tertiary", .before = 1) |>
        mutate(across(everything(), as.character))
    }
    
    if(length(data_list) == 0) {
      return(data.frame(Message = "No data downloaded yet"))
    }
    
    # Combine all data - now all columns are character type
    combined_data <- bind_rows(data_list)
    
    DT::datatable(
      combined_data,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        scrollY = 400,
        pageLength = 100,
        serverSide = TRUE,
        processing = TRUE,
        dom = 'Bfrtip',
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        buttons = list('colvis')
      )
    )
  })

  # Custom download handler for CSV export
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("Climate_Data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # Get all downloaded data
      downloaded_data <- all_downloaded_data()
      data_list <- list()
      
      if(!is.null(downloaded_data$primary)) {
        data_list[["Primary"]] <- downloaded_data$primary |> 
          mutate(Selection = "Primary", .before = 1) |>
          mutate(across(everything(), as.character))  # Convert all to character for consistent binding
      }
      if(!is.null(downloaded_data$secondary)) {
        data_list[["Secondary"]] <- downloaded_data$secondary |> 
          mutate(Selection = "Secondary", .before = 1) |>
          mutate(across(everything(), as.character))
      }
      if(!is.null(downloaded_data$tertiary)) {
        data_list[["Tertiary"]] <- downloaded_data$tertiary |> 
          mutate(Selection = "Tertiary", .before = 1) |>
          mutate(across(everything(), as.character))
      }
      
      if(length(data_list) > 0) {
        combined_data <- bind_rows(data_list)
        write.csv(combined_data, file, row.names = FALSE)
      }
    }
  )

  # Custom download handler for Excel export
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("Climate_Data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      # Get all downloaded data
      downloaded_data <- all_downloaded_data()
      data_list <- list()
      
      if(!is.null(downloaded_data$primary)) {
        data_list[["Primary"]] <- downloaded_data$primary |> 
          mutate(Selection = "Primary", .before = 1) |>
          mutate(across(everything(), as.character))  # Convert all to character for consistent binding
      }
      if(!is.null(downloaded_data$secondary)) {
        data_list[["Secondary"]] <- downloaded_data$secondary |> 
          mutate(Selection = "Secondary", .before = 1) |>
          mutate(across(everything(), as.character))
      }
      if(!is.null(downloaded_data$tertiary)) {
        data_list[["Tertiary"]] <- downloaded_data$tertiary |> 
          mutate(Selection = "Tertiary", .before = 1) |>
          mutate(across(everything(), as.character))
      }
      
      if(length(data_list) > 0) {
        combined_data <- bind_rows(data_list)
        if(requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(combined_data, file)
        } else {
          # Fallback to CSV if openxlsx not available
          write.csv(combined_data, gsub("\\.xlsx$", ".csv", file), row.names = FALSE)
        }
      }
    }
  )

  #* Data Review Tab --------------------------------
  #* Time Series Review subtab ------------------------------

  # Column available for plotting, add primary, secondary, tertiary station data
  
  # Reactive function to create combined dataset with prefixed columns
  combined_station_data <- reactive({
    # Get all downloaded data
    downloaded_data <- all_downloaded_data()
    
    # Check if at least one station has data (not all three required)
    if(is.null(downloaded_data$primary) && is.null(downloaded_data$secondary) && is.null(downloaded_data$tertiary)) {
      return(NULL)
    }
    
    # Start with an empty combined dataset
    combined <- NULL
    
    # Add primary data if available
    if(!is.null(downloaded_data$primary) && nrow(downloaded_data$primary) > 0) {
      primary_prefixed <- downloaded_data$primary |>
        remove_non_plottable() |>
        rename_with(~ paste0("primary_", .), .cols = -any_of(c("LOCAL_DATE")))
      combined <- primary_prefixed
    }
    
    # Add secondary data if available
    if(!is.null(downloaded_data$secondary) && nrow(downloaded_data$secondary) > 0) {
      secondary_prefixed <- downloaded_data$secondary |>
        remove_non_plottable() |>
        rename_with(~ paste0("secondary_", .), .cols = -any_of(c("LOCAL_DATE")))
      
      if(is.null(combined)) {
        combined <- secondary_prefixed
      } else {
        combined <- full_join(combined, secondary_prefixed, by = "LOCAL_DATE")
      }
    }
    
    # Add tertiary data if available
    if(!is.null(downloaded_data$tertiary) && nrow(downloaded_data$tertiary) > 0) {
      tertiary_prefixed <- downloaded_data$tertiary |>
        remove_non_plottable() |>
        rename_with(~ paste0("tertiary_", .), .cols = -any_of(c("LOCAL_DATE")))
      
      if(is.null(combined)) {
        combined <- tertiary_prefixed
      } else {
        combined <- full_join(combined, tertiary_prefixed, by = "LOCAL_DATE")
      }
    }
    
    if(!is.null(combined)) {
      combined <- combined |> arrange(LOCAL_DATE)
    }
    
    return(combined)
  })

  # ===== Advanced Usage: ERA5-Land infilling ================================
  era5_infill_result <- reactiveVal(NULL)

  # Sidebar button -> advanced-usage warning modal
  observeEvent(input$run_era5_infill, {
    showModal(modalDialog(
      title = "Advanced Usage - ERA5-Land Infilling",
      HTML(paste0(
        "<p>This <b>advanced</b> tool downloads ERA5-Land reanalysis and bias-corrects it ",
        "(Quantile Delta Mapping) to fill gaps in the <b>primary</b> station's <b>daily</b> record.</p>",
        "<p>It can take ~15-60 seconds. Results appear in the <b>Advanced Usage (ERA5 Infill)</b> tab.</p>",
        "<p><b>New to this tool?</b> Open the <b>Advanced Usage (ERA5 Infill) &rarr; Instructions</b> ",
        "sub-tab first &mdash; it explains how to register for a CDS API key, how the bias ",
        "correction works, how to read the results, and the limitations/disclaimer.</p>",
        "<p>Enter your own <b>CDS API Key</b> to run it. Your key is used only for this run and ",
        "is <b>not stored on the server</b>.</p>")),
      passwordInput("era5_access_code", "CDS API Key:"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_era5_infill", "Proceed", class = "btn-warning")),
      easyClose = TRUE))
  })

  # Confirm -> run the pipeline with a spinner
  observeEvent(input$confirm_era5_infill, {
    code <- if (is.null(input$era5_access_code)) "" else trimws(input$era5_access_code)
    if (!nzchar(code)) {
      showNotification("Enter your CDS API Key to proceed.", type = "error", duration = 5)
      return()  # keep the modal open
    }
    cds_key <- code  # always the user's own key -- never stored on the server
    removeModal()
    pd <- primary_data()
    if (is.null(pd) || nrow(pd) == 0) {
      showNotification("Set and download a PRIMARY station first.", type = "error", duration = 6); return() }
    if (!is.null(input$global_data_interval) && input$global_data_interval != "daily") {
      showNotification("ERA5 infilling requires the 'daily' data interval.", type = "error", duration = 6); return() }
    # ECCC GeoMet-OGC CSV uses x=lon, y=lat; support LONGITUDE/LATITUDE too
    lon_col <- intersect(c("LONGITUDE", "x", "LON", "Longitude"), names(pd))[1]
    lat_col <- intersect(c("LATITUDE",  "y", "LAT", "Latitude"),  names(pd))[1]
    if (is.na(lon_col) || is.na(lat_col)) {
      showNotification("Primary station data lacks coordinate columns (LATITUDE/LONGITUDE or x/y).", type = "error", duration = 6); return() }

    w <- Waiter$new(html = tagList(spin_3k(),
             tags$h4("Downloading & bias-correcting ERA5-Land...", style = "color:white;")),
             color = "rgba(0,0,0,0.75)")
    w$show(); on.exit(w$hide())
    tryCatch({
      lat <- stats::median(as.numeric(pd[[lat_col]]), na.rm = TRUE)
      lon <- stats::median(as.numeric(pd[[lon_col]]), na.rm = TRUE)
      cid <- primary_station()$climate_id
      off <- station_lst_offset(cid, lon = lon)
      res <- infill_station(pd, lat, lon, off, cds_key = cds_key)
      res$meta <- list(station = primary_station()$name, climate_id = cid,
                       lat = lat, lon = lon, offset = off)
      era5_infill_result(res)
      if (isTRUE(res$pre1950))
        showNotification("Note: this station has pre-1950 data, which ERA5-Land cannot infill.",
                         type = "warning", duration = 8)
      if (!isTRUE(res$cv$sufficient))
        showNotification(sprintf("Station overlap %.1f yr / too few complete months for a held-out validation: bias-correction used all overlap; no validation split.",
                                 res$cv$years), type = "warning", duration = 9)
      showNotification("ERA5 infilling complete - see the Advanced Usage (ERA5 Infill) tab.",
                       type = "message", duration = 6)
      updateTabsetPanel(session, "main_tabs", selected = "advanced_usage")
      updateTabsetPanel(session, "advanced_subtabs", selected = "CDF Review - Calibration")
    }, error = function(e)
      showNotification(paste("ERA5 infilling failed:", conditionMessage(e)),
                       type = "error", duration = 12))
  })

  # Restrict the Advanced-Usage variable pickers to the variables actually infilled -- a
  # precip-only station has no temperature columns, so don't offer them.
  observeEvent(era5_infill_result(), {
    res <- era5_infill_result(); if (is.null(res)) return()
    present <- intersect(c("MIN_TEMPERATURE", "MAX_TEMPERATURE", "MEAN_TEMPERATURE", "TOTAL_PRECIPITATION"),
                         names(res$data))
    if (!length(present)) return()
    lbl_long  <- c(MIN_TEMPERATURE = "Min Temperature", MAX_TEMPERATURE = "Max Temperature",
                   MEAN_TEMPERATURE = "Mean Temperature", TOTAL_PRECIPITATION = "Total Precipitation")
    lbl_short <- c(MIN_TEMPERATURE = "Min Temp", MAX_TEMPERATURE = "Max Temp",
                   MEAN_TEMPERATURE = "Mean Temp", TOTAL_PRECIPITATION = "Precip")
    sel <- if ("MEAN_TEMPERATURE" %in% present) "MEAN_TEMPERATURE" else present[1]
    for (id in c("era5_cdf_var_calib", "era5_cdf_var_valid", "infill_ts_var"))
      updateRadioButtons(session, id, choices = setNames(present, lbl_long[present]),
                         selected = sel, inline = TRUE)
    updateRadioButtons(session, "infill_summary_var", choices = setNames(present, lbl_short[present]),
                       selected = sel, inline = TRUE)
  })

  # Status line in the Advanced Usage tab
  output$era5_infill_status <- renderUI({
    res <- era5_infill_result()
    if (is.null(res))
      return(helpText("No infilling run yet. Set & download a PRIMARY station (daily interval), then click 'Run ERA5 Infilling' in the sidebar."))
    s <- res$data; m <- res$meta
    # infilled fraction from whichever variable is present (precip-only stations have no
    # temperature source column, so fall back to the first available *_source column)
    src_col <- grep("_source$", names(s), value = TRUE)[1]
    src <- if (is.na(src_col)) character(0) else s[[src_col]]
    # QAQC removals across all checked variables (from qaqc_summary flag counts)
    cats    <- c("range", "consistency", "spike", "outlier", "flatline")
    cat_tot <- sapply(cats, function(k) sum(sapply(res$qaqc_summary,
                        function(t) if (k %in% names(t)) as.integer(t[k]) else 0L)))
    tags$div(class = "well", style = "padding:10px;",
      tags$p(tags$b("Station: "), m$station,
             sprintf(" (Climate ID %s) - %.3f, %.3f, LST offset %+d h", m$climate_id, m$lat, m$lon, m$offset)),
      if (!is.null(res$era5_grid)) {
        km <- as.numeric(res$era5_grid["km"]); far <- is.finite(km) && km > 10
        tags$p(style = if (far) "color:#b8860b;" else NULL,
          tags$b("ERA5 grid cell: "),
          sprintf("%.3f, %.3f (%.1f km from station)",
                  res$era5_grid["lat"], res$era5_grid["lon"], km),
          if (far) " - coastal/island station snapped to the nearest ERA5-Land (land) cell; check representativeness." else NULL)
      } else NULL,
      tags$p(tags$b("Record: "), paste(min(s$date), "to", max(s$date)),
             sprintf(" - %d days; ERA5-infilled %d (%.1f%%).",
                     nrow(s), sum(src == "ERA5 bias-corrected", na.rm = TRUE),
                     100 * mean(src == "ERA5 bias-corrected", na.rm = TRUE))),
      tags$p(tags$b("QAQC removed: "),
             sprintf("%d flagged value(s) [range %d, consistency %d, spike %d, outlier %d, flatline %d]",
                     sum(cat_tot), cat_tot["range"], cat_tot["consistency"], cat_tot["spike"],
                     cat_tot["outlier"], cat_tot["flatline"])),
      if (isTRUE(res$pre1950)) tags$p(style = "color:#b8860b;", tags$b("Note: "),
        "this station has observations before 1950; ERA5-Land begins in 1950, so pre-1950 gaps are NOT infilled.") else NULL,
      downloadButton("download_infilled", "Download infilled data (Excel)", class = "btn-success btn-sm"))
  })

  # Excel export: one sheet per parameter (Date, value, Data_Source)
  output$download_infilled <- downloadHandler(
    filename = function() {
      res <- era5_infill_result()
      nm  <- if (!is.null(res)) gsub("[^A-Za-z0-9]+", "_", res$meta$station) else "station"
      paste0("infilled_", nm, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      res <- era5_infill_result(); req(res)
      s <- res$data
      vars <- intersect(c("MIN_TEMPERATURE", "MAX_TEMPERATURE", "MEAN_TEMPERATURE", "TOTAL_PRECIPITATION"), names(s))
      src_map <- c("ECCC observed" = "ECCC", "ERA5 bias-corrected" = "ERA5-Land")
      wb <- openxlsx::createWorkbook()
      for (v in vars) {
        openxlsx::addWorksheet(wb, v)
        d <- data.frame(Date = s$date, value = s[[v]],
                        Data_Source = unname(src_map[s[[paste0(v, "_source")]]]),
                        stringsAsFactors = FALSE)
        names(d)[2] <- v
        openxlsx::writeData(wb, v, d)
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # 12-month CDF review plots: calibration = most-recent years; validation = first 2 years
  .cdf_range_ui <- function(cv, which) {
    if (which == "calib") {
      if (isTRUE(cv$sufficient))
        tags$p(tags$b("Calibration: "),
               sprintf("%d complete months over %s to %s (~%.1f-year overlap, less the %d held-out validation months).",
                       cv$calib_months, cv$calib_start, cv$calib_end, cv$years, cv$valid_months))
      else
        tags$p(tags$b("Calibration: "),
               sprintf("%s to %s (%.1f years) - full overlap used; too little data for a validation split.",
                       cv$calib_start, cv$calib_end, cv$years))
    } else {
      if (isTRUE(cv$sufficient))
        tags$p(tags$b("Validation: "),
               sprintf("%d RANDOM complete months (~%d of each calendar month), held out from fitting, spanning %s to %s.",
                       cv$valid_months, cv$valid_years, cv$valid_start, cv$valid_end))
      else
        tags$p(style = "color:#b8860b;", tags$b("Validation: "),
               sprintf("not available - need >= 7 years overlap and >= 24 complete months (this station: %.1f years).", cv$years))
    }
  }
  output$cdf_calib_range <- renderUI({ res <- era5_infill_result(); req(res); .cdf_range_ui(res$cv, "calib") })
  output$cdf_valid_range <- renderUI({ res <- era5_infill_result(); req(res); .cdf_range_ui(res$cv, "valid") })

  output$era5_cdf_plot_calib <- renderPlotly({
    res <- era5_infill_result(); req(res)
    v <- input$era5_cdf_var_calib
    if (is.null(res$bc[[v]])) return(plotly_empty() |> layout(title = paste("No detail for", v)))
    cdf_monthly_plotly(res$bc[[v]], v, set = "calibration")
  })
  output$era5_cdf_plot_valid <- renderPlotly({
    res <- era5_infill_result(); req(res)
    v <- input$era5_cdf_var_valid
    if (is.null(res$bc[[v]])) return(plotly_empty() |> layout(title = paste("No detail for", v)))
    if (!isTRUE(res$cv$sufficient))
      return(plotly_empty() |> layout(title = "Validation not available (< 7 years overlap)"))
    cdf_monthly_plotly(res$bc[[v]], v, set = "validation")
  })

  # Infilled daily time series (observed vs ERA5-infilled)
  output$infill_ts_plot <- renderPlotly({
    res <- era5_infill_result(); req(res)
    v <- input$infill_ts_var; s <- res$data
    if (is.null(s[[v]])) return(plotly_empty() |> layout(title = paste("No data for", v)))
    df   <- data.frame(date = s$date, value = s[[v]], source = s[[paste0(v, "_source")]])
    obs  <- df[!is.na(df$value) & df$source == "ECCC observed", ]
    fill <- df[!is.na(df$value) & df$source == "ERA5 bias-corrected", ]
    ylab <- if (grepl("PRECIP", v)) "mm/day" else "°C"
    plot_ly() |>
      add_lines(data = df, x = ~date, y = ~value, name = "Infilled series",
                line = list(color = "rgba(140,140,140,0.45)", width = 1), hoverinfo = "skip") |>
      add_markers(data = obs, x = ~date, y = ~value, name = "ECCC observed",
                  marker = list(color = "black", size = 3)) |>
      add_markers(data = fill, x = ~date, y = ~value, name = "ERA5 bias-corrected (infill)",
                  marker = list(color = "#ff7f0e", size = 4)) |>
      layout(title = list(text = paste("Infilled daily series -", v), x = 0.02, y = 0.99,
                          yanchor = "top", font = list(size = 13)),
             xaxis = list(title = "Date"), yaxis = list(title = ylab),
             hovermode = "x unified",
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, yanchor = "top"),
             margin = list(t = 44, b = 66))
  })

  # Infilled summary data (shared by the summary plot and table)
  infill_summary_data <- reactive({
    res <- era5_infill_result(); req(res)
    s <- res$data
    vars <- intersect(c("MIN_TEMPERATURE", "MAX_TEMPERATURE", "MEAN_TEMPERATURE", "TOTAL_PRECIPITATION"), names(s))
    df <- s[, c("date", vars)]; names(df)[1] <- "LOCAL_DATE"
    period <- if (is.null(input$infill_summary_period)) "yearly" else input$infill_summary_period
    keys <- if (period == "monthly") c("year", "month") else "year"
    # infilled series is gap-free within the record, so no minimum-count threshold applies
    summ <- function(cols, stats) {
      if (!length(cols)) return(NULL)
      if (period == "monthly") summarize_monthly(df, cols, stats, min_days = 1)
      else                     summarize_yearly(df, cols, stats, min_months = 1)
    }
    t_sum <- summ(vars[grepl("TEMP",   vars)], c("mean", "min", "max"))
    p_sum <- summ(vars[grepl("PRECIP", vars)], c("sum"))
    out <- if (is.null(t_sum)) p_sum else if (is.null(p_sum)) t_sum else
      dplyr::full_join(t_sum, p_sum, by = keys)
    if (is.null(out)) return(NULL)
    out <- out |> dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
    out$plot_date <- if (period == "monthly")
      as.Date(sprintf("%d-%02d-01", out$year, out$month)) else as.Date(sprintf("%d-07-01", out$year))
    attr(out, "period") <- period
    out
  })

  # Summary time-series plot (like the raw-data summary), for the selected variable
  output$infill_summary_plot <- renderPlotly({
    out <- infill_summary_data(); req(out)
    period <- attr(out, "period")
    v <- if (is.null(input$infill_summary_var)) "MEAN_TEMPERATURE" else input$infill_summary_var
    spec <- if (grepl("PRECIP", v))
      list(cols = paste0(v, "_sum"), labs = "Total", ylab = "mm")
    else
      list(cols = paste0(v, c("_mean", "_min", "_max")), labs = c("Mean", "Min", "Max"), ylab = "°C")
    keep <- spec$cols %in% names(out); spec$cols <- spec$cols[keep]; spec$labs <- spec$labs[keep]
    if (!length(spec$cols)) return(plotly_empty() |> layout(title = "No summary data"))
    vlab <- c(MIN_TEMPERATURE = "Daily Min Temp", MAX_TEMPERATURE = "Daily Max Temp",
              MEAN_TEMPERATURE = "Daily Mean Temp", TOTAL_PRECIPITATION = "Total Precip")[[v]]
    plbl <- if (period == "monthly") "Monthly" else "Yearly"
    p <- plot_ly()
    for (i in seq_along(spec$cols)) {
      if (grepl("PRECIP", v))          # precipitation totals as vertical bars
        p <- add_bars(p, x = out$plot_date, y = out[[spec$cols[i]]],
                      name = paste0(plbl, " ", spec$labs[i]), marker = list(color = "#1f77b4"))
      else                             # temperature stats as lines
        p <- add_lines(p, x = out$plot_date, y = out[[spec$cols[i]]],
                       name = paste0(plbl, " ", spec$labs[i]), connectgaps = FALSE)
    }
    layout(p, title = list(text = paste0(plbl, " summary of ", vlab), x = 0.02, font = list(size = 13)),
           xaxis = list(title = "Date"), yaxis = list(title = spec$ylab), hovermode = "x unified")
  })

  # Pretty per-period stats table (shared by the on-screen table and the Excel export):
  # drops the plot helper column, rounds, and gives descriptive column names.
  .infill_summary_pretty <- function(out, period) {
    out$plot_date <- NULL
    out <- out |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
    plbl <- if (period == "monthly") "Monthly" else "Yearly"
    vlbl <- c(MIN_TEMPERATURE = "Daily Min Temp", MAX_TEMPERATURE = "Daily Max Temp",
              MEAN_TEMPERATURE = "Daily Mean Temp")
    slbl <- c(mean = "Mean", min = "Min", max = "Max")
    names(out) <- vapply(names(out), function(cn) {
      if (cn == "year")  return("Year")
      if (cn == "month") return("Month")
      if (startsWith(cn, "TOTAL_PRECIPITATION_")) return(paste0(plbl, " Total Precip (mm)"))
      for (v in names(vlbl)) if (startsWith(cn, paste0(v, "_"))) {
        st <- sub(paste0("^", v, "_"), "", cn)
        return(paste0(plbl, " ", slbl[[st]], " of ", vlbl[[v]], " (°C)"))
      }
      cn
    }, character(1))
    out
  }

  # % of days sourced from ERA5, per parameter group (Temperature / Precipitation). Returns
  # list(overall = 1-row df of overall %, per_period = df with period keys + one % col/group).
  .infill_pct_era5 <- function(res, period) {
    s <- res$data
    d <- data.frame(year  = as.integer(format(s$date, "%Y")),
                    month = as.integer(format(s$date, "%m")))
    keys   <- if (period == "monthly") c("year", "month") else "year"
    groups <- list(Temperature   = intersect(c("MEAN_TEMPERATURE", "MIN_TEMPERATURE", "MAX_TEMPERATURE"), names(s)),
                   Precipitation = intersect("TOTAL_PRECIPITATION", names(s)))
    groups <- groups[vapply(groups, length, 1L) > 0]
    is_era5 <- function(v) { src <- s[[paste0(v, "_source")]]; !is.na(src) & src == "ERA5 bias-corrected" }
    overall <- as.data.frame(lapply(groups, function(gv) round(100 * mean(is_era5(gv[1])), 1)),
                             check.names = FALSE)
    names(overall) <- paste0(names(groups), " (% from ERA5)")
    per <- unique(d[keys]); per <- per[do.call(order, per[keys]), , drop = FALSE]
    for (g in names(groups)) {
      agg <- aggregate(is_era5(groups[[g]][1]), by = d[keys], FUN = function(x) round(100 * mean(x), 1))
      names(agg) <- c(keys, paste0(g, " (% from ERA5)"))
      per <- merge(per, agg, by = keys, all.x = TRUE)
    }
    names(per)[names(per) == "year"]  <- "Year"
    names(per)[names(per) == "month"] <- "Month"
    # merge() re-sorts multi-column keys lexically (1,10,11,2,...); restore chronological order
    per <- per[if (period == "monthly") order(per$Year, per$Month) else order(per$Year), , drop = FALSE]
    list(overall = overall, per_period = per)
  }

  # Infilled summary statistics table (all variables, descriptive column names)
  output$infill_summary_table <- DT::renderDataTable({
    out <- infill_summary_data(); req(out)
    customized_datatable(.infill_summary_pretty(out, attr(out, "period")), bts = TRUE)
  })

  # Excel export: sheet 1 = the per-period summary stats; sheet 2 = % of data from ERA5
  # (overall + per-period), one column per parameter.
  output$download_infill_summary <- downloadHandler(
    filename = function() {
      res <- era5_infill_result()
      nm  <- if (!is.null(res)) gsub("[^A-Za-z0-9]+", "_", res$meta$station) else "station"
      paste0("infilled_summary_", nm, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      res <- era5_infill_result(); req(res)
      out <- infill_summary_data(); req(out)
      period <- attr(out, "period")
      plbl   <- if (period == "monthly") "Monthly" else "Yearly"
      stats  <- .infill_summary_pretty(out, period)
      pct    <- .infill_pct_era5(res, period)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Summary Statistics")
      openxlsx::writeData(wb, "Summary Statistics", stats)
      openxlsx::addWorksheet(wb, "Percent from ERA5")
      openxlsx::writeData(wb, "Percent from ERA5", "Overall % of data from ERA5-Land", startRow = 1)
      openxlsx::writeData(wb, "Percent from ERA5", pct$overall, startRow = 2)
      openxlsx::writeData(wb, "Percent from ERA5", paste(plbl, "% of data from ERA5-Land"),
                          startRow = 4 + nrow(pct$overall))
      openxlsx::writeData(wb, "Percent from ERA5", pct$per_period, startRow = 5 + nrow(pct$overall))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # =========================================================================


  # Column available for plotting, add primary, secondary, tertiary station data


  output$raw_plotly <- renderPlotly({

    validate(
      need(!is.null(combined_station_data()), "No combined station data available")
    )
    
    combined_data <- combined_station_data()
    
    # Get numeric columns only (exclude LOCAL_DATE and the station-ID column, which is a
    # constant identifier, not a measurement)
    numeric_cols <- combined_data |>
      select(-LOCAL_DATE) |>
      select(where(is.numeric)) |>
      names()
    numeric_cols <- numeric_cols[!grepl("STN_ID$", numeric_cols)]

    if(length(numeric_cols) == 0) {
      return(plotly_empty() |> 
        layout(title = "No numeric data available for plotting"))
    }
    
    # Rainbow colours, one per numeric series
    colors <- rainbow(length(numeric_cols))

    # Start with the first trace instead of empty plot
    first_col <- numeric_cols[1]
    
    # Create plot with first trace
    p <- plot_ly(combined_data, x = ~LOCAL_DATE, y = combined_data[[first_col]], 
                 type = 'scatter', mode = 'lines',
                 name = first_col,
                 line = list(color = colors[1]),
                 connectgaps = FALSE)
    
    # Add remaining traces if there are more than one
    if(length(numeric_cols) > 1) {
      for(i in 2:length(numeric_cols)) {
        col <- numeric_cols[i]
        
        p <- p |> add_trace(
          y = combined_data[[col]],
          name = col,
          line = list(color = colors[i]),
          connectgaps = FALSE
        )
      }
    }
    
    # Use the selected y-axis label
    y_label <- if(!is.null(input$y_axis_variable)) input$y_axis_variable else "Value"
    
    p |> layout(
      title = "Climate Station Data Time Series",
      xaxis = list(title = "Date"),
      yaxis = list(title = y_label),
      hovermode = 'x unified',
      showlegend = TRUE
    )
  })

  # Update summary variable choices based on available data
  observe({
    combined_data <- combined_station_data()
    
    if(!is.null(combined_data)) {
      # Get numeric columns only (exclude LOCAL_DATE)
      numeric_cols <- combined_data |>
        select(-LOCAL_DATE) |>
        select(where(is.numeric)) |>
        names()
      
      # Create named list for selectizeInput
      choices <- setNames(numeric_cols, numeric_cols)
      
      updateSelectizeInput(session, "summary_variable",
                          choices = choices,
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
    } else {
      updateSelectizeInput(session, "summary_variable",
                          choices = NULL)
    }
  })

  #* Summary Table subtab ------------------------------

  # Observe plotly selection events for data removal
  observeEvent(event_data("plotly_selected", source = "summary_plot"), {
    selected_data <- event_data("plotly_selected", source = "summary_plot")
    
    # Add validation to prevent errors with missing values
    if(!is.null(selected_data) && is.data.frame(selected_data) && nrow(selected_data) > 0) {
      # Check if required columns exist
      if(all(c("curveNumber", "pointNumber") %in% names(selected_data))) {
        # Get current removed points
        current_removed <- removed_points()
        
        # Extract the selected points information and ensure they are valid
        new_removed <- selected_data %>%
          select(curveNumber, pointNumber) %>%
          filter(!is.na(curveNumber) & !is.na(pointNumber)) %>%
          mutate(
            curveNumber = as.numeric(curveNumber),
            pointNumber = as.numeric(pointNumber)
          ) %>%
          filter(!is.na(curveNumber) & !is.na(pointNumber))
        
        if(nrow(new_removed) > 0) {
          # Combine with existing removed points
          all_removed <- rbind(current_removed, new_removed)
          
          # Remove duplicates
          all_removed <- unique(all_removed)
          
          # Update the reactive value
          removed_points(all_removed)
          
          showNotification(paste("Removed", nrow(new_removed), "data points. Total removed:", nrow(all_removed)), 
                          type = "message", duration = 3)
        }
      }
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Button to clear removed points
  observeEvent(input$clear_removed_points, {
    removed_points(data.frame())
    showNotification("Cleared all removed data points", type = "message", duration = 2)
  })

  # create summary plot
  # Placeholder for the summary plot that still declares the "summary_plot"
  # source and registers plotly_selected, so the observer above never warns
  # about an unregistered event when the plot is empty or errored.
  empty_summary_plot <- function(title) {
    plotly_empty(source = "summary_plot") |>
      layout(title = title) |>
      event_register("plotly_selected")
  }

  output$raw_summary_plot <- renderPlotly({
    combined_data <- combined_station_data()
    
    # Validation checks
    if(is.null(combined_data)) {
      return(empty_summary_plot("No data available for summary analysis"))
    }
    
    if(input$global_data_interval != "daily") {
      return(empty_summary_plot("Summary functionality is only available for daily data intervals"))
    }
    
    # Require user inputs - note the correct input names from UI
    req(input$summary_variable, input$summary_period, input$summary_statistic)
    
    # Get selected variables and statistics
    selected_vars <- input$summary_variable
    selected_period <- input$summary_period
    selected_stats <- input$summary_statistic
    
    # Validate inputs
    if(length(selected_vars) == 0 || length(selected_stats) == 0) {
      return(empty_summary_plot("Please select variables and statistics"))
    }
    
    # Filter combined_data to only include selected variables and LOCAL_DATE
    plot_data <- combined_data |> 
      select(LOCAL_DATE, all_of(selected_vars))
    
    # Apply date range filtering if provided
    if(!is.null(input$summary_date_range) && length(input$summary_date_range) == 2) {
      date_start <- input$summary_date_range[1]
      date_end <- input$summary_date_range[2]
      
      # Validate date range
      if(!is.na(date_start) && !is.na(date_end) && date_start <= date_end) {
        plot_data <- plot_data |>
          filter(LOCAL_DATE >= date_start & LOCAL_DATE <= date_end)
        
        # Check if any data remains after filtering
        if(nrow(plot_data) == 0) {
          return(empty_summary_plot("No data available for the selected date range"))
        }
      }
    }
    
    # Apply summarization based on selected period
    tryCatch({
      if(selected_period == "monthly") {
        min_days <- if(!is.null(input$min_days_monthly)) input$min_days_monthly else 25
        summary_data <- summarize_monthly(plot_data, selected_vars, selected_stats, min_days = min_days)
        
        # Create date column for plotting
        summary_data <- summary_data |>
          mutate(date = as.Date(paste(year, month, "01", sep = "-")))
        
      } else if(selected_period == "yearly") {
        min_days <- if(!is.null(input$min_days_yearly)) input$min_days_yearly else 300
        summary_data <- summarize_yearly(plot_data, selected_vars, selected_stats, min_months = min_days)
        
        # Create date column for plotting
        summary_data <- summary_data |>
          mutate(date = as.Date(paste(year, "07", "01", sep = "-")))  # Use July 1st as mid-year
      }
      
      # Apply data removal based on user selections
      removed <- removed_points()
      if(!is.null(removed) && nrow(removed) > 0) {
        # Create a mapping of trace indices to variable-statistic combinations
        trace_mapping <- data.frame()
        trace_index <- 0
        
        for(var in selected_vars) {
          for(stat in selected_stats) {
            col_name <- paste0(var, "_", stat)
            if(col_name %in% names(summary_data)) {
              trace_mapping <- rbind(trace_mapping, data.frame(
                trace_index = trace_index,
                col_name = col_name,
                var = var,
                stat = stat
              ))
              trace_index <- trace_index + 1
            }
          }
        }
        
        # Remove selected points with additional validation
        for(i in 1:nrow(removed)) {
          curve_num <- removed$curveNumber[i]
          point_num <- removed$pointNumber[i]
          
          # Validate curve_num and point_num are not NA
          if(!is.na(curve_num) && !is.na(point_num)) {
            # Find corresponding column
            matching_trace <- trace_mapping[trace_mapping$trace_index == curve_num, ]
            if(nrow(matching_trace) > 0) {
              col_name <- matching_trace$col_name[1]
              # Set the specific point to NA (plotly uses 0-based indexing, R uses 1-based)
              row_index <- point_num + 1
              if(row_index > 0 && row_index <= nrow(summary_data) && col_name %in% names(summary_data)) {
                summary_data[row_index, col_name] <- NA
              }
            }
          }
        }
      }
      
      # Create the plot
      p <- plot_ly(source = "summary_plot")
      
      # Add traces for each variable-statistic combination (data first)
      color_count <- 1
      colors <- rainbow(length(selected_vars) * length(selected_stats))
      
      # Determine transparency based on trend line setting
      data_opacity <- if(!is.null(input$show_trend_line) && input$show_trend_line) 0.7 else 1.0
      
      # Initialize plot with first trace to avoid empty plot warnings
      first_trace_added <- FALSE
      
      for(var in selected_vars) {
        for(stat in selected_stats) {
          col_name <- paste0(var, "_", stat)
          
          if(col_name %in% names(summary_data)) {
            trace_name <- paste(var, stat, sep = " - ")
            
            if(!first_trace_added) {
              # Create initial plot with first trace
              p <- plot_ly(
                data = summary_data,
                x = ~date,
                y = summary_data[[col_name]],
                type = 'scatter',
                mode = 'lines+markers',
                name = trace_name,
                line = list(color = colors[color_count]),
                marker = list(
                  color = colors[color_count],
                  line = list(color = "black", width = 1)
                ),
                opacity = data_opacity,
                connectgaps = FALSE,
                source = "summary_plot"
              )
              first_trace_added <- TRUE
            } else {
              # Add subsequent traces
              p <- p |> add_trace(
                data = summary_data,
                x = ~date,
                y = summary_data[[col_name]],
                type = 'scatter',
                mode = 'lines+markers',
                name = trace_name,
                line = list(color = colors[color_count]),
                marker = list(
                  color = colors[color_count],
                  line = list(color = "black", width = 1)
                ),
                opacity = data_opacity,
                connectgaps = FALSE
              )
            }
            
            color_count <- color_count + 1
          }
        }
      }
      
      # Add trend lines after all data traces (to bring them to front)
      if(!is.null(input$show_trend_line) && input$show_trend_line) {
        color_count <- 1  # Reset color counter for trend lines
        
        for(var in selected_vars) {
          for(stat in selected_stats) {
            col_name <- paste0(var, "_", stat)
            
            if(col_name %in% names(summary_data)) {
              trace_name <- paste(var, stat, sep = " - ")
              
              # Filter out NA values for trend calculation
              trend_data <- summary_data[!is.na(summary_data[[col_name]]), ]
              
              if(nrow(trend_data) > 1) {
                # Convert date to numeric for linear model
                trend_data$date_numeric <- as.numeric(trend_data$date)
                
                # Fit linear model
                lm_model <- lm(trend_data[[col_name]] ~ trend_data$date_numeric)
                
                # Generate trend line predictions
                trend_predictions <- predict(lm_model, newdata = trend_data)
                
                # Add trend line using add_lines to avoid marker inheritance
                p <- p |> add_lines(
                  data = trend_data,
                  x = ~date,
                  y = trend_predictions,
                  name = paste(trace_name, "- Trend"),
                  line = list(color = colors[color_count], dash = "dash", width = 3),
                  showlegend = TRUE,
                  inherit = FALSE
                )
              }
              
              color_count <- color_count + 1
            }
          }
        }
      }
      
      # Layout with selection tools
      title_text <- paste("Climate Data Summary -", stringr::str_to_title(selected_period), "Statistics")
      
      # Add date range info to title if filtering is applied
      if(!is.null(input$summary_date_range) && length(input$summary_date_range) == 2) {
        date_start <- input$summary_date_range[1]
        date_end <- input$summary_date_range[2]
        if(!is.na(date_start) && !is.na(date_end)) {
          title_text <- paste0(title_text, " (", format(date_start, "%Y-%m-%d"), " to ", format(date_end, "%Y-%m-%d"), ")")
        }
      }
      
      # Use the selected y-axis label
      y_label <- if(!is.null(input$summary_y_axis_variable)) input$summary_y_axis_variable else "Value"
      
      # Complete the plot layout first
      p <- p |> layout(
        title = title_text,
        xaxis = list(title = "Date"),
        yaxis = list(title = y_label),
        hovermode = 'x unified',
        showlegend = TRUE,
        dragmode = "pan"  # Default to pan mode instead of select
      ) |> 
      config(
        displayModeBar = TRUE,
        modeBarButtonsToAdd = list("select2d", "lasso2d")
      )
      
      # Register the event after plot is complete
      p <- event_register(p, "plotly_selected")
      
      return(p)
      
    }, error = function(e) {
      return(empty_summary_plot(paste("Error creating summary:", e$message)))
    })
  })
  

  
  #* Summary Table and Download functionality --------------------------------
  
  # Generate summary data table
  summary_table_data <- reactive({
    combined_data <- combined_station_data()
    
    # Validation checks
    if(is.null(combined_data)) {
      return(NULL)
    }
    
    if(input$global_data_interval != "daily") {
      return(NULL)
    }
    
    # Require user inputs
    req(input$summary_variable, input$summary_period, input$summary_statistic)
    
    # Get selected variables and statistics
    selected_vars <- input$summary_variable
    selected_period <- input$summary_period
    selected_stats <- input$summary_statistic
    
    # Validate inputs
    if(length(selected_vars) == 0 || length(selected_stats) == 0) {
      return(NULL)
    }
    
    # Filter combined_data to only include selected variables and LOCAL_DATE
    plot_data <- combined_data |> 
      select(LOCAL_DATE, all_of(selected_vars))
    
    # Apply date range filtering if provided
    if(!is.null(input$summary_date_range) && length(input$summary_date_range) == 2) {
      date_start <- input$summary_date_range[1]
      date_end <- input$summary_date_range[2]
      
      # Validate date range
      if(!is.na(date_start) && !is.na(date_end) && date_start <= date_end) {
        plot_data <- plot_data |>
          filter(LOCAL_DATE >= date_start & LOCAL_DATE <= date_end)
      }
    }
    
    # Apply summarization based on selected period
    tryCatch({
      if(selected_period == "monthly") {
        min_days <- if(!is.null(input$min_days_monthly)) input$min_days_monthly else 25
        summary_data <- summarize_monthly(plot_data, selected_vars, c(selected_stats, "count"), min_days = min_days)
        
        # Create date column for display
        summary_data <- summary_data |>
          mutate(Date = as.Date(paste(year, month, "01", sep = "-"))) |>
          select(Date, everything(), -year, -month) |>
          arrange(Date)
        
      } else if(selected_period == "yearly") {
        min_days <- if(!is.null(input$min_days_yearly)) input$min_days_yearly else 300
        summary_data <- summarize_yearly(plot_data, selected_vars, c(selected_stats, "count"), min_months = min_days)
        
        # Create date column for display (using July 1st as mid-year)
        summary_data <- summary_data |>
          mutate(Date = as.Date(paste(year, "07", "01", sep = "-"))) |>
          select(Date, everything(), -year) |>
          arrange(Date)
      }
      
      # Add station information columns at the beginning
      station_info <- data.frame(
        Climate_ID = character(nrow(summary_data)),
        Station_Name = character(nrow(summary_data)),
        stringsAsFactors = FALSE
      )
      
      # Get station information from reactive values
      if(!is.null(primary_station())) {
        station_info$Climate_ID <- paste0(station_info$Climate_ID, 
                                         ifelse(station_info$Climate_ID == "", "", "; "),
                                         "Primary: ", primary_station()$climate_id)
        station_info$Station_Name <- paste0(station_info$Station_Name,
                                           ifelse(station_info$Station_Name == "", "", "; "),
                                           "Primary: ", primary_station()$name)
      }
      
      if(!is.null(secondary_station())) {
        station_info$Climate_ID <- paste0(station_info$Climate_ID,
                                         ifelse(station_info$Climate_ID == "", "", "; "),
                                         "Secondary: ", secondary_station()$climate_id)
        station_info$Station_Name <- paste0(station_info$Station_Name,
                                           ifelse(station_info$Station_Name == "", "", "; "),
                                           "Secondary: ", secondary_station()$name)
      }
      
      if(!is.null(tertiary_station())) {
        station_info$Climate_ID <- paste0(station_info$Climate_ID,
                                         ifelse(station_info$Climate_ID == "", "", "; "),
                                         "Tertiary: ", tertiary_station()$climate_id)
        station_info$Station_Name <- paste0(station_info$Station_Name,
                                           ifelse(station_info$Station_Name == "", "", "; "),
                                           "Tertiary: ", tertiary_station()$name)
      }
      
      # Combine station info with summary data
      summary_data <- bind_cols(station_info, summary_data)
      
      return(summary_data)
      
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Render summary table
  output$summary_table <- DT::renderDataTable({
    summary_data <- summary_table_data()
    
    if(is.null(summary_data)) {
      return(data.frame(Message = "No summary data available"))
    }
    
    DT::datatable(
      summary_data,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        scrollY = 400,
        pageLength = 50,
        dom = 'Bfrtip',
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        buttons = list('colvis')
      ),
      rownames = FALSE
    ) |>
    DT::formatRound(columns = which(sapply(summary_data, is.numeric)), digits = 3)
  })
  
  # Render download button for summary data
  output$download_summary_button <- renderUI({
    summary_data <- summary_table_data()
    
    if(!is.null(summary_data) && nrow(summary_data) > 0) {
      fluidRow(
        column(6,
          downloadButton("download_summary_csv", "Download Summary as CSV", 
                        class = "btn-success btn-block",
                        icon = icon("download"))
        ),
        column(6,
          downloadButton("download_summary_excel", "Download Summary as Excel", 
                        class = "btn-info btn-block", 
                        icon = icon("file-excel"))
        )
      )
    }
  })
  
  # Download handler for summary CSV
  output$download_summary_csv <- downloadHandler(
    filename = function() {
      selected_period <- if(!is.null(input$summary_period)) input$summary_period else "summary"
      paste0("Climate_Summary_", stringr::str_to_title(selected_period), "_", 
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      summary_data <- summary_table_data()
      if(!is.null(summary_data)) {
        write.csv(summary_data, file, row.names = FALSE)
      }
    }
  )
  
  # Download handler for summary Excel
  output$download_summary_excel <- downloadHandler(
    filename = function() {
      selected_period <- if(!is.null(input$summary_period)) input$summary_period else "summary"
      paste0("Climate_Summary_", stringr::str_to_title(selected_period), "_", 
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      summary_data <- summary_table_data()
      if(!is.null(summary_data)) {
        if(requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(summary_data, file)
        } else {
          # Fallback to CSV if openxlsx not available
          write.csv(summary_data, gsub("\\.xlsx$", ".csv", file), row.names = FALSE)
        }
      }
    }
  )

} # Close the server function properly

