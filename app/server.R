rm(list = ls())

library(tidyr)
library(renv)
library(dplyr)
library(lutz) # additional package required by weathercan
library(sf) # additional package required by weathercan
library(weathercan) # download ECCC station info and data
library(DT) # datatable()
library(naniar) # for summarizing available data
library(plotly) # interactive available data plotting

# needed so shinyapps.io can find the repo for the weathercan package
# options(
#   repos =
#     c(
#       CRAN = "https://cran.rstudio.com",
#       'https://ropensci.r-universe.dev' = "https://ropensci.r-universe.dev"
#     )
# )

function(input, output, session) {
  
  # Here set up any elements that require spinner
  spin_map <- Waiter$new("MapPlot", html = spin_3k(), color = "black")
  spin_datatable <- Waiter$new("datatable", html = spin_3k(), color = "black")
  spin_plot <- Waiter$new("pctmiss_plotly", html = spin_3k(), color = "black")
  
  # Make sure to hide the global waiter after initialization
  waiter_hide()
  
  # Initial empty reactiveVal to store ECCC data & dataset title
  downloaded_ECCC <- reactiveVal(tibble())
  downloaded_title <- reactiveVal(as.character())
  
  # Add reactive values for secondary station data
  secondary_ECCC <- reactiveVal(tibble())
  secondary_title <- reactiveVal(as.character())
  
  # SideBar UI-------------------------------
  
  # Determine meta database location
  if(file.exists("./database/stations.rds")) {
    db_location <- "./database/stations.rds" # this when the app is running on local machine
  } else if (file.exists("../../database/stations.rds")) {
    db_location <- "../../database/stations.rds" # this when the app is running on KP server
  } 

  # Load Archived Station Data (new download takes about 15 seconds)
  station_meta <- reactiveFileReader(intervalMillis = 1000, 
                                     session,
                                     filePath = db_location, 
                                     readFunc = readRDS
                                     )
  
  # observe main selector to decide which one to update
  observe({
    # Use tryCatch to prevent errors from stopping the app
    tryCatch({
      if (input$main_selector == 'Climate ID'){
        # Get ALL climate IDs, not just the first 1000
        climate_ids <- station_meta()$stn$climate_id[!is.na(station_meta()$stn$climate_id)]
        
        # Make sure there are no duplicates
        climate_ids <- unique(climate_ids)
        
        # Sort IDs to make them easier to find
        climate_ids <- sort(climate_ids)
        
        # Station Climate ID Selection by User - use ALL IDs
        updateSelectizeInput(session, 'stn_id_input',
                          choices = climate_ids,
                          selected = "1047672",
                          options = list(maxOptions = 9999999))
        
        # Update secondary station input with Climate IDs as well
        updateSelectizeInput(session, 'secondary_stn_id',
                            choices = climate_ids,
                            selected = NULL,
                            options = list(maxOptions = 9999999))
        
      } else if (input$main_selector == 'WMO ID'){
        # Get ALL WMO IDs
        wmo_ids <- station_meta()$stn$WMO_id[!is.na(station_meta()$stn$WMO_id)]
        wmo_ids <- unique(sort(wmo_ids))
        
        # Station WMO ID Selection by User
        updateSelectizeInput(session, 'stn_id_input',
                          choices = wmo_ids,
                          selected = NULL,
                          options = list(maxOptions = 9999999))
        
      } else if (input$main_selector == 'TC ID'){
        # Get ALL TC IDs
        tc_ids <- station_meta()$stn$TC_id[!is.na(station_meta()$stn$TC_id)]
        tc_ids <- unique(sort(tc_ids))
        
        # Station TC ID Selection by User
        updateSelectizeInput(session, 'stn_id_input',
                          choices = tc_ids,
                          selected = NULL,
                          options = list(maxOptions = 9999999))
      }
    }, error = function(e) {
      # Log error but don't stop app
      message("Error updating selectize: ", e$message)
    })
  }) # End of updating climate ID enter
  
  # Simplified and improved ID lookup function
  id_entered <- reactive({
    validate(need(input$stn_id_input, "Invalid ID Input"))
    
    input_id <- as.character(input$stn_id_input)
    message("Looking up ID: ", input_id)
    
    # Use a consistent approach for all ID types
    if (input$main_selector == 'Climate ID') {
      matches <- station_meta()$stn[which(as.character(station_meta()$stn$climate_id) == input_id), ]
    } else if (input$main_selector == 'WMO ID') {
      matches <- station_meta()$stn[which(as.character(station_meta()$stn$WMO_id) == input_id), ]
    } else if (input$main_selector == 'TC ID') {
      matches <- station_meta()$stn[which(as.character(station_meta()$stn$TC_id) == input_id), ]
    }
    
    if(nrow(matches) > 0) {
      message("Found ", nrow(matches), " matches for ID ", input_id)
      message("First match station_id: ", matches$station_id[1])
      return(as.numeric(matches$station_id[1]))
    } else {
      message("No matches found for ID ", input_id)
      type_name <- input$main_selector
      validate(need(FALSE, paste(type_name, "not found:", input_id)))
    }
  })
  
  # Same simplified approach for secondary station ID
  secondary_id_entered <- reactive({
    validate(need(input$secondary_stn_id, "Select a secondary station"))
    
    input_id <- as.character(input$secondary_stn_id)
    message("Looking up secondary ID: ", input_id)
    
    matches <- station_meta()$stn[which(as.character(station_meta()$stn$climate_id) == input_id), ]
    
    if(nrow(matches) > 0) {
      message("Found ", nrow(matches), " matches for secondary ID ", input_id)
      message("First match station_id: ", matches$station_id[1])
      return(as.numeric(matches$station_id[1]))
    } else {
      message("No matches found for secondary ID ", input_id)
      validate(need(FALSE, paste("Secondary Climate ID not found:", input_id)))
    }
  })
  
  # Update dropdown menu, what time intervals are available
  observe({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    int_opts <- station_meta()$stn %>%
                  drop_na(start) %>%
                  filter(station_id == id_entered()) %>%
                  select(interval) %>%
                  unique()
    
    int_opts <- factor(int_opts$interval, levels=c("hour","day","month"))
    
    updateSelectInput(session, 
                      "Intervals",
                      choices = levels(factor(int_opts)),
                      selected = "month"
    )
  }) # end of observe
  
  output$stn_input_info <- renderText({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    # Return Station Name & Info
    map_plot_data() %>% filter(station_id == id_entered()) %>%
      select(text) %>% as.character()
    
    
  })
  
  # Sometimes monthly interval has shorter record length
  output$stn_warning <- renderText({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    STN <- station_meta()$stn %>% filter(station_id == id_entered())
    D <- STN %>% filter(interval == "day") %>% select(end) %>% as.numeric()
    H <- STN %>% filter(interval == "hour") %>% select(end) %>% as.numeric()
    M <- STN %>% filter(interval == "month") %>% select(end) %>% as.numeric()
    
    if(!is.na(M) & !is.na(D) & !is.na(H)){
      validate(need(
        (M>=D) & (M>=H), "CAUTION: ECCC monthly data for this station are shorter than daily or hourly"
      ))
    } else if(!is.na(M) & !is.na(D)){
      validate(need(
        (M>=D), "CAUTION: ECCC monthly data for this station are shorter than daily"
      ))
    } else if(!is.na(M) & !is.na(H)){
      validate(need(
        (M>=H), "CAUTION: ECCC monthly data for this station are shorter than hourly"
      ))
    } else if(!is.na(D) & !is.na(H)){
      validate(need(
        (D>=H), "CAUTION: ECCC daily data for this station are shorter than hourly"
      ))
    }  
    

    "" # otherwise return empty message
    
  })
  
  # update the data record range selector for daily & hourly data
  observe({
    if(input$Intervals != 'month'){
      validate(
        need(input$stn_id_input, "Invalid Station ID"))
      
      stn_info <- station_meta()$stn %>% 
                    filter(station_id == id_entered(),
                           interval == as.character(input$Intervals))
      
      # Check if data exists for the selected interval
      if(nrow(stn_info) > 0 && !is.na(stn_info$start) && !is.na(stn_info$end)) {
        stn_min = stn_info$start
        stn_max = stn_info$end
        
        updateSliderInput(session, "select_range", value = c(stn_min, stn_max),
                          min = stn_min, max = stn_max, step = 1)
      } else {
        # If no data for the interval, set reasonable defaults
        current_year <- as.numeric(format(Sys.Date(), "%Y"))
        updateSliderInput(session, "select_range", value = c(current_year-10, current_year),
                          min = 1900, max = current_year, step = 1)
      }
    }
  })
  
  # Check if the selected range is too big
  range_check <- reactive({
    input$select_range[2] - input$select_range[1]
  })
  
  # ECCC download button UI
  output$ECCC_button <- renderUI({
    
    # no limit on monthly data
    if(input$Intervals == 'month'){
      actionButton("access_data", "Request ECCC Data")
    } # max 50 years for daily OR 3 years for hourly data
    else if((input$Intervals == 'day' & range_check() > 50) ||
            (input$Intervals == 'hour' & range_check() > 3)){
      
      actionButton("exceed_button", "Request ECCC Data")
      
    } else {
      
      actionButton("access_data", "Request ECCC Data")
      
    }
  })    
  
  # pop-up window for excessive data request
  observeEvent(input$exceed_button, {

    # Show a modal when the button is pressed
    shinyalert(
      title = "Request Limit Exceeded", 
      text = paste0("Limit is 50 years for daily data or 3 years for hourly data."),
      type = "warning",
      showCancelButton = FALSE,
      animation = "slide-from-bottom"
    )
    
  }) # end of exceed_button
  
  # pop-up window for downloading ECCC data
  observeEvent(input$access_data, {
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))

    
    # Show a modal when the button is pressed
    shinyalert(
      title = "Requesting Data from ECCC", 
      text = paste0("please be patient"),
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      animation = "slide-from-bottom"
    )
    
    # download the data into memory (by updating ractive values)
    if(as.character(input$Intervals) == "month"){
      downloaded_ECCC(
        weathercan::weather_dl(station_ids = id_entered(),
                               interval = "month",
                               quiet = TRUE)
      )
      
      downloaded_title(paste0(unique(downloaded_ECCC()$station_name),
                                  " (Climate ID: ", unique(downloaded_ECCC()$climate_id),
                                  ") Monthly: ", min(downloaded_ECCC()$year), "-", 
                                  max(downloaded_ECCC()$year)
                        )
      )
      
    } else if(as.character(input$Intervals) == "day") {
      downloaded_ECCC(
        weathercan::weather_dl(station_ids = id_entered(),
                               interval = "day",
                               start = base::as.Date(paste0(input$select_range[1],"-01-01")),
                               end = base::as.Date(paste0(input$select_range[2],"-12-31")),
                               quiet = TRUE)
      )
      
      downloaded_title(paste0(unique(downloaded_ECCC()$station_name),
                              " (Climate ID: ", unique(downloaded_ECCC()$climate_id),
                              ") Daily: ", min(downloaded_ECCC()$year), "-", 
                              max(downloaded_ECCC()$year)
                        )
      )
    } else{
      downloaded_ECCC(
        weathercan::weather_dl(station_ids = id_entered(),
                               interval = "hour",
                               start = base::as.Date(paste0(input$select_range[1],"-01-01")),
                               end = base::as.Date(paste0(input$select_range[2],"-12-31")),
                               quiet = TRUE)
      )
      
      downloaded_title(paste0(unique(downloaded_ECCC()$station_name),
                              " (Climate ID: ", unique(downloaded_ECCC()$climate_id),
                              ") Hourly (local tz): ", min(downloaded_ECCC()$year), "-", 
                              max(downloaded_ECCC()$year)
                        )
      )
    }
    
    
    # close the previous pop-up message
    shinyjs::runjs("swal.close();")
    
    # Show message
    shinyalert("Request Complete!", "", type = "success")
    
  }) # end of access_data button
  
  
  output$data_preview_title_plot <- output$data_preview_title <- renderUI({
    HTML(paste0("<b> Currently displayed data: </b><br>", downloaded_title()))
  })

  
  
  # ReadMe Tab --------------------------------
  # using HTML will mess up CSS style/theme format for some reasons, use Markdown
  
  output$README <- renderUI({
    
    # When run locally, README is in parent folder (for github/gitlab)
    if(file.exists("../README.md")) {
      includeMarkdown("../README.md")
    } else {
      # when deployed, README is copied to the same deployment folder
      includeMarkdown("./README.md")
    }
    
  })
  
  
  # Station Map ------------------------------
  
  # Retrieve all station meta-data (auto-update if file changed, checked every sec)
  current_rds_date <- reactivePoll(1000, session,
                         # This function returns the time that the file was last modified
                         checkFunc = function() {
                             file.info("./database/stations.rds")$mtime[1] 
                         },
                         # This function returns the meta info of the file
                         valueFunc = function() {
                              file.info("./database/stations.rds")$mtime[1] %>% 
                                base::as.Date(tz = "America/Vancouver")
                         }
                       )
  

  # print out station meta data file last modified
  output$info_date <- renderText({current_rds_date() %>% as.character()})

  # When button is click, check if it is outldated, if yes then re-download the meta data
  observeEvent(input$update_meta, {
  
    # check how old is the file
    how_old <- base::difftime(Sys.Date(),
                              current_rds_date(),
                              units = "days") %>% as.numeric()
    
    #only update if meta database is more than 2 days old
    if(how_old>2){
      # Show a modal when the button is pressed
      shinyalert(
        title = "Confirm Updating Meta Data from ECCC", 
        text = paste0("Download can take about 20 seconds. \n
                       Please wait for the map to refresh."),
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "CONFIRM",
        confirmButtonCol = "#FF0000",
        animation = "slide-from-bottom",
        callbackR = function(value) {
          if(value == TRUE) {
            
            spin_map$show() #show spinner
            
            #takes quite long...like >10 seconds
            weathercan::stations_dl(verbose = FALSE, quiet = TRUE)
            
            meta_sys_rds <- file.path(rappdirs::user_data_dir("weathercan"), "stations.rds")
            
            file.copy(meta_sys_rds, paste0(getwd(),"/database/"), overwrite = TRUE)
            
            return()
            
          } else{
            shinyalert("Cancelled", type = 'error')
          }
        }
      ) #end of shiny alert
    } else{
      # if the meta data is new, provide pop-up
      shinyalert(
        title = "No Action Needed", 
        text = "Already up-to-date.",
        type = "error",
        showCancelButton = FALSE,
        animation = "slide-from-bottom",
      )#end of shiny alert
    }
    

  }) # end of shinyalert observeEvent button
  
  
  
  
  # Data frame required for the map plotting
  map_plot_data <- reactive({
    
    # exit required for map type rendering (need to be placed in upstream reactive)
    on.exit({
      spin_map$hide()
    })
    
    # 1. Data availability re-arrange
    record_range <- station_meta()$stn %>% 
    
                        # Grab hourly measurement record length
                        filter(interval == "hour") %>%
                        select(station_name, station_id, H_start = start, H_end = end) %>%
                        
                        # Grab daily measurement record lengths
                        left_join(
                          # Grab daily measurement record length
                          station_meta()$stn %>% 
                            filter(interval == "day") %>%
                            select(station_name, station_id, D_start = start, D_end = end),
                          by = c("station_name", "station_id")
                        ) %>%
                        
                        left_join(
                          # Grab monthly measurement record length
                          station_meta()$stn %>% 
                            filter(interval == "month") %>%
                            select(station_name, station_id, M_start = start, M_end = end),
                          by = c("station_name", "station_id")
                        ) # End of Pipe for record_range
                        
    # 2. Group availability with other info
    station_tibble <- station_meta()$stn %>% 
                          group_by(station_id) %>% 
                          slice(1) %>% 
                          ungroup() %>%
                          select(prov:tz) %>%
                          left_join(record_range, 
                                    by = c("station_name", "station_id")
                                    )
    
    
    # 3. Final tibble ready for map plot (with stn info tag)
    
    station_tibble %>%
      
      mutate(text = paste(sep = "<br/>", paste("<b>", station_name, "</b>"), 
                          paste0("Climate ID : ", climate_id),
                          paste0("Elevation (m) : ", elev),
                          paste0("Hourly Record: ", H_start, " to ", H_end, " (", H_end-H_start, " Yrs)"),
                          paste0("Daily Record: ", D_start, " to ", D_end, " (", D_end-D_start, " Yrs)"),
                          paste0("Monthly Record: ", M_start, " to ", M_end, " (", M_end-M_start, " Yrs)")
                          )
      )
    
    
  }) 
  
  

  
  # Leaflet map rendering
  output$MapPlot <- renderLeaflet({
    
    
    map_plot_data() %>%
      
      # Some locations are wrong (impossible values)
      drop_na(lon, lat) %>%
      filter(between(lon, -142, -51), # East to west boundary of Canada
             between(lat, 41, 84) # South to north boundary of Canada
      ) %>%
      
      leaflet() %>%
      addTiles(urlTemplate = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      # default zoom to BC
      setView(lng = -122.7497, lat = 53.9171, zoom = 5) %>%
      
      addMarkers(~lon, ~lat, popup = ~text, clusterOptions = markerClusterOptions())
    
  }) # End of Leaflet map rendering
  
  
  
  # Data Table --------------------------------
  
  # update download title & file name
  file_dl_name <- reactive({
    
    tt <- downloaded_title()
    # replace any spacial character with underscore
    tt2 <- gsub("[^A-Za-z0-9,;._-]","_", tt)
    # replace double underscore to single
    gsub("__", "_", tt2)
    
  })
  
  # Download button does not render if no data have been requested from ECCC
  output$downloadbutton <- renderUI({
    
    if(nrow(downloaded_ECCC())>0) {
      downloadButton('downloadData', 'Save Downloaded Data (.csv)')
    }
    
  })
  
  # Data Download Button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(file_dl_name(),
             ".csv", sep = "")
    },
    content = function(file) {
      # don't use the tidy write_csv() because it turns datetime to UTC()
      write.csv(downloaded_ECCC(), file, row.names = FALSE)
    }
  )
  
  # DataTable rendering
  # Use Server side processing
  output$datatable <- DT::renderDataTable({
    
    validate(
      need(nrow(downloaded_ECCC())>0, "No data")
    )
    
    spin_datatable$show() #show spinner
    
    # table re-render when new data downloaded
    downloaded_ECCC()
    
    # use isolate to break auto-dependency on station ID & interval 
    shiny::isolate({
      
      downloaded_ECCC() %>%
          
          DT::datatable(
            rownames= FALSE,
            extensions = c('Buttons', 'FixedColumns', 'Scroller'),
            options = list(
              
              # Options for extension "Buttons"
              dom = 'Bfrtip',
              
              buttons = list(I('colvis')),
              
              columnDefs = list(list(className = "dt-center", targets = "_all")),
              
              # Options for extension "FixedColumns"
              scrollX = TRUE,
              fixedColumns = TRUE,
              
              # Options for extension "Scroller"
              deferRender = TRUE,
              scrollY = 600,
              scroller = TRUE
              
            )

          ) # End of datatable
    })
    
  }) # End of datatable rendering
  
  
  # Missing Data Explorer ------------------------------

  
  output$pctmiss_plotly <- renderPlotly({

    validate(
      need(nrow(downloaded_ECCC())>0, "No data")
    )
    
    spin_plot$show() #show spinner
    # exit required for plotly type rendering (need to be placed in upstream reactive)
    on.exit({
      spin_plot$hide()
    })
    
    # plot re-render when new data downloaded
    downloaded_ECCC()
    
    # use isolate to break auto-dependency on station ID & interval 
    shiny::isolate({
      
        # available columns are different depends on intervals
        if(input$Intervals == "day"){
          
          VAR_COLS <- downloaded_ECCC() %>% 
                          select(year,
                              # only keep variable columns
                              cool_deg_days:total_snow, 
                              # remove any flag columns
                              -ends_with("flag")
                          )
                                    
        } else if(input$Intervals == "month") {
          
          VAR_COLS <- downloaded_ECCC() %>% 
                          select(year,
                                 # only keep variable columns
                                 dir_max_gust:total_snow, 
                                 # remove any flag columns
                                 -ends_with("flag")
                          )
          
        } else if(input$Intervals == "hour") {
          
          VAR_COLS <- downloaded_ECCC() %>% 
                          select(year,
                                 # only keep variable columns
                                 weather:wind_spd, 
                                 # remove any flag columns
                                 -ends_with("flag")
                          )
        }
        
        TICK_FULL <- unique(VAR_COLS$year)
        
        # try to maintain no more than 15 labels
        if(length(TICK_FULL)>80){
          #only label every 10 years
          TICK_REDUCED <- ifelse(as.numeric(TICK_FULL) %% 10 == 0, TICK_FULL, "")
        } else if (length(TICK_FULL)<20){
          #label every 1 years
          TICK_REDUCED <- TICK_FULL
        } else {
          #only label every 5 years
          TICK_REDUCED <- ifelse(as.numeric(TICK_FULL) %% 5 == 0, TICK_FULL, "")
        }

        
        miss_plot <- gg_miss_fct(VAR_COLS, year) + 
          scale_x_discrete(limits = TICK_FULL, 
                           breaks = TICK_FULL, 
                           labels = TICK_REDUCED)
        
        ggplotly(miss_plot)
    }) # end of isolate()
  })
  
  # End the app loading spinner----
  
  # Output secondary station information
  output$secondary_stn_info <- renderText({
    validate(
      need(input$secondary_stn_id, "Please select a secondary station")
    )
    
    # Return Station Name & Info similar to primary station
    map_plot_data() %>% 
      filter(station_id == secondary_id_entered()) %>%
      select(text) %>% as.character()
  })
  
  # Add a flag to track when both stations' data is available
  stations_data_downloaded <- reactiveVal(FALSE)
  
  # Create an output to control conditional panel visibility
  output$stations_data_ready <- reactive({
    stations_data_downloaded()
  })
  outputOptions(output, "stations_data_ready", suspendWhenHidden = FALSE)
  
  # Download status message
  output$download_status <- renderUI({
    if(stations_data_downloaded()) {
      HTML("<div style='color: green; margin-top: 10px;'><b>✓ Data downloaded successfully</b></div>")
    } else {
      HTML("")
    }
  })
  
  # Handle download button click
  observeEvent(input$download_station_data, {
    validate(
      need(input$stn_id_input, "Select a primary station"),
      need(input$secondary_stn_id, "Select a secondary station"),
      need(input$secondary_stn_id != input$stn_id_input, "Primary and secondary stations must be different")
    )
    
    # Get time range from primary station settings
    start_year <- if(input$Intervals == 'month') {
      min(station_meta()$stn %>% filter(station_id == id_entered(), interval == "day") %>% pull(start))
    } else {
      input$select_range[1]
    }
    
    end_year <- if(input$Intervals == 'month') {
      max(station_meta()$stn %>% filter(station_id == id_entered(), interval == "day") %>% pull(end))
    } else {
      input$select_range[2]
    }
    
    # Limit the time range to avoid excessive downloads
    # Only download up to 10 years of data for comparison
    if((end_year - start_year) > 10) {
      end_year <- start_year + 10
    }
    
    # Show downloading message for primary station
    shinyalert(
      title = "Downloading Primary Station Data", 
      text = "Please wait...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      animation = "slide-from-bottom"
    )
    
    # Try to download primary station data
    tryCatch({
      primary_data <- weathercan::weather_dl(
        station_ids = id_entered(),
        interval = "day",
        start = as.Date(paste0(start_year, "-01-01")),
        end = as.Date(paste0(end_year, "-12-31")),
        quiet = TRUE
      )
      
      # Verify we got data
      if(nrow(primary_data) == 0) {
        shinyjs::runjs("swal.close();")
        shinyalert(
          title = "Error", 
          text = "No data available for primary station in the selected time range.",
          type = "error"
        )
        stations_data_downloaded(FALSE)
        return()
      }
      
      # Close alert
      shinyjs::runjs("swal.close();")
      
      # Show downloading message for secondary station
      shinyalert(
        title = "Downloading Secondary Station Data", 
        text = "Please wait...",
        type = "info",
        showCancelButton = FALSE,
        showConfirmButton = FALSE,
        animation = "slide-from-bottom"
      )
      
      # Try to download secondary station data
      secondary_data <- weathercan::weather_dl(
        station_ids = secondary_id_entered(),
        interval = "day",
        start = as.Date(paste0(start_year, "-01-01")),
        end = as.Date(paste0(end_year, "-12-31")),
        quiet = TRUE
      )
      
      # Verify we got data for secondary station
      if(nrow(secondary_data) == 0) {
        shinyjs::runjs("swal.close();")
        shinyalert(
          title = "Error", 
          text = "No data available for secondary station in the selected time range.",
          type = "error"
        )
        stations_data_downloaded(FALSE)
        return()
      }
      
      # Store data in reactive values
      downloaded_ECCC(primary_data)
      secondary_ECCC(secondary_data)
      
      # Set downloaded titles - safely handle missing columns
      primary_name <- if("station_name" %in% names(primary_data)) unique(primary_data$station_name) else "Unknown"
      primary_id <- if("climate_id" %in% names(primary_data)) unique(primary_data$climate_id) else "Unknown"
      primary_start <- if("year" %in% names(primary_data)) min(primary_data$year, na.rm = TRUE) else NA
      primary_end <- if("year" %in% names(primary_data)) max(primary_data$year, na.rm = TRUE) else NA
      
      secondary_name <- if("station_name" %in% names(secondary_data)) unique(secondary_data$station_name) else "Unknown"
      secondary_id <- if("climate_id" %in% names(secondary_data)) unique(secondary_data$climate_id) else "Unknown"
      secondary_start <- if("year" %in% names(secondary_data)) min(secondary_data$year, na.rm = TRUE) else NA
      secondary_end <- if("year" %in% names(secondary_data)) max(secondary_data$year, na.rm = TRUE) else NA
      
      # Format date ranges properly
      primary_range <- if(!is.na(primary_start) && !is.na(primary_end)) {
        paste0(primary_start, "-", primary_end)
      } else {
        "Unknown range"
      }
      
      secondary_range <- if(!is.na(secondary_start) && !is.na(secondary_end)) {
        paste0(secondary_start, "-", secondary_end)
      } else {
        "Unknown range"
      }
      
      downloaded_title(paste0(primary_name, " (Climate ID: ", primary_id, ") Daily: ", primary_range))
      secondary_title(paste0(secondary_name, " (Climate ID: ", secondary_id, ") Daily: ", secondary_range))
      
      # Set flag to indicate data is ready
      stations_data_downloaded(TRUE)
      
      # Close alert and show success message
      shinyjs::runjs("swal.close();")
      shinyalert("Download Complete!", "Both stations' data has been downloaded successfully.", type = "success")
      
    }, error = function(e) {
      # Handle errors during download
      shinyjs::runjs("swal.close();")
      shinyalert(
        title = "Error", 
        text = paste("Failed to download station data:", e$message),
        type = "error"
      )
      stations_data_downloaded(FALSE) # Ensure flag is set to false
    })
  })
  
  # Modify download_both_stations to use cached data instead of initiating downloads
  download_both_stations <- reactive({
    validate(
      need(stations_data_downloaded(), "Please download station data first by clicking the 'Download Station Data' button")
    )
    
    # Return the already downloaded data
    list(
      primary = downloaded_ECCC(),
      secondary = secondary_ECCC()
    )
  })
  
  # Record comparison plot - leave as is, it depends on download_both_stations()
  # Frequency pairing plot - leave as is, it depends on download_both_stations() 
  # The infill button handler - leave as is, it depends on download_both_stations()
}