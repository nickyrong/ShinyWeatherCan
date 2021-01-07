rm(list = ls())

library(tidyr)
library(renv)
library(dplyr)
library(weathercan) # download ECCC station info and data
library(DT) # datatable()
library(naniar) # for summarizing available data
library(plotly) # interactive available data plotting


function(input, output, session) {
  
  # Here set up any elements that require spinner
  spin_map <- Waiter$new("MapPlot", html = spin_3k(), color = "black")
  spin_datatable <- Waiter$new("datatable", html = spin_3k(), color = "black")
  spin_plot <- Waiter$new("pctmiss_plotly", html = spin_3k(), color = "black")
  
  
  # SideBar UI-------------------------------
  
  # Load Archived Station Data (new download takes about 15 seconds)
  station_meta <- reactiveFileReader(intervalMillis = 1000, 
                                     session,
                                     filePath = "station_meta_data.rds", 
                                     readFunc = readRDS
                                     )
  
  # observe main selector to decide which one to update
  observe({
    
    if (input$main_selector == 'Climate ID'){
      
      # Station Climate ID Selection by User
      updateSelectInput(session, 'stn_id_input',
                        choices = station_meta()$climate_id[!is.na(station_meta()$climate_id)],
                        selected = "1047672"
      )
      
    } else if (input$main_selector == 'WMO ID'){
      
      # Station WMO ID Selection by User
      updateSelectInput(session, 'stn_id_input',
                        choices = station_meta()$WMO_id[!is.na(station_meta()$WMO_id)],
                        selected = NULL
      )
      
      
    } else if (input$main_selector == 'TC ID'){
      
      # Station TC ID Selection by User
      updateSelectInput(session, 'stn_id_input',
                        choices = station_meta()$TC_id[!is.na(station_meta()$TC_id)],
                        selected = NULL
      )
      
      
    }
  }) # End of updating climate ID enter
  
  
  # Translate the 3 IDs to the unified "station_id"
  id_entered <- reactive({
    
    validate(
      need(input$stn_id_input, "Invalid ID Input"))
    
    if (input$main_selector == 'Climate ID'){
      
      station_meta() %>% 
        filter(climate_id == input$stn_id_input) %>% 
        select(station_id) %>% slice(1) %>% as.numeric()

    } else if (input$main_selector == 'WMO ID'){
      
      station_meta() %>% 
        filter(WMO_id == input$stn_id_input) %>% 
        select(station_id) %>% slice(1) %>% as.numeric()
      
      
    } else if (input$main_selector == 'TC ID'){
      
      station_meta() %>% 
        filter(TC_id == input$stn_id_input) %>% 
        select(station_id) %>% slice(1) %>% as.numeric()

    }
    

  }) # End of translating the 3 IDs to station_id
  
  
  # Update dropdown menu, what time intervals are available
  observe({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    int_opts <- station_meta() %>%
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
    
    STN <- station_meta() %>% filter(station_id == id_entered())
    D <- STN %>% filter(interval == "day") %>% select(end) %>% as.numeric()
    H <- STN %>% filter(interval == "hour") %>% select(end) %>% as.numeric()
    M <- STN %>% filter(interval == "month") %>% select(end) %>% as.numeric()
    
    if(!is.na(M) & !is.na(D) & !is.na(H)){
      validate(need(
        (M>=D) & (M>=H), "CAUTION: ECCC monthly data for this station are shorter than daily/hourly"
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
  
  
  
  # ECCC data access pop-up message
  observeEvent(input$access_data, {
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    # Show a modal when the button is pressed
    shinyalert(
      title = "Downloading Data from ECCC", 
      text = paste0("Please be patient with the download. \n
                     Download is complete when Data Table & Completeness Figure are re-loaded."),
      type = "warning",
      showCancelButton = FALSE,
      animation = "slide-from-bottom"

    )
  }) # end of shinyalert observeEvent button
  

  
  
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
                             file.info("station_meta_data.rds")$mtime[1]
                         },
                         # This function returns the meta info of the file
                         valueFunc = function() {
                              file.info("station_meta_data.rds")$mtime[1] %>% 
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
            weathercan::stations_dl(verbose = FALSE, quiet = TRUE) %>%
              saveRDS(file = "station_meta_data.rds")
            
            return()
            
          } else{
            shinyalert("Cancelled", type = 'error')
          }
        }
      ) #end of shiny alert
    } else{
      # if the meta data is new, give them a lesson from Nate.
      shinyalert(
        title = "Stop Clicking!", 
        text = "Can't you tell it is already up-to-date?! Nate thinks you are an idiot.",
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
    record_range <- station_meta() %>% 
    
                        # Grab hourly measurement record length
                        filter(interval == "hour") %>%
                        select(station_name, station_id, H_start = start, H_end = end) %>%
                        
                        # Grab daily measurement record lengths
                        left_join(
                          # Grab daily measurement record length
                          station_meta() %>% 
                            filter(interval == "day") %>%
                            select(station_name, station_id, D_start = start, D_end = end),
                          by = c("station_name", "station_id")
                        ) %>%
                        
                        left_join(
                          # Grab monthly measurement record length
                          station_meta() %>% 
                            filter(interval == "month") %>%
                            select(station_name, station_id, M_start = start, M_end = end),
                          by = c("station_name", "station_id")
                        ) # End of Pipe for record_range
                        
    # 2. Group availability with other info
    station_tibble <- station_meta() %>% 
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

  
  # Station Dataset Download
  ECCC_data <- reactive({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))

    validate(
      need(input$Intervals %in% c("hour", "day", "month"),
           "Interval Not Found")
    )
    
    
    # Take a dependency on input$goButton
    input$access_data
    
    shiny::isolate({

        
        weathercan::weather_dl(station_ids = id_entered(),
                               interval = as.character(input$Intervals),
                               quiet = TRUE)

    })

  })
  

  file_dl_name <- reactive({
    
    ID_TYPE <- input$main_selector %>% as.character()
    ID <- input$stn_id_input %>% as.character()
    INTERVAL <- input$Intervals %>% as.character()
    
    paste(ID_TYPE, ID, INTERVAL, sep = " ")
    
  })
  
  # DataTable rendering
  output$datatable <- DT::renderDataTable({
    
    spin_datatable$show() #show spinner
    
    # Take a dependency on input$access_data button
    input$access_data
    
    # use isolate to break auto-dependency on station ID & interval 
    shiny::isolate({
        ECCC_data() %>%
          
          DT::datatable(
            
            extensions = c('Buttons', 'FixedColumns', 'Scroller'),
            options = list(
              
              # Options for extension "Buttons"
              dom = 'Bfrtip',
              
              #buttons = list(I('colvis')),
              
              buttons = 
                list(I('colvis'), list(
                  extend = 'collection',
                  buttons = list(
                    list(extend = 'csv', filename = file_dl_name()),
                    list(extend = 'excel', filename = file_dl_name()),
                    list(extend = 'pdf', filename = file_dl_name())
                  ),
                  text = 'Download Station Data'
                )),
              
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

    # Take a dependency on input$access_data button
    # Also take dependency when review period been updated
    input$access_data
    #input$plot_range
    
    spin_plot$show() #show spinner
    # exit required for plotly type rendering (need to be placed in upstream reactive)
    on.exit({
      spin_plot$hide()
    })
    

    
    # use isolate to break auto-dependency on station ID & interval 
    shiny::isolate({
      
      
        # available columns are different depends on intervals
        if(input$Intervals == "day"){
          
          VAR_COLS <- ECCC_data() %>% 
                          select(year,
                              # only keep variable columns
                              cool_deg_days:total_snow, 
                              # remove any flag columns
                              -ends_with("flag")
                          )
                                    
        } else if(input$Intervals == "month") {
          
          VAR_COLS <- ECCC_data() %>% 
                          select(year,
                                 # only keep variable columns
                                 dir_max_gust:total_snow, 
                                 # remove any flag columns
                                 -ends_with("flag")
                          )
          
        } else if(input$Intervals == "hour") {
          
          VAR_COLS <- ECCC_data() %>% 
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
        } else if (length(TICK_FULL)<30){
          #only label every 2 years
          TICK_REDUCED <- ifelse(as.numeric(TICK_FULL) %% 2 == 0, TICK_FULL, "")
        } else {
          #only label every 5 years
          TICK_REDUCED <- ifelse(as.numeric(TICK_FULL) %% 5 == 0, TICK_FULL, "")
        }
        
        
        miss_plot <- gg_miss_fct(VAR_COLS, year) + 
          labs(title = paste("Completeness report for", 
                             input$main_selector,
                             input$stn_id_input,
                             input$Intervals,
                             "data")) +
          scale_x_discrete(limits = TICK_FULL, 
                           breaks = TICK_FULL, 
                           labels = TICK_REDUCED)
        
        ggplotly(miss_plot)
        
      })
  })
  # End the app loading spinner----
  waiter_hide()
  
  
}