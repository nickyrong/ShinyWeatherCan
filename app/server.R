rm(list = ls())

library(tidyr) # we live in the tidyverse!
library(renv)
library(dplyr)
library(weathercan) # download ECCC station info and data
library(lutz) # additional package required by weathercan
library(sf) # additional package required by weathercan
library(DT) # datatable()
library(naniar) # for summarizing available data
library(plotly) # interactive available data plotting


function(input, output, session) {
  
  # Here set up any elements that require spinner
  spin_datatable <- Waiter$new("datatable", html = spin_3k(), color = "black")
  waitress_btn <- Waitress$new("#update_meta", theme = "overlay", infinite = TRUE)

  # SideBar UI-------------------------------
  
  # Load Archived Station Data (new download takes about 15 seconds)
  station_meta <- reactiveFileReader(intervalMillis = 1000, 
                                     session,
                                     filePath = "station_meta_data.csv", 
                                     readFunc = read.csv
                                     )
  
  # observe main selector to decide which one to update
  observe({
    
    if (input$main_selector == 'Climate ID'){
      
      # Station Climate ID Selection by User
      updateSelectInput(session, 'stn_id_input',
                        choices = station_meta()$climate_id,
                        selected = NULL
      )
      
    } else if (input$main_selector == 'WMO ID'){
      
      # Station WMO ID Selection by User
      updateSelectInput(session, 'stn_id_input',
                        choices = station_meta()$WMO_id,
                        selected = NULL
      )
      
      
    } else if (input$main_selector == 'TC ID'){
      
      # Station TC ID Selection by User
      updateSelectInput(session, 'stn_id_input',
                        choices = station_meta()$TC_id,
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
        (M>=D) & (M>=H), "CAUTION: monthly data on ECCC have a shorter record length than daily/hourly"
      ))
    } else if(!is.na(M) & !is.na(D)){
      validate(need(
        (M>=D), "CAUTION: monthly data on ECCC have a shorter record length than daily"
      ))
    } else if(!is.na(M) & !is.na(H)){
      validate(need(
        (M>=H), "CAUTION: monthly data on ECCC have a shorter record length than hourly"
      ))
    } else if(!is.na(D) & !is.na(H)){
      validate(need(
        (D>=H), "CAUTION: daily data on ECCC have a shorter record length than hourly"
      ))
    }  
    

    "" # otherwise return empty message
    
  })
  
  # Station Map ------------------------------
  
  # Retrieve all station meta-data (auto-update if file changed, checked every sec)
  current_csv_date <- reactivePoll(1000, session,
                         # This function returns the time that the file was last modified
                         checkFunc = function() {
                             file.info("station_meta_data.csv")$mtime[1]
                         },
                         # This function returns the meta info of the file
                         valueFunc = function() {
                              file.info("station_meta_data.csv")$mtime[1] %>% 
                                base::as.Date(tz = "America/Vancouver")
                         }
                       )
  

  # print out station meta data file last modified
  output$info_date <- renderText({current_csv_date() %>% as.character()})

  # When button is click, re-download the meta data
  observeEvent(input$update_meta, {
    
    # weathercan::stations_dl(verbose = FALSE, quiet = TRUE) %>%
    #   station_meta_data %>% 
    #   write.csv(file = "station_meta_data.csv", row.names = FALSE)


  })
  
  
  
  # Data frame required for the map plotting
  map_plot_data <- reactive({
    
    
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
  
  # Update dropdown menu, what time intervals are available
  observe({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    updateSelectInput(session, 
                      "Intervals",
                      choices = station_meta() %>%
                        drop_na(start) %>%
                        filter(station_id == id_entered()) %>%
                        select(interval) %>%
                        unique(),
                      selected = "month"
    )
  }) # end of observe
  
  
  # Station Dataset
  dataSet <- reactive({
    
    validate(
      need(input$Intervals %in% c("hour", "day", "month"),
           "Interval Not Found")
    )
    
    # use the weathercan{} package function to retrieve data
    weathercan::weather_dl(station_ids = id_entered(),
                           interval = as.character(input$Intervals),
                           quiet = TRUE
    )
    
  })
  
  # Download Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("ID_", id_entered(), "_", 
             as.character(input$Intervals),
             ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataSet(), file, row.names = FALSE)
    }
  )
  
  file_dl_name <- reactive({
    
    ID_TYPE <- input$main_selector %>% as.character()
    ID <- input$stn_id_input %>% as.character()
    INTERVAL <- input$Intervals %>% as.character()
    
    paste(ID_TYPE, ID, INTERVAL, sep = "_")
    
  })
  
  # DataTable rendering
  output$datatable <- DT::renderDataTable({
    
    spin_datatable$show() #show spinner
    
    dataSet() %>%
      
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
    
  }) # End of datatable rendering
  
  
  # Missing Data Exploring ------------------------------
  
  
  output$miss_plotly <- renderPlotly({
  
    validate(need())
    
    VAR_COLS <- test %>% select(year,
                                # only keep variable columns
                                cool_deg_days:total_snow, 
                                # remove any flag columns
                                -ends_with("flag")
    )
    
    TICK_FULL <- unique(VAR_COLS$year)
    
    # try to maintain no more than 15 labels
    if(length(TICK_FULL)>80){
      #only label every 10 years
      TICK_REDUCED <- ifelse(as.numeric(FULL) %% 10 == 0, TICK_FULL, "")
    } else if (length(TICK_FULL<30)){
      #only label every 2 years
      TICK_REDUCED <- ifelse(as.numeric(FULL) %% 2 == 0, TICK_FULL, "")
    } else {
      #only label every 5 years
      TICK_REDUCED <- ifelse(as.numeric(FULL) %% 5 == 0, TICK_FULL, "")
    }
    
    
    miss_plot <- gg_miss_fct(VAR_COLS, year) + 
      labs(title = "Data Missing") +
      scale_x_discrete(limits = TICK_FULL, 
                       breaks = TICK_FULL, 
                       labels = TICK_REDUCED)
    
    ggplotly(miss_plot)
  
  })
  # End the app loading spinner----
  waiter_hide()
  
  
}