rm(list = ls())

library(tidyverse) # we live in the tidyverse!
library(weathercan) # download ECCC station info and data
library(DT) # datatable()


function(input, output, session) {
  
    # ----------- SideBar UI -----------
  
    # Interactive side bar UI (Selection of ID Type, ID input)
    output$ui_selected <- renderUI({
        
        if (input$main_selector == 'Climate ID'){
          
          textInput("climate_id", label = h5("Enter Climate ID"), value = "")
    
        } else if (input$main_selector == 'WMO ID'){
          
          textInput("wmo_id", label = h5("Enter WMO ID"), value = "")
          
        } else if (input$main_selector == 'TC ID'){
          
          textInput("tc_id", label = h5("Enter Transport Canada ID"), value = "")
          
        }
    }) # End of render for ui_selected

    
    
    # ----------- For the Map -----------
    
    # Retrieve all station meta-data
    map_data_raw <- weathercan::stations_dl(verbose = FALSE, quiet = TRUE)
    
    # Data availability re-arrange
    record.range <- map_data_raw %>% 
      
        # Grab hourly measurement record length
        filter(interval == "hour") %>%
        select(station_name, station_id, H_start = start, H_end = end) %>%
        
        # Grab daily measurement record lengths
        left_join(
              # Grab daily measurement record length
              map_data_raw %>% 
                  filter(interval == "day") %>%
                  select(station_name, station_id, D_start = start, D_end = end),
                  by = c("station_name", "station_id")
        ) %>%
        
        left_join(
            # Grab monthly measurement record length
            map_data_raw %>% 
                filter(interval == "month") %>%
                select(station_name, station_id, M_start = start, M_end = end),
                by = c("station_name", "station_id")
        ) # End of Pipe for record.range
        
    
    # re-arrange data into format for plotting
    station.tibble <- map_data_raw %>% 
                  group_by(station_id) %>% 
                  slice(1) %>% 
                  ungroup() %>%
                  select(prov:tz) %>%
                  left_join(record.range, by = c("station_name", "station_id"))
    
    # station data tags for leaflet map
    map.data.plotting <- station.tibble %>%
        
        mutate(text = paste(sep = "<br/>", paste("<b>", station_name, "</b>"), 
                            paste0("Climate ID : ", climate_id),
                            paste0("Elevation (m) : ", elev),
                            paste0("Hourly Record: from ", H_start, " to ", H_end, " (", H_end-H_start, " Yrs)"),
                            paste0("Daily Record: from ", D_start, " to ", D_end, " (", D_end-D_start, " Yrs)"),
                            paste0("Monthly Record: from ", M_start, " to ", M_end, " (", M_end-M_start, " Yrs)")
                            
        ))
    
    # Leaflet map rendering
    output$MapPlot <- renderLeaflet({
        map.data.plotting %>% 
        
          # Some locations are wrong (impossible values)
          drop_na(lon, lat) %>%
          filter(between(lon, -142, -51), # East to west boundary of Canada
                 between(lat, 41, 84) # South to north boundary of Canada
          ) %>%
          
          leaflet() %>%
          addTiles() %>%
          addMarkers(~lon, ~lat, popup = ~text, clusterOptions = markerClusterOptions())
    }) # End of Leaflet map rendering
    
    
    # ----------- For the Data Table -----------
    
    # Reactive
    # convert Climate ID, WMO ID, or TC ID to Station ID
    # Note, the typeof(id.entered) would be closure (much like a function)
    #     therefore, make sure it's referred as "id.entered()" later on
    id.entered <- reactive({
      
        # Which type of ID?
        if (input$main_selector == 'Climate ID') {
          
            validate(
                need(input$climate_id %in% station.tibble$climate_id,
                     "Station Not Found")
            )
          
            station.tibble %>% 
                  filter(climate_id == input$climate_id) %>% 
                  "$"(station_id) 
          
        } else if (input$main_selector == 'WMO ID'){
          
            validate(
              need(input$wmo_id %in% station.tibble$WMO_id,
                   "Station Not Found")
            )
            
            station.tibble %>% 
                filter(WMO_id == input$wmo_id) %>% 
                "$"(station_id)
          
        } else if (input$main_selector == 'TC ID'){
          
            validate(
              need(input$tc_id %in% station.tibble$TC_id,
                   "Station Not Found")
            )
            
            station.tibble %>% 
                filter(TC_id == input$tc_id) %>% 
                "$"(station_id)
          
        }
      
    }) # EOF id.entered()
    
    
    # Station Name
    output$name <- renderText({
        printname <- station.tibble %>% filter(station_id == id.entered()) %>%
          select(station_name)
        printname[[1]]
    })
    
    # Update dropdown menu, what time intervals are available
    observe({
        updateSelectInput(session, 
                          "Intervals",
                          choices = map_data_raw %>%
                              drop_na(start) %>%
                              filter(station_id == id.entered()) %>%
                              select(interval) %>%
                              unique(),
                          selected = "month"
        )
    }) # end of observe

    
    # Station Dataset
    dataSet <- reactive({
      
        # use the weathercan{} package function to retrieve data
        weather_dl(station_ids = id.entered(),
                   interval = as.character(input$Intervals)
        )
    })
    
    # Download Data
    output$downloadData <- downloadHandler(
        filename = function() {
          paste0("ID_", id.entered(), "_", 
                 as.character(input$Intervals),
                 ".csv", sep = "")
        },
        content = function(file) {
          write.csv(dataSet(), file, row.names = FALSE)
        }
    )
    
    # DataTable rendering
    output$datatable <- DT::renderDataTable({
      
      
        dataSet() %>%
        
        DT::datatable(
            
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
                scrollY = 1000,
                scroller = TRUE
              
            )
          
            
        ) # End of datatable
      
    }) # End of datatable rendering
    

    
    
}
  
  


#stn_id <- stations_dl(verbose = FALSE, quiet = TRUE) %>% filter(climate_id == 1100130)

#weather_dl(station_ids = stn_id$station_id[1], interval = "day")
