rm(list = ls())

library(tidyverse) # we live in the tidyverse!
library(weathercan) # download ECCC station info and data
library(DT) # datatable()


function(input, output, session) {
    
    # Interactive side-bar function
    build_inputs <- function(choices) {
        output = tagList()
        for(i in 1:choices){
          output[[i]] = tagList()
          output[[i]][[1]] = numericInput(inputId =  paste0(i),
                                          label =  paste0(i),
                                          value = i)
        }
    return(output)
    } # EOF build_inputs()
  
    
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
      
    # Interactive side bar UI (transfer numeric ID to input)
    output$ui_numeric_inputs <- renderUI({
      
        if (input$main_selector == 'Climate ID' && 
            (!is.null(input$climateid_submenu))) {
          build_inputs(as.numeric(input$climateid_submenu))
        } else if (input$main_selector == 'WMO ID' && 
                   (!is.null(input$wmoid_submenu))){
          build_inputs(as.numeric(input$wmoid_submenu))
        } else if (input$main_selector == 'TC ID' && 
                  (!is.null(input$tcid_submenu))){
          build_inputs(as.numeric(input$tcid_submenu))
        }
      
    }) # End of ui_numeric_inputs
    
    
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
        leaflet() %>%
        addTiles() %>%
        addMarkers(~lon, ~lat, popup = ~text, clusterOptions = markerClusterOptions())
    })
    
    # DataTable rendering
    output$datatable <- DT::renderDataTable({
      
        if()
        DT::datatable({
          
            data = weather_dl(station_ids = output$ui_numeric_inputs)
            
        }) # End of datatable
      
    }) # End of datatable rendering
    
}
  
  


#stn_id <- stations_dl(verbose = FALSE, quiet = TRUE) %>% filter(climate_id == 1100130)

#weather_dl(station_ids = stn_id$station_id[1], interval = "day")
