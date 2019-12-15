rm(list = ls())

library(tidyverse)
library(weathercan)


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
          build_inputs(as.numeric(input$second_submenu))
        } else if (input$main_selector == 'TC ID' && 
                  (!is.null(input$tcid_submenu))){
          build_inputs(as.numeric(input$tcid_submenu))
        }
    }) # End of ui_numeric_inputs
    
    
    # Retrieve all station meta-data
    map_data_raw <- weathercan::stations_dl(verbose = FALSE, quiet = TRUE)
    
    # Generate the data for the map by calling coordinates, labels, and date ranges from the HYDAT database
    record.range <- map_data_raw %>% 
      
        # Grab hourly measurement record length
        filter(interval == "hour") %>%
        select(station_name, station_id, H_start = start, H_end = end) %>%
        
        # Grab daily & monthly measurement record lengths
        left_join(
              
          
              # Grab daily measurement record length
              map_data_raw %>% 
              filter(interval == "day") %>%
              select(station_name, station_id, D_start = start, D_end = end),
              
              
              # Grab monthly measurement record length
              map_data_raw %>% 
                  filter(interval == "month") %>%
                  select(station_name, station_id, M_start = start, M_end = end),
              
              by = c(station_name, station_id)
              
        ) # end of left_join()
    
    # same sation could have multiple entry (different intervals)
    # only use the first occurrence for map plotting
    unique_stn <- map_data_raw %>% group_by(station_name) %>%
    
    map_data_plotting <- map_data_raw %>%
        
        mutate(text = paste(sep = "<br/>", paste("<b>", unique_stn$station_name, "</b>"), 
                            unique_stn$station_name, 
                            HYD_STATUS,
                            paste0("Flow Record: from ", Qfrom, " to ", Qto, " (", Qn, " Yrs)"),
                            paste0("Stage Record: from ", Hfrom, " to ", Hto, " (", Hn, " Yrs)")
        ))
    
}
  
  


#stn_id <- stations_dl(verbose = FALSE, quiet = TRUE) %>% filter(climate_id == 1100130)

#weather_dl(station_ids = stn_id$station_id[1], interval = "day")
