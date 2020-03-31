rm(list = ls())


# For servers
#list of packages required
#list.of.packages <- c("tidyverse", "weathercan", "lutz", "sf", "DT", "naniar", "plotly")
#checking missing packages from list
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#install missing ones
#if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library(tidyverse) # we live in the tidyverse!
library(weathercan) # download ECCC station info and data
library(lutz) # additional package required by weathercan
library(sf) # additional package required by weathercan
library(DT) # datatable()
library(naniar) # for summarizing available data
library(plotly) # interactive available data plotting


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

    
    # ----------- For the ReadMe HTML -----------
    # Directly using includeHTML in ui.R will break Shiny (stop execution of everything follows)
    
    output$ReadMe_HTML <- renderUI({
    
        includeHTML("Intro_tab.html")
      
    })
    
    
    
    # ----------- For the Map -----------
    
    # Retrieve all station meta-data
    current.csv.date <- paste0("station_meta_data.csv") %>% 
                            file.info() %>%
                            "$"(ctime) %>% 
                            base::as.Date()
    
    output$info_date <- renderText({current.csv.date %>% as.character()})
    
    # Update meta-data list if there's no file or the file is older than 7 days
    if(is.na(current.csv.date) || (Sys.Date() - current.csv.date) > 7) {
      
        station_meta_data <- weathercan::stations_dl(verbose = FALSE, quiet = TRUE)
        station_meta_data %>% 
            write.csv(file = paste0("station_meta_data.csv"), row.names = FALSE)
        
        map_data_raw <- read.csv(paste0("station_meta_data.csv"))
        
    } else {
      
        map_data_raw <- read.csv(paste0("station_meta_data.csv"))
        
    }
    
    observeEvent(input$update_info, {
      
        station_meta_data <- weathercan::stations_dl(verbose = FALSE, quiet = TRUE)
        station_meta_data %>% 
            write.csv(file = paste0("station_meta_data.csv"), row.names = FALSE)
        map_data_raw <- read.csv(paste0("station_meta_data.csv"))
        
    })
    
    
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
                            paste0("Hourly Record: ", H_start, " to ", H_end, " (", H_end-H_start, " Yrs)"),
                            paste0("Daily Record: ", D_start, " to ", D_end, " (", D_end-D_start, " Yrs)"),
                            paste0("Monthly Record: ", M_start, " to ", M_end, " (", M_end-M_start, " Yrs)")
                            
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
                need(toupper(input$climate_id) %in% toupper(station.tibble$climate_id),
                     "Station Not Found")
            )
          
            station.tibble %>% # Capitalized all station ID
                  filter(toupper(climate_id) == toupper(input$climate_id)) %>% 
                  "$"(station_id)
          
        } else if (input$main_selector == 'WMO ID'){
          
            validate(
              need(toupper(input$wmo_id) %in% toupper(station.tibble$WMO_id),
                   "Station Not Found")
            )
            
            station.tibble %>% 
                filter(toupper(WMO_id) == toupper(input$wmo_id)) %>% 
                "$"(station_id) 
            
        } else if (input$main_selector == 'TC ID'){
          
            validate(
              need(toupper(input$tc_id) %in% toupper(station.tibble$TC_id),
                   "Station Not Found")
            )
            
            station.tibble %>% 
                filter(toupper(TC_id) == toupper(input$tc_id)) %>% 
                "$"(station_id)
          
        }
      
    }) # EOF id.entered()
    
    
    # Station Name
    output$name <- renderText({
        station.tibble %>% 
            filter(station_id == id.entered()) %>%
            pull(station_name) %>%
            as.character()
        
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
      
        validate(
          need(input$Intervals %in% c("hour", "day", "month"),
               "Interval Not Found")
        )
      
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
                scrollY = 600,
                scroller = TRUE
              
            )
          
            
        ) # End of datatable
      
    }) # End of datatable rendering
    
    # ----------- For the Missing Data Summary -----------
    output$plot <-renderPlotly({
      
      # Station name for plot title
      name <- station.tibble %>% 
                  filter(station_id == id.entered()) %>%
                  pull(station_name) %>%
                  as.character()
      
      # Remove non-climate variables (with the exception of year)
      data <- dataSet() %>%
                  select(-station_operator, -WMO_id, -TC_id, -station_name,
                         -station_id, -prov, -month, -lon, -lat, -elev, -date,
                         -climate_id, -ends_with("flag"))
      
      # Plot either full record, or facet by year
      if (input$Annual == "Annual"){
          test = data %>%
              gg_miss_var(show_pct = TRUE, facet = year) +
                labs(title = name) + 
                theme(axis.title.y = element_blank())
          
          ggplotly(test)
        
      } else {
          test = data %>% select(-year) %>%
              gg_miss_var(show_pct = TRUE) +
                labs(title = name) + 
                theme(axis.title.y = element_blank())
          
          ggplotly(test)
          
      }
        
    }) # End of available data summary
    
}
  
  