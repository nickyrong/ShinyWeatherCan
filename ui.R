rm(list=ls())
library(shiny)
library(leaflet)


shinyUI(fluidPage(
  
    pageWithSidebar(
        headerPanel('Weather Data Download in Bulk from ECCC Website'),
        
        # Sidebar panel
        sidebarPanel(
            width = 3,
            radioButtons(inputId = "main_selector", 
                         label = h3('Select ID Type'),
                         choices = list('Climate ID', 'WMO ID', 'TC ID'),
                         selected = 'Climate ID'),
            uiOutput("ui_selected"),
            uiOutput("ui_numeric_inputs")
        ), # end of side bar panel
        
        
        # Main Panel
        mainPanel(
          
            # tabset panel
            tabsetPanel(
              
                tabPanel("Instructions",
                         br(),
                         print("This app allows the user to enter a station ID to access climate data from:"),
                         print("https://www.canada.ca/en/services/environment/weather.html"),
                         br(), br(),
                         print("Usage:"),
                         br(),
                         print("Check Station Map to identify potential stations and retrieve their IDs"),
                         br(),
                         print("Check Data Page to download the data"),
                         br(), br()
      
                ), # End of Instruction tab
                
                tabPanel("Stations Map",
                         br(),
                         print(h3("Database is still loading if map is not visible")),
                         br(),
                         leafletOutput("MapPlot"),
                         print("Zoom into map to see station locations"),
                         br(),
                         print("Click on a location to see station info.")
                         
                ), # End of Instruction tab
                
                tabPanel("Data Table",
                         br(),
                         textOutput("name"),
                         br(),
                         DT::dataTableOutput("datatable")
                ) # End of Data Table tab
              
            ) # End of tab setting
            
        ) # End of main panel
    ) # End of Page with Panel
    
)) # End of Script
