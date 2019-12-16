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
              
                tabPanel("Read Me",
                         includeHTML("ReadMe.html")
      
                ), # End of Read Me tab
                
                tabPanel("Stations Map",
                         br(),
                         h3("Database is still loading if map is not visible"),
                         br(),
                         leafletOutput("MapPlot"),
                         "Zoom into map to see station locations",
                         br(),
                         "Click on a location to see station info."
                         
                ), # End of Instruction tab
                

                tabPanel("Data Table",
                         br(),
                         h3(textOutput("name")),
                         br(),
                         selectInput("Intervals", "Available Intervals", ""),
                         br(),
                         downloadButton("downloadData", "Download Selected Interval Data"),
                         br(),
                         br(),
                         h4("Data Preview"),
                         br(),
                         DT::dataTableOutput("datatable")
                ) # End of Data Table tab
              
            ) # End of tab setting
            
        ) # End of main panel
        
    ) # End of Page with Panel
    
)) # End of Script
