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
                         htmlOutput("ReadMe_HTML")
      
                ), # End of Read Me tab
                
                tabPanel("Stations Map",
                         br(),
                         "Station info last updated: ", textOutput("info_date"),
                         br(),
                         actionButton("update_info", "Optional: Update Station Info from ECCC"),
                         br(),br(),
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
                ), # End of Data Table tab
                
                tabPanel("Available Data",
                         br(),
                         "Note that these plots only show the % available for the period of record.",
                         "This does not necessarily correspond to complete years of record.",
                         "Therefore, these plots should only be used to understand the variables available at the station.",
                         br(),
                         selectInput("Annual", "Select", 
                                     choices = list('Total', 'Annual')),
                         br(),
                         plotOutput("plot")
                )
              
            ) # End of tab setting
            
        ) # End of main panel
        
    ) # End of Page with Panel
    
)) # End of Script
