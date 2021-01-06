rm(list=ls())
library(shiny)
library(renv)
library(shinythemes)
library(leaflet)
library(plotly)
library(waiter) #loading screen/spinner

shinyUI(fluidPage(
  
  use_waiter(),
  waiter_show_on_load(html = spin_3k(), color = "black"), # place at the top before content
  
  theme = shinytheme("yeti"),
  
  # Change font color of error message to red
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;

      }
    "))
  ),
  
  # SideBar UI configuration
  pageWithSidebar(
    
    headerPanel(title=div('Environment Canada Climate Data Retrieval Tool', 
                          img(src='', 
                              style = "float:right;"
                          )
    )
  ),

    
    # Sidebar panel
    sidebarPanel(
      width = 3,
      selectInput(inputId = "main_selector",
                   label = h4('Select ID Type'),
                   choices = list('Climate ID', 'WMO ID', 'TC ID'),
                   selected = 'Climate ID'),
    
      
      selectizeInput("stn_id_input", label = h4("Enter Station ID"),
                     choices = c("Loading..."),
                     multiple= FALSE,
                     options = list(maxOptions = 10)),

      h4("Station Info"),
      h6(htmlOutput("stn_input_info")),
      h6(htmlOutput("stn_warning")),
      
      #h4("Data Retrieval"),
      selectInput("Intervals", h4("Data Retrieval"), ""),
      actionButton("access_data", "Get Data from ECCC"),
      h6("When Data Table loads, all data are accessed")
      
    ), # end of side bar panel
    
    
    # Main Panel
    mainPanel(
      
      # tabset panel
      tabsetPanel(
        
        tabPanel("Read Me",
                 htmlOutput("README") #it is technically a markdown render but HTML works
                 
        ), # End of Read Me tab
        
        tabPanel("Stations Map",
                 br(),
                 "Station info last updated: ", textOutput("info_date"),
                 br(),
                 actionButton("update_meta", "Update Station Map from ECCC (~ 20 secs)"),
                 br(),br(),
                 leafletOutput("MapPlot", height = 600),
                 "Zoom into map to see station locations",
                 br(),
                 "Click on a location to see station info."
                 
        ), # End of Instruction tab
        
        
        tabPanel("Data Table",
                 br(),
                 code("Please wait for data to be downloaded from ECCC,
                       preview table & download options will appear below once download complete."),
                 br(),br(),
                 h4("Data Preview"),
                 br(),
                 DT::dataTableOutput("datatable"),
                 br()
        ), # End of Data Table tab
        
        tabPanel("Data Completeness",
                 br(),
                 code("Please wait for data to be downloaded from ECCC,
                       plot will appear below once download complete."),
                 br(),br(),
                 plotlyOutput("pctmiss_plotly"),
                 br()
        )
        
      ) # End of tab setting
      
    ) # End of main panel
    
  ) # End of Page with Panel
  
)) # End of Script
