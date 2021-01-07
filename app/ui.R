rm(list=ls())
library(shiny)
library(renv)
library(shinythemes)
library(leaflet)
library(plotly)
library(waiter) #loading screen/spinner
library(shinyalert)

# Memory limitation set for free servers (shinyapps.io/AWS EC2)
# unix package requires unix machines (not available on windows)
if(.Platform$OS.type == "unix") {
# Somehow cannot launch to shinyapps.io...
library(unix)
unix::rlimit_as(1e9)
#the maximum size of the process's virtual memory (address space) in bytes.
# 1Gb = 1e9 bytes
} else if(.Platform$OS.type == "windows"){
  
  utils::memory.size(max = TRUE)
  suppressWarnings(utils::memory.limit(size = 1000))
  #request a new limit, in Mb: 1024 Mb = 1 Gb
}

shinyUI(fluidPage(
  
  use_waiter(),
  waiter_show_on_load(html = spin_3k(), color = "black"), # place at the top before content
  
  theme = shinytheme("yeti"),
  tags$head(HTML("<title>Environment Canada Climate Data Retrieval Tool</title>")),
  
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
      useShinyalert(),
      actionButton("access_data", "Download ECCC Data")
      
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
                 useShinyalert(),
                 actionButton("update_meta", "Update Station Map from ECCC"),
                 br(),br(),
                 leafletOutput("MapPlot", height = 600),
                 "Zoom into map to see station locations",
                 br(),
                 "Click on a location to see station info."
                 
        ), # End of Instruction tab
        
        
        tabPanel("Data Table",
                 br(),
                 code("Table does not auto-update! Data must be re-download after switching station/interval."),
                 br(),br(),
                 h4("Data Preview"),
                 br(),
                 DT::dataTableOutput("datatable"),
                 br()
        ), # End of Data Table tab
        
        tabPanel("Data Completeness",
                 br(),
                 code("Figure does not auto-update! Data must be re-download after switching station/interval."),
                 h4("Due to very limited server RAM limitation, 
                    plotting can be very slow (like more than a minute),
                    especially if station has more than 50 years of daily or 20 years of hourly data"),
                 br(),br(),
                 plotlyOutput("pctmiss_plotly"),
                 br()
        )
        
      ) # End of tab setting
      
    ) # End of main panel
    
  ) # End of Page with Panel
  
)) # End of Script
