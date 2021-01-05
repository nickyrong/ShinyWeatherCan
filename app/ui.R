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
  tags$head(HTML("<title>'Environment Canada Climate Data Retrieval Tool'</title>")),
  
  # Change font color of error message to red
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight: bold;
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
      radioButtons(inputId = "main_selector",
                   label = h3('Select ID Type'),
                   choices = list('Climate ID', 'WMO ID', 'TC ID'),
                   selected = 'Climate ID'),
      
      selectizeInput("stn_id_input", label = h3("Enter Station ID"),
                     choices = c("Loading..."),
                     multiple= FALSE,
                     options = list(maxOptions = 5)),

      h3("Station Info"),
      h6(htmlOutput("stn_input_info")),
      h5(htmlOutput("stn_warning"))
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
                 h4("Select Available Intervals"),
                 selectInput("Intervals", "", ""),
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
                 h4("Select Available Intervals"),
                 selectInput("Intervals_pctmiss", "", ""),
                 br(),
                 code("Please wait for data to be downloaded from ECCC,
                       preview plot will appear below once download complete."),
                 br(),br(),
                 plotlyOutput("pctmiss_plotly"),
                 br()
        )
        
      ) # End of tab setting
      
    ) # End of main panel
    
  ) # End of Page with Panel
  
)) # End of Script
