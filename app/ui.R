rm(list=ls())
library(shiny)
library(renv)
library(shinythemes)
library(leaflet)
library(plotly)
library(waiter) #loading screen/spinner
library(shinyalert) # pop-up message
library(shinyjs) # needed for the pop-up to auto close


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
      selectInput(inputId = "main_selector",
                   label = h4('1. Select ID Type'),
                   choices = list('Climate ID', 'WMO ID', 'TC ID'),
                   selected = 'Climate ID'),
    
      
      selectizeInput("stn_id_input", label = h4("2. Enter Station ID"),
                     choices = c("Loading..."),
                     multiple= FALSE,
                     options = list(maxOptions = 10)),

      h4("3. Review Station Info"),
      h6(htmlOutput("stn_input_info")),
      h6(htmlOutput("stn_warning")),

      br(),
      selectInput("Intervals", h4("4. Request Data"), ""),
      
      # conditional panel
      conditionalPanel(
        condition = "input.Intervals != 'month'",
        sliderInput("select_range", "Select Range:", min = 1800, max = 2100,
                    value = c(1900,2020), sep = ""),
      ),
      
      useShinyalert(force = TRUE),useShinyjs(),
      uiOutput("ECCC_button")
      
      
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
                 useShinyalert(force = TRUE),
                 actionButton("update_meta", "Update Station Map from ECCC"),
                 br(),br(),
                 leafletOutput("MapPlot", height = 600),
                 "Zoom into map to see station locations",
                 br(),
                 "Click on a location to see station info."
                 
        ), # End of Instruction tab
        
        tabPanel("Raw Data Table",
                 br(),
                 htmlOutput("data_preview_title"),
                 br(),
                 uiOutput("downloadbutton"),
                 br(),
                 DT::dataTableOutput("datatable"),
                 br()
        ), # End of Data Table tab
        
        tabPanel("Data Completeness",
                 br(),
                 htmlOutput("data_preview_title_plot"),
                 br(),
                 h5("Figure is interactive, see top-right corner for available tools"),
                 plotlyOutput("pctmiss_plotly"),
                 br()
        ), # End of Data Completeness tab
        
        tabPanel("Data Summary",
                 br(),
                 br(),
                 h5("This tab is underconstruction"),
                 br()
        ) # End of Data Summary tab
        
      ) # End of tab setting
      
    ) # End of main panel
    
  ) # End of Page with Panel
  
)) # End of Script
