rm(list=ls())
library(shiny)
library(renv)
library(shinythemes)
library(leaflet)
library(plotly)
library(waiter) #loading screen/spinner
library(shinyalert) # pop-up message
library(shinyjs) # needed for the pop-up to auto close
library(DT) # for data tables
library(ggplot2) # for plotting


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
    
      
      # Simplify the selectize inputs to avoid performance issues
      selectizeInput("stn_id_input", label = h4("2. Enter Station ID"),
                     choices = NULL,
                     multiple = FALSE,
                     options = list(placeholder = 'Type to search for stations',
                                   maxOptions = 999999)),

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
        
        
        tabPanel("Data Table",
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
        
        tabPanel("Advanced Data Analysis",
             br(),
             # Create a nested tabsetPanel for sub-tabs
             tabsetPanel(
               tabPanel("Read-Me",
                 br(),
                 h4("Infill Missing Data Using Quantile Mapping from Nearby Stations"),
                 p("This feature allows you to infill missing data using quantile mapping from nearby ECCC stations."),
                 p("The process identifies nearby stations with overlapping periods of record, creates statistical relationships between the stations, and uses those relationships to estimate missing values."),
                 p("Currently only supported for daily temperature and precipitation variables."),
                 br(),
                 h4("How to use this feature:"),
                 tags$ol(
                   tags$li("Go to the 'Station Selection' tab"),
                   tags$li("Select a primary station (this is the station with missing data you want to fill)"),
                   tags$li("Select a secondary station (this should be a nearby station with a good record)"),
                   tags$li("View the record comparison to ensure sufficient overlap between stations"),
                   tags$li("Examine the frequency distributions to assess station similarity"),
                   tags$li("Click 'Infill Missing Data' to apply the quantile mapping procedure")
                 ),
                 br(),
                 h4("Technical Details:"),
                 p("The quantile mapping approach matches the statistical distributions between stations during overlapping periods. This preserves the statistical properties of the data while providing estimates for missing values."),
                 p("This method works best when:"),
                 tags$ul(
                   tags$li("Stations are geographically close to each other"),
                   tags$li("Stations have similar climate regimes"),
                   tags$li("There is a substantial period of overlapping records"),
                   tags$li("The relationship between stations is reasonably stable over time")
                 )
               ),
               
               tabPanel("Station Selection",
                 br(),
                 # Row 1: Secondary station input
                 fluidRow(
                   column(6,
                    selectizeInput("secondary_stn_id", 
                 label = h5("Enter Secondary Station Climate ID"),
                 choices = NULL,
                 multiple = FALSE,
                 options = list(placeholder = 'Type to search for stations',
                                maxOptions = 999999))
                   ),
                   column(6,
                    htmlOutput("secondary_stn_info")
                   )
                 ),
                 
                 # Add download button before showing any plots
                 fluidRow(
                   column(12,
                     br(),
                     actionButton("download_station_data", "Download Station Data for Analysis", 
                                  class = "btn-primary"),
                     htmlOutput("download_status")
                   )
                 ),
                 
                 # Make plots conditional on data being available
                 conditionalPanel(
                   condition = "output.stations_data_ready",
                   
                   # Row 2: Record length comparison
                   fluidRow(
                     column(12,
                      h5("Record Length Comparison"),
                      plotlyOutput("record_comparison_plot")
                     )
                   ),
                   
                   # Row 3: Frequency pairing / CDF
                   fluidRow(
                     column(12,
                      h5("Monthly Cumulative Distribution Curves"),
                      plotlyOutput("frequency_pairing_plot")
                     )
                   ),
                   
                   # Row 4: Infill button
                   fluidRow(
                     column(12, 
                      br(),
                      actionButton("run_infill", "Infill Missing Data", 
                            class = "btn-primary"),
                      br(),
                      htmlOutput("infill_results")
                     )
                   )
                 ),
                 br()
               ) # End of Station Selection sub-tab
             ) # End of nested tabsetPanel
        ) # End of Advanced Data Analysis tab
        
      ) # End of tab setting
      
    ) # End of main panel
    
  ) # End of Page with Panel
  
)) # End of Script
