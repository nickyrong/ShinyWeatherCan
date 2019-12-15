rm(list=ls())
library(shiny)
library(leaflet)


shinyUI(fluidPage(
  
  pageWithSidebar(
      headerPanel('Weather Data Download in Bulk from ECCC Website'),
      sidebarPanel(
          width = 3,
          radioButtons(inputId = "main_selector", 
                       label = h3('Select ID Type'),
                       choices = list('Climate ID', 'WMO ID', 'TC ID'),
                       selected = 'Climate ID'),
          uiOutput("ui_selected"),
          uiOutput("ui_numeric_inputs")
      ),
      
      mainPanel(
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

          ),
          
          tabPanel("Stations Map",
                   br(),
                   print(h3("Database is still loading if map is not visible")),
                   br(),
                   leafletOutput("MapPlot"),
                   print("Zoom into map to see station locations"),
                   br(),
                   print("Click on a location to see Station ID, Station Name, and Status: active/discontinued")
          ),
          
          tabPanel("Data Table",
                   br(),
                   textOutput("name"),
                   br(),
                   print("Select Daily, Monthly, or Yearly Resolution"),
                   selectInput("Reso", "", c("Daily" = "Daily", "Monthly" = "Monthly", "Yearly" = "Yearly"),
                               selected = "Daily"), p(""),
                   DT::dataTableOutput("table")
          )
          
        )
      )
  ))
)