rm(list=ls())
library(shiny)
library(bslib)  # supersedes shinythemes; provides Bootswatch themes (incl. "yeti")
library(leaflet)
library(plotly)
library(waiter) #loading screen/spinner
library(DT)
library(shinyjs)

shinyUI(fluidPage(
  
  use_waiter(),
  waiter_show_on_load(html = spin_3k(), color = "black"), # place at the top before content
  useShinyjs(),  # Add this line to enable shinyjs
  
  # bslib replacement for shinytheme("yeti"); version = 3 keeps the Bootstrap 3
  # look the app was built against (matches the original shinythemes rendering)
  theme = bs_theme(version = 3, bootswatch = "yeti"),
  tags$head(HTML("<title>Canadian Climate Data Portal</title>")),
  
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
    
    headerPanel(title=div('Canadian Climate Data Portal',
                img(src='https://upload.wikimedia.org/wikipedia/commons/d/d9/Flag_of_Canada_%28Pantone%29.svg',
                  alt = "Canadian Flag",
                  style = "margin-left: 10px; max-width: 36px; height: auto; vertical-align: middle;")
    )),
    
    # Sidebar panel
    sidebarPanel(
      width = 3,
      
      selectInput(inputId = "province_selector",
            label = h4('Select Province/Territory'),
            choices = list(
              'Alberta' = 'ALBERTA',
              'British Columbia' = 'BRITISH COLUMBIA',
              'Manitoba' = 'MANITOBA',
              'New Brunswick' = 'NEW BRUNSWICK',
              'Newfoundland and Labrador' = 'NEWFOUNDLAND',
              'Northwest Territories' = 'NORTHWEST TERRITORIES',
              'Nova Scotia' = 'NOVA SCOTIA',
              'Nunavut' = 'NUNAVUT',
              'Ontario' = 'ONTARIO',
              'Prince Edward Island' = 'PRINCE EDWARD ISLAND',
              'Quebec' = 'QUEBEC',
              'Saskatchewan' = 'SASKATCHEWAN',
              'Yukon' = 'YUKON TERRITORY'
            ),
            selected = 'BRITISH COLUMBIA'),
      
      br(),
      
      selectInput("global_data_interval", h4("Data Interval (Applied to All Stations):"),
                 choices = list(
                   "Hourly" = "hourly",
                   "Daily" = "daily", 
                   "Monthly" = "monthly"
                 ),
                 selected = "daily"),

      br(),

      # Direct Climate ID entry (skip the map for known stations)
      tags$div(
        style = "border: 1px dashed #6c757d; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
        h5("Enter Climate ID Directly", style = "margin-top: 0;"),
        p("Already have a Climate ID? Assign it to a station slot below without searching the map. Works for stations in any province/territory.",
          style = "font-size: 0.85em; color: #666; margin-bottom: 10px;"),

        # Primary
        tags$label("Primary", style = "color: magenta; font-weight: bold; margin-bottom: 2px;"),
        fluidRow(
          column(7, textInput("climate_id_primary", label = NULL, placeholder = "e.g. 1108447")),
          column(5, actionButton("set_primary_id", "Set", class = "btn-sm",
                                 style = "background-color: magenta; color: white; width: 100%;"))
        ),

        # Secondary
        tags$label("Secondary", style = "color: #006400; font-weight: bold; margin-bottom: 2px;"),
        fluidRow(
          column(7, textInput("climate_id_secondary", label = NULL, placeholder = "e.g. 1100120")),
          column(5, actionButton("set_secondary_id", "Set", class = "btn-sm btn-success",
                                 style = "width: 100%;"))
        ),

        # Tertiary
        tags$label("Tertiary", style = "color: darkblue; font-weight: bold; margin-bottom: 2px;"),
        fluidRow(
          column(7, textInput("climate_id_tertiary", label = NULL, placeholder = "e.g. 1101158")),
          column(5, actionButton("set_tertiary_id", "Set", class = "btn-sm",
                                 style = "background-color: darkblue; color: white; width: 100%;"))
        )
      ),

      # Primary Station Display
      conditionalPanel(
        condition = "output.has_primary_station",
        h5("Primary Station:", style = "color: magenta;"),
        verbatimTextOutput("primary_station_info"),
        br(),
        actionButton("clear_primary", "Remove Primary Station Selected", class = "btn-sm", style = "background-color: magenta; color: white;"),
        br(), br()
      ),
      
      # Secondary Station Display
      conditionalPanel(
        condition = "output.has_secondary_station",
        h5("Secondary Station:", style = "color: #006400;"),
        verbatimTextOutput("secondary_station_info"),
        br(),
        actionButton("clear_secondary", "Remove Secondary Station Selected", class = "btn-sm btn-success"),
        br(), br()
      ),
      
      # Tertiary Station Display
      conditionalPanel(
        condition = "output.has_tertiary_station",
        h5("Tertiary Station:", style = "color: blue;"),
        verbatimTextOutput("tertiary_station_info"),
        br(),
        actionButton("clear_tertiary", "Remove Tertiary Station Selected", class = "btn-sm", style = "background-color: darkblue; color: white;"),
        br(), br()
      ),
      
      # Additional controls can be added here based on selected tab
      uiOutput("sidebar_controls"),

      # --- Advanced: ERA5-Land infilling ---
      hr(),
      div(
        strong("Advanced: ERA5-Land Infilling"),
        actionButton("run_era5_infill", "Run ERA5 Infilling",
                     icon = icon("cloud-arrow-down"),
                     class = "btn-warning", style = "width:100%; margin-top:6px;"),
        helpText("Fills gaps in the PRIMARY station with bias-corrected ERA5-Land reanalysis. Results appear in the Advanced Usage (ERA5 Infill) tab; see its Instructions sub-tab for the one-time CDS API-key setup.")
      )

    ), # end of side bar panel
    
    # Main Panel
    mainPanel(
      
      # Main tabset panel
      tabsetPanel(id = "main_tabs",
        
        tabPanel("ReadMe",
                 br(),
                 div(style = "max-width: 1000px; margin: 0 auto;",
                   h3("📖 Overview", style = "color: #34495e;"),
                   p("This interactive web application provides seamless access to Environment and Climate Change Canada's comprehensive weather station network. Users can explore, download, and analyze historical climate data with powerful visualization and statistical tools."),
                   
                   br(),
                   h3("✨ Key Features", style = "color: #34495e;"),
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff;",
                   tags$ul(
                     tags$li(strong("Interactive Map:"), " Explore 8,000+ weather stations across Canada with detailed station information"),
                     tags$li(strong("Multi-Station Analysis:"), " Compare data from up to 3 stations simultaneously"),
                     tags$li(strong("Flexible Data Intervals:"), " Access hourly, daily, and monthly climate records"),
                     tags$li(strong("Time Series Visualization:"), " Interactive plots with zoom, pan, and selection tools"),
                     tags$li(strong("Statistical Analysis:"), " Generate monthly and yearly summaries with trend analysis"),
                     tags$li(strong("Data Export:"), " Download data in CSV and Excel formats"),
                     tags$li(strong("Quality Control:"), " Access to data quality flags and metadata")
                   )
                   ),
                   
                   br(),
                   h3("🚀 Getting Started Guide", style = "color: #34495e;"),
                   
                   h4("Step 1: Select Your Region", style = "color: #495057;"),
                   p("Use the", strong("Province/Territory selector"), "in the sidebar to focus on your area of interest. The map will automatically zoom to your selected region."),
                   
                   h4("Step 2: Choose Data Interval", style = "color: #495057;"),
                   p("Select your preferred data resolution:"),
                   tags$ul(
                   tags$li(strong("Hourly:"), " Detailed meteorological observations (large datasets - use with caution)"),
                   tags$li(strong("Daily:"), " Daily summaries including min/max temperatures, precipitation totals"),
                   tags$li(strong("Monthly:"), " Monthly climate summaries and averages")
                   ),
                   
                   h4("Step 3: Explore Weather Stations", style = "color: #495057;"),
                   p("Navigate to the", strong("Map tab"), "to explore available weather stations:"),
                   tags$ul(
                   tags$li("Station markers are color-coded by record length (red: <10 years, blue: 60+ years)"),
                   tags$li("Numbers on markers indicate the last year of daily records"),
                   tags$li("Click any marker to view detailed station information"),
                   tags$li("Use Primary (magenta), Secondary (green), or Tertiary (blue) buttons to select stations"),
                   tags$li(strong("Already know the Climate ID?"), " Skip the map — type it into the ", strong("Enter Climate ID Directly"), " box in the sidebar and click 'Set' to assign it to a station slot (works for any province/territory)")
                   ),
                   
                   h4("Step 4: Download Climate Data", style = "color: #495057;"),
                   p("In the", strong("Data Download tab"), ":"),
                   tags$ul(
                   tags$li("Select date ranges for each chosen station"),
                   tags$li("Click 'Retrieve Data from ECCC' to download historical records"),
                   tags$li("Preview downloaded data in the interactive table"),
                   tags$li("Export combined datasets as CSV or Excel files")
                   ),
                   
                   h4("Step 5: Analyze Your Data", style = "color: #495057;"),
                   p("Use the", strong("Raw Data Review tab"), "for analysis:"),
                   tags$ul(
                   tags$li(strong("Raw Time Series:"), " Plot and compare multiple variables across stations"),
                   tags$li(strong("Summary Statistics:"), " Generate monthly/yearly summaries with trend lines"),
                   tags$li("Remove outliers using interactive selection tools"),
                   tags$li("Customize visualizations with different y-axis labels and color schemes")
                   ),
                   
                   br(),
                   h3("💡 Tips for Effective Use", style = "color: #34495e;"),
                   div(style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;",
                   tags$ul(
                     tags$li(strong("Start Small:"), " Begin with daily data and shorter time periods before exploring larger datasets"),
                     tags$li(strong("Check Data Availability:"), " Review station record periods before downloading"),
                     tags$li(strong("Compare Nearby Stations:"), " Use multiple stations to validate patterns and fill data gaps"),
                     tags$li(strong("Use Summary Tools:"), " Monthly/yearly summaries are ideal for climate trend analysis"),
                     tags$li(strong("Export Your Work:"), " Download processed summaries for use in other applications")
                   )
                   ),
                   
                   br(),
                   h3("📊 Data Sources & Quality", style = "color: #34495e;"),
                   p("All data is sourced directly from Environment and Climate Change Canada's historical weather database, which includes:"),
                   tags$ul(
                   tags$li("Over 8,000 weather stations across Canada"),
                   tags$li("Records dating back to the 1840s for some locations"),
                   tags$li("Quality-controlled and standardized measurements"),
                   tags$li("Real-time updates for recent observations")
                   ),
                   
                   br(),
                   h3("🙏 Acknowledgements", style = "color: #34495e;"),
                   div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 4px solid #28a745;",
                   p("This application was developed with gratitude to the following individuals and organizations:"),
                   tags$ul(
                     tags$li(strong("Environment and Climate Change Canada (ECCC):"), " For providing open access to Canada's comprehensive historical climate database and maintaining the extensive network of weather monitoring stations."),
                     tags$li(strong("Professor Dan Moore, University of British Columbia, Department of Geography:"), " For developing the foundational data retrieval methodologies and providing technical guidance on accessing ECCC climate data programmatically."),
                     tags$li(strong("The R Community:"), " For developing and maintaining the open-source packages that power this application, including Shiny, leaflet, plotly, and tidyverse."),
                     tags$li(strong("Knight Piésold Ltd.:"), " For supporting the development of this application for the Canadian climate research community.")
                   ),
                   hr(style = "margin: 15px 0; border-color: #dee2e6;"),
                   p(style = "font-style: italic; margin-bottom: 0;", "Special recognition goes to the dedicated meteorologists, technicians, and data management professionals who ensure the continuity and quality of Canada's climate monitoring network.")
                   ),
                   
                   br(),
                   h3("❓ Need Help?", style = "color: #34495e;"),
                   div(style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; border-left: 4px solid #0dcaf0;",
                   p("If you encounter issues or have questions:"),
                   tags$ul(
                     tags$li("Check the notification messages for real-time feedback"),
                     tags$li("Ensure your selected stations have data for your chosen time period"),
                     tags$li("Try refreshing the page if plots don't appear"),
                     tags$li("Report bugs or request features by ",
                             tags$a(href = "https://github.com/nickyrong/ShinyWeatherCan/issues",
                                    target = "_blank", "opening an issue on GitHub"))
                   )
                   ),
                   
                   br(),
                   h3("📄 License & Disclaimer", style = "color: #34495e;"),
                   div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 4px solid #6c757d;",
                   h5("License", style = "color: #495057;"),
                   p("Copyright © 2025 Nick Rong"),
                     div(style = "display: flex; align-items: center; margin: 10px 0;",
                     p("Released under the License: GNU General Public License v3.0", style = "margin: 0; margin-right: 10px;"),
                     a(href = "https://www.gnu.org/licenses/gpl-3.0", target = "_blank",
                       img(src = "https://www.gnu.org/graphics/gplv3-88x31.png", 
                         alt = "GNU GPL v3",
                         style = "height: 20px; width: auto;")
                     )
                     ),
                   p("Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License."),
                   hr(style = "margin: 15px 0; border-color: #dee2e6;"),
                   h5("Disclaimer", style = "color: #495057;"),
                   p(style = "font-size: 0.9em; color: #6c757d;", "This application is provided for research and educational purposes. While we strive for accuracy, users should verify data for critical applications. The developers are not responsible for any decisions made based on the data provided through this application.")
                   ),
                   
                   br()
                 )
        ), # End of ReadMe tab
        
        tabPanel("Map",
                 br(),
                 h4("Weather Stations Map"),
                 p("Explore weather stations across the selected province/territory."),
                 br(),
                 leafletOutput("MapPlot", height = 600),
                 br(),
                 p("Zoom into map to see station locations. Click on a station marker to see detailed information.")
        ), # End of Map tab
        
        tabPanel("Data Download",
           br(),
           h4("Download Climate Data"),
           p("Download historical climate data for selected weather stations."),
           br(),
           
           # Download speed information
           div(
             style = "background-color: #e7f3ff; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff; margin: 10px 0;",
             HTML("<strong>⏱️ Download Speed Information:</strong><br>
                  • Daily data: Approximately 1 second per year of data<br>
                  • Hourly data: Approximately 15 seconds per year of data<br>
                  • Larger date ranges will take longer - please be patient during the download process")
           ),
           br(),
           
           br(),
         
           # Dynamic station download controls (rendered server-side)
           uiOutput("station_download_controls"),
           
           br(),
           conditionalPanel(
             condition = "output.has_primary_data || output.has_secondary_data || output.has_tertiary_data",
             div(
               style = "color: green; font-weight: bold; background-color: #e6ffe6; padding: 10px; border: 1px solid green; border-radius: 5px; margin: 10px 0;",
               HTML("<strong>✓ Info:</strong> Use the buttons below to download the raw ECCC data as CSV or Excel files.")
             ),
             br(),
             fluidRow(
               column(6,
                 downloadButton("download_csv", "Download as CSV", 
                               class = "btn-success btn-block",
                               icon = icon("download"))
               ),
               column(6,
                 downloadButton("download_excel", "Download as Excel", 
                               class = "btn-info btn-block", 
                               icon = icon("file-excel"))
               )
             ),
             br()
           ),
           br(),
           DT::dataTableOutput("download_preview")
        ), # End of Data Download tab
        
        tabPanel("Raw Data Review",
           br(),
           # Secondary tabset for Data Review
             tabsetPanel(id = "data_review_tabs",
             
             tabPanel("Raw Time Series",
              br(),
              h4("Raw Time Series Review"),
              htmlOutput("timeseries_info"),
              br(),
              fluidRow(
                column(6,
                  selectInput("y_axis_variable", "Select Y-axis Label:",
                      choices = list(
                          "Value" = "Value",
                          "Max Temperature (°C)" = "Max Temperature (°C)",
                          "Min Temperature (°C)" = "Min Temperature (°C)", 
                          "Mean Temperature (°C)" = "Mean Temperature (°C)",
                          "Heat Degree Days" = "Heat Degree Days",
                          "Cool Degree Days" = "Cool Degree Days",
                          "Total Rain (mm)" = "Total Rain (mm)",
                          "Total Snow (cm)" = "Total Snow (cm)",
                          "Total Precipitation (mm)" = "Total Precipitation (mm)",
                          "Snow on Ground (cm)" = "Snow on Ground (cm)",
                          "Direction of Max Gust (10's deg)" = "Direction of Max Gust (10's deg)",
                          "Speed of Max Gust (km/h)" = "Speed of Max Gust (km/h)",
                          "Temperature (°C)" = "Temperature (°C)",
                          "Dew Point (°C)" = "Dew Point (°C)",
                          "Relative Humidity (%)" = "Relative Humidity (%)",
                          "Precipitation Amount (mm)" = "Precipitation Amount (mm)",
                          "Wind Direction (10's deg)" = "Wind Direction (10's deg)",
                          "Wind Speed (km/h)" = "Wind Speed (km/h)",
                          "Visibility (km)" = "Visibility (km)",
                          "Station Pressure (kPa)" = "Station Pressure (kPa)",
                          "Humidex" = "Humidex",
                          "Wind Chill" = "Wind Chill"
                       ),
                       selected = "Value")
                )
              ),
              br(),
              plotlyOutput("raw_plotly", height = 500),
              br(),
              h5("Interactive controls:"),
              p("Use the toolbar above the plot to zoom, pan, and explore the data. Double-click to reset the view."),
              br()
             ), # End of Station Time Series subtab
             
             tabPanel("Summary Statistics (Raw Data)",
              br(),
              p("Generate summary statistics for the downloaded climate data. Select variables, time periods, and statistics to analyze patterns and trends in your data."),
              br(),
              
              # Warning message for non-daily data
              conditionalPanel(
                condition = "input.global_data_interval != 'daily'",
                div(
                  style = "color: orange; font-weight: bold; background-color: #fff3cd; padding: 10px; border: 1px solid orange; border-radius: 5px; margin: 10px 0;",
                  HTML("<strong>⚠️ Notice:</strong> The data summary functionality is currently only available for daily data intervals. Please select 'Daily' in the Data Download tab to use this feature.")
                )
              ),
              
              # User input options row - only show for daily data
              conditionalPanel(
                condition = "input.global_data_interval == 'daily'",
                fluidRow(
                  column(3,
                    selectizeInput("summary_variable", "Variable to Summarize:",
                       choices = NULL,
                       multiple = TRUE,
                       options = list(placeholder = "Select variable(s)"))
                    ),
                  column(3,
                  selectInput("summary_period", "Summary Period:",
                         choices = list(
                         "Monthly" = "monthly",
                         "Yearly" = "yearly"
                         ),
                         selected = "monthly")
                  ),
                  column(3,
                    selectizeInput("summary_statistic", "Statistic(s):",
                       choices = list(
                         "Mean" = "mean",
                         "Median" = "median",
                         "Min" = "min",
                         "Max" = "max",
                         "Sum" = "sum",
                         "Standard Deviation" = "sd",
                         "Skewness" = "skewness"
                       ),
                       selected = "mean",
                       multiple = TRUE,
                       options = list(placeholder = "Select statistic(s)"))
                    ),
                  column(3,
                    conditionalPanel(
                      condition = "input.summary_period == 'monthly'",
                      sliderInput("min_days_monthly", "Min Days for Monthly:",
                                  min = 15, max = 31, value = 20, step = 1)
                    ),
                    conditionalPanel(
                      condition = "input.summary_period == 'yearly'",
                      sliderInput("min_days_yearly", "Min Days for Yearly:",
                                  min = 200, max = 366, value = 300, step = 5)
                    )
                  )

                ),
                br(),
                
                # Summary plot controls and visualization
                fluidRow(
                  column(3,
                    selectInput("summary_y_axis_variable", "Select Y-axis Label:",
                        choices = list(
                            "Value" = "Value",
                            "Max Temperature (°C)" = "Max Temperature (°C)",
                            "Min Temperature (°C)" = "Min Temperature (°C)", 
                            "Mean Temperature (°C)" = "Mean Temperature (°C)",
                            "Heat Degree Days" = "Heat Degree Days",
                            "Cool Degree Days" = "Cool Degree Days",
                            "Total Rain (mm)" = "Total Rain (mm)",
                            "Total Snow (cm)" = "Total Snow (cm)",
                            "Total Precipitation (mm)" = "Total Precipitation (mm)",
                            "Snow on Ground (cm)" = "Snow on Ground (cm)",
                            "Direction of Max Gust (10's deg)" = "Direction of Max Gust (10's deg)",
                            "Speed of Max Gust (km/h)" = "Speed of Max Gust (km/h)",
                            "Temperature (°C)" = "Temperature (°C)",
                            "Dew Point (°C)" = "Dew Point (°C)",
                            "Relative Humidity (%)" = "Relative Humidity (%)",
                            "Precipitation Amount (mm)" = "Precipitation Amount (mm)",
                            "Wind Direction (10's deg)" = "Wind Direction (10's deg)",
                            "Wind Speed (km/h)" = "Wind Speed (km/h)",
                            "Visibility (km)" = "Visibility (km)",
                            "Station Pressure (kPa)" = "Station Pressure (kPa)",
                            "Humidex" = "Humidex",
                            "Wind Chill" = "Wind Chill"
                         ),
                         selected = "Value")
                  ),
                  column(2,
                    br(),
                    checkboxInput("show_trend_line", "Show Linear Trend Line", value = FALSE)
                  ),
                  column(2,
                    br(),
                    actionButton("clear_removed_points", "Clear Removed Points", 
                                class = "btn btn-warning btn-sm")
                  ),
                  column(5,
                    dateRangeInput("summary_date_range", "Date Range for Summary:",
                                  start = as.Date("1900-01-01"),
                                  end = Sys.Date(),
                                  min = as.Date("1840-01-01"),
                                  max = Sys.Date(),
                                  format = "yyyy-mm-dd",
                                  separator = " to ")
                  )
                ),
                br(),
                
                # Summary plot
                h5("Summary Visualization:"),
                plotlyOutput("raw_summary_plot", height = 400),
                br(),
                
                # Summary table section
                h5("Summary Data Table:"),
                uiOutput("download_summary_button"),
                br(),
                DT::dataTableOutput("summary_table"),
                br()
              )

             ) # End of Station Summary Table subtab
             
           ) # End of Data Review secondary tabset
        ), # End of Raw Data Review tab
        
        tabPanel("Advanced Usage (ERA5 Infill)", value = "advanced_usage",
                 br(),
                 h4("ERA5-Land Infilling"),
                 uiOutput("era5_infill_status"),
                 br(),
                 tabsetPanel(
                   id = "advanced_subtabs",

                   tabPanel("Instructions", value = "instructions",
                     br(),
                     h4("Setting up ERA5-Land infilling"),
                     p("This tool fills gaps in the PRIMARY station's daily record with ",
                       "bias-corrected ", tags$b("ERA5-Land"), " reanalysis from the Copernicus ",
                       "Climate Data Store (CDS). To run it you need a free CDS account and a ",
                       "Personal Access Token (API key). ",
                       tags$b("This is a one-time setup.")),

                     tags$div(class = "well", style = "background:#fff3cd;border-color:#ffe69c;",
                       h5(tags$b("Disclaimer")),
                       p("This tool and its bias-corrected output are provided ", tags$b("“as is”"),
                         ", ", tags$b("without warranty of any kind"), ", express or implied, ",
                         "including but not limited to fitness for a particular purpose. ",
                         "Infilled values are ", tags$b("model-based estimates, not measurements"),
                         ", and are used at your own risk and professional judgment."),
                       p("Always seek ", tags$b("independent verification"), " before using infilled ",
                         "data in analysis or design — for example, compare against nearby ECCC ",
                         "stations or stations from other networks (in BC: FLNRORD/FLNR, MoTI/RWIS, ",
                         "BC Hydro, or regional/municipal gauges).")
                     ),

                     tags$hr(),
                     h5("1. Register / log in to the CDS"),
                     tags$ul(
                       tags$li(tags$a(href = "https://cds.climate.copernicus.eu/how-to-api",
                                      target = "_blank", "Open the CDS \"How to use the API\" page"),
                               " and register (or log in if you already have an account).")
                     ),
                     h5("2. Copy your Personal Access Token (API key)"),
                     tags$ul(
                       tags$li("Once logged in, the ", tags$code("how-to-api"),
                               " page shows your credentials. Copy the value on the ",
                               tags$code("key:"), " line (your Personal Access Token) — that ",
                               "is what you paste into this app.")
                     ),
                     if (file.exists("www/cds_api_key.png"))
                       tags$img(src = "cds_api_key.png", alt = "CDS how-to-api page showing the key",
                                style = "max-width:100%;border:1px solid #ccc;margin:6px 0;")
                     else helpText(tags$em("[screenshot placeholder — add app/www/cds_api_key.png]")),
                     h5("3. Accept ALL dataset licences"),
                     tags$ul(
                       tags$li("Open your ",
                               tags$a(href = "https://cds.climate.copernicus.eu/profile?tab=licences",
                                      target = "_blank", "CDS profile → Licences"), " page and ",
                               tags$b("accept every licence listed"), " (there are several — the ",
                               "Terms of use, the CC-BY licence, the CCI product licence, the ",
                               "additional non-European contributions licence, etc.). ERA5-Land ",
                               "downloads fail until the relevant licences are accepted.")
                     ),
                     if (file.exists("www/cds_licences.png"))
                       tags$img(src = "cds_licences.png", alt = "CDS profile Licences page",
                                style = "max-width:100%;border:1px solid #ccc;margin:6px 0;")
                     else helpText(tags$em("[screenshot placeholder — add app/www/cds_licences.png]")),
                     h5("4. Run the infilling"),
                     tags$ul(
                       tags$li("Set and download a PRIMARY station on the ", tags$b("daily"),
                               " interval, then click ", tags$b("Run ERA5 Infilling"), " in the sidebar."),
                       tags$li("Paste your own ", tags$b("CDS API Key"), " into the prompt. ",
                               tags$b("Your key is used only for that run and is not stored on the server."))
                     ),

                     tags$hr(),
                     h4("Interpreting & using the results"),

                     h5("A. How much record do you need?"),
                     p("The bias correction is fit on the ", tags$b("overlap"), " between the ",
                       "station's QAQC'd observations and ERA5 (which starts in 1950), separately ",
                       "for each calendar month. Rough guidance on overlap length:"),
                     tags$ul(
                       tags$li(tags$b("~3–4 years: bare minimum."), " Enough to fit a monthly ",
                               "quantile map (each calendar month then has ~90–120 overlapping days). ",
                               "Any calendar month with fewer than ~30 overlapping days falls back to ",
                               "a simple mean-shift (temperature) or scale (precipitation) for that month."),
                       tags$li(tags$b("7+ years: enables verification."), " The tool holds out ",
                               tags$b("random complete months, evenly spread across the calendar"),
                               " (the same number of Januarys, Februarys, … as an independent check) and ",
                               "fits on all the remaining overlap. The number per calendar month scales ",
                               "with record: >30 yr → 10, >20 → 5, >10 → 3, ≥7 → 2. Below ~7 years / 24 ",
                               "complete months there is no held-out validation."),
                       tags$li(tags$b("10+ years of calibration: ideal."), " More overlap gives a ",
                               "more stable quantile map, especially in the distribution tails.")
                     ),

                     h5("B. How the correction works — calibration vs. verification (CDF Review)"),
                     p("Quantile Delta Mapping (QDM) reshapes the ERA5 distribution to match the ",
                       "observed distribution, quantile-by-quantile, per calendar month:"),
                     tags$ul(
                       tags$li(tags$b("Calibration"), " = the overlap months the map is fitted on ",
                               "(all overlap except the held-out validation months)."),
                       tags$li(tags$b("Verification"), " = ", tags$b("random complete months, evenly ",
                               "spread across the calendar"), " (same count per calendar month) and across ",
                               "the record, ", tags$b("held out"), " from fitting and used only to check the ",
                               "result. Random (rather than the earliest years) so a non-stationary early ",
                               "period can't confound the check.")
                     ),
                     p("The two ", tags$b("CDF Review"), " sub-tabs plot the empirical cumulative ",
                       "distribution (CDF) for each month: ", tags$b("observed (black)"), ", ",
                       tags$b("raw ERA5 (red dashed)"), ", and ", tags$b("bias-corrected (blue)"),
                       ". A good result: the blue curve tracks the black curve. On the ",
                       tags$b("Validation"), " tab, blue tracking black on the ", tags$em("held-out"),
                       " years indicates the correction generalizes to data it was not trained on."),
                     tags$div(style = "color:#b8860b;",
                       tags$b("Caveat — non-stationarity: "),
                       "QDM assumes the ERA5-vs-station relationship is stable over time. If the ",
                       "station record has strong non-stationarity (a site relocation, instrument ",
                       "or exposure change, urbanization, or a regime/trend shift), a good ",
                       "calibration fit does ", tags$b("not"), " guarantee the correction is valid ",
                       "elsewhere in the record. Check the Validation CDFs and the time series."),

                     h5("C. Examine the infilled time series"),
                     p("On the ", tags$b("Infilled Time Series"), " sub-tab, ",
                       tags$b("black"), " markers are ECCC observations and ", tags$b("orange"),
                       " markers are ERA5-infilled values. Scan the orange points for implausible ",
                       "high or low outliers — especially near the start/end of the record and ",
                       "during extreme events."),
                     p(tags$b("Extrapolation: "),
                       "QDM corrects by quantile. For an infilled value more extreme than anything ",
                       "seen in the calibration overlap, the quantile saturates at the last ",
                       "(highest / lowest) calibration quantile, so the applied correction is held ",
                       "constant at that ", tags$b("last-quantile delta"), " (an additive offset for ",
                       "temperature; a capped multiplicative ratio for precipitation). This is ",
                       "standard QDM behaviour, but it means corrections in the far tails are the ",
                       "least certain — sanity-check extreme infilled values before relying on them.")
                   ),

                   tabPanel("CDF Review - Calibration",
                     br(),
                     uiOutput("cdf_calib_range"),
                     p("QDM is fit on the calibration months (all overlap except the held-out ",
                       "validation months). For each month: empirical CDFs of observed vs raw ERA5 ",
                       "vs bias-corrected. The corrected (blue) curve should track the observed (black) curve."),
                     radioButtons("era5_cdf_var_calib", "Variable to review:",
                                  choices = c("Min Temperature"     = "MIN_TEMPERATURE",
                                              "Max Temperature"     = "MAX_TEMPERATURE",
                                              "Mean Temperature"    = "MEAN_TEMPERATURE",
                                              "Total Precipitation" = "TOTAL_PRECIPITATION"),
                                  selected = "MEAN_TEMPERATURE", inline = TRUE),
                     plotlyOutput("era5_cdf_plot_calib", height = 820)
                   ),

                   tabPanel("CDF Review - Validation",
                     br(),
                     uiOutput("cdf_valid_range"),
                     p("Independent check on a set of random complete months spread across the ",
                       "record (held out from fitting; see above). The bias-corrected (blue) curve ",
                       "tracking observed (black) here indicates the correction generalizes to data ",
                       "it was not trained on."),
                     radioButtons("era5_cdf_var_valid", "Variable to review:",
                                  choices = c("Min Temperature"     = "MIN_TEMPERATURE",
                                              "Max Temperature"     = "MAX_TEMPERATURE",
                                              "Mean Temperature"    = "MEAN_TEMPERATURE",
                                              "Total Precipitation" = "TOTAL_PRECIPITATION"),
                                  selected = "MEAN_TEMPERATURE", inline = TRUE),
                     plotlyOutput("era5_cdf_plot_valid", height = 820)
                   ),

                   tabPanel("Infilled Time Series",
                     br(),
                     p("QAQC'd station observations, with internal gaps filled by ",
                       "bias-corrected ERA5-Land. Black = ECCC observed; orange = ERA5-infilled."),
                     radioButtons("infill_ts_var", "Variable:",
                                  choices = c("Min Temperature"     = "MIN_TEMPERATURE",
                                              "Max Temperature"     = "MAX_TEMPERATURE",
                                              "Mean Temperature"    = "MEAN_TEMPERATURE",
                                              "Total Precipitation" = "TOTAL_PRECIPITATION"),
                                  selected = "MEAN_TEMPERATURE", inline = TRUE),
                     plotlyOutput("infill_ts_plot", height = 520)
                   ),

                   tabPanel("Infilled Summary Statistics",
                     br(),
                     p("Summary statistics on the infilled series (QAQC'd observations + ",
                       "bias-corrected ERA5 in the gaps). Temperature: mean/min/max; ",
                       "precipitation: sum."),
                     fluidRow(
                       column(6, radioButtons("infill_summary_period", "Period:",
                                    choices = c("Yearly" = "yearly", "Monthly" = "monthly"),
                                    selected = "yearly", inline = TRUE)),
                       column(6, radioButtons("infill_summary_var", "Variable to plot:",
                                    choices = c("Min Temp"  = "MIN_TEMPERATURE",
                                                "Max Temp"  = "MAX_TEMPERATURE",
                                                "Mean Temp" = "MEAN_TEMPERATURE",
                                                "Precip"    = "TOTAL_PRECIPITATION"),
                                    selected = "MEAN_TEMPERATURE", inline = TRUE))
                     ),
                     plotlyOutput("infill_summary_plot", height = 380),
                     br(),
                     downloadButton("download_infill_summary", "Download summary (Excel)",
                                    class = "btn-success btn-sm"),
                     br(), br(),
                     DT::dataTableOutput("infill_summary_table")
                   )
                 )
                 
                 # # Secondary tabset for Advanced Usage
                 # tabsetPanel(id = "advanced_tabs",
                 #   
                 #   tabPanel("Infilling",
                 #            br(),
                 #            h4("Data Infilling Methods"),
                 #            p("Tools for filling gaps in climate data using statistical methods."),
                 #            br(),
                 #            fluidRow(
                 #              column(6,
                 #                selectInput("infill_method", "Infilling Method:",
                 #                           choices = list(
                 #                             "Linear Interpolation" = "linear",
                 #                             "Spline Interpolation" = "spline",
                 #                             "Seasonal Average" = "seasonal"
                 #                           ))
                 #              ),
                 #              column(6,
                 #                numericInput("max_gap", "Maximum Gap Length (days):", 
                 #                           value = 7, min = 1, max = 365)
                 #              )
                 #            ),
                 #            br(),
                 #            actionButton("apply_infill", "Apply Infilling", class = "btn-primary"),
                 #            br(), br(),
                 #            plotlyOutput("infill_plot", height = 400)
                 #   ), # End of Infilling subtab
                 #   
                 #   tabPanel("Corrected Time Series",
                 #            br(),
                 #            h4("Quality-Corrected Time Series"),
                 #            p("View and analyze quality-controlled and corrected climate data."),
                 #            br(),
                 #            htmlOutput("corrected_timeseries_info"),
                 #            br(),
                 #            plotlyOutput("corrected_timeseries_plot", height = 500),
                 #            br(),
                 #            checkboxInput("show_flags", "Show Quality Control Flags", value = FALSE),
                 #            conditionalPanel(
                 #              condition = "input.show_flags == true",
                 #              DT::dataTableOutput("qc_flags_table")
                 #            )
                 #   ), # End of Corrected Time Series subtab
                 #   
                 #   tabPanel("Corrected Station Summary Table",
                 #            br(),
                 #            h4("Corrected Station Data Summary"),
                 #            p("Summary statistics for quality-controlled climate data."),
                 #            br(),
                 #            htmlOutput("corrected_summary_info"),
                 #            br(),
                 #            uiOutput("download_corrected_button"),
                 #            br(),
                 #            DT::dataTableOutput("corrected_summary_table"),
                 #            br()
                 #   ) # End of Corrected Station Summary Table subtab
                 #   
                 # ) # End of Advanced Usage secondary tabset
        ) # End of Advanced Usage tab
        
      ) # End of main tabset
      
    ) # End of main panel
    
  ) # End of Page with Panel
  
)) # End of Script