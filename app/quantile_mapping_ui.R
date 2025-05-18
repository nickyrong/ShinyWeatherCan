# UI and server functions for the quantile mapping infill feature

# UI for the quantile mapping tab
quantile_mapping_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h4("Infill Settings"),
          
          # Target station selection (using the same selector from the main app)
          uiOutput(ns("target_station_ui")),
          
          # Variable to infill
          selectInput(ns("variable_to_infill"), 
                     "Variable to Infill", 
                     choices = NULL, 
                     selected = NULL),
          
          # Date range selector
          dateRangeInput(ns("date_range"), 
                        "Date Range",
                        start = Sys.Date() - 3650, 
                        end = Sys.Date()),
          
          # Advanced settings collapsible panel
          tags$div(
            class = "panel panel-default",
            tags$div(
              class = "panel-heading",
              tags$h4(
                class = "panel-title",
                tags$a(
                  "data-toggle" = "collapse",
                  "href" = paste0("#", ns("advanced_settings")),
                  "Advanced Settings"
                )
              )
            ),
            tags$div(
              id = ns("advanced_settings"),
              class = "panel-collapse collapse",
              tags$div(
                class = "panel-body",
                numericInput(ns("max_distance"), 
                            "Maximum Distance (km)", 
                            value = 100, 
                            min = 10, 
                            max = 500),
                
                numericInput(ns("min_overlap"), 
                            "Minimum Overlap (years)", 
                            value = 5, 
                            min = 1, 
                            max = 30),
                
                checkboxInput(ns("by_month"), 
                             "Process by Month", 
                             value = TRUE),
                
                numericInput(ns("min_datapoints"), 
                            "Minimum Data Points per Mapping", 
                            value = 30, 
                            min = 10, 
                            max = 100)
              )
            )
          ),
          
          # Action buttons
          actionButton(ns("find_stations_btn"), 
                      "Find Nearby Stations", 
                      class = "btn-primary"),
          
          actionButton(ns("run_infill_btn"), 
                      "Run Infill", 
                      class = "btn-success", 
                      style = "margin-top: 10px;")
        )
      ),
      
      column(
        width = 9,
        
        # Tabs for different outputs
        tabsetPanel(
          id = ns("result_tabs"),
          
          # Tab for nearby stations
          tabPanel(
            "Nearby Stations",
            DT::dataTableOutput(ns("nearby_stations_table")),
            plotlyOutput(ns("nearby_stations_map"), height = "500px")
          ),
            # Tab for infill results
          tabPanel(
            "Infill Results",
            fluidRow(
              column(
                width = 12,
                plotlyOutput(ns("infill_timeseries"), height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                h4("Infill Donor Stations Summary"),
                DT::dataTableOutput(ns("infill_summary_table"))
              ),
              column(
                width = 6,
                h4("Monthly Infill Statistics"),
                plotlyOutput(ns("infill_validation"), height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 12,
                h4("Data Availability Heatmap"),
                plotlyOutput(ns("infill_heatmap"), height = "400px")
              )
            )
          ),
          
          # Tab for before/after summary statistics
          tabPanel(
            "Summary Statistics",
            plotlyOutput(ns("stats_comparison"), height = "400px"),
            DT::dataTableOutput(ns("stats_table"))
          ),
          
          # Tab for downloading results
          tabPanel(
            "Download Results",
            wellPanel(
              h4("Download Infilled Data"),
              radioButtons(ns("download_format"), 
                          "File Format", 
                          choices = c("CSV", "RDS"),
                          selected = "CSV"),
              
              downloadButton(ns("download_data_btn"), 
                            "Download Infilled Data"),
              
              hr(),
              
              h4("Download Summary Statistics"),
              downloadButton(ns("download_stats_btn"), 
                            "Download Summary Statistics")
            )
          )
        )
      )
    )
  )
}

# Server function for the quantile mapping module
quantile_mapping_server <- function(id, parent_id, station_data, stations_meta) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store module state
    values <- reactiveValues(
      target_station = NULL,
      nearby_stations = NULL,
      infilled_data = NULL,
      original_data = NULL,
      validation_metrics = NULL
    )
    
    # Pass the station selection UI from the parent module
    output$target_station_ui <- renderUI({
      # Get the UI from the parent module
      # This will need to be implemented based on your main app's station selection UI
      tagList(
        selectInput(session$ns("station_id"), 
                   "Target Station", 
                   choices = NULL)
      )
    })
    
    # Update variable choices based on the selected station's data
    observe({
      req(station_data())
      
      # Get available numeric variables
      data <- station_data()
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      
      # Filter out non-environmental variables
      exclude_vars <- c("station_id", "lat", "lon", "elev", "climate_id", "WMO_id", "TC_id",
                        "year", "month", "day", "hour", "minute", "time", "date")
      var_choices <- setdiff(numeric_vars, exclude_vars)
      
      # Remove flag variables
      var_choices <- var_choices[!grepl("_flag", var_choices)]
      
      # Update the selectInput
      updateSelectInput(session, "variable_to_infill",
                        choices = var_choices,
                        selected = if(length(var_choices) > 0) var_choices[1] else NULL)
    })
    
    # Find nearby stations when button is clicked
    observeEvent(input$find_stations_btn, {
      req(input$station_id, input$max_distance, input$min_overlap)
      
      # Show a progress indicator
      withProgress(message = "Finding nearby stations...", {
        
        # Call the function to find nearby stations
        nearby <- find_nearby_stations(
          target_station_id = as.numeric(input$station_id),
          max_distance_km = input$max_distance,
          min_overlap_years = input$min_overlap
        )
        
        # Store the result
        values$nearby_stations <- nearby
        
        # Update the UI
        updateTabsetPanel(session, "result_tabs", selected = "Nearby Stations")
      })
    })
    
    # Display nearby stations in a table
    output$nearby_stations_table <- DT::renderDataTable({
      req(values$nearby_stations)
      
      # Format the table
      stations_df <- values$nearby_stations %>%
        select(station_id, station_name, distance_km, start, end, overlap_years) %>%
        rename(
          "Station ID" = station_id,
          "Station Name" = station_name,
          "Distance (km)" = distance_km,
          "Start Date" = start,
          "End Date" = end,
          "Overlap (years)" = overlap_years
        )
      
      DT::datatable(
        stations_df,
        options = list(
          pageLength = 10,
          searchHighlight = TRUE,
          order = list(list(2, 'asc')) # Sort by distance
        )
      )
    })
    
    # Map of nearby stations
    output$nearby_stations_map <- renderPlotly({
      req(values$nearby_stations, input$station_id)
      
      # Get target station info
      target_station <- stations_meta() %>%
        filter(station_id == as.numeric(input$station_id))
      
      # Combine target and nearby stations
      all_stations <- bind_rows(
        target_station %>% 
          select(station_id, station_name, lat, lon) %>%
          mutate(type = "Target"),
        values$nearby_stations %>% 
          select(station_id, station_name, lat, lon, distance_km) %>%
          mutate(type = "Nearby")
      )
      
      # Create map
      p <- plot_geo(all_stations, lat = ~lat, lon = ~lon) %>%
        add_markers(
          text = ~paste(
            "Station ID:", station_id, "<br>",
            "Station Name:", station_name, "<br>",
            if_else(type == "Nearby", 
                    paste("Distance:", round(distance_km, 1), "km"), 
                    "Target Station")),
          color = ~type,
          colors = c("Target" = "red", "Nearby" = "blue"),
          symbol = ~type,
          symbols = c("Target" = "circle", "Nearby" = "diamond"),
          size = ~if_else(type == "Target", 15, 10)
        ) %>%
        layout(
          title = "Target Station and Nearby Stations",
          geo = list(
            scope = "north america",
            projection = list(type = "mercator"),
            showland = TRUE,
            landcolor = "rgb(243, 243, 243)",
            countrycolor = "rgb(204, 204, 204)",
            showlakes = TRUE,
            lakecolor = "rgb(220, 240, 255)"
          )
        )
      
      return(p)
    })
      # Run infill process when button is clicked
    observeEvent(input$run_infill_btn, {
      req(input$station_id, input$variable_to_infill, 
          input$date_range, values$nearby_stations)
      
      # Create a progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Running infill process", value = 0)
      
      # Get original data for the selected station
      progress$set(detail = "Loading original data...", value = 0.1)
      original_data <- station_data()
      values$original_data <- original_data
      
      # Filter to date range
      date_filtered <- original_data %>%
        filter(date >= input$date_range[1],
               date <= input$date_range[2])
      
      # Run the infill process
      progress$set(detail = "Finding suitable donor stations...", value = 0.2)
      
      # Get top ranked stations if we have more than 3 nearby stations
      if(nrow(values$nearby_stations) > 3) {
        progress$set(detail = "Ranking donor stations by suitability...", value = 0.3)
        ranked_stations <- rank_stations_for_infill(
          target_station_id = as.numeric(input$station_id),
          nearby_stations = values$nearby_stations,
          variable = input$variable_to_infill,
          test_period = 365  # Use last year of data for ranking
        )
        
        # Use top ranked stations (up to 5) based on score
        if(nrow(ranked_stations) > 0 && !all(is.na(ranked_stations$rank_score))) {
          top_stations <- ranked_stations %>% 
            filter(!is.na(rank_score)) %>% 
            head(5)
          
          if(nrow(top_stations) > 0) {
            progress$set(detail = paste("Using top", nrow(top_stations), "ranked stations..."), value = 0.4)
            # Use these stations for infilling
            values$nearby_stations <- top_stations
          }
        }
      }
      
      progress$set(detail = "Performing quantile mapping infill...", value = 0.5)
      infilled_result <- tryCatch({
        infill_station_data(
          target_station_id = as.numeric(input$station_id),
          variable = input$variable_to_infill,
          start_date = input$date_range[1],
          end_date = input$date_range[2],
          max_distance_km = input$max_distance,
          min_overlap_years = input$min_overlap,
          by_month = input$by_month,
          min_overlap = input$min_datapoints
        )
      }, error = function(e) {
        showNotification(paste("Error during infill:", e$message), 
                        type = "error", duration = 10)
        return(NULL)
      })
      
      if(is.null(infilled_result)) {
        return(NULL)
      }
      
      # Store the result
      progress$set(detail = "Calculating validation metrics...", value = 0.8)
      values$infilled_data <- infilled_result
      
      # Calculate validation metrics
      values$validation_metrics <- list(
        infilled_count = sum(infilled_result$infilled, na.rm = TRUE),
        total_points = nrow(infilled_result),
        pct_infilled = round(sum(infilled_result$infilled, na.rm = TRUE) / 
                             nrow(infilled_result) * 100, 1)
      )
      
      progress$set(detail = "Generating visualizations...", value = 0.9)
      
      # Update the UI
      updateTabsetPanel(session, "result_tabs", selected = "Infill Results")
      progress$set(detail = "Complete!", value = 1)
      
      # Show a notification
      showNotification(
        paste("Infill completed! Infilled", values$validation_metrics$infilled_count, 
              "data points (", values$validation_metrics$pct_infilled, "% of total)"),
        type = "message",
        duration = 5
      )
    })
    
    # Display the infill time series
    output$infill_timeseries <- renderPlotly({
      req(values$infilled_data, input$variable_to_infill)
      
      # Get station name
      station_name <- stations_meta() %>%
        filter(station_id == as.numeric(input$station_id)) %>%
        pull(station_name) %>%
        first()
      
      # Create plot object
      p <- plot_infilled_data(values$infilled_data, input$variable_to_infill, station_name)
      
      # Convert to plotly
      plotly::ggplotly(p) %>%
        layout(
          legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center")
        )
    })
    
    # Display infill summary table
    output$infill_summary_table <- DT::renderDataTable({
      req(values$infilled_data, values$validation_metrics)
      
      # Create summary by donor station
      donor_summary <- values$infilled_data %>%
        filter(infilled) %>%
        group_by(donor_station_id, donor_station_name, donor_distance_km) %>%
        summarise(
          points_infilled = n(),
          pct_of_total_infilled = round(n() / values$validation_metrics$infilled_count * 100, 1),
          .groups = "drop"
        ) %>%
        arrange(donor_distance_km)
      
      # Format table
      DT::datatable(
        donor_summary,
        colnames = c(
          "Donor Station ID", "Donor Station Name", "Distance (km)",
          "Points Infilled", "% of Total Infilled"
        ),
        options = list(
          pageLength = 5,
          dom = 'tip'
        )
      )
    })
    
    # Display validation plot
    output$infill_validation <- renderPlotly({
      req(values$infilled_data, values$validation_metrics)
      
      # Create a simple information plot about the infill results
      infill_data <- values$infilled_data
      
      # Create a monthly summary of infill rates
      monthly_summary <- infill_data %>%
        mutate(year_month = format(date, "%Y-%m")) %>%
        group_by(year_month) %>%
        summarise(
          total_points = n(),
          infilled_points = sum(infilled, na.rm = TRUE),
          pct_infilled = round(infilled_points / total_points * 100, 1),
          .groups = "drop"
        ) %>%
        # Convert back to Date for proper plotting (use the 1st of each month)
        mutate(date = as.Date(paste0(year_month, "-01")))
      
      # Create the plot
      p <- ggplot(monthly_summary, aes(x = date, y = pct_infilled)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_line(aes(y = total_points / max(total_points) * 100), 
                 color = "red", size = 1) +
        geom_point(aes(y = total_points / max(total_points) * 100), 
                  color = "red", size = 2) +
        scale_y_continuous(
          name = "% of Data Infilled",
          sec.axis = sec_axis(~.*max(monthly_summary$total_points)/100, 
                             name = "Total Data Points")
        ) +
        labs(
          title = "Monthly Infill Summary",
          subtitle = paste("Total points infilled:", values$validation_metrics$infilled_count,
                           "out of", values$validation_metrics$total_points,
                           paste0("(", values$validation_metrics$pct_infilled, "%)")),
          x = "Date"
        ) +
        theme_minimal() +
        theme(
          axis.title.y.left = element_text(color = "steelblue"),
          axis.text.y.left = element_text(color = "steelblue"),
          axis.title.y.right = element_text(color = "red"),
          axis.text.y.right = element_text(color = "red")
        )
      
      # Convert to plotly
      plotly::ggplotly(p)
    })
    
    # Display data availability heatmap
    output$infill_heatmap <- renderPlotly({
      req(values$infilled_data, input$variable_to_infill)
      
      # Get station name
      station_name <- stations_meta() %>%
        filter(station_id == as.numeric(input$station_id)) %>%
        pull(station_name) %>%
        first()
      
      # Create heatmap
      heatmap <- plot_infill_heatmap(values$infilled_data, input$variable_to_infill, station_name)
      
      # Convert to plotly
      plotly::ggplotly(heatmap) %>%
        layout(
          title = list(text = paste0(heatmap$labels$title, "<br>", 
                                    "<sup>", heatmap$labels$subtitle, "</sup>")),
          margin = list(t = 100)  # Add margin for the title
        )
    })
    
    # Summary statistics comparison
    output$stats_comparison <- renderPlotly({
      req(values$original_data, values$infilled_data, input$variable_to_infill)
      
      # Prepare data for summarization
      # Original data
      original_data <- values$original_data %>%
        filter(date >= input$date_range[1],
               date <= input$date_range[2])
      
      # Reconstructed data with infilled values
      reconstructed_data <- original_data
      infilled_data <- values$infilled_data
      
      # Replace values in the reconstructed dataset
      reconstructed_data[[input$variable_to_infill]] <- 
        infilled_data$target_value[match(reconstructed_data$date, infilled_data$date)]
      
      # Compute monthly summaries
      if(nrow(original_data) > 0 && nrow(reconstructed_data) > 0) {
        # Create monthly summaries
        original_monthly <- summarize_monthly(original_data, 
                                             cols = c(input$variable_to_infill), 
                                             min_days = 1)
        
        infilled_monthly <- summarize_monthly(reconstructed_data, 
                                             cols = c(input$variable_to_infill), 
                                             min_days = 1)
        
        # Join the summaries
        comparison <- original_monthly %>%
          select(year, month, contains(input$variable_to_infill)) %>%
          mutate(date = as.Date(paste(year, month, "15", sep = "-"))) %>%
          left_join(
            infilled_monthly %>%
              select(year, month, contains(input$variable_to_infill)) %>%
              rename_with(~paste0(., "_infilled"), contains(input$variable_to_infill)),
            by = c("year", "month")
          )
        
        # Create a long format for plotting
        mean_col <- paste0(input$variable_to_infill, "_mean")
        mean_col_infilled <- paste0(input$variable_to_infill, "_mean_infilled")
        
        plot_data <- comparison %>%
          select(date, original = !!sym(mean_col), infilled = !!sym(mean_col_infilled)) %>%
          pivot_longer(cols = c(original, infilled),
                      names_to = "series", 
                      values_to = "value")
        
        # Create plot
        p <- ggplot(plot_data, aes(x = date, y = value, color = series)) +
          geom_line() +
          geom_point(size = 1) +
          scale_color_manual(values = c("original" = "black", "infilled" = "red"),
                            labels = c("Original", "Infilled"),
                            name = "Data Series") +
          labs(
            title = paste("Monthly Mean", input$variable_to_infill, "Comparison"),
            subtitle = "Original vs. Infilled Data",
            x = "Date",
            y = paste("Monthly Mean", input$variable_to_infill)
          ) +
          theme_minimal()
        
        # Convert to plotly
        plotly::ggplotly(p)
      }
    })
    
    # Summary statistics table
    output$stats_table <- DT::renderDataTable({
      req(values$original_data, values$infilled_data, input$variable_to_infill)
      
      # Similar to above, but returning a data table of statistics
      # Original data
      original_data <- values$original_data %>%
        filter(date >= input$date_range[1],
               date <= input$date_range[2])
      
      # Reconstructed data with infilled values
      reconstructed_data <- original_data
      infilled_data <- values$infilled_data
      
      # Replace values in the reconstructed dataset
      reconstructed_data[[input$variable_to_infill]] <- 
        infilled_data$target_value[match(reconstructed_data$date, infilled_data$date)]
      
      # Create yearly summaries for a more compact table
      if(nrow(original_data) > 0 && nrow(reconstructed_data) > 0) {
        original_yearly <- summarize_yearly(original_data, 
                                          cols = c(input$variable_to_infill), 
                                          min_months = 1)
        
        infilled_yearly <- summarize_yearly(reconstructed_data, 
                                          cols = c(input$variable_to_infill), 
                                          min_months = 1)
        
        # Join the summaries
        comparison <- original_yearly %>%
          select(station_id, year, contains(input$variable_to_infill)) %>%
          left_join(
            infilled_yearly %>%
              select(station_id, year, contains(input$variable_to_infill)) %>%
              rename_with(~paste0(., "_infilled"), contains(input$variable_to_infill)),
            by = c("station_id", "year")
          )
        
        # Format table for display
        DT::datatable(
          comparison,
          options = list(
            pageLength = 10,
            scrollX = TRUE
          )
        )
      }
    })
    
    # Download handlers
    output$download_data_btn <- downloadHandler(
      filename = function() {
        if(input$download_format == "CSV") {
          paste0("infilled_", input$station_id, "_", input$variable_to_infill, ".csv")
        } else {
          paste0("infilled_", input$station_id, "_", input$variable_to_infill, ".rds")
        }
      },
      content = function(file) {
        if(input$download_format == "CSV") {
          write.csv(values$infilled_data, file, row.names = FALSE)
        } else {
          saveRDS(values$infilled_data, file)
        }
      }
    )
    
    output$download_stats_btn <- downloadHandler(
      filename = function() {
        paste0("stats_", input$station_id, "_", input$variable_to_infill, ".csv")
      },
      content = function(file) {
        # Create statistics summary
        original_data <- values$original_data %>%
          filter(date >= input$date_range[1],
                 date <= input$date_range[2])
        
        # Reconstructed data with infilled values
        reconstructed_data <- original_data
        infilled_data <- values$infilled_data
        
        # Replace values in the reconstructed dataset
        reconstructed_data[[input$variable_to_infill]] <- 
          infilled_data$target_value[match(reconstructed_data$date, infilled_data$date)]
        
        # Create monthly summaries
        original_monthly <- summarize_monthly(original_data, 
                                           cols = c(input$variable_to_infill), 
                                           min_days = 1)
        
        infilled_monthly <- summarize_monthly(reconstructed_data, 
                                           cols = c(input$variable_to_infill), 
                                           min_days = 1)
        
        # Join the summaries
        comparison <- original_monthly %>%
          select(station_id, year, month, contains(input$variable_to_infill)) %>%
          mutate(date = as.Date(paste(year, month, "15", sep = "-"))) %>%
          left_join(
            infilled_monthly %>%
              select(station_id, year, month, contains(input$variable_to_infill)) %>%
              rename_with(~paste0(., "_infilled"), contains(input$variable_to_infill)),
            by = c("station_id", "year", "month")
          )
        
        write.csv(comparison, file, row.names = FALSE)
      }
    )
  })
}
