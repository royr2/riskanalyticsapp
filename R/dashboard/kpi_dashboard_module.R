# KPI Dashboard Module

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

#' UI function for KPI dashboard module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
kpi_dashboard_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "KPI Configuration", status = "primary", solidHeader = TRUE, width = 4,
        
        h4("Select KPI Metrics"),
        
        selectInput(ns("primary_metric"), "Primary KPI:",
                   choices = NULL),
        
        selectInput(ns("secondary_metrics"), "Secondary KPIs:",
                   choices = NULL, multiple = TRUE),
        
        hr(),
        
        h4("Time Period"),
        dateRangeInput(ns("date_range"), "Date Range:",
                      start = Sys.Date() - 30,
                      end = Sys.Date()),
        
        conditionalPanel(
          condition = "false", # Hide for now since we don't have date columns
          ns = ns,
          selectInput(ns("date_column"), "Date Column:",
                     choices = NULL)
        ),
        
        hr(),
        
        h4("Grouping & Filters"),
        selectInput(ns("group_by"), "Group by:",
                   choices = c("None" = "none"), selected = "none"),
        
        uiOutput(ns("filter_controls")),
        
        hr(),
        
        actionButton(ns("update_kpis"), "Update KPIs", class = "btn-primary", width = "100%"),
        
        br(), br(),
        
        actionButton(ns("export_kpis"), "Export KPI Report", class = "btn-info", width = "100%")
      ),
      
      box(
        title = "KPI Overview", status = "success", solidHeader = TRUE, width = 8,
        
        fluidRow(
          valueBoxOutput(ns("primary_kpi_box"), width = 12)
        ),
        
        fluidRow(
          uiOutput(ns("secondary_kpi_boxes"))
        ),
        
        hr(),
        
        h4("KPI Trends"),
        plotlyOutput(ns("kpi_trend_chart"), height = "300px"),
        
        hr(),
        
        h4("KPI Comparison"),
        plotlyOutput(ns("kpi_comparison_chart"), height = "300px")
      )
    ),
    
    fluidRow(
      box(
        title = "Detailed KPI Analysis", status = "warning", solidHeader = TRUE, width = 12,
        
        tabsetPanel(
          tabPanel("KPI Table",
                   br(),
                   DTOutput(ns("kpi_table"))
          ),
          
          tabPanel("Statistical Summary",
                   br(),
                   fluidRow(
                     column(6,
                            h4("Descriptive Statistics"),
                            verbatimTextOutput(ns("kpi_stats"))
                     ),
                     column(6,
                            h4("Distribution Analysis"),
                            plotlyOutput(ns("kpi_distribution"), height = "300px")
                     )
                   )
          ),
          
          tabPanel("Correlation Analysis",
                   br(),
                   fluidRow(
                     column(6,
                            h4("KPI Correlation Matrix"),
                            plotlyOutput(ns("kpi_correlation"), height = "400px")
                     ),
                     column(6,
                            h4("Correlation Insights"),
                            verbatimTextOutput(ns("correlation_insights"))
                     )
                   )
          ),
          
          tabPanel("Performance Targets",
                   br(),
                   fluidRow(
                     column(6,
                            h4("Set Performance Targets"),
                            uiOutput(ns("target_controls"))
                     ),
                     column(6,
                            h4("Target vs Actual Performance"),
                            plotlyOutput(ns("target_performance"), height = "400px")
                     )
                   )
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Custom KPI Calculator", status = "info", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,
        
        fluidRow(
          column(4,
                 h4("Create Custom KPI"),
                 textInput(ns("custom_kpi_name"), "KPI Name:", 
                          placeholder = "e.g., Conversion Rate"),
                 
                 selectInput(ns("custom_numerator"), "Numerator:",
                           choices = NULL),
                 
                 selectInput(ns("custom_denominator"), "Denominator:",
                           choices = NULL),
                 
                 selectInput(ns("custom_operation"), "Operation:",
                           choices = c("Division (A/B)" = "division",
                                     "Percentage (A/B*100)" = "percentage",
                                     "Ratio (A:B)" = "ratio",
                                     "Difference (A-B)" = "difference",
                                     "Sum (A+B)" = "sum"),
                           selected = "percentage"),
                 
                 actionButton(ns("calculate_custom_kpi"), "Calculate KPI", class = "btn-success")
          ),
          
          column(8,
                 h4("Custom KPI Results"),
                 verbatimTextOutput(ns("custom_kpi_result")),
                 
                 br(),
                 
                 plotlyOutput(ns("custom_kpi_chart"), height = "300px")
          )
        )
      )
    )
  )
}

#' Server function for KPI dashboard module
#'
#' @param id The namespace id for the module
#' @param data_input Reactive expression containing the data
#' @return NULL
#' @export
kpi_dashboard_Server <- function(id, data_input) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive data
    current_data <- reactive({
      req(data_input())
      data_input()
    })
    
    # Store calculated KPIs
    calculated_kpis <- reactiveVal(NULL)
    custom_kpis <- reactiveVal(list())
    
    # Update variable choices when data changes
    observe({
      req(current_data())
      data <- current_data()
      
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      all_vars <- names(data)
      categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      
      # Update KPI metric choices
      updateSelectInput(session, "primary_metric", choices = numeric_vars)
      updateSelectInput(session, "secondary_metrics", choices = numeric_vars)
      
      # Update grouping choices
      updateSelectInput(session, "group_by", 
                       choices = c("None" = "none", setNames(categorical_vars, categorical_vars)))
      
      # Update custom KPI choices
      updateSelectInput(session, "custom_numerator", choices = numeric_vars)
      updateSelectInput(session, "custom_denominator", choices = numeric_vars)
    })
    
    # Calculate KPIs when button is clicked
    observeEvent(input$update_kpis, {
      req(current_data(), input$primary_metric)
      
      data <- current_data()
      
      # Calculate primary KPI
      primary_value <- if (input$group_by == "none") {
        mean(data[[input$primary_metric]], na.rm = TRUE)
      } else {
        data %>%
          group_by(.data[[input$group_by]]) %>%
          summarise(kpi_value = mean(.data[[input$primary_metric]], na.rm = TRUE), .groups = 'drop')
      }
      
      # Calculate secondary KPIs
      secondary_values <- list()
      if (!is.null(input$secondary_metrics) && length(input$secondary_metrics) > 0) {
        for (metric in input$secondary_metrics) {
          secondary_values[[metric]] <- if (input$group_by == "none") {
            mean(data[[metric]], na.rm = TRUE)
          } else {
            data %>%
              group_by(.data[[input$group_by]]) %>%
              summarise(kpi_value = mean(.data[[metric]], na.rm = TRUE), .groups = 'drop')
          }
        }
      }
      
      calculated_kpis(list(
        primary = list(name = input$primary_metric, value = primary_value),
        secondary = secondary_values
      ))
      
      showNotification("KPIs calculated successfully!", type = "message", duration = 3)
    })
    
    # Primary KPI value box
    output$primary_kpi_box <- renderValueBox({
      kpis <- calculated_kpis()
      
      if (is.null(kpis)) {
        value <- "No data"
        subtitle <- "Click 'Update KPIs' to calculate"
        color <- "light-blue"
      } else {
        primary_kpi <- kpis$primary
        if (is.data.frame(primary_kpi$value)) {
          value <- round(mean(primary_kpi$value$kpi_value, na.rm = TRUE), 2)
        } else {
          value <- round(primary_kpi$value, 2)
        }
        subtitle <- paste("Primary KPI:", primary_kpi$name)
        color <- "blue"
      }
      
      valueBox(
        value = value,
        subtitle = subtitle,
        icon = icon("tachometer-alt"),
        color = color,
        width = 12
      )
    })
    
    # Secondary KPI value boxes
    output$secondary_kpi_boxes <- renderUI({
      kpis <- calculated_kpis()
      
      if (is.null(kpis) || length(kpis$secondary) == 0) {
        return(div(h4("No secondary KPIs selected", style = "text-align: center; color: #999;")))
      }
      
      boxes <- list()
      colors <- c("green", "yellow", "red", "purple", "maroon", "navy")
      
      for (i in seq_along(kpis$secondary)) {
        metric_name <- names(kpis$secondary)[i]
        metric_data <- kpis$secondary[[metric_name]]
        
        if (is.data.frame(metric_data)) {
          value <- round(mean(metric_data$kpi_value, na.rm = TRUE), 2)
        } else {
          value <- round(metric_data, 2)
        }
        
        color <- colors[(i - 1) %% length(colors) + 1]
        
        boxes[[i]] <- valueBox(
          value = value,
          subtitle = metric_name,
          icon = icon("chart-line"),
          color = color,
          width = if (length(kpis$secondary) <= 2) 6 else if (length(kpis$secondary) <= 4) 3 else 2
        )
      }
      
      do.call(fluidRow, boxes)
    })
    
    # KPI trend chart
    output$kpi_trend_chart <- renderPlotly({
      kpis <- calculated_kpis()
      
      if (is.null(kpis)) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "Click 'Update KPIs' to view trends", size = 6) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Create dummy trend data for demonstration
      dates <- seq(from = Sys.Date() - 30, to = Sys.Date(), by = "day")
      trend_data <- data.frame(
        Date = dates,
        Value = rnorm(length(dates), mean = if (is.data.frame(kpis$primary$value)) 
                     mean(kpis$primary$value$kpi_value) else kpis$primary$value, sd = 5)
      )
      
      p <- ggplot(trend_data, aes(x = Date, y = Value)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue") +
        labs(title = paste("Trend for", kpis$primary$name),
             x = "Date", y = "KPI Value") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # KPI comparison chart
    output$kpi_comparison_chart <- renderPlotly({
      kpis <- calculated_kpis()
      
      if (is.null(kpis) || length(kpis$secondary) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "Select secondary KPIs to view comparison", size = 6) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Create comparison data
      comparison_data <- data.frame(
        KPI = c(kpis$primary$name, names(kpis$secondary)),
        Value = c(
          if (is.data.frame(kpis$primary$value)) mean(kpis$primary$value$kpi_value) else kpis$primary$value,
          sapply(kpis$secondary, function(x) if (is.data.frame(x)) mean(x$kpi_value) else x)
        )
      )
      
      p <- ggplot(comparison_data, aes(x = reorder(KPI, Value), y = Value, fill = KPI)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "KPI Comparison",
             x = "KPI", y = "Value") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p)
    })
    
    # KPI table
    output$kpi_table <- renderDT({
      req(current_data())
      data <- current_data()
      
      if (!is.null(input$primary_metric)) {
        # Show relevant data columns
        relevant_cols <- c(input$primary_metric, input$secondary_metrics)
        relevant_cols <- relevant_cols[!is.null(relevant_cols)]
        
        if (input$group_by != "none") {
          relevant_cols <- c(input$group_by, relevant_cols)
        }
        
        display_data <- data[, relevant_cols, drop = FALSE]
        
        datatable(display_data,
                 options = list(scrollX = TRUE, pageLength = 15),
                 rownames = FALSE) %>%
          formatRound(columns = sapply(display_data, is.numeric), digits = 3)
      } else {
        datatable(data.frame(Message = "Please select KPI metrics first"),
                 options = list(dom = 't'), rownames = FALSE)
      }
    })
    
    # KPI statistics
    output$kpi_stats <- renderText({
      kpis <- calculated_kpis()
      req(current_data())
      data <- current_data()
      
      if (is.null(kpis) || is.null(input$primary_metric)) {
        return("Please calculate KPIs first")
      }
      
      primary_data <- data[[input$primary_metric]]
      
      stats_text <- paste(
        "Primary KPI Statistics:",
        paste("Metric:", input$primary_metric),
        paste("Count:", sum(!is.na(primary_data))),
        paste("Mean:", round(mean(primary_data, na.rm = TRUE), 3)),
        paste("Median:", round(median(primary_data, na.rm = TRUE), 3)),
        paste("Std Dev:", round(sd(primary_data, na.rm = TRUE), 3)),
        paste("Min:", round(min(primary_data, na.rm = TRUE), 3)),
        paste("Max:", round(max(primary_data, na.rm = TRUE), 3)),
        sep = "\n"
      )
      
      if (!is.null(input$secondary_metrics) && length(input$secondary_metrics) > 0) {
        stats_text <- paste(stats_text, "\n\nSecondary KPIs:", paste(input$secondary_metrics, collapse = ", "))
      }
      
      stats_text
    })
    
    # KPI distribution plot
    output$kpi_distribution <- renderPlotly({
      req(current_data(), input$primary_metric)
      data <- current_data()
      
      p <- ggplot(data, aes_string(x = input$primary_metric)) +
        geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
        labs(title = paste("Distribution of", input$primary_metric),
             x = input$primary_metric, y = "Frequency") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Custom KPI calculation
    observeEvent(input$calculate_custom_kpi, {
      req(current_data(), input$custom_numerator, input$custom_denominator, input$custom_kpi_name)
      
      data <- current_data()
      numerator <- data[[input$custom_numerator]]
      denominator <- data[[input$custom_denominator]]
      
      result <- switch(input$custom_operation,
                      "division" = numerator / denominator,
                      "percentage" = (numerator / denominator) * 100,
                      "ratio" = paste(round(numerator, 2), ":", round(denominator, 2)),
                      "difference" = numerator - denominator,
                      "sum" = numerator + denominator)
      
      # Store custom KPI
      current_custom <- custom_kpis()
      current_custom[[input$custom_kpi_name]] <- list(
        numerator = input$custom_numerator,
        denominator = input$custom_denominator,
        operation = input$custom_operation,
        result = result
      )
      custom_kpis(current_custom)
      
      showNotification(paste("Custom KPI", input$custom_kpi_name, "calculated!"), 
                      type = "success", duration = 3)
    })
    
    # Custom KPI result
    output$custom_kpi_result <- renderText({
      custom <- custom_kpis()
      
      if (length(custom) == 0) {
        return("No custom KPIs calculated yet.")
      }
      
      results <- character()
      for (name in names(custom)) {
        kpi_info <- custom[[name]]
        if (is.numeric(kpi_info$result)) {
          avg_result <- round(mean(kpi_info$result, na.rm = TRUE), 3)
          results <- c(results, paste(name, ":", avg_result))
        } else {
          results <- c(results, paste(name, ": Complex result (see chart)"))
        }
      }
      
      paste(results, collapse = "\n")
    })
    
    # Custom KPI chart
    output$custom_kpi_chart <- renderPlotly({
      custom <- custom_kpis()
      
      if (length(custom) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "Calculate a custom KPI to see results", size = 6) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Show the most recent custom KPI
      latest_kpi <- custom[[length(custom)]]
      if (is.numeric(latest_kpi$result)) {
        chart_data <- data.frame(
          Index = 1:length(latest_kpi$result),
          Value = latest_kpi$result
        )
        
        p <- ggplot(chart_data, aes(x = Index, y = Value)) +
          geom_line(color = "coral") +
          geom_point(color = "coral") +
          labs(title = paste("Custom KPI:", names(custom)[length(custom)]),
               x = "Observation", y = "KPI Value") +
          theme_minimal()
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "Cannot chart non-numeric KPI", size = 6) +
          theme_void()
      }
      
      ggplotly(p)
    })
    
    # Export KPIs
    observeEvent(input$export_kpis, {
      showNotification("KPI export functionality would be implemented here", 
                      type = "message", duration = 3)
    })
  })
}
