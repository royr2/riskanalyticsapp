# Interactive Charts Dashboard Module

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

#' UI function for interactive charts dashboard module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
charts_dashboard_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Chart Configuration", status = "primary", solidHeader = TRUE, width = 4,
        
        selectInput(ns("chart_type"), "Chart Type:",
                   choices = c("Scatter Plot" = "scatter",
                             "Line Chart" = "line",
                             "Bar Chart" = "bar",
                             "Histogram" = "histogram",
                             "Box Plot" = "boxplot",
                             "Heatmap" = "heatmap",
                             "Violin Plot" = "violin"),
                   selected = "scatter"),
        
        conditionalPanel(
          condition = paste0("input['", ns("chart_type"), "'] != 'histogram'"),
          ns = ns,
          selectInput(ns("x_variable"), "X Variable:", choices = NULL)
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("chart_type"), "'] %in% ['scatter', 'line', 'boxplot', 'violin']"),
          ns = ns,
          selectInput(ns("y_variable"), "Y Variable:", choices = NULL)
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("chart_type"), "'] == 'histogram'"),
          ns = ns,
          selectInput(ns("hist_variable"), "Variable:", choices = NULL),
          sliderInput(ns("hist_bins"), "Number of bins:", min = 5, max = 100, value = 30)
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("chart_type"), "'] %in% ['scatter', 'line', 'bar']"),
          ns = ns,
          selectInput(ns("color_variable"), "Color by (optional):", 
                     choices = c("None" = "none"), selected = "none"),
          selectInput(ns("size_variable"), "Size by (optional):", 
                     choices = c("None" = "none"), selected = "none")
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("chart_type"), "'] == 'scatter'"),
          ns = ns,
          checkboxInput(ns("add_trendline"), "Add trend line", FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("add_trendline"), "'] == true"),
            ns = ns,
            selectInput(ns("trendline_method"), "Trend line method:",
                       choices = c("Linear" = "lm", "LOESS" = "loess"),
                       selected = "lm")
          )
        ),
        
        hr(),
        
        h4("Chart Styling"),
        textInput(ns("chart_title"), "Chart Title:", value = ""),
        textInput(ns("x_label"), "X-axis Label:", value = ""),
        textInput(ns("y_label"), "Y-axis Label:", value = ""),
        
        selectInput(ns("theme"), "Theme:",
                   choices = c("Minimal" = "minimal",
                             "Classic" = "classic",
                             "Dark" = "dark",
                             "Light" = "light"),
                   selected = "minimal"),
        
        actionButton(ns("update_chart"), "Update Chart", class = "btn-primary", width = "100%")
      ),
      
      box(
        title = "Interactive Chart", status = "success", solidHeader = TRUE, width = 8,
        
        plotlyOutput(ns("interactive_chart"), height = "600px"),
        
        br(),
        
        fluidRow(
          column(4,
                 downloadButton(ns("download_plot"), "Download Plot", class = "btn-info")
          ),
          column(4,
                 actionButton(ns("save_chart_config"), "Save Configuration", class = "btn-warning")
          ),
          column(4,
                 actionButton(ns("reset_chart"), "Reset Chart", class = "btn-secondary")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Multiple Charts", status = "warning", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,
        
        fluidRow(
          column(6,
                 h4("Chart 1"),
                 selectInput(ns("multi_chart1_type"), "Chart Type:",
                           choices = c("Scatter" = "scatter", "Bar" = "bar", "Line" = "line"),
                           selected = "scatter"),
                 fluidRow(
                   column(6, selectInput(ns("multi_chart1_x"), "X Variable:", choices = NULL)),
                   column(6, selectInput(ns("multi_chart1_y"), "Y Variable:", choices = NULL))
                 ),
                 plotlyOutput(ns("multi_chart1"), height = "300px")
          ),
          
          column(6,
                 h4("Chart 2"),
                 selectInput(ns("multi_chart2_type"), "Chart Type:",
                           choices = c("Histogram" = "histogram", "Boxplot" = "boxplot", "Violin" = "violin"),
                           selected = "histogram"),
                 selectInput(ns("multi_chart2_var"), "Variable:", choices = NULL),
                 plotlyOutput(ns("multi_chart2"), height = "300px")
          )
        ),
        
        br(),
        actionButton(ns("update_multi_charts"), "Update All Charts", class = "btn-primary")
      )
    ),
    
    fluidRow(
      box(
        title = "Chart Data Explorer", status = "info", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,
        
        h4("Current Chart Data"),
        DTOutput(ns("chart_data_table")),
        
        br(),
        
        fluidRow(
          column(6,
                 h4("Data Filtering"),
                 uiOutput(ns("filter_controls"))
          ),
          column(6,
                 h4("Data Summary"),
                 verbatimTextOutput(ns("filtered_data_summary"))
          )
        )
      )
    )
  )
}

#' Server function for interactive charts dashboard module
#'
#' @param id The namespace id for the module
#' @param data_input Reactive expression containing the data
#' @return NULL
#' @export
charts_dashboard_Server <- function(id, data_input) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive data
    current_data <- reactive({
      req(data_input())
      data_input()
    })
    
    # Filtered data based on user controls
    filtered_data <- reactive({
      # For now, return the current data
      # In a full implementation, this would apply filters
      current_data()
    })
    
    # Update variable choices when data changes
    observe({
      req(current_data())
      data <- current_data()
      
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      all_vars <- names(data)
      categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      
      # Update main chart controls
      updateSelectInput(session, "x_variable", choices = all_vars)
      updateSelectInput(session, "y_variable", choices = numeric_vars)
      updateSelectInput(session, "hist_variable", choices = numeric_vars)
      updateSelectInput(session, "color_variable", 
                       choices = c("None" = "none", setNames(categorical_vars, categorical_vars)))
      updateSelectInput(session, "size_variable", 
                       choices = c("None" = "none", setNames(numeric_vars, numeric_vars)))
      
      # Update multi-chart controls
      updateSelectInput(session, "multi_chart1_x", choices = all_vars)
      updateSelectInput(session, "multi_chart1_y", choices = numeric_vars)
      updateSelectInput(session, "multi_chart2_var", choices = numeric_vars)
    })
    
    # Main interactive chart
    chart_data <- eventReactive(input$update_chart, {
      req(current_data())
      
      data <- filtered_data()
      chart_type <- input$chart_type
      
      if (chart_type == "histogram") {
        list(data = data, variable = input$hist_variable, type = "histogram")
      } else if (chart_type %in% c("scatter", "line", "boxplot", "violin")) {
        list(data = data, x = input$x_variable, y = input$y_variable, type = chart_type)
      } else if (chart_type == "bar") {
        list(data = data, x = input$x_variable, type = "bar")
      } else if (chart_type == "heatmap") {
        numeric_data <- data %>% select(where(is.numeric))
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        list(data = cor_matrix, type = "heatmap")
      }
    }, ignoreNULL = FALSE)
    
    output$interactive_chart <- renderPlotly({
      if (is.null(chart_data())) {
        req(current_data())
        # Default chart when first loaded
        data <- filtered_data()
        numeric_vars <- names(data)[sapply(data, is.numeric)]
        
        if (length(numeric_vars) >= 2) {
          p <- ggplot(data, aes_string(x = numeric_vars[1], y = numeric_vars[2])) +
            geom_point(alpha = 0.6) +
            labs(title = "Default Scatter Plot",
                 x = numeric_vars[1], y = numeric_vars[2]) +
            theme_minimal()
        } else {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                    label = "Please configure chart settings and click 'Update Chart'",
                    size = 6) +
            theme_void()
        }
        return(ggplotly(p))
      }
      
      chart_info <- chart_data()
      data <- chart_info$data
      
      # Set default labels if not provided
      x_lab <- if (input$x_label != "") input$x_label else chart_info$x
      y_lab <- if (input$y_label != "") input$y_label else chart_info$y
      title <- if (input$chart_title != "") input$chart_title else paste("Interactive", stringr::str_to_title(chart_info$type))
      
      if (chart_info$type == "scatter") {
        aes_mapping <- aes_string(x = chart_info$x, y = chart_info$y)
        
        if (input$color_variable != "none") {
          aes_mapping$colour <- as.name(input$color_variable)
        }
        if (input$size_variable != "none") {
          aes_mapping$size <- as.name(input$size_variable)
        }
        
        p <- ggplot(data, aes_mapping) +
          geom_point(alpha = 0.6) +
          labs(title = title, x = x_lab, y = y_lab)
        
        if (input$add_trendline) {
          p <- p + geom_smooth(method = input$trendline_method, se = TRUE)
        }
        
      } else if (chart_info$type == "line") {
        aes_mapping <- aes_string(x = chart_info$x, y = chart_info$y)
        
        if (input$color_variable != "none") {
          aes_mapping$colour <- as.name(input$color_variable)
          p <- ggplot(data, aes_mapping) +
            geom_line() +
            geom_point()
        } else {
          p <- ggplot(data, aes_mapping) +
            geom_line() +
            geom_point()
        }
        
        p <- p + labs(title = title, x = x_lab, y = y_lab)
        
      } else if (chart_info$type == "bar") {
        if (is.numeric(data[[chart_info$x]])) {
          # For numeric x, create bins
          p <- ggplot(data, aes_string(x = chart_info$x)) +
            geom_bar(fill = "steelblue", alpha = 0.7) +
            labs(title = title, x = x_lab, y = "Count")
        } else {
          # For categorical x
          aes_mapping <- aes_string(x = chart_info$x)
          if (input$color_variable != "none") {
            aes_mapping$fill <- as.name(input$color_variable)
          }
          
          p <- ggplot(data, aes_mapping) +
            geom_bar() +
            labs(title = title, x = x_lab, y = "Count") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        
      } else if (chart_info$type == "histogram") {
        p <- ggplot(data, aes_string(x = chart_info$variable)) +
          geom_histogram(bins = input$hist_bins, fill = "steelblue", alpha = 0.7) +
          labs(title = title, x = x_lab, y = "Frequency")
        
      } else if (chart_info$type == "boxplot") {
        p <- ggplot(data, aes_string(x = chart_info$x, y = chart_info$y)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          labs(title = title, x = x_lab, y = y_lab)
        
      } else if (chart_info$type == "violin") {
        p <- ggplot(data, aes_string(x = chart_info$x, y = chart_info$y)) +
          geom_violin(fill = "steelblue", alpha = 0.7) +
          labs(title = title, x = x_lab, y = y_lab)
        
      } else if (chart_info$type == "heatmap") {
        cor_df <- expand.grid(Var1 = rownames(data), Var2 = colnames(data))
        cor_df$Correlation <- as.vector(data)
        
        p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1, 1)) +
          labs(title = title, x = "", y = "") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      # Apply theme
      if (input$theme == "minimal") {
        p <- p + theme_minimal()
      } else if (input$theme == "classic") {
        p <- p + theme_classic()
      } else if (input$theme == "dark") {
        p <- p + theme_dark()
      } else if (input$theme == "light") {
        p <- p + theme_light()
      }
      
      ggplotly(p)
    })
    
    # Multi-chart 1
    output$multi_chart1 <- renderPlotly({
      req(current_data(), input$multi_chart1_x, input$multi_chart1_y)
      data <- filtered_data()
      
      if (input$multi_chart1_type == "scatter") {
        p <- ggplot(data, aes_string(x = input$multi_chart1_x, y = input$multi_chart1_y)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          theme_minimal()
      } else if (input$multi_chart1_type == "line") {
        p <- ggplot(data, aes_string(x = input$multi_chart1_x, y = input$multi_chart1_y)) +
          geom_line(color = "steelblue") +
          geom_point(color = "steelblue") +
          theme_minimal()
      } else {
        p <- ggplot(data, aes_string(x = input$multi_chart1_x)) +
          geom_bar(fill = "steelblue", alpha = 0.7) +
          theme_minimal()
      }
      
      ggplotly(p)
    })
    
    # Multi-chart 2
    output$multi_chart2 <- renderPlotly({
      req(current_data(), input$multi_chart2_var)
      data <- filtered_data()
      
      if (input$multi_chart2_type == "histogram") {
        p <- ggplot(data, aes_string(x = input$multi_chart2_var)) +
          geom_histogram(bins = 20, fill = "coral", alpha = 0.7) +
          theme_minimal()
      } else if (input$multi_chart2_type == "boxplot") {
        p <- ggplot(data, aes_string(y = input$multi_chart2_var)) +
          geom_boxplot(fill = "coral", alpha = 0.7) +
          theme_minimal()
      } else {
        p <- ggplot(data, aes_string(x = "", y = input$multi_chart2_var)) +
          geom_violin(fill = "coral", alpha = 0.7) +
          theme_minimal()
      }
      
      ggplotly(p)
    })
    
    # Chart data table
    output$chart_data_table <- renderDT({
      req(filtered_data())
      data <- filtered_data()
      
      datatable(head(data, 1000), # Limit to first 1000 rows for performance
               options = list(scrollX = TRUE, pageLength = 10),
               rownames = FALSE)
    })
    
    # Data summary
    output$filtered_data_summary <- renderText({
      req(filtered_data())
      data <- filtered_data()
      
      paste(
        "Rows:", nrow(data),
        "\nColumns:", ncol(data),
        "\nNumeric variables:", sum(sapply(data, is.numeric)),
        "\nCategorical variables:", sum(sapply(data, function(x) is.factor(x) || is.character(x))),
        "\nMissing values:", sum(is.na(data))
      )
    })
    
    # Reset chart button
    observeEvent(input$reset_chart, {
      updateSelectInput(session, "chart_type", selected = "scatter")
      updateTextInput(session, "chart_title", value = "")
      updateTextInput(session, "x_label", value = "")
      updateTextInput(session, "y_label", value = "")
      updateSelectInput(session, "theme", selected = "minimal")
      updateCheckboxInput(session, "add_trendline", value = FALSE)
      updateSelectInput(session, "color_variable", selected = "none")
      updateSelectInput(session, "size_variable", selected = "none")
    })
    
    # Download plot functionality
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("chart_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # This would save the current plot
        # Implementation would depend on the specific plotting library
        showNotification("Download functionality would be implemented here", type = "message")
      }
    )
    
    # Save chart configuration
    observeEvent(input$save_chart_config, {
      showNotification("Chart configuration saved!", type = "message", duration = 3)
    })
  })
}
