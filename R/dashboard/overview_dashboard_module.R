# Overview Dashboard Module

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

#' UI function for overview dashboard module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
overview_dashboard_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Summary metrics boxes
      valueBoxOutput(ns("total_records"), width = 3),
      valueBoxOutput(ns("total_variables"), width = 3),
      valueBoxOutput(ns("missing_data"), width = 3),
      valueBoxOutput(ns("data_types"), width = 3)
    ),
    
    fluidRow(
      # Data overview section
      box(
        title = "Data Overview", status = "primary", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        
        tabsetPanel(
          tabPanel("Summary Statistics",
                   br(),
                   DTOutput(ns("summary_table"))
          ),
          
          tabPanel("Data Types",
                   br(),
                   plotlyOutput(ns("data_types_plot"))
          ),
          
          tabPanel("Missing Data Analysis",
                   br(),
                   fluidRow(
                     column(6, plotlyOutput(ns("missing_data_plot"))),
                     column(6, DTOutput(ns("missing_data_table")))
                   )
          )
        )
      )
    ),
    
    fluidRow(
      # Distribution plots
      box(
        title = "Variable Distributions", status = "success", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,
        
        fluidRow(
          column(3,
                 selectInput(ns("variable_select"), "Select Variable:",
                           choices = NULL)
          ),
          column(3,
                 selectInput(ns("plot_type"), "Plot Type:",
                           choices = c("Histogram" = "histogram",
                                     "Boxplot" = "boxplot",
                                     "Density" = "density"),
                           selected = "histogram")
          ),
          column(3,
                 conditionalPanel(
                   condition = paste0("input['", ns("plot_type"), "'] == 'histogram'"),
                   ns = ns,
                   sliderInput(ns("bins"), "Number of bins:",
                              min = 5, max = 50, value = 20)
                 )
          ),
          column(3,
                 checkboxInput(ns("show_outliers"), "Show outliers", TRUE)
          )
        ),
        
        plotlyOutput(ns("distribution_plot"), height = "400px")
      )
    ),
    
    fluidRow(
      # Correlation analysis
      box(
        title = "Correlation Analysis", status = "warning", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,
        
        fluidRow(
          column(4,
                 selectInput(ns("correlation_method"), "Correlation Method:",
                           choices = c("Pearson" = "pearson",
                                     "Spearman" = "spearman",
                                     "Kendall" = "kendall"),
                           selected = "pearson")
          ),
          column(4,
                 sliderInput(ns("correlation_threshold"), "Correlation Threshold:",
                            min = 0, max = 1, value = 0.5, step = 0.05)
          ),
          column(4,
                 checkboxInput(ns("show_correlation_values"), "Show values", TRUE)
          )
        ),
        
        plotlyOutput(ns("correlation_plot"), height = "500px")
      )
    )
  )
}

#' Server function for overview dashboard module
#'
#' @param id The namespace id for the module
#' @param data_input Reactive expression containing the data
#' @return NULL
#' @export
overview_dashboard_Server <- function(id, data_input) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive data
    current_data <- reactive({
      req(data_input())
      data_input()
    })
    
    # Update variable choices
    observe({
      req(current_data())
      vars <- names(current_data())
      updateSelectInput(session, "variable_select", choices = vars, selected = vars[1])
    })
    
    # Summary metrics value boxes
    output$total_records <- renderValueBox({
      data <- current_data()
      if (is.null(data)) {
        value <- 0
      } else {
        value <- nrow(data)
      }
      
      valueBox(
        value = formatC(value, format = "d", big.mark = ","),
        subtitle = "Total Records",
        icon = icon("database"),
        color = "blue"
      )
    })
    
    output$total_variables <- renderValueBox({
      data <- current_data()
      if (is.null(data)) {
        value <- 0
      } else {
        value <- ncol(data)
      }
      
      valueBox(
        value = value,
        subtitle = "Variables",
        icon = icon("columns"),
        color = "green"
      )
    })
    
    output$missing_data <- renderValueBox({
      data <- current_data()
      if (is.null(data)) {
        value <- "0%"
      } else {
        missing_pct <- round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
        value <- paste0(missing_pct, "%")
      }
      
      valueBox(
        value = value,
        subtitle = "Missing Data",
        icon = icon("exclamation-triangle"),
        color = if (as.numeric(gsub("%", "", value)) > 10) "red" else "yellow"
      )
    })
    
    output$data_types <- renderValueBox({
      data <- current_data()
      if (is.null(data)) {
        value <- 0
      } else {
        types <- sapply(data, function(x) class(x)[1])
        value <- length(unique(types))
      }
      
      valueBox(
        value = value,
        subtitle = "Data Types",
        icon = icon("list"),
        color = "purple"
      )
    })
    
    # Summary statistics table
    output$summary_table <- renderDT({
      req(current_data())
      data <- current_data()
      
      # Create summary for numeric columns
      numeric_cols <- sapply(data, is.numeric)
      if (sum(numeric_cols) > 0) {
        summary_df <- data %>%
          select(where(is.numeric)) %>%
          summarise(across(everything(), list(
            Count = ~ sum(!is.na(.)),
            Mean = ~ round(mean(., na.rm = TRUE), 3),
            Median = ~ round(median(., na.rm = TRUE), 3),
            SD = ~ round(sd(., na.rm = TRUE), 3),
            Min = ~ min(., na.rm = TRUE),
            Max = ~ max(., na.rm = TRUE),
            Missing = ~ sum(is.na(.))
          ))) %>%
          pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
          separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
          pivot_wider(names_from = Statistic, values_from = Value)
        
        datatable(summary_df, 
                 options = list(scrollX = TRUE, pageLength = 15),
                 rownames = FALSE) %>%
          formatRound(columns = c("Mean", "Median", "SD"), digits = 3)
      } else {
        datatable(data.frame(Message = "No numeric variables found"), 
                 options = list(dom = 't'), rownames = FALSE)
      }
    })
    
    # Data types plot
    output$data_types_plot <- renderPlotly({
      req(current_data())
      data <- current_data()
      
      types_df <- data.frame(
        Type = sapply(data, function(x) class(x)[1]),
        Variable = names(data)
      ) %>%
        count(Type, name = "Count")
      
      p <- ggplot(types_df, aes(x = Type, y = Count, fill = Type)) +
        geom_bar(stat = "identity") +
        labs(title = "Data Types Distribution",
             x = "Data Type", y = "Number of Variables") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Missing data analysis
    output$missing_data_plot <- renderPlotly({
      req(current_data())
      data <- current_data()
      
      missing_df <- data %>%
        summarise(across(everything(), ~ sum(is.na(.)))) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
        mutate(Percentage = round(Missing / nrow(data) * 100, 2)) %>%
        arrange(desc(Missing))
      
      p <- ggplot(missing_df, aes(x = reorder(Variable, Missing), y = Percentage)) +
        geom_bar(stat = "identity", fill = "coral") +
        coord_flip() +
        labs(title = "Missing Data by Variable",
             x = "Variable", y = "Missing Data (%)") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    output$missing_data_table <- renderDT({
      req(current_data())
      data <- current_data()
      
      missing_df <- data %>%
        summarise(across(everything(), ~ sum(is.na(.)))) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
        mutate(
          Total = nrow(data),
          Percentage = round(Missing / nrow(data) * 100, 2)
        ) %>%
        arrange(desc(Missing))
      
      datatable(missing_df, 
               options = list(pageLength = 10),
               rownames = FALSE) %>%
        formatRound(columns = "Percentage", digits = 2)
    })
    
    # Distribution plots
    output$distribution_plot <- renderPlotly({
      req(current_data(), input$variable_select)
      data <- current_data()
      var_data <- data[[input$variable_select]]
      
      if (is.numeric(var_data)) {
        if (input$plot_type == "histogram") {
          p <- ggplot(data, aes_string(x = input$variable_select)) +
            geom_histogram(bins = input$bins, fill = "steelblue", alpha = 0.7) +
            labs(title = paste("Distribution of", input$variable_select),
                 x = input$variable_select, y = "Frequency") +
            theme_minimal()
        } else if (input$plot_type == "density") {
          p <- ggplot(data, aes_string(x = input$variable_select)) +
            geom_density(fill = "steelblue", alpha = 0.7) +
            labs(title = paste("Density of", input$variable_select),
                 x = input$variable_select, y = "Density") +
            theme_minimal()
        } else {
          p <- ggplot(data, aes_string(y = input$variable_select)) +
            geom_boxplot(fill = "steelblue", alpha = 0.7) +
            labs(title = paste("Boxplot of", input$variable_select),
                 y = input$variable_select) +
            theme_minimal()
          
          if (!input$show_outliers) {
            p <- p + geom_boxplot(outlier.shape = NA)
          }
        }
      } else {
        # For categorical variables
        freq_data <- as.data.frame(table(var_data))
        names(freq_data) <- c("Category", "Frequency")
        
        p <- ggplot(freq_data, aes(x = Category, y = Frequency)) +
          geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Distribution of", input$variable_select),
               x = input$variable_select, y = "Frequency") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      ggplotly(p)
    })
    
    # Correlation plot
    output$correlation_plot <- renderPlotly({
      req(current_data())
      data <- current_data()
      
      # Get only numeric columns
      numeric_data <- data %>% select(where(is.numeric))
      
      if (ncol(numeric_data) > 1) {
        cor_matrix <- cor(numeric_data, use = "complete.obs", method = input$correlation_method)
        
        # Convert to long format for ggplot
        cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
        cor_df$Correlation <- as.vector(cor_matrix)
        
        # Filter by threshold
        cor_df$Significant <- abs(cor_df$Correlation) >= input$correlation_threshold
        
        p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1, 1)) +
          labs(title = paste("Correlation Matrix -", stringr::str_to_title(input$correlation_method)),
               x = "", y = "") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        if (input$show_correlation_values) {
          p <- p + geom_text(aes(label = round(Correlation, 2)), size = 3)
        }
      } else {
        # Create empty plot with message
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Not enough numeric variables for correlation analysis") +
          theme_void()
      }
      
      ggplotly(p)
    })
  })
}
