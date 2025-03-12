library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(rmarkdown)
library(shinyjs)
library(htmltools)
library(plotly)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main Analysis", tabName = "main", icon = icon("dashboard")),
      menuItem("Generate Report", tabName = "report", icon = icon("file"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                column(4, selectInput("filter_disag_var_1", "Select Disaggregation Level:", choices = NULL)),
                column(4, selectInput("filter_disag_val_1", "Select area value:", choices = NULL, multiple = T)),
                column(8, selectInput("selected_question", "Select Question:", choices = NULL))
              ),
              plotOutput("basic_plot"),
              downloadButton("download_plot", "Download Plot")
      ),
      tabItem(tabName = "report",
              h3("Generate Report"),
              fluidRow(
                column(4, selectInput("report_disag_var", "Select Disaggregation Level:", choices = NULL)),
                column(4, selectInput("report_disag_val", "Select area values:", choices = NULL, multiple = TRUE)),
                column(8, selectInput("report_questions", "Select Questions:", choices = NULL, multiple = TRUE))
              ),
              actionButton("generate_html", "Generate HTML Report"),
              downloadButton("download_html", "Download HTML Report")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  analysis_data <- reactive({
    data <- read_excel(list.files('input', pattern="analysis_data_test", full.names = TRUE)) %>% mutate(mean = mean * 100)
    return(data)
  })
  
  
  # observe({
  #   data <- analysis_data()
  # 
  #   updateSelectInput(session, "filter_disag_var_1", choices = unique(data$disag_var_1), selected = unique(data$disag_var_1)[1])
  # })

  filtered_data <- reactive({
    req(input$filter_disag_var_1)
    data %>% filter(disag_var_1 == input$filter_disag_var_1)
  })

  observe({
    data <- analysis_data()  # Get the full dataset
    req(data)  # Ensure data exists
    
    updateSelectInput(session, "filter_disag_var_1", 
                      choices = unique(data$disag_var_1), 
                      selected = unique(data$disag_var_1)[1])
    
    updateSelectInput(session, "report_disag_var",
                      choices = unique(data$disag_var_1),
                      selected = unique(data$disag_var_1)[1])
  })
  
  rep_filtered_data <- reactive({
    req(input$report_disag_var)
    data %>% filter(disag_var_1 == input$report_disag_var)
  })
  
  
  
  observe({
    updateSelectInput(session, "filter_disag_val_1", choices = unique(filtered_data()$disag_val_1), selected = unique(filtered_data()$disag_val_1)[1])
    updateSelectInput(session, "selected_question", choices = unique(data$label), selected = unique(data$label)[1])
    # updateSelectInput(session, "report_disag_var", choices = unique(data$disag_var_1), selected = unique(filtered_data()$disag_val_1)[1])
    updateSelectInput(session, "report_disag_val", choices = unique(rep_filtered_data()$disag_val_1))
    updateSelectInput(session, "report_questions", choices = unique(data$label))
  })
  
  # output$basic_plot <- renderPlot({
  #   req(input$selected_question, input$filter_disag_val_1)
  #   data <- analysis_data() %>% filter(label == input$selected_question, disag_val_1 == input$filter_disag_val_1)
  #   ggplot(data, aes(x = reorder(label.choice, mean), y = mean, fill = disag_val_1)) +
  #     geom_bar(stat = "identity") +
  #     coord_flip() +
  #     theme_minimal()
  # })
  
  basic_plot <- reactive({
    req(input$selected_question, input$filter_disag_val_1)
    
    # Filter data for selected question and selected disag_val_1 values
    data_filtered <- data %>% 
      filter(label == input$selected_question, disag_val_1 %in% input$filter_disag_val_1) %>%
      # filter(label ==selected_question, disag_val_1 %in% params$selected_disag_vals) %>%
      group_by(choice) %>% 
      mutate(total_percentage = sum(mean)) %>% ungroup() %>% 
      arrange(desc(total_percentage), desc(mean))
    
    resp_sum <- data_filtered %>% select(disag_val_1, resp) %>% unique() %>% pull(resp) %>% sum()
    
    # Create stacked bar plot
    p <- ggplot(data_filtered, aes(x = reorder(label.choice, total_percentage), y = mean, fill = disag_val_1)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = RColorBrewer::brewer.pal(n = length(unique(data_filtered$disag_val_1)), "Set1")) +
      scale_x_discrete(labels = ~str_wrap(., width = 60)) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(data_filtered$total_percentage)+0.02)) +
      labs(x = "", y = "% of respondents", fill = "Area",
           title = paste0("\n", str_wrap(unique(data_filtered$label), width = 65), "\n\n"),
           caption = paste0(resp_sum, " respondents answered the question.")) +
      theme_minimal() + coord_flip() +
      theme(plot.subtitle = element_text(size = 9),
            panel.grid = element_blank(),
            axis.text.x = element_blank())+
      geom_text(aes(label = ifelse(count > 0, 
                                   ifelse(mean > 0.09, 
                                          paste0(round(mean * 100, 0), "% (", count, ")"),
                                          paste0(round(mean * 100, 0), "%")), 
                                   "")),             position = position_stack(vjust = 0.5), size = 3, color = "white")
    p
      })
  
  output$basic_plot <- renderPlot({
    basic_plot()
    
  })
  
  # output$download_plot <- downloadHandler(
  #   filename = function() { paste("plot_", Sys.Date(), ".png", sep = "") },
  #   content = function(file) {
  #     ggsave(file, plot = last_plot(), device = "png")
  #   }
  # )
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      # Use the plot directly from renderPlot
      ggsave(file, plot = basic_plot(), device = "png")
    }
  )
  
  
  observeEvent(input$generate_html, {
    req(input$report_disag_val, input$report_questions)
    rmarkdown::render("generate_report.Rmd", output_file = "analysis_output.html", params = list(
      selected_disag_vals = input$report_disag_val,
      selected_questions = input$report_questions
    ))
  })
  
  output$download_html <- downloadHandler(
    filename = "analysis_output.html",
    content = function(file) {
      file.copy("analysis_output.html", file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
