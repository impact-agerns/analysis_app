library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(rmarkdown)
library(shinyjs)
library(htmltools)

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
                column(4, selectInput("filter_disag_val_1", "Select area value:", choices = NULL)),
                column(8, selectInput("selected_question", "Select Question:", choices = NULL))
              ),
              plotOutput("basic_plot"),
              downloadButton("download_plot", "Download Plot")
      ),
      tabItem(tabName = "report",
              h3("Generate Report"),
              fluidRow(
                # column(4, selectInput("filter_disag_var_1", "Select Disaggregation Level:", choices = NULL)),
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
    
    # updateSelectInput(session, "report_disag_var",  
    #                   choices = unique(data$disag_var_1),  
    #                   selected = unique(data$disag_var_1)[1])
  })
  
  observe({
    updateSelectInput(session, "filter_disag_val_1", choices = unique(filtered_data()$disag_val_1), selected = unique(filtered_data()$disag_val_1)[1])
    updateSelectInput(session, "selected_question", choices = unique(data$label), selected = unique(data$label)[1])
    # updateSelectInput(session, "report_disag_var", choices = unique(data$disag_var_1), selected = unique(filtered_data()$disag_val_1)[1])
    updateSelectInput(session, "report_disag_val", choices = unique(filtered_data()$disag_val_1))
    updateSelectInput(session, "report_questions", choices = unique(data$label))
  })
  
  output$basic_plot <- renderPlot({
    req(input$selected_question, input$filter_disag_val_1)
    data <- analysis_data() %>% filter(label == input$selected_question, disag_val_1 == input$filter_disag_val_1)
    ggplot(data, aes(x = reorder(label.choice, mean), y = mean, fill = disag_val_1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
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
