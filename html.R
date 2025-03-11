library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(rmarkdown)
library(shinyjs)

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
                column(4, selectInput("filter_disag_var_1", "Select Disaggregation Level:", choices = NULL, multiple = TRUE)),
                column(8, selectInput("selected_questions", "Select Questions:", choices = NULL, multiple = TRUE))
              ),
              plotOutput("basic_plot")
      ),
      tabItem(tabName = "report",
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
  
  observe({
    data <- analysis_data()
    updateSelectInput(session, "filter_disag_var_1", choices = unique(data$disag_val_1), selected = unique(data$disag_val_1)[1])
    updateSelectInput(session, "selected_questions", choices = unique(data$label), selected = unique(data$label)[1])
  })
  
  output$basic_plot <- renderPlot({
    req(input$selected_questions, input$filter_disag_var_1)
    data <- analysis_data() %>% filter(label %in% input$selected_questions, disag_val_1 %in% input$filter_disag_var_1)
    ggplot(data, aes(x = reorder(label.choice, mean), y = mean, fill = disag_val_1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      facet_wrap(~label, scales = "free") +
      theme_minimal()
  })
  
  observeEvent(input$generate_html, {
    rmarkdown::render("generate_report.Rmd", output_file = "analysis_output.html")
  })
  
  output$download_html <- downloadHandler(
    filename = "analysis_output.html",
    content = function(file) {
      file.copy("analysis_output.html", file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
