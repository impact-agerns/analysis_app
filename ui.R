# UI
ui <- shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML("
        body, .container-fluid {
            background-color: white !important; /* Set background for full page */
            color: black;
        }
        .markdown-content {
            background-color: white;
            color: black;
            padding: 15px;
            border-radius: 5px;
        }
        /* Optional: Add padding to the bottom to ensure no cut-off appearance */
        .container-fluid {
            padding-bottom: 50px;
        }
    "))
    ),
    includeCSS("www/style.css"),
    dashboardPage(
      dashboardHeader(
        title = "KI data analysis",
        titleWidth = 300,
        tags$li(
          class = "dropdown", 
          tags$img(src = "REACH.png", height = "50px", width = "225px")
        )
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("Analysis", tabName = "analysis", icon = icon("table")),
          menuItem("Plot", tabName = "plot", icon = icon("chart-bar"))
        )
      ),
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "analysis",
                  # First Row: Import Files (side by side)
                  fluidRow(
                    column(width = 6, 
                           box(
                             title = NULL,
                             status = "primary",
                             solidHeader = TRUE,
                             width = NULL,
                             tags$div(
                               tags$h4("Import dataset", style = "color: var(--primary-color);"),
                               tags$h5(style = "color: gray;", "Important: Data should be cleaned before importing & clean data should be saved in the first sheet.")
                             ),
                             fileInput("data_file", 
                                       label = tags$span(style = "color: var(--primary-color);", "Upload Data File (xlsx)"), accept = ".xlsx")
                           )
                    ),
                    column(width = 6, 
                           box(
                             title = NULL,
                             status = "primary",
                             solidHeader = TRUE,
                             width = NULL,
                             tags$div(
                               tags$h4("Import Kobo file", style = "color: var(--primary-color);"),
                               tags$h5(style = "color: gray;", "Important: Kobo tool has to match data. Make sure `label::English (en)` is specified correctly in survey & choice sheet.")
                             ),
                             fileInput("kobo_file", 
                                       label = tags$span(style = "color: var(--primary-color);", "Upload Kobo Tool File (xlsx)"), accept = ".xlsx")
                           )
                    )
                  ),
                  
                  # Second Row: Data Aggregation (spans the entire row)
                  fluidRow(
                    column(width = 12, 
                           box(
                             title = NULL,
                             status = "primary",
                             solidHeader = TRUE,
                             width = NULL,
                             tags$div(
                               tags$h4("Data aggregation", style = "color: var(--primary-color);"),
                               tags$h5(style = "color: gray;", "Pick all variables relevant for aggregation (i.e., admin1, admin2, admin3).")
                             ),
                             selectInput("aggregation_option", 
                                         label = tags$span(style = "color: var(--primary-color);", "Do you want to aggregate the data?"), 
                                         choices = c("Aggregate data" = "aggregate", "Leave data at KI level" = "no_aggregate")),
                             uiOutput("aggregation_vars_ui"),
                             actionButton("run_aggregation", "Run Aggregation"),
                             # actionButton("reset_aggregation", "Reset Aggregation"),  # Reset button added
                             verbatimTextOutput("aggregation_status"),
                             downloadButton("download_aggregated_data", "Download Aggregated Data")
                           )
                    )
                  ),
                  
                  # Third Row: Data Analysis (spans the entire row)
                  fluidRow(
                    column(width = 12, 
                           box(
                             title = NULL,
                             status = "primary",
                             solidHeader = TRUE,
                             width = NULL,
                             tags$div(
                               tags$h4("Data Analysis", style = "color: var(--primary-color);"),
                               tags$h5(style = "color: gray;", "If data aggregated, pick one of the aggregation variables.")
                             ),
                             selectInput("disaggregate_by_1", 
                                         label = tags$span(style = "color: var(--primary-color);", "Choose variable(s) for main analysis (required)"),  
                                         choices = NULL, multiple = TRUE),
                             selectInput("disaggregate_by_2", 
                                         label = tags$span(style = "color: var(--primary-color);", "Choose variable(s) for second (optional) analysis"), 
                                         choices = NULL, multiple = TRUE),
                             actionButton("run_analysis", "Run Analysis"),
                             # actionButton("reset_analysis", "Reset Analysis"),  # Reset button added
                             verbatimTextOutput("analysis_status"),
                             uiOutput("progress_bar"),
                             downloadButton("download_analysis_data", "Download Analysis Data")
                           )
                    )
                  )
          ),
          tabItem(tabName = "home",
                  fluidRow(column(width = 12,
                                  div(class = "markdown-content", includeMarkdown("README.md"))
                  ))
          ),
          tabItem(tabName = "plot",
                  fluidRow(
                    box(title = "Plot Settings", 
                        selectInput("plot_type", 
                                    label = tags$span(style = "color: var(--primary-color);",  "Plot Type"),
                                    choices = c("Bar Chart" = "bar", "Scatterplot" = "scatter", "Violin Plot" = "violin")),
                        selectInput("x_axis", 
                                    label = tags$span(style = "color: var(--primary-color);",  "X-Axis"), choices = NULL),
                        selectInput("y_axis", 
                                    label = tags$span(style = "color: var(--primary-color);",  "Y-Axis"), choices = NULL),
                        selectInput("measure", 
                                    label = tags$span(style = "color: var(--primary-color);",  "Measure"),choices = c("mean", "count", "median"))
                    ),
                    box(title = "Plot Output",
                        plotOutput("plot_output", height = "500px")
                    )
                  )
          )
        )
      )# dashboard body
    )
  ) # fluid page end
) # shinyui end
