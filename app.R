# Required Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(openxlsx)
library(janitor)
library(tidyverse)
library(purrr)
library(DT)

source('src/functions.R', local=T)
source('src/Mode.R', local=T)
source('src/process_data_for_aggregation.R', local=T)
source('src/aggregate_data.R', local=T)
options(shiny.maxRequestSize = 30 * 1024^2) # 10 MB limit

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Processing and Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Plot", tabName = "plot", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(title =, 
                     tags$div(
                       tags$h4("Import dataset"),
                       tags$h5(style = "color: gray;", "Data should be cleaned before importing.")
                       ),
                  fileInput("data_file", "Upload Data File (xlsx)", accept = ".xlsx")),
                box(title =, 
                    tags$div(
                      tags$h4("Import kobo file"),
                      tags$h5(style = "color: gray;", "Hint: Make sure `label::English` is specified correctly in survey & choice sheet.")
                    ),
                    fileInput("kobo_file", "Upload Kobo Tool File (xlsx)", accept = ".xlsx"))
              ),
                box(title =, 
                    tags$div(
                      tags$h4("1. Data aggregation"),
                      tags$h5(style = "color: gray;", "Pick all variables relevant for aggregation (i.e. admin1, admin2, admin3)")
                    ),
                selectInput("aggregation_option", "Choose Aggregation level", 
                            choices = c("Aggregate data by" = "aggregate", "Leave data at KI level" = "no_aggregate")),
                uiOutput("aggregation_vars_ui"),
                actionButton("run_aggregation", "Run Aggregation"),
                verbatimTextOutput("aggregation_status")
              ),
              box(title =, 
                  tags$div(
                    tags$h4("2. Analysis of data for reporting"),
                    tags$h5(style = "color: gray;", "If data aggregated pick one of the aggregation variables.")
                  ),
                  selectInput("disaggregate_by", "Choose Analysis Level", choices = NULL, multiple = TRUE),
                  actionButton("run_analysis", "Run Analysis"),
                  verbatimTextOutput("analysis_status"),
                  uiOutput("progress_bar"))
      ),
      
      tabItem(tabName = "table",
              fluidRow(
                selectInput("view_type", "Data Type", choices = c("Raw Data" = "raw", "Aggregated Data" = "aggregated", "Analysis Data" = "analysis")),
                downloadButton("download_data", "Download Processed Data"),
                dataTableOutput("table_output")
              )
      ),
      
      tabItem(tabName = "plot",
              fluidRow(
                selectInput("plot_type", "Plot Type", choices = c("Bar Chart" = "bar", "Scatterplot" = "scatter", "Violin Plot" = "violin")),
                selectInput("x_axis", "X-Axis", choices = NULL),
                selectInput("y_axis", "Y-Axis", choices = NULL),
                selectInput("measure", "Measure", choices = c("mean", "count", "median")),
                plotOutput("plot_output")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive variable to store the loaded data
  data_in <- reactiveVal(NULL)
  
  # Observe the data file input
  observeEvent(input$data_file, {
    req(input$data_file)
    
    tryCatch({
      data <- read_excel(input$data_file$datapath, sheet=1)
      data_in(data)
      
      data_source_reactive <<- reactive({
        data
      })
      # Debug: Print names of data to confirm data is read correctly
      print("Data successfully loaded:")
      print(names(data))
      
      # Update disaggregation choices
      updateSelectInput(session, "disaggregate_by", choices = names(data), selected = NULL)
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  # Ensure data_in is not NULL before attempting to rename columns
  observeEvent(data_in(), {
    req(data_in())  # Only proceed if data_in is not NULL
    
    # Retrieve the data, modify column names, and reset data_in
    data <- data_in()
    names(data) <- str_replace_all(names(data), "/", ".")
    print("okay this worked finally!")
    # Update the modified data back into data_in
    data_in(data)
    print('data is in reactive data_in')
  })
  kobo_tool <- reactive({
    req(input$kobo_file)
    read_xlsx(input$kobo_file$datapath)
  })
  
  # UI output for aggregation variables
  output$aggregation_vars_ui <- renderUI({
    req(data_in())
    if (input$aggregation_option == "aggregate") {
      selectInput("agg_vars", "Select Variables to Aggregate By", choices = names(data_in()), multiple = TRUE)
    }
  })
  
  # Run aggregation button functionality
  observeEvent(input$run_aggregation, {
    req(data_in())
    
    if (input$aggregation_option == "aggregate") {
      agg_vars <- input$agg_vars
      
      if (length(agg_vars) == 0) {
        showNotification("Please select at least one variable for aggregation.", type = "error")
        return()
      }
      #
      survey <- read_xlsx(input$kobo_file$datapath, guess_max = 50000, na = c("NA","#N/A",""," ","N/A"), sheet = 'survey')
      choices <- read_xlsx(input$kobo_file$datapath, guess_max = 50000, na = c("NA","#N/A",""," ","N/A"), sheet = 'choices')
      print('loaded survey data successfully!')
      # Combine the survey and choices
      tool.combined <- combine_tool(survey = survey, responses = choices)
      print('tool.combined created')
      # Process column names for aggregation
      col.sm <- tool.combined %>% filter(q.type == "select_multiple") %>% pull(name) %>% unique()
      col.so <- tool.combined %>% filter(q.type == "select_one") %>% pull(name) %>% unique()
      col.int <- survey %>% filter(type == "integer") %>% pull(name) %>% unique()
      col.text <- survey %>% filter(type=="text") %>% pull(name) %>% unique
      
      if (length(col.sm)<=1){
        print("Check if label column is specified correctly in combine_tool function")
        stop("No select_multiple questions found in the tool")
      }else(print("Select multiple questions found in the tool"))
      
      if (any(str_detect(names(data_in()), "/"))){
        sm_separator <-  "/"
        if (sm_separator == "/"){
          print("The separator is /")
          names(data_in()) <- str_replace_all(names(data_in()), "/", ".")
          print("Separator has been replaced to .") 
        }
        
      } else if (any(str_detect(names(data_in()), "."))){
        print("The separator is . Which is good :D")
      }
      
      
      
      # Example aggregation process (modify as needed)
      vedelete <- c("dk", "DK", "Not_to_sure", "not_sure")
      
      data_cleaned <- process_data_for_aggregation(data_in(), replace_vec_na = vedelete)
      print('processed data for aggregation')
      
      req(data_cleaned, agg_vars)  # Ensure these are available
      
      tryCatch({
        aok_aggregated <- aggregate_data(data_cleaned, agg_vars, 
                                         col_so = col.so, col_sm = col.sm, 
                                         col_int = col.int, col_text = col.text)
      
        data_in(aok_aggregated)
        df_aggregated_react <<- reactive({ aok_aggregated })
        
      }, error = function(e) {
        showNotification(paste("Error aggregating data:", e$message), type = "error")
      })      
      print('aggregated data successfully!')
      # Store aggregated data (add any additional processing)
      
      
      output$aggregation_status <- renderText("Aggregation completed successfully!")
    } else {
      output$aggregation_status <- renderText("Data left at KI level. No aggregation performed.")
    }
  })
  
  # Analysis button functionality
  # This code snippet integrates progress updates during the processing of data.
  observeEvent(input$run_analysis, {
    req(data_in())  # Ensure data is available
    
    # Use withProgress to show a progress bar
    withProgress(message = "Running analysis...", {
      # Steps for reading survey and choices from Kobo tool
      survey <- read_xlsx(input$kobo_file$datapath, guess_max = 50000, na = c("NA","#N/A",""," ","N/A"), sheet = 'survey')
      choices <- read_xlsx(input$kobo_file$datapath, guess_max = 50000, na = c("NA","#N/A",""," ","N/A"), sheet = 'choices')
      
      # Combine the survey and choices
      tool.combined <- combine_tool(survey = survey, responses = choices)
      
      # Process column names for aggregation
      col.sm <- tool.combined %>% filter(q.type == "select_multiple") %>% pull(name) %>% unique()
      col.so <- tool.combined %>% filter(q.type == "select_one") %>% pull(name) %>% unique()
      col.int <- survey %>% filter(type == "integer") %>% pull(name) %>% unique()
      
      # Check if select_one questions are found
      if (length(col.so) <= 1) {
        stop("Check if label column is specified correctly in combine_tool function: No select_one questions found in the tool")
      } else {
        print("Select one questions found in the tool")
      }
      
      # Define disaggregation levels
      # dis <- list("admin2", c("admin2", "admin3"))
      dis <- input$disaggregate_by
      
      if (length(dis) == 0) {
        showNotification("Please select at least one variable for analysis", type = "error")
        return()
      }
      
      # Clean expanded data
      clean_expanded <- data_in() %>%
        expand.select.one.vec(col.so[col.so %in% names(.)])
      print('clean_expanded run!')
      
      # Initialize the results list for aggregation
      res <- list()
      
      # Efficient aggregation
      for (d in dis) {
        d <- d %>% unlist
        df <- clean_expanded
        if (sum(d %in% names(df)) > 0) {
          df <- df %>% group_by(across(any_of(d)))
        }
        
        df <- df %>%
          mutate(across(matches(paste0("^", c(col.sm, col.so), "\\.")), as.numeric), 
                 across(any_of(col.int), as.numeric)) %>%
          summarise(across(matches(paste0("^", c(col.sm, col.so), "\\.")), 
                           list(mean = ~mean(., na.rm = TRUE), 
                                count = ~sum(., na.rm = TRUE), 
                                resp = ~sum(!is.na(.)), 
                                n = ~n()), 
                           .names = "{.col}--{.fn}"),
                    across(any_of(col.int), 
                           list(mean = ~mean(., na.rm = TRUE), 
                                sum = ~sum(., na.rm = TRUE), 
                                median = ~median(., na.rm = TRUE), 
                                resp = ~sum(!is.na(.)), 
                                n = ~n()), 
                           .names = "{.col}--{.fn}")) %>%
          ungroup() %>%
          pivot_longer(-any_of(d)) %>% 
          separate(name, c("col", "function"), "--", remove = FALSE) %>% 
          separate(col, c("question", "choice"), "\\.", remove = FALSE) %>%  
          select(-name) %>%
          pivot_wider(names_from = "function", values_from = "value")
        
        # Rename and handle disaggregation variables
        if (sum(str_detect(d, "^all$")) == 0) {
          d <- d %>% setNames(paste0("disag_var_", 1:length(d)))
          df <- df %>% cbind(setNames(as.list(d), names(d))) %>% 
            rename_with(~paste0("disag_val_", 1:length(d)), any_of(unname(d)))
        } 
        
        res[[paste(d, collapse = "_")]] <- df %>%
          mutate(mean = ifelse(is.nan(mean), NA, mean))
        
        print(paste0("Analysis disaggregated by ", d, " done"))
      }
      
      # Combine results
      df_res <- res %>% bind_rows() %>%
        select(any_of(c("disag_var_1", "disag_val_1", "disag_var_2", "disag_val_2")), everything())
      
      df_res_labelled <- df_res %>% 
        left_join(select(tool.combined, name, label, name.choice, label.choice, q.type), by=c("question"="name", "choice"="name.choice"))%>% 
        # rename(aggregation_level1 = disag_var_1, aggregation_value1 = disag_val_1, 
        # 			 aggregation_level2 = disag_var_2, aggregation_value2 = disag_val_2, 
        # ) %>% 
        # mutate(aggregation_level1 = ifelse(is.na(aggregation_level1), "all_settlements", aggregation_level1)) %>%
        unique()
      
      # Output the results in the main panel
      df_res_labelled_reactive <<- reactive({ df_res_labelled })
      
      # Success message
      output$analysis_status <- renderText("Analysis completed successfully!")
    })
  })
  
  
  # Populate column choices for x_axis and y_axis based on uploaded data
  observeEvent(data_in(), {
    choices <- names(data_in())
    updateSelectInput(session, "x_axis", choices = choices)
    updateSelectInput(session, "y_axis", choices = choices)
  })
  
  
  
  # Display raw or processed data in table based on view type
  output$table_output <- renderDataTable({
    req(input$view_type)
    
    # Determine which dataset to display based on view type
    table_data <- switch(input$view_type,
                         "raw" = data_in(),
                         "analysis" = df_res_labelled_reactive(),
                         "aggregated" = df_aggregated_react())
    
    # Render DataTable with enhanced features
    datatable(table_data,
              options = list(
                scrollX = TRUE,                   # Enable horizontal scrolling
                scrollY = "500px",                # Set a fixed height for vertical scrolling
                paging = TRUE,                    # Enable pagination
                pageLength = -1,                  # Display all rows (infinite)
                searching = TRUE,                 # Enable search bar
                dom = 'Bfrtip',                  # Include buttons and filter
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Add export options
                initComplete = JS("function(settings, json) {",
                                  "$('.dataTables_filter').css({'float': 'none', 'text-align': 'right'});",  # Align search bar
                                  "$('.dataTables_length').css({'float': 'none', 'text-align': 'right'});",  # Align length selection
                                  "$('.dataTables_info').css({'float': 'none', 'text-align': 'right'});",   # Align info
                                  "}")
              ),
              filter = 'top'                     # Place filters above each column header
    ) %>%
      formatStyle(  # Make the table visually appealing
        columns = names(table_data),  # Apply to all columns
        backgroundColor = styleEqual("highlight", "yellow")  # Example style
      )
  })
  
  # Display summary statistics for selected variables
  output$stats_output <- renderTable({
    req(df_res_labelled_reactive())
    data_stats <- df_res_labelled_reactive() %>% 
      summarise(across(where(is.numeric), list(mean = ~ mean(.x, na.rm = TRUE), 
                                               sd = ~ sd(.x, na.rm = TRUE),
                                               min = ~ min(.x, na.rm = TRUE),
                                               max = ~ max(.x, na.rm = TRUE))))
    data_stats
  })
  
  # Plot output based on user choices
  output$plot_output <- renderPlot({
    req(input$x_axis, input$y_axis, input$plot_type)  # Ensure all inputs are selected
    
    # Dynamically select columns from data based on user input
    x_var <- sym(input$x_axis)
    y_var <- sym(input$y_axis)
    
    # Base ggplot object
    p <- ggplot(data = data_source_reactive(), aes(x = !!x_var, y = !!y_var))
    
    # Conditional layer based on selected plot type
    p <- p + 
      switch(input$plot_type,
             "bar" = geom_col(),                    # Bar chart
             "scatter" = geom_point(),              # Scatterplot
             "violin" = geom_violin()               # Violin plot
      ) +
      labs(
        x = input$x_axis,
        y = input$y_axis,
        title = paste("Plot of", input$y_axis, "vs", input$x_axis)
      ) +
      theme_minimal()  # Optional: use any theme you prefer
    
    # Render the plot
    p
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(
        if (input$view_type == "raw") {
          "raw_data_"
        } else if (input$view_type == "analysis") {
          "analysis_data_"
        } else {
          "aggregated_data_"
        },
        Sys.Date(), ".xlsx", sep = ""
      )
    },
    content = function(file) {
      wb <- createWorkbook()
      
      # Determine worksheet name and data based on view_type
      if (input$view_type == "raw") {
        sheet_name <- "Raw Data"
        data_to_write <- data_in()
      } else if (input$view_type == "analysis") {
        sheet_name <- "Analysis Data"
        data_to_write <- df_res_labelled_reactive()
      } else { # view_type == "aggregated"
        sheet_name <- "Aggregated Data"
        data_to_write <- df_aggregated_react()  # Use the reactive expression here
      }
      
      # Write data to the worksheet
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, data_to_write)
      
      # Save workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  
}

shinyApp(ui, server)
