# Required Libraries
# gc()
library(pacman)

p_load(shiny, shinydashboard, writexl, dplyr, readxl, openxlsx, janitor, tidyverse, purrr, DT, markdown)
source('src/functions.R', local=T)
source('src/Mode.R', local=T)
source('src/process_data_for_aggregation.R', local=T)
source('src/aggregate_data.R', local=T)
source('src/format.R', local=T)
source('src/utils.R', local=T)
source('src/si_functions.R', local=T)
options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB limit

ui <- source('ui.R', local=T)


server <- function(input, output, session) {
  
  data_in <- reactiveVal(NULL) # used for clean and aggregated data
  
  survey_data <- reactiveVal(NULL)
  choices_data <- reactiveVal(NULL)
  label_global <- reactiveVal(NULL)
  
  dis_global <- reactiveVal(NULL)
  analysis_data_out <- reactiveVal(NULL) # analysis data
  
  
  local_admin_bounds <- reactiveVal(NULL)
  official_admin_boundaries <- reactiveVal(
    c("admin1", "admin2", "admin3", "admin4")
    )
  
  data_index_out <- reactiveVal()
  
  ##### KOBO ####
  
  observeEvent(input$kobo_file, {
    req(input$kobo_file)
    
    tryCatch({
      # Read both sheets and store them in reactive values (assume survey_data and choices_data are defined)
      survey_data(read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA", "#N/A", "", " ", "N/A"), sheet = 'survey'))
      choices_data(read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA", "#N/A", "", " ", "N/A"), sheet = 'choices'))
      
      print('kobo is read and active')
      
      # Extract label columns (e.g., those that start with "label::")
      label_columns <- names(survey_data())[grepl("^lab", names(survey_data()))]
      
      # Update the label selector input with a default selection (first label)
      updateSelectInput(session, "label_selector", 
                        choices = label_columns, 
                        selected = label_columns[1])
      updateSelectInput(session, "select_admin_bounds", choices = survey_data()$name, selected = NULL)
      
      # updateSelectInput(session, "select_admin_bounds", choices = official_admin_boundaries(), selected = NULL)
        
      
      
    }, error = function(e) {
      print(paste("Error loading kobo:", e$message))
      showNotification(paste("Error loading kobo:", e$message), type = "error")
    })
  })
  

  
  # Observe the data file input
  observeEvent(input$data_file, {
    req(input$data_file)
    print("File upload initiated.")
    
    temp_file_path <- tempfile(fileext = ".xlsx")
    file.copy(input$data_file$datapath, temp_file_path, overwrite = TRUE)
    cat(temp_file_path)
    # cat(input$data_file$datapath)
    print("File copied to temp path.")
    
    # # local:
    # data_path <- choose.files(caption ="Please select the data path", multi = F)
    # data_in <-  openxlsx::read.xlsx(data_path, sheet = 1) 
    
    # label_name <- 'label::English (en)'
    
    # local run
    # tool_path <- choose.files(caption ="Please select the tool to create the dummy data.", multi = F)
    # choices <- read_excel(tool_path, sheet="choices")
    # survey <- read_excel(tool_path, sheet="survey")
    
    tryCatch({
      data <- openxlsx::read.xlsx(temp_file_path, sheet = 1, na.strings = c("NA", "#N/A", "", " ", "N/A"), )
      
      # data <- readxl::read_excel(temp_file_path, sheet = 1, guess_max = 500, na = c("NA", "#N/A", "", " ", "N/A"))
      
      data_in(data) 
        
      print("Data successfully read.")
      
      # str(data)
      # updateSelectInput(session, "disaggregate_by_1", choices = names(data), selected = NULL)
      # updateSelectInput(session, "disaggregate_by_2", choices = names(data), selected = NULL)
      
    }, error = function(e) {
      print(paste("Error loading data:", e$message))
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
    tryCatch({
      updateSelectInput(session, "disaggregate_by_1", choices = names(data), selected = NULL)
      updateSelectInput(session, "disaggregate_by_2", choices = names(data), selected = NULL)
    }, error = function(e) {
      showNotification(paste("Error updating input fields:", e$message), type = "error")
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
  
  observeEvent(input$select_admin_bounds, {
    req(input$select_admin_bounds)
    local_admin_bounds(input$select_admin_bounds)
    
    # data <- data_in() %>% 
    #   rename_with(.fn = ~official_admin_boundaries(), .cols = all_of(local_admin_bounds()))
    # 
    # data_in(data) 
    # print('official admin added to data_in')
    
  })
  
  
  # Separate observer to update label_global when the label selector changes
  observeEvent(input$label_selector, {
    req(input$label_selector)
    label_global(input$label_selector)
    
    if (nchar(label_global()) == 0) {
      print("label_global is empty!")
    } else {
      print(paste("label_global has a value:", label_global()))
    }

    
  })
  
  #### Aggregation ####
  
  # UI output for aggregation variables
  output$aggregation_vars_ui <- renderUI({
    req(data_in())
    if (input$aggregation_option == "aggregate") {
      selectInput("agg_vars", 
                  label = tags$span(style = "color: var(--primary-color);",  "Choose variable(s) for aggregation"),
                  choices = names(data_in()), multiple = TRUE)
    }
  })
  gc()
  # Run aggregation button functionality
  observeEvent(input$run_aggregation, {
    req(data_in())
    
    if (input$aggregation_option == "aggregate") {
      agg_vars <- input$agg_vars
      
      if (length(agg_vars) == 0) {
        showNotification("Please select at least one variable for aggregation.", type = "error")
        return()
      }
      
      # survey <- read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA","#N/A",""," ","N/A"), sheet = 'survey')
      # choices <- read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA","#N/A",""," ","N/A"), sheet = 'choices')
      # print(head(survey_data()))
      
      print('loaded survey data successfully!')
      # print('label is:', label_global()) 
      # print(label_global())
      # print('did it print?')
      
      if (nchar(label_global()) == 0) {
        print("label_global is empty!")
      } else {
        print("label_global has a value.")
      }
      # if (is.null(label_global()) || nrow(label_global()) == 0) {
      #   print("label_global is empty or NULL")
      # } else {
      #   print("label_global has a value")
      # }
      # Combine the survey and choices
      print('about to run combine_tool_global')
      tool.combined <- combine_tool_global_label(survey = survey_data(), responses = choices_data(), label_col=label_global())
      print('tool.combined created')
      # Process column names for aggregation
      col.sm <- tool.combined %>% filter(q.type == "select_multiple") %>% pull(name) %>% unique()
      col.so <- tool.combined %>% filter(q.type == "select_one") %>% pull(name) %>% unique()
      col.int <- survey_data() %>% filter(type == "integer") %>% pull(name) %>% unique()
      col.text <- survey_data() %>% filter(type=="text") %>% pull(name) %>% unique
      
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
      vedelete <- c("dk", "DK", "dnk","Not_to_sure", "not_sure",  "Not_sure", "pnta", "prefer_not_to_answer", "Prefer_not_to_answer")
      
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
    
    updateSelectInput(session, "agg_vars", selected = input$agg_vars)
    
  })
  
  # observeEvent(input$reset_aggregation, {
  #   updateSelectInput(session, "aggregation_option", selected = "no_aggregate")
  #   updateSelectInput(session, "agg_vars", selected = NULL)
  #   data_in(NULL)  # Reset data to original
  #   output$aggregation_status <- renderText("")  # Clear status
  # })
  
  #### Analysis ####
  # Analysis button functionality
  # This code snippet integrates progress updates during the processing of data.
  observeEvent(input$run_analysis, {
    req(data_in())  # Ensure data is available
    
    # Use withProgress to show a progress bar
    withProgress(message = "Running analysis...", {
      # Steps for reading survey and choices from Kobo tool
      # survey <- read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA","#N/A",""," ","N/A"), sheet = 'survey')
      # choices <- read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA","#N/A",""," ","N/A"), sheet = 'choices')
      
      survey <- survey_data()
      choices <- choices_data()
      # Combine the survey and choices
      tool.combined <- combine_tool_global_label(survey = survey_data(), responses = choices_data(), label_col=label_global())
      
      # Process column names for aggregation
      col.sm <- tool.combined %>% filter(q.type == "select_multiple") %>% pull(name) %>% unique()
      col.so <- tool.combined %>% filter(q.type == "select_one") %>% pull(name) %>% unique()
      col.int <- survey_data() %>% filter(type == "integer") %>% pull(name) %>% unique()
      
      # Check if select_one questions are found
      if (length(col.so) <= 1) {
        stop("Check if label column is specified correctly in combine_tool function: No select_one questions found in the tool")
      } else {
        print("Select one questions found in the tool")
      }
      
      # Define disaggregation levels
      # dis <- list("admin2", c("admin2", "admin3"))
      dis1 <- input$disaggregate_by_1  # Analysis 1 selection
      dis2 <- input$disaggregate_by_2  # Analysis 2 selection
      
      # Combine disaggregations into one list for processing
      dis <- list(dis1, dis2)      
      dis <- dis[!sapply(dis, is.null)]
      
      dis_global(dis)
      
      
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
      
      analysis_data_out(df_res_labelled)
      
      # Success message
      output$analysis_status <- renderText("Analysis completed successfully!")
    })
  })
  
  #### Data exploration ####
  
  #### RUN Data Exploration
  filtered_disag_var_1 <- reactive({
    req(input$filter_disag_var_1)
    analysis_data_out() %>% filter(disag_var_1 == input$filter_disag_var_1)
  })
  
  observe({
    analysis_data <- analysis_data_out()  # Get the full dataset
    req(analysis_data)  # Ensure data exists
    
    updateSelectInput(session, "filter_disag_var_1", 
                      choices = unique(analysis_data$disag_var_1), 
                      selected = unique(analysis_data$disag_var_1)[1])
    
    updateSelectInput(session, "report_disag_var",
                      choices = unique(analysis_data$disag_var_1),
                      selected = unique(analysis_data$disag_var_1)[1])
  })
  
  rep_filtered_data <- reactive({
    req(input$report_disag_var)
    analysis_data_out() %>% filter(disag_var_1 == input$report_disag_var)
  })
  
  
  
  observe({
    updateSelectInput(session, "filter_disag_val_1", choices = unique(filtered_disag_var_1()$disag_val_1))
    updateSelectInput(session, "selected_question", choices = unique(analysis_data_out()$label), selected = unique(analysis_data_out()$label)[1])
    updateSelectInput(session, "report_disag_val", choices = unique(rep_filtered_data()$disag_val_1))
    updateSelectInput(session, "report_questions", choices = unique(analysis_data_out()$label))
  })
  
  observe({
    req(analysis_data_out())
    
    area_values <- unique(analysis_data_out()$disag_val_1)  # Get unique values
    
    updateSelectizeInput(session, "filter_disag_val_1", 
                         choices = area_values, 
                         # selected = area_values,  # Select all initially
                         server = TRUE
    )
  })
  
  basic_plot_exploration <- reactive({
    req(input$selected_question, input$filter_disag_val_1)

    # Filter data for selected question and selected disag_val_1 values
    data_filtered <- analysis_data_out() %>%
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
  
  
  
  output$basic_plot_exploration <- renderPlot({
    basic_plot_exploration()
    
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = basic_plot_exploration(), device = "png", width = 12, height = 6, units = "in")
    }
  )
  #### Report generation ####
  
  observeEvent(input$generate_html, {
    req(input$report_disag_val, input$report_questions)
    req(analysis_data_out())
    
    # Define the file path
    if (Sys.getenv("SHINY_PORT") != "") {
      # Running on shinyapps.io → Use temp directory
      temp_file <- file.path(tempdir(), "analysis_data.rds")
    } else {
      # Running locally → Use a local folder
      temp_file <- "analysis_data.rds"
    }
    
    saveRDS(analysis_data_out(), file = temp_file)
    
    
    rmarkdown::render("generate_report.Rmd", output_file = "analysis_output.html", 
                      params = list(
      selected_disag_vals = input$report_disag_val,
      selected_questions = input$report_questions,
      data_file = temp_file
    ))
  })
  
  output$download_html <- downloadHandler(
    filename = "analysis_output.html",
    content = function(file) {
      file.copy("analysis_output.html", file, overwrite = TRUE)
    }
  )
  
  # observeEvent(input$label_selector,input$kobo_file,{
  #   dap_template <- combine_tool_global_label(survey = survey_data(), 
  #                                             responses = choices_data(), 
  #                                             label_col=label_global()) %>% 
  #     select(question = name, q.type, choice = name.choice) %>% 
  #     mutate(severity_value = NA_real_, sector = NA_character_)
  #   
  # })
  
  
  ##### INDEX #### 
  
  output$download_dap <- downloadHandler(
    filename = function() { paste("DAP_Template_", Sys.Date(), ".xlsx", sep = "") },
    
    content = function(file) {
      # Ensure dap_template is properly defined inside the function
      
      aok_si_dap <- read_excel(list.files('input/data/', pattern="standard_si_dap", full.names = T), sheet="standard_aok_si_dap")
      aok_si_dap_clean <- aok_si_dap %>% 
        select(sector, type, question, question_label, choice,
              choice_label, severity_value, comment) %>% 
        mutate(source = "aok_ib_core")
      undac_si_dap <- read_excel(list.files('input/data/', pattern="standard_si_dap", full.names = T), sheet="standard_undac_si_dap") %>% 
        mutate(source = "undac_ib")
      sources <- bind_rows(aok_si_dap_clean, undac_si_dap) %>% unique() %>% 
        rename(choice_label_standard = choice_label, question_label_standard = question_label)
      
      dap_template <- combine_tool_global_label(survey = survey_data(), 
                                                responses = choices_data(), 
                                                label_col = label_global()) %>% 
        select(question = name, question_label = label, type = q.type, choice = name.choice, choice_label = label.choice) %>% 
        left_join(sources, by=c("question", "choice", "type")) %>% 
        filter(!str_detect(question, 'admin|role|consent|age'))
      
      
      list_dap <- list("updated_template" = dap_template, 
                       "AoK_si_standard_dap" = aok_si_dap,
                       "UNDAC_si_standard_dap" = undac_si_dap)
      
      
      # Save as Excel file
      write_xlsx(list_dap, path = file)
    }
  )
  severity_dap <- reactive({
    req(input$dap_file)
    print('loading severity_dap')
    openxlsx::read.xlsx(input$dap_file$datapath, sheet = 1, 
                                        na.strings = c("NA", "#N/A", "", " ", "N/A")) %>%
      select(question, type = type, choice, severity_value, sector) %>%
      mutate(severity_value = as.numeric(severity_value))
    
  })
  print('loaded severity dap')
  
  # check_indicators <- reactive({
  #   data_index <- inner_join(data_in(), severity_dap(), by = c("question", "choice"))
  #   num_unique_values <- length(unique(data_index$question))
  #   num_indicators <- severity_dictionary %>% select(question) %>% unique() %>% nrow()
  #   non_matching_values <- anti_join(severity_dictionary, data_index, by = c("question", "choice"))
  #   
  #   list(num_unique_values = num_unique_values, 
  #        num_indicators = num_indicators, 
  #        non_matching_values = non_matching_values)
  # })
  
  # Download report
  # output$download_quality_report <- downloadHandler(
  #   filename = function() { "Indicator_Check_Report.xlsx" },
  #   content = function(file) {
  #     report_data <- check_indicators()
  #     write.xlsx(report_data$non_matching_values, file)
  #   }
  # )
  
  # 
  observeEvent(input$run_index, {
    req(data_in(), severity_dap(), local_admin_bounds())
    
    selected_methods <- input$selected_index_method
    cat('selected method: ', selected_methods)
    # local_admin_bounds <- c("admin12", "admin22")
    official_admin_boundaries <- c("admin1", "admin2", "admin3", "admin4")
    official_admin_boundaries <- official_admin_boundaries[seq_len(length(local_admin_bounds()))]
    
    # Process data_in() and store in a new reactive expression
    data_index <- reactive({
      data_index <- data_in()
      if (any(str_detect(names(data_index), "/"))) {
        names(data_index) <- str_replace_all(names(data_index), "/", ".")
      }
      
      data_index %>%
        rename_with(.fn = ~official_admin_boundaries, .cols = all_of(local_admin_bounds())) %>% 
        reshape_long(admin_bounds= official_admin_boundaries)
      
    })

    tot_indicators <- severity_dap() %>%
        mutate(question = str_remove(question, "\\..*")) %>%
        pull(question) %>% unique()
    len_all_indicators <- length(tot_indicators)
    
    data_index_clean <- data_index() %>% 
      inner_join(severity_dap(), by = c("question", "choice")) %>% 
      max_aggregate_sm(severity_dictionary = severity_dap(), admin_bounds = official_admin_boundaries) %>% 
      ungroup() %>% 
      mutate(total_number_core_indicators = len_all_indicators)
    
    print('initial data_index_clean created')
    
    if ("flag3" %in% selected_methods) {
      data_index_clean <- data_index_clean %>%
        add_flag3() %>%
        add_flag3_per_sector(official_admin_boundaries) %>% 
        add_flag3_per_settlement(official_admin_boundaries) %>% 
        add_mean_flag3_area(official_admin_boundaries)
      print('added flag3 index')
    }
    if ("flag4" %in% selected_methods) {
      data_index_clean <- data_index_clean %>%
        add_flag4() %>%
        add_flag4_per_sector(official_admin_boundaries) %>% 
        add_flag4_per_settlement(official_admin_boundaries) %>% 
        add_mean_flag4_area(official_admin_boundaries)
      print('added flag4 index')
    }
    if ("flag4+" %in% selected_methods) {
      data_index_clean <- data_index_clean %>%
        add_flag4plus() %>%
        add_flag4_plus_per_sector(official_admin_boundaries) %>% 
        add_flag4_plus_per_settlement(official_admin_boundaries) %>% 
        add_mean_flag4_plus_area(official_admin_boundaries)
      print('added flag4+ index')
    }
    if ("proportion3" %in% selected_methods) {
      data_index_clean <- data_index_clean %>%
        add_proportion3_per_sector(official_admin_boundaries) %>% 
        add_proportion3_per_settlement(official_admin_boundaries, len_all_indicators = len_all_indicators) %>% 
        add_mean_proportion3_area(official_admin_boundaries)
      print('added proportion3 index')
    }
    if ("proportion4" %in% selected_methods) {
      data_index_clean <- data_index_clean %>%
        add_proportion4_per_sector(official_admin_boundaries) %>% 
        add_proportion4_per_settlement(official_admin_boundaries, len_all_indicators = len_all_indicators) %>% 
        add_mean_proportion4_area(official_admin_boundaries)
      print('added proportion4 index')
    }
    if ("proportion4+" %in% selected_methods) {
      data_index_clean <- data_index_clean %>%
        add_proportion4_plus_per_sector(official_admin_boundaries) %>% 
        add_proportion4_plus_per_settlement(official_admin_boundaries, len_all_indicators = len_all_indicators) %>% 
        add_mean_proportion4_plus_area(official_admin_boundaries)
      print('added proportion4+ index')
    }
    if ("score" %in% selected_methods) {
      data_index_clean <- data_index_clean %>%
        add_sector_proportion_score(official_admin_boundaries) %>% 
        add_settlement_proportion_score(official_admin_boundaries) %>% 
        add_area_score_index_proportion_25(official_admin_boundaries)
      print('added Score Index')
    }
    
    raw <- data_index_clean
    indicator <- data_index_clean %>% select(-contains('area'), -contains('sector'), -contains('settlement'))
    sector <- data_index_clean %>% select(official_admin_boundaries,contains('sector')) %>% unique()
    settlement <- data_index_clean %>% select(official_admin_boundaries, 
                                              contains('settlement'), -contains('area'), -contains('sector')) %>% unique()
    area <- data_index_clean %>% select(official_admin_boundaries[c(-3,-4)], contains('area')) %>% unique()
    
    list_out <- list("Indicator level severity" = indicator, 'sector level' = sector, 'settlement level' = settlement, "area level" = area, "Raw data" = raw)
    
    
    data_index_out(data_index_clean)
    
    
    output$run_message <- renderText({
      "Index calculation successfully completed!"
    })
    
    
    # Write Excel file with all sheets
    # write_xlsx(list_out, paste0(output_path, country, "/AoK_SI_clean_", country, "_", date_round, ".xlsx"))
    
    # Download handler for Excel
    output$download_index_data <- downloadHandler(
      filename = function() { "severity_index_data.xlsx" },
      content = function(file) {
        req(data_index_out())
        write.xlsx(list_out, file)
      }
    )
  })

  
  

  # Read and process severity_dap
  #   severity_dap <- openxlsx::read.xlsx(input$dap_file$datapath, sheet = 1, 
  #                                     na.strings = c("NA", "#N/A", "", " ", "N/A")) %>%
  #     select(question, type = q.type, choice, severity_value, sector) %>%
  #     mutate(severity_value = as.numeric(severity_value))
  # 
  #   tot_indicators <- severity_dap %>%
  #     mutate(question = str_remove(question, "\\..*")) %>%
  #     pull(question) %>% unique()
  # 
  #   len_all_indicators <- length(tot_indicators)
  # 
  # # Print for debugging (optional)
  #   print(paste("Total Indicators:", len_all_indicators))
  # })


  #     }
  # 
  # 
  # 
  #     # Example aggregation process (modify as needed)
  #     vedelete <- c("dk", "DK", "dnk","Not_to_sure", "not_sure",  "Not_sure", "pnta", "prefer_not_to_answer", "Prefer_not_to_answer")
  # 
  #     data_cleaned <- process_data_for_aggregation(data_in(), replace_vec_na = vedelete)
  #     print('processed data for aggregation')
  # 
  #     req(data_cleaned, agg_vars)  # Ensure these are available
  # 
  #     tryCatch({
  #       aok_aggregated <- aggregate_data(data_cleaned, agg_vars,
  #                                        col_so = col.so, col_sm = col.sm,
  #                                        col_int = col.int, col_text = col.text)
  # 
  #       data_in(aok_aggregated)
  #       df_aggregated_react <<- reactive({ aok_aggregated })
  # 
  #     }, error = function(e) {
  #       showNotification(paste("Error aggregating data:", e$message), type = "error")
  #     })
  #     print('aggregated data successfully!')
  #     # Store aggregated data (add any additional processing)
  # 
  # 
  #     output$aggregation_status <- renderText("Aggregation completed successfully!")
  #   } else {
  #     output$aggregation_status <- renderText("Data left at KI level. No aggregation performed.")
  #   }
  # 
  #   updateSelectInput(session, "agg_vars", selected = input$agg_vars)
  # 
  # })
  
  
  #####
  
  
  
  
  #### PLOTTING
  
  
  # # Reset Analysis functionality
  # observeEvent(input$reset_analysis, {
  #   updateSelectInput(session, "disaggregate_by_1", selected = NULL)
  #   updateSelectInput(session, "disaggregate_by_2", selected = NULL)
  #   output$analysis_status <- renderText("")  # Clear status
  #   output$progress_bar <- renderUI(NULL)  # Clear progress bar
  # })
  # 
  # UI element for switching data source
  # output$data_source_selector <- renderUI({
  #   selectInput("selected_data_source", 
  #               "Choose Data Source:", 
  #               choices = c("Clean/Aggregated Data" = "data_in", "Analyzed Data" = "analysis_data_out"))
  # })
  # 
  # Reactive dataset that switches based on user input
  # active_data <- reactive({
  #   if (input$selected_data_source == "data_in") {
  #     data_in()
  #   } else {
  #     analysis_data_out()
  #   }
  # })
  
  
  # Populate column choices for x_axis and y_axis based on uploaded data
  observeEvent(data_in(), {
    choices <- names(data_in())
    updateSelectInput(session, "x_axis", choices = choices)
    updateSelectInput(session, "y_axis", choices = choices)
  })
  
  
  
  # Display raw or processed data in table based on view type
  # output$table_output <- renderDataTable({
  #   req(input$view_type)
    
    # Determine which dataset to display based on view type
    # table_data <- switch(input$view_type,
    #                      "raw" = data_in(),
    #                      "analysis" = df_res_labelled_reactive(),
    #                      "aggregated" = df_aggregated_react())
    
    # Render DataTable with enhanced features
  #   datatable(table_data,
  #             options = list(
  #               scrollX = TRUE,                   # Enable horizontal scrolling
  #               scrollY = "500px",                # Set a fixed height for vertical scrolling
  #               paging = TRUE,                    # Enable pagination
  #               pageLength = 100, # -1 for inifinite                  # Display all rows (infinite)
  #               searching = TRUE,                 # Enable search bar
  #               dom = 'Bfrtip',                  # Include buttons and filter
  #               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Add export options
  #               initComplete = JS("function(settings, json) {",
  #                                 "$('.dataTables_filter').css({'float': 'none', 'text-align': 'right'});",  # Align search bar
  #                                 "$('.dataTables_length').css({'float': 'none', 'text-align': 'right'});",  # Align length selection
  #                                 "$('.dataTables_info').css({'float': 'none', 'text-align': 'right'});",   # Align info
  #                                 "}")
  #             ),
  #             filter = 'top'                     # Place filters above each column header
  #   ) %>%
  #     formatStyle(  # Make the table visually appealing
  #       columns = names(table_data),  # Apply to all columns
  #       backgroundColor = styleEqual("highlight", "yellow")  # Example style
  #     )
  # })
  
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
    p <- ggplot(data = data_in(), aes(x = !!x_var, y = !!y_var))
    
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
  
  output$download_aggregated_data <- downloadHandler(
    filename = function() {
      paste("aggregated_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      sheet_name <- "Aggregated Data"
      data_to_write <- df_aggregated_react()  # Reactive expression for aggregated data
      
      # Write data to the worksheet
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, data_to_write)
      
      # Save workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # Server-side logic for downloading analysis data
  output$download_analysis_data <- downloadHandler(
    filename = function() {
      paste("analysis_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      sheet_name <- "Analysis Data"
      data_to_write <- df_res_labelled_reactive()  # Reactive expression for analysis data
      
      # Write data to the worksheet
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, data_to_write)
      
      # Save workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # Server-side logic for downloading analysis data
  output$download_analysis_data <- downloadHandler(
    filename = function() {
      paste("analysis_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      sheet_name <- "Analysis Data"
      data_to_write <- df_res_labelled_reactive()  # Reactive expression for analysis data
      
      # Write data to the worksheet
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, data_to_write)
      
      # Save workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  
} # server end

shinyApp(ui, server)
