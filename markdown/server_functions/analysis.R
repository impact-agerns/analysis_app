run_analysis <- function(input, output, session, data_in, tool.combined) {
    req(data_in())  # Ensure data exists
    
    withProgress(message = "Running analysis...", {
      
      survey <- read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA", "#N/A", "", " ", "N/A"), sheet = 'survey')
      choices <- read_xlsx(input$kobo_file$datapath, guess_max = 100, na = c("NA", "#N/A", "", " ", "N/A"), sheet = 'choices')
      
      tool.combined <- combine_tool(survey = survey, responses = choices)
      
      col.so <- tool.combined %>% filter(q.type == "select_one") %>% pull(name) %>% unique()
      
      if (length(col.so) <= 1) {
        stop("Check if label column is specified correctly in combine_tool function: No select_one questions found in the tool")
      } else {
        print("Select one questions found in the tool")
      }
      print('issue before data_in()')
      
      observe({
        req(data_in())  
        if (ncol(data_in()) > 0) {
          updateSelectInput(session, "disaggregate_by_1", choices = names(data_in()))
          updateSelectInput(session, "disaggregate_by_2", choices = names(data_in()))
        } else {
          showNotification("No columns found in the dataset", type = "error")
        }
        print('issue after')
        
        print('here')
        
      })
      
      dis1 <- input$disaggregate_by_1
      dis2 <- input$disaggregate_by_2
      dis <- list(dis1, dis2) %>% discard(is.null)
      
      if (length(dis) == 0) {
        showNotification("Please select at least one variable for analysis", type = "error")
        return()
      }
      print('issue now?')
      
      observe({
        req(data_in())  
      clean_expanded <- data_in() %>%
        expand.select.one.vec(col.so[col.so %in% names(.)])
      print('clean_expanded run!')
      
      
      res <- list()
      
      for (d in dis) {
        d <- unlist(d)
        df <- clean_expanded
        print('where is it?')
        if (any(d %in% names(df))) {
          df <- df %>% group_by(across(any_of(d)))
        }
        
        df <- df %>%
          mutate(across(matches(paste0("^", col.so, "\\.")), as.numeric)) %>%
          summarise(across(matches(paste0("^", col.so, "\\.")), 
                           list(mean = ~mean(., na.rm = TRUE), 
                                count = ~sum(., na.rm = TRUE), 
                                resp = ~sum(!is.na(.)), 
                                n = ~n()), 
                           .names = "{.col}--{.fn}")) %>%
          ungroup() %>%
          pivot_longer(-any_of(d)) %>% 
          separate(name, c("col", "function"), "--", remove = FALSE) %>% 
          pivot_wider(names_from = "function", values_from = "value")
        
        res[[paste(d, collapse = "_")]] <- df %>%
          mutate(mean = ifelse(is.nan(mean), NA, mean))
        
        print(paste0("Analysis disaggregated by ", d, " done"))
      }
      
      df_res <- res %>% bind_rows()
      
      })
      
      df_res_labelled_reactive <<- reactive({ df_res })
      
      output$analysis_status <- renderText("Analysis completed successfully!")
    })
}
