run_aggregation <- function(input, data_in, tool, output, session) {
  req(data_in())
  
  if (input$aggregation_option == "aggregate") {
    agg_vars <- input$agg_vars
    if (length(agg_vars) == 0) {
      showNotification("Please select at least one variable for aggregation.", type = "error")
      return()
    }
    
    # Extract necessary columns from tool
    col.sm <- tool %>% filter(q.type == "select_multiple") %>% pull(name) %>% unique()
    col.so <- tool %>% filter(q.type == "select_one") %>% pull(name) %>% unique()
    col.int <- tool %>% filter(type == "integer") %>% pull(name) %>% unique()
    col.text <- tool %>% filter(type == "text") %>% pull(name) %>% unique()
    
    if (length(col.sm) <= 1) {
      print("Check if label column is specified correctly in combine_tool function")
      stop("No select_multiple questions found in the tool")
    } else {
      print("Select multiple questions found in the tool")
    }
    # Replace special characters in column names
    # names(data_in()) <- str_replace_all(names(data_in()), "/", ".")
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
    
    # Clean and aggregate data
    data_cleaned <- process_data_for_aggregation(data_in(), replace_vec_na = c("dk", "DK", "Not_sure", "prefer_not_to_answer"))
    print("Processed data for aggregation")
    
    tryCatch({
      aggregated_data <- aggregate_data(data_cleaned, agg_vars, col_so = col.so, col_sm = col.sm, col_int = col.int, col_text = col.text)
      data_in(aggregated_data)
      print("Aggregated data successfully!")
      
      output$aggregation_status <- renderText("Aggregation completed successfully!")
    }, error = function(e) {
      showNotification(paste("Error aggregating data:", e$message), type = "error")
    })
  } else {
    output$aggregation_status <- renderText("Data left at KI level. No aggregation performed.")
  }
  
  updateSelectInput(session, "agg_vars", selected = input$agg_vars)
  
  print('aggregation step ended')
}
