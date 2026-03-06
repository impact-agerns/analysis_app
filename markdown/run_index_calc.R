observeEvent(input$run_index, {
  req(data_in(), severity_dap(), local_admin_bounds(), admin_bounds_area_index())

  selected_methods <- input$selected_index_method
  cat('selected method: ', selected_methods, '\n')
  # local_admin_bounds <- c("admin12", "admin22")
  official_admin_boundaries <- c("admin1", "admin2", "admin3", "admin4")
  official_admin_boundaries <- official_admin_boundaries[seq_len(length(local_admin_bounds()))]

  tot_indicators <- severity_dap() %>%
    mutate(question = str_remove(question, "\\..*")) %>%
    select(question, sector) %>% unique()

  num_sec_indicators <- tot_indicators %>% group_by(sector) %>% summarise(total_ind_per_sector = n())
  len_all_indicators <- length(tot_indicators %>% select(question) %>% unique() %>% pull())



  # Process data_in() and store in a new reactive expression
  data_index <- reactive({
    data_index <- data_in()
    if (any(str_detect(names(data_index), "/"))) {
      names(data_index) <- str_replace_all(names(data_index), "/", ".")
    }

    data_index %>%
      rename_with(.fn = ~official_admin_boundaries, .cols = all_of(local_admin_bounds())) %>%
      reshape_long(admin_bounds= official_admin_boundaries) %>%
      filter(
        (str_detect(question, "\\.") & choice == "1") |
          !str_detect(question, "\\.")
      ) %>%
      mutate(
        question_full = question,
        question = if_else(str_detect(question_full, "\\."), str_extract(question_full, "^[^.]+"), question),
        choice   = if_else(str_detect(question_full, "\\."), str_extract(question_full, "(?<=\\.)[^.]+"), choice)
      ) %>%
      select(-question_full) %>%
      inner_join(severity_dap(), by = c("question", "choice")) %>%
      max_aggregate_sm(severity_dictionary = severity_dap(), admin_bounds = official_admin_boundaries) %>%
      ungroup() %>%
      mutate(total_number_core_indicators = len_all_indicators)

  })
  data_index_clean <- data_index()


  # library(readxl)
  # list.files('input/data', full.names = T)
  local=F
  if (local==T) {
    fsl_vars <-c("afrontamiento_mdv", "cantidad_personas_alimentos")
    si_dap <- read_excel(list.files('input/countries/ven/', full.names = T,pattern="DAP"))
    dat <- read_excel(list.files('input/countries/ven/', full.names = T,pattern="data"))
    survey <- read_excel(list.files('input/countries/ven/', full.names = T,pattern="kobo"), sheet="survey")
    choices <- read_excel(list.files('input/countries/ven/', full.names = T,pattern="kobo"), sheet="choices")
    tool.combined <- combine_tool_global_label(survey = survey, responses = choices, label_col='label::Spanish (es)')

    not_joined <- si_dap %>%
      anti_join(tool.combined, by = c("question"="name","choice"= "name.choice"))

    if (any(str_detect(names(dat), "/"))) {
      names(dat) <- str_replace_all(names(dat), "/", ".")
    }

    official_admin_boundaries <-c("admin1", "admin2", "admin3")
    local_admin_bounds <- c("admin1_label", "admin2_label", "admin3_label")
    data_index <- dat %>%
      rename_with(.fn = ~official_admin_boundaries, .cols = all_of(local_admin_bounds)) %>%
      reshape_long(admin_bounds= official_admin_boundaries) %>% arrange(admin1) %>%
      filter(
        (str_detect(question, "\\.") & choice == "1") |
          !str_detect(question, "\\.")
      ) %>%
      mutate(
        question_full = question,
        question = if_else(str_detect(question_full, "\\."), str_extract(question_full, "^[^.]+"), question),
        choice   = if_else(str_detect(question_full, "\\."), str_extract(question_full, "(?<=\\.)[^.]+"), choice)
      ) %>%
      select(-question_full)
    tot_indicators <- si_dap%>%
      mutate(question = str_remove(question, "\\..*")) %>%
      select(question, sector) %>% unique()
    num_sec_indicators <- tot_indicators %>% group_by(sector) %>% summarise(total_ind_per_sector = n())
    len_all_indicators <- length(tot_indicators %>% select(question) %>% unique() %>% pull())
    data_index_clean <- data_index %>%
      inner_join(si_dap, by = c("question", "choice")) %>%
      max_aggregate_sm(severity_dictionary = si_dap, admin_bounds = official_admin_boundaries) %>%
      ungroup() %>%
      mutate(total_number_core_indicators = len_all_indicators)
  }
  # tot_indicators <- dap %>%




  cat('initial data_index_clean created.\n')

  cat('admin bounds for area index:',admin_bounds_area_index(), '\n')

  evaluated_admin_bounds <- admin_bounds_area_index()

  cat('admin bounds for area index:',evaluated_admin_bounds, '\n')
  print('Evaluated')

  if ("flag3" %in% selected_methods) {
    data_index_clean <- data_index_clean %>%
      add_flag3() %>%
      add_flag3_per_sector(evaluated_admin_bounds) %>%
      add_flag3_per_settlement(official_admin_boundaries) %>%
      add_mean_flag3_area(admin_bounds = evaluated_admin_bounds)
    print('added flag3 index')
  }
  if ("flag4" %in% selected_methods) {
    data_index_clean <- data_index_clean %>%
      add_flag4() %>%
      add_flag4_per_sector(evaluated_admin_bounds) %>%
      add_flag4_per_settlement(official_admin_boundaries) %>%
      add_mean_flag4_area(evaluated_admin_bounds)
    print('added flag4 index')
  }
  if ("flag4+" %in% selected_methods) {
    data_index_clean <- data_index_clean %>%
      add_flag4_plus() %>%
      add_flag4_plus_per_sector(evaluated_admin_bounds) %>%
      add_flag4_plus_per_settlement(official_admin_boundaries) %>%
      add_mean_flag4_plus_area(evaluated_admin_bounds)
    print('added flag4+ index')
  }
  if ("proportion3" %in% selected_methods) {
    data_index_clean <- data_index_clean %>%
      add_proportion3_per_sector(official_admin_boundaries, evaluated_admin_bounds, num_sec_indicators) %>%
      add_proportion3_per_settlement(official_admin_boundaries, len_all_indicators = len_all_indicators) %>%
      add_mean_proportion3_area(evaluated_admin_bounds, len_all_indicators)
    print('added proportion3 index')
  }
  if ("proportion4" %in% selected_methods) {
    data_index_clean <- data_index_clean %>%
      add_proportion4_per_sector(official_admin_boundaries, evaluated_admin_bounds, num_sec_indicators) %>%
      add_proportion4_per_settlement(official_admin_boundaries, len_all_indicators = len_all_indicators) %>%
      add_mean_proportion4_area(evaluated_admin_bounds, len_all_indicators)
    print('added proportion4 index')
  }
  if ("proportion4+" %in% selected_methods) {
    data_index_clean <- data_index_clean %>%
      add_proportion4_plus_per_sector(official_admin_boundaries, evaluated_admin_bounds, num_sec_indicators) %>%
      add_proportion4_plus_per_settlement(official_admin_boundaries, len_all_indicators = len_all_indicators) %>%
      add_mean_proportion4_plus_area(evaluated_admin_bounds,len_all_indicators)
    print('added proportion4+ index')
  }
  if ("score" %in% selected_methods) {
    data_index_clean <- data_index_clean %>%
      add_sector_proportion_score(evaluated_admin_bounds) %>%
      add_settlement_proportion_score(official_admin_boundaries) %>%
      add_area_score_index_proportion_25(evaluated_admin_bounds)
    print('added Score Index')
  }
  # data_index_clean <- reduce(outputs, left_join, by = names(data_index_clean))



  raw <- data_index_clean
  indicator <- data_index_clean %>% select(-contains('area'), -contains('_sector'), -contains('settlement'))
  sector <- data_index_clean %>% select(evaluated_admin_bounds,contains('sector')) %>% unique()
  settlement <- data_index_clean %>% select(official_admin_boundaries,
                                            contains('settlement'), -contains('area'), -contains('_sector')) %>% unique()
  area <- data_index_clean %>% select(evaluated_admin_bounds, contains('area')) %>% unique()

  list_out <- list("Indicator level severity" = indicator, 'sector level' = sector,
                   'settlement level' = settlement,
                   'area level' = area,
                   "Raw data" = raw)


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
