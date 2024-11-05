aggregate_data <- function(data, agg_vars, col_so, col_text, col_sm, col_int) {
  # 1. Aggregation for select_one questions (Mode)
  data_agg_so <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    summarise(across(all_of(setdiff(col_so, agg_vars)), Mode), .groups = "drop")
  
  # 2. Aggregation for select text questions (Mode)
  data_agg_text <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    summarise(across(all_of(col_text), Mode), .groups = "drop")
  
  # 3. Aggregation for select_multiple questions (maximum)
  data_agg_sm <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    select(all_of(agg_vars), matches(col_sm))%>%
    select(all_of(agg_vars), contains('.'))%>%
    mutate(across(matches(col_sm), as.numeric)) %>% 
    summarise(across(matches(col_sm), ~if (all(is.na(.))) NA_real_ else max(., na.rm = TRUE)))  %>%
    ungroup()
  
  # # 4. Aggregation for select_multiple in single-column format (Mode with "do_not_know" as NA)
  data_agg_sm_first <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    select(all_of(agg_vars), matches(col_sm)) %>% 
    select(all_of(agg_vars), -contains('.')) %>%
    summarise_all( ~ Mode(replace(., . == "do_not_know", NA)), .groups = "drop")
  
  # 5. Aggregation for integer columns (mean)
  data_agg_int <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    summarise(across(all_of(col_int), ~ mean(as.numeric(na.omit(.)), na.rm = TRUE)), .groups = "drop")
  
  # Combine all aggregation results into a single data frame
  aok_aggregated <- data_agg_so %>%
    left_join(data_agg_sm, by = agg_vars) %>%
    left_join(data_agg_sm_first, by = agg_vars) %>%
    left_join(data_agg_int, by = agg_vars) %>% 
    left_join(data_agg_text, by = agg_vars) 
  
  return(aok_aggregated)
}
