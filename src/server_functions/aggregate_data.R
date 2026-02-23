aggregate_data <- function(data, agg_vars, col_so, col_text, col_sm, col_int) {

  # 1. select_one → Mode with 2/3 majority, else "NC"
  data_agg_so <- data %>%
    select(all_of(agg_vars), matches(col_so)) %>%
    group_by(across(all_of(agg_vars))) %>%
    mutate(across(any_of(col_so), as.character)) %>%
    summarise(across(any_of(col_so), Mode), .groups = "drop")

  # 2. text → Mode with 2/3 majority (do_not_know treated as NA)
  data_agg_text <- data %>%
    select(all_of(agg_vars), matches(col_text)) %>%
    group_by(across(all_of(agg_vars))) %>%
    mutate(across(matches(col_text), as.character)) %>%
    summarise_all(~Mode(replace(., . == "do_not_know", NA)), .groups = "drop")

  # 3a. select_multiple binary dummy columns → 2/3 majority
  data_agg_sm <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    select(all_of(agg_vars), matches(col_sm)) %>%
    select(all_of(agg_vars), contains('.')) %>%
    mutate(across(matches(col_sm), as.numeric)) %>%
    summarise(across(matches(col_sm),
                     ~ {
                       vals <- na.omit(.)
                       if (length(vals) == 0) return(NA_real_)
                       if (sum(vals == 1) / length(vals) >= 0.5) 1L else 0L
                     })) %>%
    ungroup()

  # 3b. Reconstruct parent SM columns from aggregated binaries
  sm_parent_cols <- data %>%
    select(matches(col_sm)) %>%
    select(-contains('.')) %>%
    names()

  data_agg_sm_parent <- data_agg_sm

  for (parent in sm_parent_cols) {
    binary_cols <- names(data_agg_sm_parent)[startsWith(names(data_agg_sm_parent), paste0(parent, "."))]

    if (length(binary_cols) == 0) next

    # Reconstruct parent string from binaries
    data_agg_sm_parent <- data_agg_sm_parent %>%
      rowwise() %>%
      mutate(!!parent := {
        vals <- c_across(all_of(binary_cols))
        suffixes <- sub(paste0("^", parent, "\\."), "", binary_cols)
        chosen <- suffixes[!is.na(vals) & vals == 1]
        if (all(is.na(vals))) NA_character_
        else if (length(chosen) == 0) "NC"
        else paste(chosen, collapse = " ")
      }) %>%
      ungroup()

    # Reorder: parent column just before its first binary dummy
    first_binary_pos <- which(names(data_agg_sm_parent) == binary_cols[1])
    col_order <- names(data_agg_sm_parent)
    col_order <- col_order[col_order != parent]
    col_order <- append(col_order, parent, after = first_binary_pos - 1)
    data_agg_sm_parent <- data_agg_sm_parent %>% select(all_of(col_order))
  }

  # 4. integer → mean
  data_agg_int <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    select(all_of(agg_vars), matches(col_int)) %>%
    summarise_all(~mean(as.numeric(na.omit(.)), na.rm = TRUE), .groups = "drop")

  # weight column
  data_agg_weight_column <- data %>%
    group_by(across(all_of(agg_vars))) %>%
    mutate(weight = 1) %>%
    summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop")

  # Combine all
  aok_aggregated <- data_agg_so %>%
    left_join(data_agg_weight_column, by = agg_vars) %>%
    left_join(data_agg_sm_parent,     by = agg_vars) %>%
    left_join(data_agg_int,           by = agg_vars) %>%
    left_join(data_agg_text,          by = agg_vars)

  return(aok_aggregated)
}
