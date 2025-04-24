process_data_for_aggregation <- function(data, replace_vec_na, pattern="\\.") {
  # Find target columns based on the pattern
  target_cols <- names(data)[grepl("\\.", names(data))]
  
  if (length(target_cols) == 0) {
    warning("No columns with the specified pattern found in data. No changes made.")
    return(data)
  }
  
  # Proceed with other transformations as needed
  data <- data %>%
    mutate_if(is.logical, as.numeric) %>%
    mutate_all(as.character) %>%
    mutate(across(all_of(target_cols), ~na_if(., "NA"))) %>%  # Convert "NA" strings to actual NA
    mutate(across(everything(), ~replace(., . %in% replace_vec_na, NA)))%>%
    mutate(across(everything(), ~replace(., . %in% c(NA, "NA"), NA_character_)))%>%
    mutate(across(all_of(target_cols), function(col) {
      if (all(is.na(col) | col %in% c("0", "1"))) {
        as.numeric(col)  # Convert to numeric if only '0', '1', or NA
      } else {
        col  # Return original if not
      }
    }))
  
  return(data)
}