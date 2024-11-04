Mode <- function(x) {
  # Filter out both NA and "NA" values
  x <- x[!(is.na(x) | x == "NA")]
  
  # Check if all remaining values are non-existent or empty after removing NA and "NA"
  if (length(x) == 0) {
    return("")
  }
  
  ux <- unique(x)
  tabulated <- tabulate(match(x, ux))
  max_count <- max(tabulated)
  
  tied_indices <- which(tabulated == max_count)
  tied_values <- ux[tied_indices]
  
  # Rule 0: Handle if it is not a tie and there are multiple modes
  if (length(tied_indices) == 1){
    return(tied_values)
  }
  
  # Rule 1: Handle ties
  if (length(tied_indices) > 1) {
    # If 'do_not_know' is one of the modes, give priority to other modes
    if ("do_not_know" %in% tied_values || any(is.na(tied_values))) {
      valid_values <- tied_values[!(tied_values %in% c("do_not_know", NA))]
      return(valid_values[1])  # Return the first valid non-'do_not_know', non-NA value
    }
    valid_values <- tied_values
    return(valid_values[1])  # Return the first valid non-'do_not_know', non-NA value
  }
  # Return 'NC' for no consensus if none of the above conditions are met
  return("NC")
}