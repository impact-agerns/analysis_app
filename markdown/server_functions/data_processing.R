generate_aggregation_ui <- function(data, aggregation_option) {
  req(data)
  # print('aggregation step ended successfully')
  
  if (aggregation_option == "aggregate") {
    return(selectInput("agg_vars", 
                       label = tags$span(style = "color: var(--primary-color);", "Choose variable(s) for aggregation"),
                       choices = names(data), multiple = TRUE))
  }
}
