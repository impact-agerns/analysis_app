read_data_file <- function(file_input) {
  req(file_input)
  temp_file_path <- tempfile(fileext = ".xlsx")
  file.copy(file_input$datapath, temp_file_path, overwrite = TRUE)
  
  tryCatch({
    data <- openxlsx::read.xlsx(temp_file_path, sheet = 1, na.strings = c("NA", "#N/A", "", " ", "N/A"))
    print("Data successfully read.")
    return(data)
  }, error = function(e) {
    print(paste("Error loading data:", e$message))
    showNotification(paste("Error loading data:", e$message), type = "error")
    return(NULL)
  })
}
