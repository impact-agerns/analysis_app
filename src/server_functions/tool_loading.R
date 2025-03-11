load_kobo_tool <- function(filepath) {
  req(filepath)
  
  tryCatch({
    survey <- read_xlsx(filepath, guess_max = 100, na = c("NA", "#N/A", "", " ", "N/A"), sheet = 'survey')
    choices <- read_xlsx(filepath, guess_max = 100, na = c("NA", "#N/A", "", " ", "N/A"), sheet = 'choices')
    
    tool.combined <- combine_tool(survey = survey, responses = choices)
    print("Loaded Kobo tool successfully!")
    return(tool.combined)
  }, error = function(e) {
    print(paste("Error loading Kobo tool:", e$message))
    showNotification(paste("Error loading Kobo tool:", e$message), type = "error")
    return(NULL)
  })
}
