# Load required libraries
library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)
library(janitor)
library(tidyverse)
library(purrr)

ui <- fluidPage(
	titlePanel("Data Processing and Analysis App"),
	
	sidebarLayout(
		sidebarPanel(
			fileInput("data_file", "Upload Data File (xlsx)", accept = ".xlsx"),
			fileInput("kobo_file", "Upload Kobo Tool File (xlsx)", accept = ".xlsx"),
			selectInput("disaggregate_by", "Choose Disaggregation Level", choices = NULL, multiple = TRUE),
			actionButton("run_analysis", "Run Analysis"),
			verbatimTextOutput("analysis_status"),  # For displaying analysis completion message
			uiOutput("progress_bar"),  # For progress bar			
			selectInput("view_type", "Choose View Type", choices = c("Raw Data" = "raw", "Analysis Data" = "analysis")),
			
			# Analysis Filters
			selectInput("x_axis", "X-Axis", choices = NULL),
			selectInput("y_axis", "Y-Axis", choices = NULL),
			selectInput("measure", "Measure", choices = c("mean", "count", "median")),
			selectInput("plot_type", "Plot Type", choices = c("Bar Chart" = "bar", "Scatterplot" = "scatter", "Violin Plot" = "violin")),
			
			downloadButton("download_data", "Download Processed Data")
		),
		
		mainPanel(
			tabsetPanel(
				tabPanel("Table", dataTableOutput("table_output")),
				tabPanel("Analysis", plotOutput("plot_output"))
			)
		)
	)
)


# Define the Server
server <- function(input, output, session) {

	
	# Reactive variable to store the loaded data
	data_in <- reactiveVal(NULL)
	
	# Observe the data file input
	observeEvent(input$data_file, {
		req(input$data_file)
		
		# Try to read the data and handle potential errors
		tryCatch({
			data <- read_excel(input$data_file$datapath)
			
			# Store the data in the reactive variable
			data_in(data)
			
			# Update choices for disaggregation level based on the data columns
			updateSelectInput(session, "disaggregate_by", 
												choices = names(data),
												selected = NULL)
		}, error = function(e) {
			showNotification(paste("Error loading data:", e$message), type = "error")
		})
	})
	
	
	kobo_tool <- reactive({
		req(input$kobo_file)
		read_xlsx(input$kobo_file$datapath)
	})
	
	# Populate disaggregation choices based on uploaded data columns
	output$disaggregate_ui <- renderUI({
		req(data_in())
		choices <- names(data_in())
		selectInput("disaggregate_by", "Choose Disaggregation Level", choices = choices, multiple = TRUE)
	})
	
	# After loading the data
	# observeEvent(input$data_file, {
	# 	req(input$data_file)
	# 	data <- readxl::read_excel(input$data_file$datapath)
	# 	
	# 	# Store the data in a reactive variable
	# 	data_in(data)
	# 	
	# 	# Update choices for disaggregation level based on the data columns
	# 	updateSelectInput(session, "disaggregate_by", 
	# 										choices = names(data),
	# 										selected = NULL)  # or specify any default selected options
	# })
	
	# This code snippet integrates progress updates during the processing of data.
	observeEvent(input$run_analysis, {
		req(data_in())  # Ensure data is available
		
		# Use withProgress to show a progress bar
		withProgress(message = "Running analysis...", {
			# Steps for reading survey and choices from Kobo tool
			survey <- read_xlsx(input$kobo_file$datapath, guess_max = 50000, na = c("NA","#N/A",""," ","N/A"), sheet = 'survey')
			choices <- read_xlsx(input$kobo_file$datapath, guess_max = 50000, na = c("NA","#N/A",""," ","N/A"), sheet = 'choices')
			
			# Combine the survey and choices
			tool.combined <- combine_tool(survey = survey, responses = choices)
			
			# Process column names for aggregation
			col.sm <- tool.combined %>% filter(q.type == "select_multiple") %>% pull(name) %>% unique()
			col.so <- tool.combined %>% filter(q.type == "select_one") %>% pull(name) %>% unique()
			col.int <- tool %>% filter(type == "integer") %>% pull(name) %>% unique()
			
			# Check if select_one questions are found
			if (length(col.so) <= 1) {
				stop("Check if label column is specified correctly in combine_tool function: No select_one questions found in the tool")
			} else {
				print("Select one questions found in the tool")
			}
			
			# Define disaggregation levels
			dis <- list("admin2", c("admin2", "admin3"))
			
			# Clean expanded data
			clean_expanded <- data_in() %>%
				expand.select.one.vec(col.so[col.so %in% names(.)])
			
			# Initialize the results list for aggregation
			res <- list()
			
			# Efficient aggregation
			for (d in dis) {
				d <- d %>% unlist
				df <- clean_expanded
				if (sum(d %in% names(df)) > 0) {
					df <- df %>% group_by(across(any_of(d)))
				}
				
				df <- df %>%
					mutate(across(matches(paste0("^", c(col.sm, col.so), "\\.")), as.numeric), 
								 across(any_of(col.int), as.numeric)) %>%
					summarise(across(matches(paste0("^", c(col.sm, col.so), "\\.")), 
													 list(mean = ~mean(., na.rm = TRUE), 
													 		 count = ~sum(., na.rm = TRUE), 
													 		 resp = ~sum(!is.na(.)), 
													 		 n = ~n()), 
													 .names = "{.col}--{.fn}"),
										across(any_of(col.int), 
													 list(mean = ~mean(., na.rm = TRUE), 
													 		 sum = ~sum(., na.rm = TRUE), 
													 		 median = ~median(., na.rm = TRUE), 
													 		 resp = ~sum(!is.na(.)), 
													 		 n = ~n()), 
													 .names = "{.col}--{.fn}")) %>%
					ungroup() %>%
					pivot_longer(-any_of(d)) %>% 
					separate(name, c("col", "function"), "--", remove = FALSE) %>% 
					separate(col, c("question", "choice"), "\\.", remove = FALSE) %>%  
					select(-name) %>%
					pivot_wider(names_from = "function", values_from = "value")
				
				# Rename and handle disaggregation variables
				if (sum(str_detect(d, "^all$")) == 0) {
					d <- d %>% setNames(paste0("disag_var_", 1:length(d)))
					df <- df %>% cbind(setNames(as.list(d), names(d))) %>% 
						rename_with(~paste0("disag_val_", 1:length(d)), any_of(unname(d)))
				} 
				
				res[[paste(d, collapse = "_")]] <- df %>%
					mutate(mean = ifelse(is.nan(mean), NA, mean))
				
				print(paste0("Analysis disaggregated by ", d, " done"))
			}
			
			# Combine results
			df_res <- res %>% bind_rows() %>%
				select(any_of(c("disag_var_1", "disag_val_1", "disag_var_2", "disag_val_2")), everything())
			
			# Output the results in the main panel
			# output$table_output <- renderDataTable({ df_res })
			
			# Success message
			output$analysis_status <- renderText("Analysis completed successfully!")
		})
	})
	
	
	
	# Populate column choices for x_axis and y_axis based on uploaded data
	observeEvent(data_in(), {
		choices <- names(data_in())
		updateSelectInput(session, "x_axis", choices = choices)
		updateSelectInput(session, "y_axis", choices = choices)
	})
	
	
	
	
	# df_res <- eventReactive(input$run_analysis, {  # Use eventReactive for the analysis
	# 	req(data_in(), kobo_tool(), input$disaggregate_by)
	# 	
	# 	# Ensure disaggregation level is selected
	# 	if (length(input$disaggregate_by) == 0) return(NULL)
	# 	
	# 	clean_expanded <- data_in()
	# 	tool.combined <- kobo_tool()
	# 	res <- list()
	# 	dis <- input$disaggregate_by
	# 	
	# 	# Efficient aggregation
	# 	for (d in dis) {
	# 		d <- d %>% unlist
	# 		df <- clean_expanded
	# 		if (sum(d %in% names(df)) > 0) df <- df %>% group_by(across(any_of(d)))
	# 		
	# 		columns_to_pivot <- names(df) %>% str_detect("^col\\.sm|^col\\.so|col\\.int")
	# 		if (sum(columns_to_pivot) == 0) next
	# 		
	# 		df <- df %>% 
	# 			mutate(across(matches(paste0("^", c("col.sm", "col.so"), "\\.")), as.numeric), across(any_of("col.int"), as.numeric)) %>%
	# 			summarise(across(matches(paste0("^", c("col.sm", "col.so"), "\\.")), list(mean=~mean(., na.rm=T), count=~sum(., na.rm=T), resp=~sum(!is.na(.)), n=~n()), .names="{.col}--{.fn}"),
	# 								across(any_of("col.int"), list(mean=~mean(., na.rm=T), sum=~sum(., na.rm=T), median=~median(., na.rm=T), resp=~sum(!is.na(.)), n=~n()), .names="{.col}--{.fn}")) %>% 
	# 			ungroup %>%
	# 			pivot_longer(cols = names(df)[columns_to_pivot], names_to = "col", values_to = "value") %>%
	# 			separate(name, c("col", "function"), "--", remove=FALSE) %>% 
	# 			separate(col, c("question", "choice"), "\\.", remove=FALSE) %>% 
	# 			select(-name) %>%
	# 			pivot_wider(names_from="function", values_from="value")
	# 		
	# 		res[[paste(d, collapse = "_")]] <- df
	# 	}
	# 	
	# 	# Combine and label data efficiently
	# 	df_res <- res %>% bind_rows() %>% 
	# 		select(any_of(c("disag_var_1", "disag_val_1", "disag_var_2", "disag_val_2")), everything())
	# 	
	# 	df_res_labelled <- df_res %>% 
	# 		left_join(select(tool.combined, name, label, name.choice, label.choice, q.type), by=c("question"="name", "choice"="name.choice")) %>%
	# 		rename(aggregation_level1 = disag_var_1, aggregation_value1 = disag_val_1, 
	# 					 aggregation_level2 = disag_var_2, aggregation_value2 = disag_val_2) %>%
	# 		mutate(aggregation_level1 = ifelse(is.na(aggregation_level1), "all_settlements", aggregation_level1)) %>%
	# 		unique()
	# 	
	# 	df_res_labelled
	# })
	
	
	# Display raw or processed data in table based on view type
	output$table_output <- renderDataTable({
		req(input$view_type)
		if (input$view_type == "raw") {
			data_in()
		} else {
			df_res
		}
	})
	
	# Display summary statistics for selected variables
	output$stats_output <- renderTable({
		req(df_res)
		data_stats <- df_res %>% 
			summarise(across(where(is.numeric), list(mean = ~ mean(.x, na.rm = TRUE), 
																							 sd = ~ sd(.x, na.rm = TRUE),
																							 min = ~ min(.x, na.rm = TRUE),
																							 max = ~ max(.x, na.rm = TRUE))))
		data_stats
	})
	
	# Plot output based on user choices
	output$plot_output <- renderPlot({
		req(df_res, input$x_axis, input$y_axis)
		
		plot_data <- df_res
		
		x_col <- sym(input$x_axis)
		y_col <- sym(input$y_axis)
		
		ggplot(plot_data, aes(x = !!x_col, y = !!y_col)) +
			{
				if (input$plot_type == "bar") geom_bar(stat = "identity")
				else if (input$plot_type == "scatter") geom_point()
				else geom_violin()
			} +
			labs(x = input$x_axis, y = input$y_axis)
	})
	
	# Download processed data
	output$download_data <- downloadHandler(
		filename = function() {
			paste("df_res", Sys.Date(), ".xlsx", sep = "")
		},
		content = function(file) {
			wb <- createWorkbook()
			addWorksheet(wb, "Processed Data")
			writeData(wb, "Processed Data", df_res)
			saveWorkbook(wb, file, overwrite = TRUE)
		}
	)
}

# Run the application 
shinyApp(ui = ui, server = server)
