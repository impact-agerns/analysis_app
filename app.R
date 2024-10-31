# Required Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(openxlsx)
library(janitor)
library(tidyverse)
library(purrr)

# UI
ui <- dashboardPage(
	dashboardHeader(title = "Data Processing and Analysis"),
	
	dashboardSidebar(
		sidebarMenu(
			menuItem("Home", tabName = "home", icon = icon("home")),
			menuItem("Table", tabName = "table", icon = icon("table")),
			# selectInput("disaggregate_by", "Choose Disaggregation Level", choices = NULL, multiple = TRUE),
			menuItem("Plot", tabName = "plot", icon = icon("chart-bar"))
		)
	),
	
	dashboardBody(
		tabItems(
			# Home Tab with File Inputs and Analysis Trigger
			tabItem(tabName = "home",
							fluidRow(
								box(fileInput("data_file", "Upload Data File (xlsx)", accept = ".xlsx")),
								box(fileInput("kobo_file", "Upload Kobo Tool File (xlsx)", accept = ".xlsx"))
							),
							verbatimTextOutput("analysis_status"),  # For displaying analysis completion message
							uiOutput("progress_bar"),
							box(selectInput("disaggregate_by", "Choose Disaggregation Level", choices = NULL, multiple = TRUE),
									actionButton("run_analysis", "Run Analysis"),
									verbatimTextOutput("analysis_status"),
									uiOutput("progress_bar"))),
			
			# Table Tab
			tabItem(tabName = "table",
							fluidRow(
								selectInput("view_type", "Data Type", choices = c("Raw Data" = "raw", "Analysis Data" = "analysis")),
								downloadButton("download_data", "Download Processed Data"),
								dataTableOutput("table_output"))),
			
			
			# Plot Tab with Controls
			tabItem(tabName = "plot",
							fluidRow(
								selectInput("plot_type", "Plot Type", choices = c("Bar Chart" = "bar", "Scatterplot" = "scatter", "Violin Plot" = "violin")),
								selectInput("x_axis", "X-Axis", choices = NULL),
								selectInput("y_axis", "Y-Axis", choices = NULL),
								selectInput("measure", "Measure", choices = c("mean", "count", "median")),
								plotOutput("plot_output"))
			)
		)
	)
)

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
			# dis <- list("admin2", c("admin2", "admin3"))
			dis <- input$disaggregate_by
			
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
			
			df_res_labelled <- df_res %>% 
				left_join(select(tool.combined, name, label, name.choice, label.choice, q.type), by=c("question"="name", "choice"="name.choice"))%>% 
				# rename(aggregation_level1 = disag_var_1, aggregation_value1 = disag_val_1, 
				# 			 aggregation_level2 = disag_var_2, aggregation_value2 = disag_val_2, 
				# ) %>% 
				# mutate(aggregation_level1 = ifelse(is.na(aggregation_level1), "all_settlements", aggregation_level1)) %>%
				unique()
			
			# Output the results in the main panel
			df_res_labelled_reactive <<- reactive({ df_res_labelled })
			
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
	
	
	
	
	# Display raw or processed data in table based on view type
	output$table_output <- renderDataTable({
		req(input$view_type)
		if (input$view_type == "raw") {
			data_in()
		} else {
			df_res_labelled_reactive()
		}
	})
	
	# Display summary statistics for selected variables
	output$stats_output <- renderTable({
		req(df_res_labelled_reactive())
		data_stats <- df_res_labelled_reactive() %>% 
			summarise(across(where(is.numeric), list(mean = ~ mean(.x, na.rm = TRUE), 
																							 sd = ~ sd(.x, na.rm = TRUE),
																							 min = ~ min(.x, na.rm = TRUE),
																							 max = ~ max(.x, na.rm = TRUE))))
		data_stats
	})
	
	# Plot output based on user choices
	output$plot_output <- renderPlot({
		req(input$x_axis, input$y_axis, input$plot_type)  # Ensure all inputs are selected
		
		# Dynamically select columns from data based on user input
		x_var <- sym(input$x_axis)
		y_var <- sym(input$y_axis)
		
		# Base ggplot object
		p <- ggplot(data = data.in, aes(x = !!x_var, y = !!y_var))
		
		# Conditional layer based on selected plot type
		p <- p + 
			switch(input$plot_type,
						 "bar" = geom_col(),                    # Bar chart
						 "scatter" = geom_point(),              # Scatterplot
						 "violin" = geom_violin()               # Violin plot
			) +
			labs(
				x = input$x_axis,
				y = input$y_axis,
				title = paste("Plot of", input$y_axis, "vs", input$x_axis)
			) +
			theme_minimal()  # Optional: use any theme you prefer
		
		# Render the plot
		p
	})
	
	
	
	output$download_data <- downloadHandler(
		filename = function() {
			paste(if (input$view_type == "raw") "raw_data_" else "analysis_data_", Sys.Date(), ".xlsx", sep = "")
		},
		content = function(file) {
			wb <- createWorkbook()
			addWorksheet(wb, if (input$view_type == "raw") "Raw Data" else "Analysis Data")
			
			if (input$view_type == "raw") {
				writeData(wb, "Raw Data", data_in())
			} else {
				writeData(wb, "Analysis Data", df_res_labelled_reactive())
			}
			
			saveWorkbook(wb, file, overwrite = TRUE)
		}
	)
	
}

# Run the application 
shinyApp(ui = ui, server = server)
