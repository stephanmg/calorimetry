#############################################################################
#' add_example_data sets
#' 
#' Adds two loadable example data sets 
#############################################################################
add_example_data_sets <- function(input, session, output, global_data) {
	#############################################################################
	# Load example data UCP1KO
	#############################################################################
	observeEvent(input$example_data_single, {
		storeSession(session$token, "use_example_data", TRUE, global_data)
		storeSession(session$token, "example_data_single", TRUE, global_data)
		storeSession(session$token, "example_data_single_name", "UCP1KO", global_data)
		storeSession(session$token, "example_data_single_length", 4, global_data)
		output$nFiles <- renderUI(numericInput("nFiles", "Number of data files", value = 4, min = 4, step = 4))

		output$fileInputs <- renderUI({
		html_ui <- " "
		for (i in seq_along(1:4)) {
			html_ui <- paste0(html_ui, fileInput(paste0("File", i),
			label = paste0("Cohort #", i), placeholder = paste0("example data set ", i, ".csv")))
			}
		HTML(html_ui)
		})

		updateCheckboxInput(session, "havemetadata", value = TRUE)
		output$metadatafile <- renderUI({
			html_ui <- " "
			html_ui <- paste0(html_ui,
			fileInput("metadatafile",
			label = "Metadata file",
			placeholder = "example metadata.xlsx"))
			HTML(html_ui)
		})
	})

	#############################################################################
	# Load example data DAKO
	#############################################################################
	observeEvent(input$example_data_single_alternative, {
		storeSession(session$token, "use_example_data", TRUE, global_data)
		storeSession(session$token, "example_data_single", TRUE, global_data)
		storeSession(session$token, "example_data_single_name", "DAKO", global_data)
		storeSession(session$token, "example_data_single_length", 2, global_data)
		output$nFiles <- renderUI(numericInput("nFiles", "Number of data files", value = 2, min = 2, step = 2))

		output$fileInputs <- renderUI({
		html_ui <- " "
		for (i in seq_along(1:2)) {
			html_ui <- paste0(html_ui, fileInput(paste0("File", i),
			label = paste0("Cohort #", i), placeholder = paste0("example data set ", i, ".csv")))
			}
		HTML(html_ui)
		})

		updateCheckboxInput(session, "havemetadata", value = TRUE)
		output$metadatafile <- renderUI({
			html_ui <- " "
			html_ui <- paste0(html_ui,
			fileInput("metadatafile",
			label = "Metadata file",
			placeholder = "example metadata.xlsx"))
			HTML(html_ui)
		})
	})
}