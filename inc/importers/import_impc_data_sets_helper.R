#############################################################################
#' load_impc_data_sets
#' 
#' This function loads data sets from the IMPC database
#' 
#' @param input shiny input
#' @param session shiny session
#' @param output shiny output
#' @param global_data global data
#' @export
#############################################################################
add_impc_data_sets <- function(input, session, output, global_data) {
	#############################################################################
	# Load example gene symbol
	#############################################################################
	observeEvent(input$load_impc, {
		storeSession(session$token, "use_impc_data", TRUE, global_data)
		storeSession(session$token, "IMPC_data_set_name", input$impc_gene_symbol, global_data)
		output$nFiles <- renderUI(numericInput("nFiles", "Number of data files", value = 3, min = 3, step = 3))

		gene_symbol <- input$impc_gene_symbol

		if (is.null(gene_symbol) || gene_symbol == "") {
         shinyalert("Error", paste("No gene symbol given to access IMPC database."), showCancelButton=TRUE)
		 return()
		}


		output$fileInputs <- renderUI({
		html_ui <- " "
		for (i in seq_along(1:3)) {
			html_ui <- paste0(html_ui, fileInput(paste0("File", i),
			label = paste0("Cohort #", i), placeholder = paste0("example data set ", i, ".csv")))
			}
		HTML(html_ui)
		})

		updateCheckboxInput(session, "havemetadata", value = FALSE)
		click("plotting")
	})
}

