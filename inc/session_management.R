################################################################################
#' store value in session
#' 
#' This function stores the content of a variable into a global dict with given name
#' @param session_id
#' @param variable_name
#' @param variable_value
#' @param global_data
################################################################################
storeSession <- function(session_id, variable_name, variable_value, global_data) {
	global_data[[session_id]][[variable_name]] = variable_value
}
################################################################################
#' get value stored in session
#' 
#' This function retrieves a variable's content by name from a global dict
#' @param session_id
#' @param global_data
################################################################################
getSession <- function(session_id, global_data) {
	return(global_data[[session_id]])
}