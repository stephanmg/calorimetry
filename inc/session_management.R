################################################################################
# store value in session
################################################################################
storeSession <- function(session_id, variable_name, variable_value, global_data) {
	global_data[[session_id]][[variable_name]] = variable_value
}
################################################################################
# get value stored in session
################################################################################
getSession <- function(session_id, global_data) {
	return(global_data[[session_id]])
}