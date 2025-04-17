################################################################################
#' check_column_consistency
#' 
#' The function checks if we have a valid csv file, i.e. same # columns for rows
#' 
#' @param file_path path to file
#' @param sep column separator
#' @examples 
#' check_column_consistency(file, ";")
#' @export
################################################################################
check_column_consistency <- function(file_path, sep) {
  # Read the file line by line
  lines <- readLines(file(file_path, encoding = "ISO-8859-1"), warn=FALSE)
  
  # Count semicolons in each line
  semicolon_counts <- sapply(lines, function(line) {
    nchar(gsub(paste0("[^", sep, ";]"), "", line))  
  })
  
  # Check if all counts are the same
  all(semicolon_counts == semicolon_counts[1])
}

