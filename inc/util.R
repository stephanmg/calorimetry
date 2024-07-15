source("constants.R")

################################################################################
# get factor columns
################################################################################
get_factor_columns <- function(df) {
	return(names(df)[sapply(df, is.factor)])
}

################################################################################
# get non-factor columns
################################################################################
get_non_factor_columns <- function(df) {
	return(names(df)[sapply(df, Negate(is.factor))])
}

################################################################################
# get non-factor columns
################################################################################
get_new_download_buttons <- function() {
   new_buttons <- list(
      list(
      name = "Download plot as a SVG",
      icon = list(path = svg_path_svg, width=30, height=30),
      click = htmlwidgets::JS("
         function(gd) {
            Plotly.downloadImage(gd, {format: 'svg', filename: 'plot'});
         }
      ")
      ),
      list(
      name = "Download plot as a PDF",
      icon = list(path = svg_path_pdf, width=30, height=30,  transform="matrix(0.59119871,0,0,0.48085484,21.328186,-59.600539)"),
      click = htmlwidgets::JS("
         function(gd) {
            Plotly.downloadImage(gd, {format: 'pdf', filename: 'plot'});
         }
      ")
      )
   )
   return(new_buttons)
}
