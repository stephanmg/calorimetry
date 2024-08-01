source("inc/constants.R")

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
      #list(
      #name = "Download plot as a PDF",
      #icon = list(path = svg_path_pdf, width=30, height=30, transform="matrix(0.59119871,0,0,0.48085484,21.328186,-59.600539)"),
      #click = htmlwidgets::JS("
      #   function(gd) {
      #      Plotly.downloadImage(gd, {format: 'pdf', filename: 'plot'});
      #  }
      #")
      #),
      list(
      name = "Download plot as a PDF",
      icon = list(path = svg_path_pdf, width=30, height=30, transform="matrix(0.59119871,0,0,0.48085484,21.328186,-59.600539)"),
      click = htmlwidgets::JS("
         function(gd) {
            var plotDiv = document.getElementById('plot');
            var width = plotDiv.offsetWidth;
            var height = plotDiv.offsetHeight;
            Plotly.toImage(gd, {format: 'png', width: width, height: height, scale: 4, compression: 'NONE'}).then(function(dataUrl) {
               const { jsPDF } = window.jspdf;
               const doc = new jsPDF('landscape');
               const ARimage = width/height;
               /// internal pagesize should be always around 297 (mm) since landscape a4 format
               var widthPDF = doc.internal.pageSize.getWidth(); // or: 280 and 
               var widthPDFoffset = 17; // to avoid image boundaries out of PDF size
               doc.addImage(dataUrl, 'eps', 10, 10, widthPDF-widthPDFoffset, Math.floor(widthPDF/ARimage)); // or: remove -17 and use 280 above
               doc.save('plot.pdf');
               /// TODO: Crop PDF image
            })
         }
      ")
      )
   )
   return(new_buttons)
}
