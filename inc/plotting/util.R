################################################################################
# add visualization shape (geom shape)
################################################################################
add_visualization_type <- function(p, type) {
	if (type == "Violinplot") { 
			p <- p + geom_violin()
		} else if (type == "Dotplot") {
			p <- p + geom_boxplot()
		} else {
			p <- p + geom_boxplot()
		}
	return(p)
}