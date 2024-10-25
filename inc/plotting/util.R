################################################################################
# add visualization shape (geom shape)
################################################################################
add_visualization_type <- function(p, type, with_point=TRUE) {
	if (type == "Violinplot") { 
			if (with_point) {
				p <- p + geom_point()
			}
			p <- p + geom_violin(fill="grey80", colour="#3366FF", alpha=0.3) 
		} else if (type == "Dotplot") {
			if (with_point) {
				p <- p + geom_point()
			}
			p <- p + geom_boxplot(fill="grey80", colour="#3366FF", alpha=0.3)
		} else {
			if (with_point) {
				p <- p + geom_point()
			}
			p <- p + geom_boxplot(fill="grey80", colour="#3366FF", alpha=0.3)
		}
	return(p)
}