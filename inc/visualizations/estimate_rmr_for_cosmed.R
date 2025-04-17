################################################################################
#' estimate_rmr_for_cosmed
#' 
#' This function calculates RMR for the COSMED platform
#' @param finalC1 input data
#' @param finalC1meta combined metadata
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param global_data dictionary to store variables session-based for users
#' @param scaleFactor used to scale energy expenditure units correctly
#' @export
################################################################################
estimate_rmr_for_cosmed <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
	C1meta_tmp <- C1meta
	colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
	df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")
	endtime <- input$rmr_method_begin # in minutes for ss methods
	stepwidth <- input$rmr_method_frequency # in seconds
	endindex <- endtime * 60 / stepwidth
	n <- nrow(df_to_plot) / endindex

	CV_RER_MAX <- input$SS_method_RER # in %
	CV_VO2_MAX <- input$SS_method_VO2 # in %
	CV_VCO2_MAX <- input$SS_method_VCO2 # in %

	indices_to_keep <-  c()
	indices_to_discard <- c()
	for (i in 1:n) {
		indices <- seq((i - 1) + (endindex - 1) * (i - 1), i * (endindex) - 1)
		values <- df_to_plot[indices, "RER_NA"]
		cv_rer <- 100 * (sd(values) / mean(values))
		values <- df_to_plot[indices, "VO2(3)_[ml/h]"]
		cv_vo2 <- 100 * (sd(values) / mean(values))
		values <- df_to_plot[indices, "VCO2(3)_[ml/h]"]
		cv_vco2 <- 100 * (sd(values) / mean(values))
		if ((cv_rer < CV_RER_MAX) && (cv_vo2 < CV_VO2_MAX) && (cv_vco2 < CV_VCO2_MAX)) {
		indices_to_keep <- append(indices_to_keep, i)
		} else {
		indices_to_discard <- append(indices_to_keep, i)
		}
	}

	total_indices <- c()
	for (i in indices_to_keep) {
		indices <- seq((i - 1) + (endindex - 1) * (i - 1), i * (endindex) - 1)
		total_indices <- append(total_indices, indices)
	}

	if (input$rmr_method == "SS") {
		min_rer <- min(df_to_plot[total_indices, "RER_NA"])
		min_vco2 <- min(df_to_plot[total_indices, "VCO2(3)_[ml/h]"])
		min_vo2 <- min(df_to_plot[total_indices, "VO2(3)_[ml/h]"])
	} else {
		min_rer <- min(df_to_plot[c(indices_to_discard, indices_to_keep), "RER_NA"])
		min_vco2 <- min(df_to_plot[c(indices_to_discard, indices_to_keep), "VCO2(3)_[ml/h]"])
		min_vo2 <- min(df_to_plot[c(indices_to_discard, indices_to_keep), "VO2(3)_[ml/h]"])
	}

	# weir formula (COSMED studies use Weir formular as default always)
	rmr <- 1440 * (3.9 * min_vo2 / 1000 + 1.1 * min_vco2 / 1000)
	
	# plot RMR as histogram
	df <- data.frame(c(rmr))
	names(df) <- rmr
	p <- ggplot(df, aes(x = rmr)) + geom_histogram(fill = "green")
	p <- p + ggtitle("Resting metabolic rate for COSMED")
	return(ggplotly(p))
}