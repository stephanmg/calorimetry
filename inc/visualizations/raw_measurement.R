library(DT)
source("inc/statistics/lme_model.R")

################################################################################
#' raw_measurement
#' 
#' This function displays raw measurements
#' @param finalC1 input data
#' @param finalC1meta combined metadata
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param global_data dictionary to store variables session-based for users
#' @param scaleFactor used to scale energy expenditure units correctly
#' @examples 
#' raw_measurement(values, full_metadata, input, output, session, global_data, 1)
#' @export
################################################################################
raw_measurement <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
	# colors for plotting as factor
	finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))
	# get metadata from tse header only
	metadatafile <- get_metadata_datapath(input, session, global_data)
	data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, metadatafile)
	finalC1 <- data_and_metadata$data
	true_metadata <- data_and_metadata$metadata

	# Select sexes
	if (!is.null(input$checkboxgroup_gender)) {
		if ("Sex" %in% names(finalC1)) {
			finalC1 <- finalC1 %>% filter(Sex %in% c(input$checkboxgroup_gender))
		}
	}

	# filter conditions
	if (input$with_grouping) {
		my_var <- input$condition_type
		if (!is.null(input$select_data_by) && !is.null(input$condition_type)) {
			finalC1 <- finalC1 %>% filter((!!sym(my_var)) == input$select_data_by)
		}
	}

	# default from UI for light cycle start 
	light_on <- input$light_cycle_start 

	# in case we have metadata, override with values from sheet
	if (input$havemetadata) {
		light_on <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
	}

	# in case no information in metadata sheet, override light cycle manually
	if (input$override_metadata_light_cycle) {
		light_on <- input$light_cycle_start
	}

	# display zeitgeber zeit
	write.csv2(finalC1, "before_to_zeitgeber.csv")
	finalC1 <- zeitgeber_zeit(finalC1, input$light_cycle_start)
	write.csv2(finalC1, "to_zeitgeber.csv")

	# format variable from UI to compatible TSE format
	mylabel <- paste0(input$myr, sep = "", "_[%]")
	if (startsWith(input$myr, "V")) { mylabel <- paste0(input$myr, sep = "", "(3)_[ml/h]") }

	# rename Temp
	if (startsWith(input$myr, "Temp")) { mylabel <- paste0(input$myr, sep = "", "_C") }

	# rename RER_NA to RER (but finalC1 still has RER_NA)
	if (startsWith(input$myr, "RER")) { mylabel <- "RER_NA" }

	# annotate days and animals (already shifted by above correction)
	day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, mylabel, input$with_facets)

	# rename RER_NA to RER (but finalC1 still has RER_NA)
	if (startsWith(input$myr, "RER")) { mylabel <- "RER" }

	# annotations
	finalC1 <- day_annotations$df_annotated

	# create input select fields for animals and days
	num_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
	if (input$only_full_days_zeitgeber) {
		finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour > 0, running_total.hrs.halfhour < (24*num_days))
	}
	finalC1$DayCount <- ceiling((finalC1$running_total.hrs.halfhour / 24) + 1)
	days_and_animals_for_select <- get_days_and_animals_for_select_alternative(finalC1)

	selected_days <- getSession(session$token, global_data)[["selected_days"]]
	# set default for animals and selected days: typically all selected at the beginning
	if (is.null(selected_days)) {
		output$select_day <- renderUI({
			selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = days_and_animals_for_select$days, multiple = TRUE)
		})
		selected_days = days_and_animals_for_select$days
		storeSession(session$token, "selected_days", selected_days, global_data)
	} else {
		output$select_day <- renderUI({
			selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = selected_days, multiple = TRUE)
		})
	}

	selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
	if (is.null(selected_animals)) {
		output$select_animal <- renderUI({
			selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = days_and_animals_for_select$animals, multiple = TRUE)
		})
		selected_animals = days_and_animals_for_select$animals
		storeSession(session$token, "selected_animals", selected_animals, global_data)
	} else {
		output$select_animal <- renderUI({
			selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = selected_animals, multiple = TRUE)
		})
	}

	# filter for selected days and animals in data set
	selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
	finalC1 <- finalC1 %>% filter(`Animal No._NA` %in% selected_animals)

	# trim times from end and beginning of measurements 
	if (input$curate) {
		finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour >= min(running_total.hrs.halfhour) + input$exclusion_start, running_total.hrs.halfhour <= (max(finalC1$running_total.hrs.halfhour) - input$exclusion_end))
	}

	finalC1 <- finalC1 %>% filter(DayCount %in% intersect(selected_days, levels(as.factor(finalC1$DayCount))))

	finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < 12, "Day", "Night")
	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	finalC1$NightDay <- as.factor(finalC1$NightDay)

	df_to_plot <- finalC1
	# if we do not have metadata, this comes from some not-clean TSE headers
	if (!input$havemetadata) { df_to_plot$`Animal.No.` <- df_to_plot$Animals }

	# format labels for plot
	mylabel <- paste0(input$myr, sep = "", "_[%]")
	myvar <- input$myr
	if (startsWith(input$myr, "V")) {
		mylabel <- paste0(input$myr, sep = "", "(3)_[ml/h]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "Temp")) {
		mylabel <- paste0(input$myr, sep = "", "_C")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "RER")) {
		mylabel <- "RER"
	}

	names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	names(df_to_plot)[names(df_to_plot) == "RER_NA"] <- "RER"

	# plot basic plot
	write.csv2(df_to_plot, "df_to_plot_failing.csv")

	# TODO: v0.6 - factor this out as utility or method, can be re-used in other panels after discussion
	# replot outlier removed data, only if toggled: outlier removal by selection
	if (input$toggle_outliers) {
		if (!is.null(getSession(session$token, global_data)[["reactive_data"]])) {
		df_to_plot <- getSession(session$token, global_data)[["reactive_data"]]()
		print("replotting done?!")
		print(nrow(df_to_plot))
		}
		# note, it is prohibited to do any other filtering when outliers removal is toggled on
	} else {
		storeSession(session$token, "reactive_data", reactiveVal(df_to_plot), global_data)
	}


	p <- ggplot(data = df_to_plot, aes_string(y = input$myr, x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
	mylabel <- gsub("_", " ", mylabel)

	# annotate timeline
	# TODO: Raw does not work df_to_plot does not contain only full days, but finalC1 does, see GOxLox and EE works too with finalC1, change here too
	lights <- data.frame(x = df_to_plot["running_total.hrs.halfhour"], y = df_to_plot[input$myr])
	colnames(lights) <- c("x", "y")
	if (input$timeline) {
		print(input$light_cycle_start)
		print(input$light_cycle_stop)
		my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, 0, input$light_cycle_day_color, input$light_cycle_night_color)
		print("my lights:")
		print(my_lights)
		print("no lights?!")
		p <- p + my_lights
	}

	p <- p + ylab(pretty_print_variable(mylabel, metadatafile))
	p <- p + xlab("Zeitgeber time [h]")

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][1], "", sep = "")
	}

	# df to plot now contains the summed oxidation over individual days   
	df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))
	df_to_plot$GoxLox = df_to_plot[input$myr]
	GoxLox <- aggregate(df_to_plot$GoxLox, by = list(Animals = df_to_plot$Animals, Days = df_to_plot$DayCount), FUN = sum) %>% rename("Raw" = input$myr)

	# add cohort information
	interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
	GoxLox$Cohort <- sapply(GoxLox$Animals, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)
	
	# store calculated results
	storeSession(session$token, "df_raw", GoxLox, global_data)
	storeSession(session$token, "selected_indep_var", "Genotype", global_data)
	
	add_anova_ancova_panel(input, output, session, global_data, true_metadata, GoxLox, metadatafile, mylabel)
	
	if (input$with_facets) {
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
			} else {
				p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
			}
		}
		if (input$add_average_with_se) {
			p <- p + geom_smooth(aes_string(x = "running_total.hrs.halfhour", y = input$myr), method = input$averaging_method_with_facets, se = TRUE, inherit.aes = FALSE, multiplier=3)
		}
	}

	# if we have full days based on zeitgeber time, we kindly switch to Full Day annotation instead of Day
	if (input$only_full_days_zeitgeber) {
		day_annotations$annotations <- day_annotations$annotations %>% mutate(label=gsub("Day", "Full Day", label))
	}

	# need to start at 0 and 12 for zeitgeber time
	light_offset <- -12 # otherwise outside 0 on left
	light_offset <- light_offset 
	# add day annotations and indicators vertical lines
	# +2 for annotation inside plotting
	p <- p + geom_text(data=day_annotations$annotations, aes(x = x+light_offset+2, y = y, label=label), vjust=1.5, hjust=0.5, size=4, color="black")
	# indicate new day
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
	# indicate night start
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
	# set title and display buttons
	p <- p + ggtitle(paste0("Raw measurement: ", pretty_print_variable(mylabel, metadatafile), " using equation ", pretty_print_equation(input$variable1)))
	# add points only if toggle outliers
	if (input$toggle_outliers) { p <- p + geom_point() }
	# center x axis
	p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(finalC1$running_total.hrs.halfhour), max(finalC1$running_total.hrs.halfhour)))
	print(min(lights$x))
	print(max(lights$x))
	# basic plotly config
	# toggle outliers
	if (input$toggle_outliers) {
		exceed_indices <- which(df_to_plot[[input$myr]] > input$threshold_toggle_outliers)
		p <- ggplotly(p)
		for (i in seq_along(exceed_indices)) {
			p <- p %>% add_segments(x = df_to_plot$running_total.hrs.halfhour[exceed_indices[i]]-0.25, xend = df_to_plot$running_total.hrs.halfhour[exceed_indices[i]]+0.25, y = input$threshold_toggle_outliers, yend = input$threshold_toggle_outliers, line = list(color="red", width=8), name = paste("Outlier #", i)) # showlegend=FALSE
		}
	}
	# store number of total curves already present in plotly
	storeSession(session$token, "all_curves_plotly", length(plotly_build(p)$x$data), global_data)
	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))

	# create LME model UI
	create_lme_model_ui(input, output, true_metadata, df_to_plot, input$myr, session, global_data)

	# return current plot of raw measurements
	return(p)
}