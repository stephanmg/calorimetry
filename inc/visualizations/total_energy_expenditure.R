################################################################################
#' total_energy_expenditure
#' 
#' This function calculates the total energy expenditure
#' 
#' @param finalC1 input data
#' @param C1meta basic metadata
#' @param finalC1meta combined metadata
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param global_data dictionary to store variables session-based for users
#' @param scaleFactor used to scale energy expenditure units correctly
#' @examples 
#' total_energy_expenditure(values, basic_metadata, full_metadata, input, output, session, global_data, 1)
#' @export
################################################################################
total_energy_expenditure <- function(finalC1, C1meta, finalC1meta, input, output, session, global_data, scaleFactor) {
	# assign colors based on animals
	colors <- as.factor(`$`(finalC1, "Animal No._NA"))
	finalC1$Animals <- colors

	# enrich with metadata
	metadatafile <- get_metadata_datapath(input, session, global_data)
	data_and_metadata <- enrich_with_metadata(finalC1, C1meta, input$havemetadata, metadatafile)
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

	# use defaults for light, cycle then try metadata, otherwise force override
	light_on <- input$light_cycle_start
	light_off <- input$light_cycle_stop

	if (input$havemetadata) {
		light_on <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
		light_off <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_off"))) %>% select(2) %>% pull())
	}

	if (input$override_metadata_light_cycle) {
		light_on <- input$light_cycle_start
		light_off <- input$light_cycle_stop
	}

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}

	# when zeitgeber time should be used  
	if (input$use_zeitgeber_time) {
		finalC1 <- zeitgeber_zeit(finalC1, light_off)
		num_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
		if (input$only_full_days_zeitgeber) {
			finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour > 0, running_total.hrs.halfhour < (24*num_days))
		} 
		finalC1$DayCount <- ceiling((finalC1$running_total.hrs.halfhour / 24) + 1)
		finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < 12, "Night", "Day")
	} else {
		finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)
		finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime2)) * 60 + minute(hms(finalC1$Datetime2)) < (light_on * 60), "Day", "Night")
		finalC1$NightDay <- as.factor(finalC1$NightDay)
		finalC1$DayCount <- floor((finalC1$running_total.sec / (24*60*60)) + 1)
		if (input$time_scale_for_plot != "s") {
			# TODO: Dense Rank won't work in case of %d/%m/%Y %H:%M:%S format, because currently we do not support seconds resolution
			finalC1 <- finalC1 %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
		}
	}


	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	colors <- as.factor(`$`(finalC1, "Animal No._NA"))
	finalC1$Animals <- colors

	# if we do not have metadata, this comes from some not-clean TSE headers
	if (!input$havemetadata) { finalC1$`Animal.No.` <- finalC1$Animals }

	convert2 <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}

	finalC1$Datetime2 <- lapply(finalC1$Datetime, convert2)
	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][1], "", sep = "")
	}

	interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
	pretty_print_interval_length_list(interval_length_list)
	finalC1$CohortTimeDiff <- sapply(finalC1$Animals, lookup_interval_length, interval_length_list_per_cohort_and_animals=interval_length_list)

	df_to_plot <- finalC1


	write.csv2(apply(finalC1, 2, as.character), "before_scaling_finalC1.csv")
	finalC1 <- finalC1 %>% mutate(HP = (HP/60) * CohortTimeDiff)
	finalC1 <- finalC1 %>% mutate(HP2 = (HP2/60) * CohortTimeDiff)
	finalC1$Datetime <- day(dmy(lapply(finalC1$Datetime, convert)))

	TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount), FUN = sum, na.rm = T)

	print(TEE1)
	TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount), FUN = sum, na.rm = T) 

	if (input$with_facets) {
		if (input$facets_by_data_one %in% names(finalC1)) {
			TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum, na.rm = T)
			TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum, na.rm = T) 
		}
	} 

	#TEE <- rbind(TEE1, TEE2)
	#names(TEE)[names(TEE) == "x"] <- "TEE"
	#TEE$Equation <- as.factor(c(rep(input$variable1, nrow(TEE1)), rep(input$variable2, nrow(TEE2))))
	#TEE$Days <- as.factor(TEE$Days)
	#TEE$Animals <- as.factor(TEE$Animals)

	TEE <- TEE1
	names(TEE)[names(TEE) == "x"] <- "TEE"

	print(TEE)
	TEE$Equation <- as.factor(c(rep(input$variable1, nrow(TEE1))))
	TEE$Days <- as.factor(TEE$Days)
	TEE$Animals <- as.factor(TEE$Animals)
	if (input$with_facets) {
		if (input$facets_by_data_one %in% names(finalC1)) {
			TEE$Facet <- as.factor(TEE$Facet)
		}
	}
	write.csv2(TEE, "tee.csv")
	storeSession(session$token, "TEE", TEE, global_data)
	TEE <- TEE %>% filter(Equation == input$variable1)

	interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
	TEE$Cohort <- sapply(TEE$Animals, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)

	# Filtering for animals and Days
	TEE <- add_filtering_for_days_and_animals(input, session, output, TEE, global_data)

	p <- ggplot(data = TEE, aes(x = Animals, y = TEE, label = Days, color=Cohort)) 
	p <- add_visualization_type(p, input$box_violin_or_other, TRUE)
	p <- p + ylab(paste("TotalHeatProduction [", input$kj_or_kcal, "/day]", sep = ""))

	if (input$with_facets) {
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(".~Facet"), scales="free_x")
			} else {
				p <- p + facet_grid(as.formula("Facet~."), scales="free_y")
			}
		}
	}

	# add anova/ancova panel
	storeSession(session$token, "selected_indep_var", "Genotype", global_data)
	add_anova_ancova_panel(input, output, session, global_data, true_metadata, TEE, metadatafile, "TEE", "TEE")

	output$explanation <- renderText(results$statistics$p)
	output$explanation <- renderUI({
		str1 <- "<h3> Total heat production (THP) for animal for each day are displayed as violin plots</h3>"
		str2 <- "Depending on the chosen heat production equation, THP might slightly change, usually there is no significant differences between calculated TEEs from different heat production equations."
		str3 <- "Usually there is no large discrepancy between THPs calculated from different heat production formulas"
		str4 <- "<hr/>Statistical testing based on condition like genotype can be conducted in the statistical testing panel by ANCOVA or ANOVA. Post-hoc testing is summarized in the Details panel. To return to the violin plots of TEE stratified by animal ID select the Main plot panel."
		HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
	})

	p <- p + ggtitle(paste0("Total heat production (days=", length(levels(TEE$Days)), ") using equation ", pretty_print_equation(input$variable1), sep = ""))
	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	storeSession(session$token, "is_TEE_calculated", TRUE, global_data)

	# create LME model UI
	TEE_for_model <- getSession(session$token, global_data)[["TEE"]]
	if (!is.null(TEE_for_model)) {
		TEE_for_model <- TEE_for_model %>% full_join(y = true_metadata, by = c("Animals")) %>% na.omit() 
		write.csv2(TEE_for_model, "tee_before_lme_model.csv")
		create_lme_model_ui(input, output, true_metadata, TEE_for_model, "TEE", session, global_data)
	}

	# add time trace for EE (as difference between TEE and RMR) if RMR was available
	p2 <- NULL
	rmr_time_trace <- getSession(session$token, global_data)[["RMR_time_trace"]]
	p3 <- NULL
	if (!is.null(rmr_time_trace)) {
		# TODO: has no grouping by facet genotype rmr_time_trace needs to be joined with metadata first,
		# this will allow to have facets for the rmr time trace plot
		rmr_time_trace$Time <- rmr_time_trace$Time / 60
		rmr_time_trace <- rmr_time_trace %>% filter(Component == "CO2")
		df_to_plot$Time <- df_to_plot$running_total.hrs.halfhour
		df_to_plot$Time <- df_to_plot$Time - (min(df_to_plot$Time))
		result <- rmr_time_trace %>%
		rename(Time1 = Time, Meas1 = HP) %>%
		inner_join(df_to_plot %>% rename(Time2 = Time, Meas2 = HP), by = "Animals") %>%
		mutate(TimeDiff = abs(Time1 - Time2)) %>%  # Compute time differences
		group_by(Animals, Time1) %>%
		filter(TimeDiff == min(TimeDiff)) %>%     # Keep only the smallest time difference
		ungroup() %>%
		mutate(MeasDiff = abs(Meas2 - Meas1)) %>%      # Compute measurement difference
		select(Animals, Time1, Time2, MeasDiff, TimeDiff, Meas1, Meas2)
		p2 <- ggplot(data = result, aes_string(y = "MeasDiff", x = "Time1", color = "Animals", group = "Animals")) + geom_line()
		p2 <- p2 + ylab(paste0("Heat Production [", input$kj,"/ h]"))
		p2 <- p2 + xlab("Zeitgeber time [h]")
		p2 <- p2 + ggtitle("Time trace")
		p2 <- ggplotly(p2) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
		if (input$windowed_plot == TRUE) {
			# offset is minimum value for time (on x-axis)
			offset <- min(finalC1$running_total.hrs.halfhour)
			# windowed time trace plot
			window_plot <- add_windowed_plot(input, output, session, global_data, true_metadata, metadatafile, df_to_plot, "EE", offset, "HP")
			p3 <- window_plot$plot
			p3 <- p3 + xlab("Zeitgeber time [h]") 
			p3 <- p3 + ylab(paste0("Total Heat Production [", input$kj_or_kcal,"/ h]")) + ggtitle("Total Heat Production in window")
			annotations_window_plot <<- window_plot$annotations

			# group with group from metadata
			if (input$with_facets) {
				if (!is.null(input$facets_by_data_one)) {
					if (input$orientation == "Horizontal") {
						#p2 <- p2 + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
						if (!is.null(input$facet_medians)) {
							if (!input$facet_medians) {
								p3 <- p3 + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
							} else {
								if (!is.null(input$facet_medians_in_one_plot)) {
									if (!input$facet_medians_in_one_plot) {
										p3 <- p3 + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
									}
								}
							}
						}
					} else {
						#p2 <- p2 + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
						if (!is.null(input$facet_medians)) {
							if (!input$facet_medians) {
								p3 <- p3 + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
							} else {
								if (!is.null(input$facet_medians_in_one_plot)) {
									if (!input$facet_medians_in_one_plot) {
										p3 <- p3 + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
									}
								}
							}
						}
					}
				}
			}
		}
	}

	if (input$windowed_plot == TRUE) {
		if (!is.null(p3)) {
			p3 <- ggplotly(p3) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
		}
	}

	# store plot and indicate that Raw has been calculated
	storeSession(session$token, "plot_for_tee", p, global_data)
	storeSession(session$token, "plot_for_tee_window", p3, global_data)
	storeSession(session$token, "is_TEE_window_calculated", length(p3) > 0, global_data)

	return(list("time_trace"=p2, "plot"=p, "window_plot"=p3))
}
