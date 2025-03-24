################################################################################
#' goxlox
#' 
#' This function calculates lipid and glucose oxidation
#' @param finalC1 input data
#' @param finalC1meta combined metadata
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param global_data dictionary to store variables session-based for users
#' @param scaleFactor used to scale energy expenditure units correctly
#' @examples 
#' goxlox(values, full_metadata, input, output, session, global_data, 1)
#' @export
################################################################################
goxlox <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
	# get metadata
	metadatafile <- get_metadata_datapath(input, session, global_data)

	# only join data frame if not already joined 
	if (!is.null(getSession(session$token, global_data)[["is_GoxLox_calculated"]])) {
		data_and_metadata <- getSession(session$token, global_data)[["GoxLox_df"]]
		finalC1 <- data_and_metadata$data
		true_metadata <- data_and_metadata$metadata
	} else {
		finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))
		data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, metadatafile)
		finalC1 <- data_and_metadata$data
		true_metadata <- data_and_metadata$metadata
		storeSession(session$token, "GoxLox_df", data_and_metadata, global_data)
	}

	# Select sexes
	if (!is.null(input$checkboxgroup_gender)) {
		if ("Sex" %in% names(finalC1)) {
			finalC1 <- finalC1 %>% filter(Sex %in% c(input$checkboxgroup_gender))
		}
	}

	# Filter a group by condition type
	if (input$with_grouping) {
		my_var <- input$condition_type
		if (!is.null(input$select_data_by) && !is.null(input$condition_type)) {
			finalC1 <- finalC1 %>% filter((!!sym(my_var)) == input$select_data_by)
		}
	}

	# default is the provided light cycle from metadata sheet if available, otherwise default from UI is taken
	light_on <- input$light_cycle_start 
	light_off <- input$light_cycle_stop

	if (input$havemetadata) {
		light_on <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
		light_off <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_off"))) %>% select(2) %>% pull())
	}

	# if one wishes, one can override the light cycle configuration from metadata sheet
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
		finalC1 <- finalC1 %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
	}

	# annotate days and animals (already shifted by above correction, thus light_on is now 0: Zeitgeber!)
	day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, "HP2", input$with_facets)
	finalC1 <- day_annotations$df_annotated

	# create input select fields for animals and days
	days_and_animals_for_select <- get_days_and_animals_for_select_alternative(finalC1)

	# selected calendrical days
	selected_days <- getSession(session$token, global_data)[["selected_days"]]
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

	# selected animals
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

	# get default selection of animals and calendrical days
	selected_days <- getSession(session$token, global_data)[["selected_days"]]
	selected_animals <- getSession(session$token, global_data)[["selected_animals"]]

	# Select temperature
	if (!is.null(input$select_temperature)) {
		if (input$select_temperature) {
			finalC1 <- finalC1[finalC1$`Temp_[°C]` >= (input$temperature_mean-input$temperature_deviation) & finalC1$`Temp_[°C]` <= (input$temperature_mean+input$temperature_deviation), ]
		}
	}

	# filter now for selected animals
	finalC1 <- finalC1 %>% filter(`Animal No._NA` %in% selected_animals)

	# trim times from end and beginning of measurements 
	if (input$curate) {
		finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour >= min(running_total.hrs.halfhour) + input$exclusion_start, running_total.hrs.halfhour <= (max(finalC1$running_total.hrs.halfhour) - input$exclusion_end))
	}

	finalC1 <- finalC1 %>% filter(DayCount %in% intersect(selected_days, levels(as.factor(finalC1$DayCount))))

	# Day Night filtering
	finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < 12, "Night", "Day")
	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	finalC1$NightDay <- as.factor(finalC1$NightDay)

	# store full days and also potential half days, when not selected full days only zeitgeber
	finalC1 <- finalC1 %>% filter(DayCount %in% intersect(selected_days, levels(as.factor(finalC1$DayCount))))
	storeSession(session$token, "selected_days", intersect(selected_days, levels(as.factor(finalC1$DayCount))), global_data)

	# filtering for Day and Night based on light_cycle length
	finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < light_off-light_on, "Day", "Night")
	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	finalC1$NightDay <- as.factor(finalC1$NightDay)

	df_to_plot <- finalC1
	# if we do not have metadata, this comes from some not-clean TSE headers
	if (!input$havemetadata) { df_to_plot$`Animal.No.` <- df_to_plot$Animals }

	# MK formulas
	if (input$goxlox == "Glucose oxidation") {
		df_to_plot$GoxLox <- scaleFactor * 4.55 * df_to_plot$`VCO2(3)_[ml/h]` - scaleFactor * 3.21 * df_to_plot$`VO2(3)_[ml/h]`
	} else if (input$goxlox == "Lipid oxidation" || input$goxlox == "Fat oxidation") {
		df_to_plot$GoxLox <- scaleFactor * 1.67 * df_to_plot$`VO2(3)_[ml/h]` - scaleFactor * 1.67 * df_to_plot$`VCO2(3)_[ml/h]`
	# Turku formulas
	} else if (input$goxlox == "Nitrogen oxidation" || input$goxlox == "Protein oxidation") {
		df_to_plot$GoxLox <- 6.25 # this is constant 6.25 g N per minute, questionable if of much use
	}

	# df to plot prepared for a line plot over time, next few lines sum over whole day
	colors <- as.factor(`$`(df_to_plot, "Animal No._NA"))
	df_to_plot$Animals <- colors

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][1], "", sep = "")
	}

	# df to plot now contains the summed oxidation over individual days   
	df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))
	GoxLox <- df_to_plot %>% group_by(Animals, DayCount) %>% summarize(GoxLox = (max(GoxLox, na.rm=TRUE)+min(GoxLox, na.rm=TRUE))/2, .groups = "drop") %>% rename(Days=DayCount)

	interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
	GoxLox$Cohort <- sapply(GoxLox$Animals, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)
	storeSession(session$token, "df_gox_lox", GoxLox, global_data)
	storeSession(session$token, "selected_indep_var", "Genotype", global_data)

	# add anova/ancova panel
	add_anova_ancova_panel(input, output, session, global_data, true_metadata, GoxLox, metadatafile, "GoxLox", "GoxLox")
	
	p2 <- NULL
	# add windowed plot
	if (input$windowed_plot == TRUE) {
		# offset is minimum value for time (on x-axis)
		offset <- min(finalC1$running_total.hrs.halfhour)
		# windowed time trace plot
		window_plot <- add_windowed_plot(input, output, session, global_data, true_metadata, metadatafile, df_to_plot, "FuelOxidation", offset, "GoxLox")
		p2 <- window_plot$plot
		p2 <- p2 + ggtitle(paste0("Average ", input$goxlox, "in window")) + ylab(paste(input$goxlox, "[ml/h]", sep = " "))
		annotations_window_plot <<- window_plot$annotations
	}


	# add smoothing
	gam_model <- NULL
	grouped_gam <- NULL
	if (input$add_average_with_se) {
		if (input$with_facets) {
			if (!is.null(input$facets_by_data_one)) {
				signal <- "GoxLox"
				group = input$facets_by_data_one
				# Fit GAM for each group
				grouped_gam <- df_to_plot %>%
				group_by(!!sym(group)) %>%
				group_map(~ {
					group_value <- .y[[group]][1]
					gam_model <- mgcv::gam(as.formula(paste(signal, " ~ s(running_total.hrs.halfhour, k = ", as.numeric(input$averaging_method_with_facets_basis_functions), ", bs = 'cr')")), data= .x)
					pred <- predict(gam_model, se.fit = TRUE)
					.x %>%
					mutate(
						fit = pred$fit,
						upper = pred$fit + input$averaging_method_with_facets_confidence_levels * pred$se.fit,
						lower = pred$fit - input$averaging_method_with_facets_confidence_levels * pred$se.fit,
						trend = group_value
					)
				}) %>%
				bind_rows()  # Combine predictions for all groups
			}
		} else {
			gam_model <- mgcv::gam(df_to_plot[["GoxLox"]] ~ s(running_total.hrs.halfhour, k=input$averaging_method_with_facets_basis_functions, bs=input$averaging_method_with_facets_basis_function), data=df_to_plot)
			pred <- predict(gam_model, se.fit=TRUE)
			df_to_plot <- df_to_plot %>% mutate(fit=pred$fit, upper = fit + input$averaging_method_with_facets_confidence_levels * pred$se.fit, lower = fit - input$averaging_method_with_facets_confidence_levels * pred$se.fit)
		}
	}

	
	p <- ggplot(data = df_to_plot, aes_string(y = "GoxLox", x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()

	# timeline
	lights <- data.frame(x = df_to_plot["running_total.hrs.halfhour"], y = df_to_plot$GoxLox)
	colnames(lights) <- c("x", "y")
	if (input$timeline) {
		if (!is.null(input$only_full_days_zeitgeber)) {
			if (input$only_full_days_zeitgeber == TRUE) {
				my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle, input$only_full_days_zeitgeber)
				p <- p + my_lights
			} else {
				my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle)
				p <- p + my_lights
			}
		}
	}

	# group with group from metadata
	if (input$with_facets) {
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
				if (!is.null(input$facet_medians)) {
					if (!input$facet_medians) {
						p2 <- p2 + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
					} else {
						if (!is.null(input$facet_medians_in_one_plot)) {
							if (!input$facet_medians_in_one_plot) {
								p2 <- p2 + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
							}
						}
					}
				}
			} else {
				p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
				if (!is.null(input$facet_medians)) {
					if (!input$facet_medians) {
						p2 <- p2 + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
					} else {
						if (!is.null(input$facet_medians_in_one_plot)) {
							if (!input$facet_medians_in_one_plot) {
								p2 <- p2 + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
							}
						}
					}
				}
			}
		}
	}

	# add trend lines
	if (input$add_average_with_se) {
		if (input$with_facets) {
			if (!is.null(input$facets_by_data_one)) {
				grouped_gam$trend <- as.factor(grouped_gam$trend)
				if (!is.null(input$add_average_with_se_one_plot)) {
					if (input$add_average_with_se_one_plot) {
						p <- ggplot(data = df_to_plot, aes_string(y = "GoxLox", x = "running_total.hrs.halfhour"))
						p <- p + geom_ribbon(data = grouped_gam, aes(ymin = lower, ymax = upper, group = trend, color=trend, fill=trend), alpha =input$averaging_method_with_facets_alpha_level) 
						p <- p + labs(colour=input$facets_by_data_one, fill=input$facets_by_data_one)
						# set y-axis label
						mylabel <- paste(input$goxlox, "[ml/h]", sep = " ")
						p <- p + ylab(pretty_print_variable(mylabel, metadatafile))
						# set x-axis label
						if (input$use_zeitgeber_time) {
							p <- p + xlab("Zeitgeber time [h]")
						} else {
							p <- p + xlab("Time [h]")
						}
						# add back timeline
						if (input$timeline) {
							if (!is.null(input$only_full_days_zeitgeber)) {
								if (input$only_full_days_zeitgeber == TRUE) {
									my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle, input$only_full_days_zeitgeber)
									p <- p + my_lights
								} else {
									my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle)
									p <- p + my_lights
								}
							}
						}
					} else {
						p <- p + geom_ribbon(data = grouped_gam, aes(ymin = lower, ymax = upper, group = trend, color=trend, fill=trend), alpha =input$averaging_method_with_facets_alpha_level) 
					}
				}
			}
		} else {
				p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), alpha=input$averaging_method_with_facets_alpha_level, fill=input$averaging_method_with_facets_color)
		}
	}

	# if we have full days based on zeitgeber time, we kindly switch to Full Day annotation instead of Day
	if (input$only_full_days_zeitgeber) {
		day_annotations$annotations <- day_annotations$annotations %>% mutate(label=gsub("Day", "Full Day", label))
	}

	# with zeitgeber zeit, the offset is always 0
	light_offset <- -12
	# add day annotations and indicators vertical lines
	p <- p + geom_text(data=day_annotations$annotations, aes(x = x+light_offset+2, y = min(df_to_plot$GoxLox)+6, label=label), vjust=1.5, hjust=0.5, size=4, color="black")
	# indicate new day
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
	# indicate night start
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
	# re-center at 0
	p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df_to_plot$running_total.hrs.halfhour), max(df_to_plot$running_total.hrs.halfhour)))
	# legends
	p <- p + ylab(paste(input$goxlox, "[ml/h]", sep = " ")) + ggtitle(input$goxlox)
	# set x-axis label
	if (input$use_zeitgeber_time) {
		p <- p + xlab("Zeitgeber time [h]")
	} else {
		p <- p + xlab("Time [h]")
	}

	if (input$windowed_plot == TRUE) {
		if (!is.null(p2)) {
			p2 <- ggplotly(p2) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
		}
	}

	# store plot and indicate GoxLox has been calculated
	storeSession(session$token, "plot_for_goxlox", p, global_data)
	storeSession(session$token, "is_GoxLox_calculated", TRUE, global_data)
	storeSession(session$token, "plot_for_goxlox_window", p2, global_data)
	storeSession(session$token, "is_GoxLox_window_calculated", length(p2) > 0, global_data)

	create_lme_model_ui(input, output, true_metadata, df_to_plot, "GoxLox", session, global_data)

	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))

	# return plot p
	return(list("window_plot"=p2, "plot"=p))
}