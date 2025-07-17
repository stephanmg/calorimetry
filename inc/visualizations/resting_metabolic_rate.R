################################################################################
#' resting_metabolic_rate
#' 
#' This function calculates the resting metabolic rate
#' @param finalC1 input data
#' @param finalC1meta combined metadata
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param global_data dictionary to store variables session-based for users
#' @param scaleFactor used to scale energy expenditure units correctly
#' @examples 
#' resting_metabolic_rate(values, full_metadata, input, output, session, global_data, 1)
#' @export
################################################################################
resting_metabolic_rate <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor, true_metadata) {
	storeSession(session$token, "is_RMR_calculated", TRUE, global_data)
	metadatafile <- get_metadata_datapath(input, session, global_data)
	component2 <- ""
	if (length(input$cvs) == 2) {
		component <- input$cvs[[1]]
		component2 <- input$cvs[[2]]
	} else {
		component <- input$cvs
		component2 <- component
	}
	component <- paste("V", component, "(3)_[ml/h]", sep = "")
	component2 <- paste("V", component2, "(3)_[ml/h]", sep = "")

	if (length(input$cvs) == 0) {
		component <- "HP2"
		component2 <- "HP"
	}

	light_on <- input$light_cycle_start
	light_off <- input$light_cycle_stop

	if (input$havemetadata) {
		light_on <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
		light_off <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_off"))) %>% select(2) %>% pull())
	}

	if (input$override_metadata_light_cycle) {
		light_on <- input$light_cycle_start
		light_off <- input$light_cycle_start
	}

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}
	finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)

	# when zeitgeber time should be used  
	if (input$use_zeitgeber_time) {
		finalC1 <- zeitgeber_zeit(finalC1 %>% ungroup(), light_off)
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

	# df already prepared to be day and night summed activities
	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)

	# create input select fields for animals and days
	days_and_animals_for_select <- get_days_and_animals_for_select_alternative(finalC1)

	# set default for animals and selected days: typically all selected at the beginning
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



	# first component, typically O2
	df <- data.frame(Values = finalC1[[component]],
		Group = `$`(finalC1, "Animal No._NA"),
		Values2 = finalC1$HP)

	df_new <- partition(df)
	# Note that this might introduce NAs by coercion through cv(...) method.
	## This is correct, since the time length of recordings per sample are not identical typically
	df_new <- cv(df_new, input$window)
	df_new <- reformat(df_new) %>% na.omit()

	# second component, typically CO2
	df2 <- data.frame(Values = finalC1[[component2]],
		Group = `$`(finalC1, "Animal No._NA"),
		Values2 = finalC1$HP)
	df_new2 <- partition(df2)
	# Note that this might introduce NAs by coercion through cv(...) method.
	## This is correct, since the time length of recordings per sample are not identical typically
	df_new2 <- cv(df_new2, input$window)
	df_new2 <- reformat(df_new2) %>% na.omit()

	finalC1$Datetime <- lapply(finalC1$Datetime, convert)

	# if coefficient of variation is used in analysis, we might end up with 1 or more less timepoint (depending on averaging window!)
	# thus we need to make sure to always take the minimum of these three dataframes or pad accordingly the df_new and df_new2 data frames for each sample
	## Note that this happens when input$window 
	do_select_n <- min(nrow(finalC1), nrow(df_new), nrow(df_new2))

	finalC1 <- finalC1 %>% ungroup() %>% slice(1:do_select_n) %>% group_by(`Animal No._NA`)
	df_new <- df_new %>% slice(1:nrow(finalC1))
	df_new2 <- df_new2 %>% slice(1:nrow(finalC1))

	my_order <- unique(df_new$Group)
	df_sorted <- finalC1
	df_sorted$`Animal No._NA` = as.factor(df_sorted$`Animal No._NA`)
	df_sorted$`Animal No._NA` = factor(df_sorted$`Animal No._NA`, levels=my_order)
	df_sorted <- df_sorted %>% arrange(`Animal No._NA`)
	df_to_plot <- cbind(df_new, `$`(df_sorted, "running_total.hrs.halfhour"), `$`(df_sorted, "Animal No._NA"))

	df_to_plot2 <- cbind(df_new2, `$`(finalC1, "running_total.hrs.halfhour"))
	df_to_plot$Group <- as.factor(df_to_plot$Group)
	df_to_plot2$Group <- as.factor(df_to_plot$Group)

	colnames(df_to_plot) <- c("RestingMetabolicRate", "Animal", "Time")
	colnames(df_to_plot2) <- c("RestingMetabolicRate2", "Animal", "Time")

	df_for_cov_analysis <- cbind(df_to_plot, `$`(finalC1, "VO2(3)_[ml/h]"),
		`$`(finalC1, "VCO2(3)_[ml/h]"), `$`(finalC1, "HP"),
		df_to_plot2$RestingMetabolicRate2)
	df_for_cov_analysis$Group <- df_to_plot$Group
	colnames(df_for_cov_analysis) <- c("CoV1", "Animal", "Time", "O2", "CO2", "HP", "CoV2")
	write.csv2(df_for_cov_analysis, file = "df_for_cov_analysis.csv")

	# TODO: Check RMR params: mean interval length of cohorts, 1, 1, 5, seems to be a robust choice
	# to reconstruct reliably RMR, but needs to be validated with additional analysis, e.g. BA analysis
	interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
	AVERAGE_INTERVAL_LENGTH <- mean(sapply(interval_length_list, function(x) x$interval_length))
	SLIDING_WINDOW_SIZE_M <- input$window
	PERCENTAGE_BEST <- input$percentage_best
	AVERAGING_WIDTH <- input$rmr_averaging
	df_plot_total <- extract_rmr_helper(AVERAGE_INTERVAL_LENGTH, PERCENTAGE_BEST, AVERAGING_WIDTH)
	df_plot_total$HP <- as.numeric(df_plot_total$HP) 
	df_plot_total$Time <- as.numeric(df_plot_total$Time)
	df_plot_total$Type <- sapply(df_plot_total$Animal, lookup_interval_length, interval_length_list_per_cohort_and_animals=interval_length_list)
	df_plot_total$Time <- df_plot_total$Time * df_plot_total$Type
	df_plot_total$Cohort <- sapply(df_plot_total$Animal, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)

	df_plot_total$Animals = df_plot_total$Animal
	# since NAs might be introduced due to un-even measurement lengths in the not full days case, we need to remove NAs here (overhang)
	# this is from multiple cohorts case, for a single cohrot we should never have the nas introduced in the first place
	#with_metadata <- enrich_with_metadata(df_plot_total, finalC1meta, input$havemetadata, metadatafile)$data %>% na.omit()
	with_metadata <- enrich_with_metadata(df_plot_total, finalC1meta, input$havemetadata, metadatafile)
	df_plot_total <- with_metadata$data %>% na.omit()
	true_metadata <- with_metadata$metadata

	# Select sexes
	if (!is.null(input$checkboxgroup_gender)) {
		if ("Sex" %in% names(df_plot_total)) {
			df_plot_total <- df_plot_total %>% filter(Sex %in% c(input$checkboxgroup_gender))
		}
	}

	# filter conditions
	if (input$with_grouping) {
		my_var <- input$condition_type
		if (!is.null(input$select_data_by) && !is.null(input$condition_type)) {
			df_plot_total <- df_plot_total %>% filter((!!sym(my_var)) == input$select_data_by)
		}
	}

	p <- NULL 

	df_plot_total$running_total.hrs.halfhour = df_plot_total$Time
	df_plot_total$running_total.sec = df_plot_total$Time * 60
	df_to_plot <- df_plot_total

	if (input$add_average_with_se) {
		if (input$with_facets) {
			if (!is.null(input$facets_by_data_one)) {
				signal <- "HP"
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
						upper = pred$fit + input$averaging_method_with_facets_confidence_levels* pred$se.fit,
						lower = pred$fit - input$averaging_method_with_facets_confidence_levels * pred$se.fit,
						trend = group_value
					)
				}) %>%
				bind_rows()  # Combine predictions for all groups
			}
		} else {
			gam_model <- mgcv::gam(df_to_plot[["HP"]] ~ s(running_total.hrs.halfhour, k=input$averaging_method_with_facets_basis_functions, bs=input$averaging_method_with_facets_basis_function), data=df_to_plot)
			pred <- predict(gam_model, se.fit=TRUE)
			df_to_plot <- df_to_plot %>% mutate(fit=pred$fit, upper = fit + input$averaging_method_with_facets_confidence_levels * pred$se.fit, lower = fit - input$averaging_method_with_facets_confidence_levels * pred$se.fit)
		}
	}
	df_plot_total <- df_to_plot

	# group with group from metadata
	if (input$with_facets) {
		p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, color=Animal)) + geom_line()
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
			} else {
				p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
			}
		}
	} else {
		p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, color=Cohort)) + geom_line()
		p <- p + facet_wrap(~Animal)
	}

	# add trend lines: 
	# TODO: Should also be factored out to remove code duplication
	if (input$add_average_with_se) {
		if (input$with_facets) {
			if (!is.null(input$facets_by_data_one)) {
				grouped_gam$trend <- as.factor(grouped_gam$trend)
				if (!is.null(input$add_average_with_se_one_plot)) {
					if (input$add_average_with_se_one_plot) {
						p <- ggplot(data = df_to_plot, aes_string(y = "HP", x = "running_total.hrs.halfhour"))
						p <- p + geom_ribbon(data = grouped_gam, aes(ymin = lower, ymax = upper, group = trend, color=trend, fill=trend), alpha =input$averaging_method_with_facets_alpha_level) 
						p <- p + labs(colour=input$facets_by_data_one, fill=input$facets_by_data_one)
						# set y-axis label
						mylabel <- "RMR"
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

	p2 <- NULL
	if (input$windowed_plot == TRUE) {
		# offset is minimum value for time (on x-axis)
		offset <- min(df_plot_total$Time)
		df_plot_total$running_total.hrs.halfhour = df_plot_total$Time
		df_plot_total$running_total.sec = df_plot_total$Time * 60
		df_plot_total$DayCount = ceiling(df_plot_total$running_total.sec / (3600*24))

		# windowed time trace plot
		window_plot <- add_windowed_plot(input, output, session, global_data, true_metadata, metadatafile, df_plot_total, "HP", offset, "HP")

		p2 <- window_plot$plot
		mylabel <- "RMR"
		p2 <- p2 + ggtitle(paste0("Average measurement of ", mylabel, " in window")) + ylab(mylabel)
		annotations_window_plot <<- window_plot$annotations
	}

	# add light cycle annotation
	lights <- data.frame(x = df_plot_total$Time, y = df_plot_total$HP)
	colnames(lights) <- c("x", "y")
	df_annos <- annotate_rmr_days(df_plot_total) %>% na.omit()
	p <- p + geom_text(data = df_annos, aes(x=Time, y = min(df_plot_total$HP, na.rm=TRUE), label = Label), vjust = 1.5, hjust = 0.5, size = 3, color='black')

	day_length <- 24
	# if selected either Day or Night, the day length is assumed to be 12 hours
	if (length(input$light_cycle) != 2) {
		day_length = 12
	}

	light_offset <- 0
	first_night_start <- 0
	p <- p + geom_vline(xintercept = as.numeric(seq(day_length*60, max(max(df_plot_total$Time, na.rm = TRUE), day_length*60), day_length*60)), linetype="dashed", color="black")
	p <- p + geom_vline(xintercept = as.numeric(seq((day_length/2)*60, max(max(df_plot_total$Time, na.rm = TRUE), day_length*60), day_length*60)), linetype="dashed", color="gray")

	if (input$timeline) {
		lights <- seq(min(df_plot_total$Time), max(df_plot_total$Time), by = 60 * (light_off - light_on))
		lights <- data.frame(Start=head(lights, -1), End=tail(lights, -1))
		days <- 0
		nights <- 0
		last_day_or_night <- NULL
		lapply(1:nrow(lights), function(i) {
			pair <- lights[i,]
			if (i %% 2 == 0) {
				days <<- days + 1
				p <<- p + annotate("rect", xmin=pair$Start, xmax=pair$End, ymin = min(df_plot_total$HP, na.rm=TRUE), ymax = max(df_plot_total$HP, na.rm=TRUE), fill=input$light_cycle_night_color, alpha=0.1)
			} else {
				nights <<- nights + 1
				p <<- p + annotate("rect", xmin=pair$Start, xmax=pair$End, ymin = min(df_plot_total$HP, na.rm=TRUE), ymax = max(df_plot_total$HP, na.rm=TRUE), fill=input$light_cycle_day_color, alpha=0.1)
			}
			last_day_or_night <<- pair$End
		})

		# if not aligns, say 2 days is 48 hours, but we have 40 hours, we have 1 light phase, 1 dark phase, 1 light phase (36 hours)
		# so 2 light phases 1 dark phase, so the remainder must be dark phase
		if (days > nights) {
			p <- p + annotate("rect", xmin=last_day_or_night, xmax=max(df_plot_total$Time, na.rm=TRUE),  ymin = min(df_plot_total$HP, na.rm=TRUE), ymax = max(df_plot_total$HP, na.rm=TRUE), fill=input$light_cycle_day_color, alpha=0.1)
		} else {
			p <- p + annotate("rect", xmin=last_day_or_night, xmax=max(df_plot_total$Time, na.rm=TRUE),  ymin = min(df_plot_total$HP, na.rm=TRUE), ymax = max(df_plot_total$HP, na.rm=TRUE), fill=input$light_cycle_night_color, alpha=0.1)
		}
	}
	
	p <- p + ylab(paste0("RMR [", input$kj_or_kcal, "/ h]"))
	p <- p + xlab("Zeitgeber time [h]")
	p <- p + ggtitle("Resting metabolic rates")

	convert_minutes_to_hours <- function(x) x / 60

	p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df_plot_total$Time), max(df_plot_total$Time)), breaks=seq(0, max(df_plot_total$Time), by=(4*60)), labels=convert_minutes_to_hours(seq(0, max(df_plot_total$Time), by=(4*60))))
	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	storeSession(session$token, "is_RMR_calculated", TRUE, global_data)

	if (input$windowed_plot == TRUE) {
		if (!is.null(p2)) {
			p2 <- ggplotly(p2) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
		}
	}

	finalC1 <- df_plot_total

	storeSession(session$token, "plot_for_RMR", p, global_data)
	storeSession(session$token, "plot_for_RMR_window", p2, global_data)
	storeSession(session$token, "is_RMR_window_calculated", length(p2) > 0, global_data)

	return(list("finalC1"=finalC1, "plot"=p, "window_plot"=p2))
}