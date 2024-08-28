resting_metabolic_rate <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
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

	light_on <- 720
	if (input$havemetadata) {
		light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
	}

	if (input$override_metadata_light_cycle) {
		light_on <- 60 * input$light_cycle_start
	}

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}
	finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)

	# when zeitgeber time should be used  
	if (input$use_zeitgeber_time) {
		finalC1 <- zeitgeber_zeit(finalC1 %>% ungroup(), input$light_cycle_start)
		num_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
		if (input$only_full_days_zeitgeber) {
			finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour > 0, running_total.hrs.halfhour < (24*num_days))
		} 
		finalC1$DayCount <- ceiling((finalC1$running_total.hrs.halfhour / 24) + 1)
		finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < 12, "Day", "Night")
	} else {
		finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)
		finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime2)) * 60 + minute(hms(finalC1$Datetime2)) < light_on, "Day", "Night")
		finalC1$NightDay <- as.factor(finalC1$NightDay)
		finalC1 <- finalC1 %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
	}

	# TODO: Add back animal and days selection as in EE, Raw, and GoxLox panels

	# df already prepared to be day and night summed activities
	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)

	# first component, typically O2
	df <- data.frame(Values = finalC1[[component]],
		Group = `$`(finalC1, "Animal No._NA"),
		Values2 = finalC1$HP)

	df_new <- partition(df)
	write.csv2(df_new, "df_new_before.csv")
	# Note that this might introduce NAs by coercion through cv(...) method.
	## This is correct, since the time length of recordings per sample are not identical typically
	df_new <- cv(df_new, input$window)
	write.csv2(df_new, "df_new_after.csv")
	df_new <- reformat(df_new) %>% na.omit()
	write.csv2(df_new, "df_new_after_reformat.csv")
	write.csv2(df_new, "df_new.csv")

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
	#to_pad <- nrow(finalC1) - nrow(df_new) # difference between energy expenditure data frame and RMR
	#df_new <- padding_helper(df_new) # pads by replicating the last value for each sample in timeline and inserting a new row after the last row for each sample
	#df_new2 <- padding_helper(df_new2) # pads by replicting the last value for each sample in timeline and inserting a new row after the last row for each sample
	write.csv2(df_new, "df_new_after_padding_before_join.csv")

	finalC1 <- finalC1 %>% ungroup() %>% slice(1:do_select_n) %>% group_by(`Animal No._NA`)
	df_new <- df_new %>% slice(1:nrow(finalC1))
	df_new2 <- df_new2 %>% slice(1:nrow(finalC1))

	#df_to_plot <- cbind(df_new, `$`(finalC1, "running_total.hrs.halfhour"))
	my_order <- unique(df_new$Group)
	df_sorted <- finalC1
	df_sorted$`Animal No._NA` = as.factor(df_sorted$`Animal No._NA`)
	df_sorted$`Animal No._NA` = factor(df_sorted$`Animal No._NA`, levels=my_order)
	df_sorted <- df_sorted %>% arrange(`Animal No._NA`)
	write.csv2(apply(df_sorted, 2, as.character), "df_sorted_new.csv")
	df_to_plot <- cbind(df_new, `$`(df_sorted, "running_total.hrs.halfhour"), `$`(df_sorted, "Animal No._NA"))

	df_to_plot2 <- cbind(df_new2, `$`(finalC1, "running_total.hrs.halfhour"))
	df_to_plot$Group <- as.factor(df_to_plot$Group)
	df_to_plot2$Group <- as.factor(df_to_plot$Group)

	write.csv2(df_new, file = "df_new.csv")
	colnames(df_to_plot) <- c("RestingMetabolicRate", "Animal", "Time")
	colnames(df_to_plot2) <- c("RestingMetabolicRate2", "Animal", "Time")
	write.csv2(df_to_plot, file = "df_to_plot.csv")

	df_for_cov_analysis <- cbind(df_to_plot, `$`(finalC1, "VO2(3)_[ml/h]"),
		`$`(finalC1, "VCO2(3)_[ml/h]"), `$`(finalC1, "HP"),
		df_to_plot2$RestingMetabolicRate2)
	df_for_cov_analysis$Group <- df_to_plot$Group
	colnames(df_for_cov_analysis) <- c("CoV1", "Animal", "Time", "O2", "CO2", "HP", "CoV2")
	write.csv2(df_for_cov_analysis, file = "df_for_cov_analysis.csv")

	# TODO: Check RMR params: mean interval length of cohorts, 1, 1, 5, seems to be a robust choice
	# to reconstruct reliably RMR, but needs to be validated with additional analysis, e.g. BA analysis
	interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
	print(interval_length_list)
	AVERAGE_INTERVAL_LENGTH <- mean(sapply(interval_length_list, function(x) x$interval_length))
	SLIDING_WINDOW_SIZE_M <- input$window
	PERCENTAGE_BEST <- input$percentage_best
	AVERAGING_WIDTH <- input$rmr_averaging
	df_plot_total <- extract_rmr_helper(AVERAGE_INTERVAL_LENGTH, PERCENTAGE_BEST, AVERAGING_WIDTH)
	write.csv2(df_plot_total, file = "df_for_comparison_with_calimera.csv")
	df_plot_total$HP <- as.numeric(df_plot_total$HP) 
	df_plot_total$Time <- as.numeric(df_plot_total$Time)
	df_plot_total$Type <- sapply(df_plot_total$Animal, lookup_interval_length, interval_length_list_per_cohort_and_animals=interval_length_list)
	df_plot_total$Time <- df_plot_total$Time * df_plot_total$Type
	df_plot_total$Cohort <- sapply(df_plot_total$Animal, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)

	df_plot_total$Animals = df_plot_total$Animal
	# since NAs might be introduced due to un-even measurement lengths in the not full days case, we need to remove NAs here (overhang)
	# this is from multiple cohorts case, for a single cohrot we should never have the nas introduced in the first place
	df_plot_total <- enrich_with_metadata(df_plot_total, finalC1meta, input$havemetadata, input$metadatafile)$data %>% na.omit()
	write.csv2(df_plot_total, "after_enriching_again.csv")

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

	# we have O2 and CO2 components, but as  they are pretty similar we instead color RMR traces of samples by membership in cohorts
	# p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, group = Component, color = Component)) + geom_line() + facet_wrap(~Animal)
	p <- NULL 
	print(colnames(df_plot_total))

	# group with group from metadata
	if (input$with_facets) {
		p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, color=Animal)) + geom_line()
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
			} else {
				p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
			}
		}
	} else {
		p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, color=Cohort)) + geom_line()
		p <- p + facet_wrap(~Animal)
	}



	write.csv2(df_plot_total, "before_anno_rmr.csv")
	df_annos <- annotate_rmr_days(df_plot_total) %>% na.omit()
	write.csv2(df_annos, "before_annos.csv")
	print("annos:")
	print(df_annos)
	p <- p + geom_text(data = df_annos, aes(x=Time, y = 0, label = Label), vjust = 1.5, hjust = 0.5, size = 3, color='black')

	day_length <- 24
	# if selected either Day or Night, the day length is assumed to be 12 hours
	if (length(input$light_cycle) != 2) {
		day_length = 12
	}

	light_offset <- 0
	print(max(df_plot_total$Time))
	print(day_length*60)
	# We need to take the max over max time in df_plot_total and day length, because we might have varying measurement intervals
	# in a single cohort already... leading to shorter total time, we assume all measurements are taken at fixed spaced time intervals!
	# TODO: in the case of uneven measurement intervals, this might be not as precise, and uneven measurement intervals als oproblematic when other averging during data import is used...
	p <- p + geom_vline(xintercept = as.numeric(seq(day_length*60, max(max(df_plot_total$Time, na.rm = TRUE), day_length*60), day_length*60)), linetype="dashed", color="black")
	p <- p + geom_vline(xintercept = as.numeric(seq((day_length/2)*60, max(max(df_plot_total$Time, na.rm = TRUE), day_length*60), day_length*60)), linetype="dashed", color="gray")

	# add light cycle annotation
	lights <- data.frame(x = df_plot_total$Time, y = df_plot_total$HP)
	colnames(lights) <- c("x", "y")
	
	#if (input$timeline) {
	#	light_offset <- 0
		# this is already corrected with zeitgeber zeit above (shifted towards the beginning of the light cycle, then re-centered at 0)
	#	my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, light_offset, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle)
	#	p <- p + my_lights
	#}


	p <- p + ylab(paste0("RMR [", input$kj_or_kcal, "/ h]"))
	p <- p + xlab("Zeitgeber time [h]")
	p <- p + ggtitle("Resting metabolic rates")

	convert_minutes_to_hours <- function(x) x / 60

	p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df_plot_total$Time), max(df_plot_total$Time)), breaks=seq(0, max(df_plot_total$Time), by=(4*60)), labels=convert_minutes_to_hours(seq(0, max(df_plot_total$Time), by=(4*60))))
	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	storeSession(session$token, "is_RMR_calculated", TRUE, global_data)
	finalC1 <- df_plot_total
	return(list("finalC1"=finalC1, "plot"=p))
}