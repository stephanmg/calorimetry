# This is an example on how to use the metadata (sheet) in different functions.
## The metadata from the data files (e.g. TSE) could be joined directly with the metadata sheet. For compability, we 
# currently require a valid TSE metadata header corresponding with entries in the columns of metadata sheet, issue #62.
energy_expenditure <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
# colors for plotting as factor
	finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))

	# join either metadata from sheet or tse supported header columns (see above) to measurement data
	# enrich with metadata from TSE header (C1meta) or from metadata sheet (input$metadatafile)
	data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
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

	# default from UI
	light_on <- input$light_cycle_start 

	# otherwise take from metadata sheet  
	if (input$havemetadata) {
		light_on <- as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
	}

	# force override if metadata was available
	if (input$override_metadata_light_cycle) {
		light_on <- input$light_cycle_start
	}

	# display zeitgeber zeit
	finalC1 <- zeitgeber_zeit(finalC1, light_on)

	# already shifted by zeitgeber zeit above, so light_on is now 0
	day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, "HP2", input$with_facets)
	finalC1 <- day_annotations$df_annotated

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}
	finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)

	# corrected filtering for day and night based on light cycle information
	finalC1 <- finalC1 %>% select(-Datetime2)

	# create input select fields for animals and days
	num_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
	if (input$only_full_days_zeitgeber) {
		finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour > 0, running_total.hrs.halfhour < (24*num_days))
	}
	finalC1$DayCount <- ceiling((finalC1$running_total.hrs.halfhour / 24) + 1)
	days_and_animals_for_select <- get_days_and_animals_for_select_alternative(finalC1)
	# TODO: v0.4.0 - add possibility to not use zeitgeber zeit if (input$use_zeitgeber_zeit) { ... }
	# basically look at TEE; GoxLox, or DayNightAcvitiy panel...
	# for days and animals selection use get_days_and_animals_for_select(finalC1) not alternative,
	# code above must be wrapped into if statements accordingly...

	# select days
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

	# select animals
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

	# store selected animals and days in session
	selected_days <- getSession(session$token, global_data)[["selected_days"]]
	selected_animals <- getSession(session$token, global_data)[["selected_animals"]]

	# filter for selected days and animals in data set
	finalC1 <- finalC1 %>% filter(DayCount %in% selected_days)
	finalC1 <- finalC1 %>% filter(`Animal No._NA` %in% selected_animals)

	# trim times from end and beginning of measurements   
	if (input$curate) {
		finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour >= min(running_total.hrs.halfhour) + input$exclusion_start, running_total.hrs.halfhour <= (max(finalC1$running_total.hrs.halfhour) - input$exclusion_end))
	}

	finalC1 <- finalC1 %>% filter(DayCount %in% intersect(selected_days, levels(as.factor(finalC1$DayCount))))

	# Day Night filtering
	finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < 12, "Day", "Night")
	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	finalC1$NightDay <- as.factor(finalC1$NightDay)

	# if we do not have metadata, this comes from some non-clean TSE headers
	if (!input$havemetadata) { finalC1$`Animal.No.` <- finalC1$Animals }

	# calculate running averages
	if (input$running_average > 0) {
		p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = "HP2", color = "Animals", group = "Animals"))
		if (input$running_average_method == "Mean") {
		p <- p + geom_line(aes(y = rollmean(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
		} else if (input$running_average_method == "Max") {
		p <- p + geom_line(aes(y = rollmax(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
		} else if (input$running_average_method == "Median") {
		p <- p + geom_line(aes(y = rollmedian(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
		} else if (input$running_average_method == "Sum") {
		p <- p + geom_line(aes(y = rollsum(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
		} else {
		p <- p + geom_line(aes(y = HP2), group = "Animals")
		}
	} else {
		p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = "HP2", color = "Animals", group = "Animals"))
		p <- p + geom_line()
	}
		output$test <- renderUI({
		tagList(
			h4("Configuration"),
			selectInput("test_statistic", "Test", choices = c("1-way ANCOVA")),
			selectInput("dep_var", "Dependent variable", choice = c("EE")),
			selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
			selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
			selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
			conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
			hr(style = "width: 50%"),
			h4("Advanced"),
			selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman"), selected = "Sidak"),
			sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
			checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
			hr(style = "width: 75%"),
			renderPlotly({
				if (!getSession(session$token, global_data)[["is_RMR_calculated"]]) {
					shinyalert("Error:", "Resting metabolic rate needs to be calculated before!")
					return()
				}

				EE <- getSession(session$token, global_data)[["TEE_and_RMR"]]
				EE <- EE %>% filter(TEE == "non-RMR") %>% select(-TEE) 

				p <- do_ancova_alternative(EE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "EE", input$test_statistic, input$post_hoc_test,input$connected_or_independent_ancova)$plot_summary 
				p <- p + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_variable("EE", input$metadatafile$datapath)) + ggtitle(input$study_description)
				ggplotly(p)
			}),
			hr(style = "width: 75%"),
			conditionalPanel("input.num_covariates == '2'", 
				renderPlotly({
					EE <- getSession(session$token, global_data)[["TEE_and_RMR"]]
					EE <- EE %>% filter(TEE == "non-RMR") %>% select(-TEE) 
					p <- do_ancova_alternative(EE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "EE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 
					p <- p + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_variable("EE", input$metadatafile$datapath)) + ggtitle(input$study_description)
					ggplotly(p)
				})),
		)
		})

		output$details <- renderUI({
		EE <- getSession(session$token, global_data)[["TEE_and_RMR"]]
		EE <- EE %>% filter(TEE == "non-RMR") %>% select(-TEE)
		results <- do_ancova_alternative(EE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "EE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
		tagList(
			h3("Post-hoc analysis"),
			renderPlotly(results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")),
			hr(style = "width: 75%"),
			h4("Results of statistical testing"),
			tags$table(
				tags$thead(
					tags$tr(
					tags$th("p-value", style="width: 100px"),
					tags$th("p-value (adjusted)", style="width: 100px"),
					tags$th("significance level", style="width: 100px"),
					tags$th("degrees of freedom", style="width: 100px" ),
					tags$th("test statistic", style="width: 100px")
					)
				),
				tags$tbody(
					generate_statistical_table(results)
				)
			),
			h4("Test assumptions"),
			tags$table(
				tags$thead(
					tags$tr(
					tags$th("Description", style="width:200px"),
					tags$th("Name of significance test", style="width:200px"),
					tags$th("Null hypothesis", style="width:400px"),
					tags$th("p-value", style="width:200px"),
					tags$th("Status", style="width:200px")
					)
				),
				tags$tbody(
					tags$tr(
					tags$td("Homogeneity of variances", style="width:200px"),
					tags$td("Levene's test", style="width:200px"),
					tags$td("Tests the null hypothesis that the population variances are equal (homoscedasticity). If the p-value is below a chosen signficance level, the obtained differences in sample variances are unlikely to have occured based on random sampling from a population with equal variances, thus the null hypothesis of equal variances is rejected.", style="width: 400px"),
					tags$td(round(as.numeric(results$levene$p), digits=6), style="width:200px"),
					tags$td(
						if (as.numeric(results$levene$p) < 0.05) {
							icon("times")
						} else {
							icon("check")
						}
					,style="width: 200px"
					)
					),
					tags$tr(
					tags$td("Normality of residuals", style="width:200px"),
					tags$td("Shapiro-Wilk test", style="width:200px"),
					tags$td("Tests the null hypothesis that the residuals (sample) came from a normally distributed population. If the p-value is below a chosen significance level, the null hypothesis of normality of residuals is rejected.", style="width: 400px"),
					tags$td(round(as.numeric(results$shapiro$p.value), digits=6), style="width:200px"),
					tags$td(
						if (as.numeric(results$shapiro$p.value) < 0.05) {
							icon("times")
						} else {
							icon("check")
						}
					,style="width: 200px"
					)
					)
				)
			),
		)
		})

	# add means
	if (input$wmeans) {
		p <- p + geom_smooth(method = input$wmeans_choice)
	}

	# add stats
	if (input$wstats) {
		p <- p + stat_cor(method = input$wmethod)
	}

	# axis labels
	p <- p + xlab("Zeitgeber time [h]")
	if (input$kj_or_kcal == "mW") {
		p <- p + ylab(paste("Energy expenditure [", input$kj_or_kcal, "[J/s]", sep = " "))
	} else {
		p <- p + ylab(paste("Energy expenditure [", input$kj_or_kcal, "/ h]", sep = " "))
	}

	# add light cycle annotation
	lights <- data.frame(x = finalC1["running_total.hrs.halfhour"], y = finalC1["HP2"])
	colnames(lights) <- c("x", "y")
	
	if (input$timeline) {
		light_offset <- 0
		# this is already corrected with zeitgeber zeit above (shifted towards the beginning of the light cycle, then re-centered at 0)
		my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, light_offset, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle)
		p <- p + my_lights
	}

	# add title
	p <- p + ggtitle(paste0("Energy expenditure [", input$kj_or_kcal, "/ h]", " using equation ", pretty_print_equation(input$myp)))

	# group with group from metadata
	if (input$with_facets) {
	if (!is.null(input$facets_by_data_one)) {
		if (input$orientation == "Horizontal") {
			p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
		} else {
			p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
		}
	}
	}

	# if we have full days based on zeitgeber time, we kindly switch to Full Day annotation instead of Day
	if (input$only_full_days_zeitgeber) {
		day_annotations$annotations <- day_annotations$annotations %>% mutate(label=gsub("Day", "Full Day", label))
	}

	light_offset <- -12
	# add day annotations and indicators vertical lines
	p <- p + geom_text(data=day_annotations$annotations, aes(x = x+light_offset+2, y = y, label=label), vjust=1.5, hjust=0.5, size=4, color="black")
	# indicate new day
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
	# indicate night start
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
	# re-center at 0
	p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(lights$x), max(lights$x)))
	print(max(lights$x))
	#p <- p + scale_y_continuous(expand = c(0, 0), limits = c(min(lights$y), max(lights$y)))
	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))

	# create LME model UI
	create_lme_model_ui(input, output, true_metadata, finalC1, "HP2")

	return(p)
}