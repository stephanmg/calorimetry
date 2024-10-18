goxlox <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
	# colors for plotting as factor
	finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))

	# get metadata from tse header only
	data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
	finalC1 <- data_and_metadata$data
	true_metadata <- data_and_metadata$metadata

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

	if (input$havemetadata) {
		light_on <- as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
	}

	# if one wishes, one can override the light cycle configuration from metadata sheet
	if (input$override_metadata_light_cycle) {
		light_on <- input$light_cycle_start
	}

	# calculate zeitgeber time
	finalC1 <- zeitgeber_zeit(finalC1, input$light_cycle_start)

	# annotate days and animals (already shifted by above correction, thus light_on is now 0: Zeitgeber!)
	day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, "HP2", input$with_facets)
	finalC1 <- day_annotations$df_annotated

	# create input select fields for animals and days
	num_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
	if (input$only_full_days_zeitgeber) {
		finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour > 0, running_total.hrs.halfhour < (24*num_days))
	}
	finalC1$DayCount <- ceiling((finalC1$running_total.hrs.halfhour / 24) + 1)
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

	# filter now for selected animals
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

	# store full days and also potential half days, when not selected full days only zeitgeber
	finalC1 <- finalC1 %>% filter(DayCount %in% intersect(selected_days, levels(as.factor(finalC1$DayCount))))
	storeSession(session$token, "selected_days", intersect(selected_days, levels(as.factor(finalC1$DayCount))), global_data)

	# filtering for Day and Night based on light_cycle length
	finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < input$light_cycle_stop-input$light_cycle_start, "Day", "Night")
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
	GoxLox <- aggregate(df_to_plot$GoxLox, by = list(Animals = df_to_plot$Animals, Days = df_to_plot$Datetime), FUN = sum)
	GoxLox <- GoxLox %>% rename(GoxLox = x)
	storeSession(session$token, "df_gox_lox", GoxLox, global_data)

	output$test <- renderUI({
		tagList(
			h4("Configuration"),
			selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
			selectInput("dep_var", "Dependent variable", choice = c("GoxLox")),
			selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
			selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
			selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
			conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
			hr(style = "width: 50%"),
			h4("Advanced"),
			selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman")),
			sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
			checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
			hr(style = "width: 75%"),
			renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)$plot_summary + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)),
			hr(style = "width: 75%"),
			conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)))
		)
	})

	output$details <- renderUI({
		results <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
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
					tags$tr(
					tags$td(round(as.numeric(results$statistics$p), digits=6), style="width: 100px"),
					tags$td(round(as.numeric(results$statistics$p.adj), digits=6), style="width: 100px"),
					tags$td(results$statistics$p.adj.signif, style="width: 100px"),
					tags$td(results$statistics$df, style="width: 100px"),
					tags$td(round(as.numeric(results$statistics$statistic), digits=6), style="width: 100px")
					)
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

	p <- ggplot(data = df_to_plot, aes_string(y = "GoxLox", x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()

	# timeline
	# TODO: v0.3.14 - Debug why this is not displayed locally but on server
	lights <- data.frame(x = df_to_plot["running_total.hrs.halfhour"], y = df_to_plot$GoxLox)
	colnames(lights) <- c("x", "y")
	if (input$timeline) {
		my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, 0, input$light_cycle_day_color, input$light_cycle_night_color)
		p <- p + my_lights
	}

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

	# with zeitgeber zeit, the offset is always 0
	light_offset <- -12
	# add day annotations and indicators vertical lines
	p <- p + geom_text(data=day_annotations$annotations, aes(x = x+light_offset+2, y = min(df_to_plot$GoxLox), label=label), vjust=1.5, hjust=0.5, size=4, color="black")
	# indicate new day
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
	# indicate night start
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
	# re-center at 0
	p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df_to_plot$running_total.hrs.halfhour), max(df_to_plot$running_total.hrs.halfhour)))
	# legends
	p <- p + ylab(paste(input$goxlox, "[ml/h]", sep = " ")) + xlab("Zeitgeber time [h]") + ggtitle(input$goxlox)
	# return plot p
	return(p)
}