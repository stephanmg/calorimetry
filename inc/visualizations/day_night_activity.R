day_night_activity <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
	# colors for plotting
	finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))

	# get metadata from tse header only
	data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
	finalC1 <- data_and_metadata$data
	true_metadata <- data_and_metadata$metadata

	
	# if we do not have metadata, this comes from some not-clean TSE headers
	if (!input$havemetadata) { finalC1$`Animal.No.` <- finalC1$Animals }

	df_to_plot <- finalC1

	write.csv2(apply(df_to_plot, 2, as.character), "debug_new_night_day_before3.csv")

	light_on <- 720
	if (input$havemetadata) {
		light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
	}

	if (input$override_metadata_light_cycle) {
		light_on <- 60 * input$light_cycle_start
	}

	

	write.csv2(apply(df_to_plot, 2, as.character), "debug_new_night_day_before2.csv")

	write.csv2(apply(df_to_plot, 2, as.character), "debug_new_night_day_before.csv")

	# when zeitgeber time should be used  
	if (input$use_zeitgeber_time) {
#     convert <- function(x) {
#     splitted <- strsplit(as.character(x), " ")
#     paste(splitted[[1]][2], ":00", sep = "")
#  }

#     df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))
	df_to_plot <- zeitgeber_zeit(df_to_plot, input$light_cycle_start)
	num_days <- floor(max(df_to_plot$running_total.hrs.halfhour) / 24)
	if (input$only_full_days_zeitgeber) {
		df_to_plot <- df_to_plot %>% filter(running_total.hrs.halfhour > 0, running_total.hrs.halfhour < (24*num_days))
	} 
	df_to_plot$DayCount <- ceiling((df_to_plot$running_total.hrs.halfhour / 24) + 1)
	df_to_plot$NightDay <- ifelse((df_to_plot$running_total.hrs %% 24) < 12, "Day", "Night")
	} else {
convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}


	df_to_plot$Datetime2 <- lapply(df_to_plot$Datetime, convert)
	df_to_plot$Datetime <- lapply(df_to_plot$Datetime, convert)
	# insert day to group by different days in ANCOVA

convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][1], "", sep = "")
	}
	df_to_plot$DayCount <- day(dmy(lapply(df_to_plot$Datetime, convert)))
	df_to_plot$Datetime <- lapply(df_to_plot$Datetime, convert)


		df_to_plot$NightDay <- ifelse(hour(hms(df_to_plot$Datetime)) * 60 + minute(hms(df_to_plot$Datetime)) < light_on, "Day", "Night")
		write.csv2(apply(df_to_plot, 2, as.character), "debug_new_night_day_after_ifelse.csv")
		df_to_plot$NightDay <- as.factor(df_to_plot$NightDay)
		#df_to_plot <- df_to_plot %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
	}

	write.csv2(apply(df_to_plot, 2, as.character), "debug_new_night_day.csv")


	# df already prepared to be day and night summed activities
	df_to_plot$NightDay <- as.factor(df_to_plot$NightDay)

	# TODO: Add back day selection for animals and days as in EE, GoxLox, and Raw panel

	#DayNight <- df_to_plot %>% group_by(NightDay, Animals) %>% summarize(HP=sum(HP, na.rm=TRUE), unique_days = n_distinct(Days), Days=Days) %>% na.omit()
	DayNight <- df_to_plot %>% group_by(NightDay, Animals) %>% summarize(HP=sum(HP, na.rm=TRUE), Days=DayCount) %>% na.omit()
	df_unique_days <- df_to_plot %>% group_by(Animals) %>% summarize(unique_days = n_distinct(Datetime))
	DayNight <- left_join(DayNight, df_unique_days, by = "Animals")
	DayNight <- DayNight %>% mutate(HP = HP / unique_days) 
	DayNight$NightDay <- as.factor(DayNight$NightDay)
	write.csv2(apply(DayNight, 2, as.character), "test_before_day_night.csv")

		output$test <- renderUI({
		tagList(
			h4("Configuration"),
			selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
			selectInput("dep_var", "Dependent variable", choice = c("HP")),
			selectInput("indep_var", "Independent grouping variable #1", choices = "NightDay", selected = "NightDay"),
			selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
			selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
			conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Genotype", get_factor_columns(true_metadata)), selected = "Days")),
			hr(style = "width: 50%"),
			h4("Advanced"),
			selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman")),
			sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
			checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
			hr(style = "width: 75%"),
			renderPlotly(do_ancova_alternative(DayNight, true_metadata, input$covar, input$covar2, "NightDay", input$indep_var2, "HP", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)$plot_summary + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)),
			hr(style = "width: 75%"),
			conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(DayNight, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "HP", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)))
		)
		})


output$details <- renderUI({
		results <- do_ancova_alternative(DayNight, true_metadata, input$covar, input$covar2, "NightDay", input$indep_var2, "HP", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
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

	p <- ggplot(df_to_plot, aes(x = Animals, y = HP, fill = NightDay)) + geom_violin()
	p <- p + ggtitle("Day Night Activity")

	if (input$with_facets) {
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
			} else {
				p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
			}
		}
	}

	p <- p + ylab(paste0("Energy expenditure [", input$kj_or_kcal, "/ h]"))
	if (input$with_facets) {
		p <- ggplotly(p) %>% layout(boxmode = "group") %>% # nolint: pipe_continuation_linter.
		config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	} else {
		p <- ggplotly(p) %>% layout(boxmode = "group") %>% # nolint: pipe_continuation_linter.
		config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	}
	return(p)
}
