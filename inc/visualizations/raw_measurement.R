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
	#finalC1 <- zeitgeber_zeit(finalC1, input$light_cycle_start)

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}

	# when zeitgeber time should be used  
	if (input$use_zeitgeber_time) {
		finalC1 <- zeitgeber_zeit(finalC1, input$light_cycle_start)
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

	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	colors <- as.factor(`$`(finalC1, "Animal No._NA"))
	finalC1$Animals <- colors


	print("here?")
	#write.csv2(finalC1, "to_zeitgeber.csv")

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
	print("there?")
	#write.csv2(df_to_plot, "df_to_plot_failing.csv")

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



	print(df_to_plot)
	#write.csv2(df_to_plot, "before_ancova_we_wanna_see.csv")
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
	
	choices = c(get_columns_with_at_least_two_levels(true_metadata), has_cohorts(GoxLox))

	# Statistics section st art
	output$test <- renderUI({
		tagList(
			h4("Configuration"),
			#conditionalPanel("input.test_statistic == '1-way ANOVA' || input.test_statistic == '2-way ANOVA", checkboxInput("add_points_to_anova", "Add points")),
			selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA", "1-way ANOVA", "2-way ANOVA"), selected = "1-way ANOVA"),
			selectInput("dep_var", "Dependent variable", choice = c("Raw")),
			conditionalPanel("input.test_statistic == '1-way ANCOVA' || input.test_statistic == '2-way ANCOVA'", selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1')),
			selectInput("indep_var", "Independent grouping variable #1", choices = c(get_columns_with_at_least_two_levels(true_metadata), "Animals", has_cohorts(GoxLox)), selected = getSession(session$token, global_data)[["selected_indep_var"]]),
			conditionalPanel("input.test_statistic == '1-way ANCOVA' || input.test_statistic == '2-way ANCOVA'", selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA' || input.test_statistic == '2-way ANOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", setdiff(get_columns_with_at_least_two_levels(true_metadata), input$indep_var)), selected = "Days")),
			#conditionalPanel("input.test_statistic == '2-way ANCOVA' || input.test_statistic == '2-way ANOVA'", selectInput("facet_wrap_for_2_way_analysis", "Wrapping facet", choices = c(input$indep_var2))),
			#conditionalPanel("input.test_statistic == '2-way ANCOVA' || input.test_statistic == '2-way ANOVA'", selectInput("facet_wrap_for_2_way_analysis_orientation", "Wrap orientation", choices = c("Horizontal", "Vertical"))),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
			conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
			conditionalPanel("input.test_statistic == '1-way ANCOVA' || input.test_statistic == '1-way ANOVA'",
			hr(style = "width: 50%"),
			h4("Advanced"),
			checkboxInput("add_points_to_anova_or_ancova", "Add points"),
			selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman"), selected = "Sidak"),
			sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
			checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE)),
			hr(style = "width: 75%"),
			# here we fill the plot below with data
			plotlyOutput("plot_statistics_details"),
			hr(style = "width: 50%"),
			h4("Plotting control"),
			fluidRow(
				column(6,
				h5("x-axis limits"),
				checkboxInput("auto_scale_rmr_plot_limits_x", "Auto-scale", value = TRUE),
				numericInput("x_min_rmr_plot", "min", value = 0, min = 0),
				numericInput("x_max_rmr_plot", "max", value = 100, max = 100)
				),
				column(6,
				h5("y-axis limits"),
				checkboxInput("auto_scale_rmr_plot_limits_y", "Auto-scale", value = TRUE),
				numericInput("y_min_rmr_plot", "min", value = 0, min = 0),
				numericInput("y_max_rmr_plot", "max", value = 100, max = 100)
				)
			),
			hr(style = "width: 75%"),
			conditionalPanel("input.num_covariates == '2'", 
			plotlyOutput("plot_statistics_details2"),
			hr(style = "width: 50%"),
			h4("Plotting control"),
			fluidRow(
				column(6,
				h5("x-axis limits"),
				checkboxInput("auto_scale_rmr_plot_limits_x2", "Auto-scale", value = TRUE),
				numericInput("x_min_rmr_plot2", "min", value = 0, min = 0),
				numericInput("x_max_rmr_plot2", "max", value = 100, max = 100)
				),
				column(6,
				h5("y-axis limits"),
				checkboxInput("auto_scale_rmr_plot_limits_y2", "Auto-scale", value = TRUE),
				numericInput("y_min_rmr_plot2", "min", value = 0, min = 0),
				numericInput("y_max_rmr_plot2", "max", value = 100, max = 100)
				)
			)
			))
		})

	# TODO: v0.4.0 - example how to get plot download for selected plot only, add everywhere else too?
	# get_new_download_buttons("...") will allow to specify an output plot rendered by ID to download

	output$plot_statistics_details <- renderPlotly({
		p <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test,input$connected_or_independent_ancova)$plot_summary
		if (input$test_statistic == '1-way ANOVA' || input$test_statistic == '2-way ANOVA') {
            hideTab(inputId = "additional_content", target = "Details")
			p <- p + xlab(pretty_print_label(input$depvar, metadatafile)) + ylab(pretty_print_variable(mylabel, metadatafile))
			p <- p + labs(color = input$indep_var)
			if (input$test_statistic == '1-way ANOVA') {
				if (input$add_points_to_anova_or_ancova) {
					p <- p + geom_jitter(width = 0.2, aes(color = group)) + labs(input$indep_var)
				}
			} else {
				p <- p + facet_wrap(as.formula(paste("~", input$indep_var2)))
			}
		} else {
            showTab(inputId = "additional_content", target = "Details")
			p <- p + xlab(pretty_print_label(input$covar, metadatafile)) + ylab(pretty_print_variable(mylabel, metadatafile))
		}

		if (input$test_statistic == '1-way ANCOVA' || input$test_statistic == '2-way ANCOVA') {
			if (!input$auto_scale_rmr_plot_limits_x) {
				p <- p + xlim(c(input$x_min_rmr_plot, input$x_max_rmr_plot))
			}

			if (!input$auto_scale_rmr_plot_limits_y) {
				p <- p + ylim(c(input$y_min_rmr_plot, input$y_max_rmr_plot))
			}
		}

		p <- p + ggtitle(input$study_description) 
		ggplotly(p) %>% config(displaylogo = FALSE, 
				modeBarButtons = list(c("toImage", get_new_download_buttons("plot_statistics_details")), 
				list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), 
				list("hoverClosestCartesian", "hoverCompareCartesian")))
	})

	# required to keep the currently selected indep_var
	observeEvent(input$indep_var, {
		storeSession(session$token, "selected_indep_var", input$indep_var, global_data)
		updateSelectInput(session, "indep_var", choices=choices, selected = getSession(session$token, global_data)[["selected_indep_var"]])
	})

	output$plot_statistics_details2 <- renderPlotly({
		p <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 
		p <- p + xlab(pretty_print_label(input$covar2, metadatafile)) 
		p <- p + ylab(pretty_print_variable(mylabel, metadatafile)) 
		p <- p + ggtitle(input$study_description)

		if (!input$auto_scale_rmr_plot_limits_x2) {
			p <- p + xlim(c(input$x_min_rmr_plot2, input$x_max_rmr_plot2))
		}

		if (!input$auto_scale_rmr_plot_limits_y2) {
			p <- p + ylim(c(input$y_min_rmr_plot2, input$y_max_rmr_plot2))
		}

		p <- p + ggtitle(input$study_description) 
		ggplotly(p) %>% config(displaylogo = FALSE, 
				modeBarButtons = list(c("toImage", get_new_download_buttons("plot_statistics_details2")), 
				list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), 
				list("hoverClosestCartesian", "hoverCompareCartesian")))

	})

	output$details <- renderUI({
		results <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
		tagList(
			h3("Post-hoc analysis"),
			plotlyOutput("post_hoc_plot"),
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
						if (as.numeric(results$levene$p) < input$alpha_level) {
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
						if (as.numeric(results$shapiro$p.value) < input$alpha_level) {
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

	# TODO: results is calculated multiple times, in fact only once should be necessary... optimize this.
	output$post_hoc_plot <- renderPlotly({
		results <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
		p <- results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")
		ggplotly(p) %>% config(displaylogo = FALSE, 
			modeBarButtons = list(c("toImage", get_new_download_buttons("post_hoc_plot")), 
			list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), 
			list("hoverClosestCartesian", "hoverCompareCartesian")))
	})

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