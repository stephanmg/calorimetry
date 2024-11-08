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
	light_on <- 720
	if (input$havemetadata) {
		light_on <- 60 * as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
	}
	if (input$override_metadata_light_cycle) {
		light_on <- 60 * input$light_cycle_start
	}

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

	# TODO: v0.4.0 - Add back animal and days selection as in EE, Raw, and GoxLox panels

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

	write.csv2(apply(finalC1, 2, as.character), "before_scaling_finalC1.csv")
	finalC1 <- finalC1 %>% mutate(HP = (HP/60) * CohortTimeDiff)
	finalC1 <- finalC1 %>% mutate(HP2 = (HP2/60) * CohortTimeDiff)
	finalC1$Datetime <- day(dmy(lapply(finalC1$Datetime, convert)))

	TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount), FUN = sum, na.rm = T)
	TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount), FUN = sum, na.rm = T) 

	if (input$with_facets) {
		if (input$facets_by_data_one %in% names(finalC1)) {
		TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum, na.rm = T)
		TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum, na.rm = T) 
		}
	} 

	TEE <- rbind(TEE1, TEE2)
	names(TEE)[names(TEE) == "x"] <- "TEE"
	TEE$Equation <- as.factor(c(rep(input$variable1, nrow(TEE1)), rep(input$variable2, nrow(TEE2))))
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


	p <- ggplot(data = TEE, aes(x = Animals, y = TEE, label = Days, color=Cohort)) 
	p <- add_visualization_type(p, input$box_violin_or_other, TRUE)
	p <- p + ylab(paste("TEE [", input$kj_or_kcal, "/day]", sep = ""))

	if (input$with_facets) {
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(".~Facet"), scales="free_x")
			} else {
				p <- p + facet_grid(as.formula("Facet~."), scales="free_y")
			}
		}
	}
	
	output$test <- renderUI({
		tagList(
			h4("Configuration"),
			selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
			selectInput("dep_var", "Dependent variable", choice = c("TEE")),
			selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
			selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
			selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
			conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
			hr(style = "width: 50%"),
			h4("Advanced"),
			selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman"), selected = "Sidak"),
			sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
			checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
			hr(style = "width: 75%"),
			renderPlotly(do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary + xlab(pretty_print_label(input$covar, metadatafile)) + ylab(pretty_print_label(input$dep_var, metadatafile)) + ggtitle(input$study_description)),
			hr(style = "width: 75%"),
			conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, metadatafile)) + ylab(pretty_print_label(input$dep_var, metadatafile)) + ggtitle(input$study_description)))
		)
		})

		output$details <- renderUI({
		results <- do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)
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

		output$explanation <- renderText(results$statistics$p)
		output$explanation <- renderUI({
		str1 <- "<h3> Total energy expenditures (TEEs) for animal for each day are displayed as violin plots</h3>"
		str2 <- "Depending on the chosen heat production equation, TEE might slightly change, usually there is no significant differences between calculated TEEs from different heat production equations."
		str3 <- "Usually there is no large discrepancy between TEEs calculated from different heat production formulas"
		str4 <- "<hr/>Statistical testing based on condition like genotype can be conducted in the statistical testing panel by ANCOVA or ANOVA. Post-hoc testing is summarized in the Details panel. To return to the violin plots of TEE stratified by animal ID select the Basic plot panel."
		HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
		})

	p <- p + ggtitle(paste0("Total energy expenditure (days=", length(levels(TEE$Days)), ") using equation ", pretty_print_equation(input$variable1), sep = ""))
	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	storeSession(session$token, "is_TEE_calculated", TRUE, global_data)

	# create LME model UI
	TEE_for_model <- getSession(session$token, global_data)[["TEE"]]
	if (!is.null(TEE_for_model)) {
		TEE_for_model <- TEE_for_model %>% full_join(y = true_metadata, by = c("Animals")) %>% na.omit() 
		write.csv2(TEE_for_model, "tee_before_lme_model.csv")
		#create_lme_model_ui(input, output, true_metadata, finalC1, "HP2")
		create_lme_model_ui(input, output, true_metadata, TEE_for_model, "TEE")
	}


	return(p)
}