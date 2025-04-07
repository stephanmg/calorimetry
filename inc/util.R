source("inc/constants.R")
source("inc/metadata/read_metadata.R")
library(glue)


################################################################################
#' add_windowed_plot_statistics
#' 
#' This function adds statistics to the windowed time trace plot
#' @param data
#' @param input
#' @param total_length
#' @param variable
################################################################################
add_windowed_plot_statistics <- function(data, input, total_length, variable) {
   # Function to test differences per interval
   facet = "Genotype"
   if (!is.null(input$facets_by_data_one)) {
      if (facet != "Animals") {
         facet = input$facets_by_data_one
      } else {
         facet = "Genotype"
      }
   }
test_interval <- function(df) {
  groups <- unique(df[[facet]])
  meas = sym(variable)
  if (length(groups) == 2) {
    # Perform t-test for two groups
    formula = reformulate(facet, response=as.character(meas))
    test_result <- t.test(formula, data=df)
    test_result <- wilcox.test(formula, data=df)
    return(data.frame(
      p_value = test_result$p.value
    ))
  } else if (length(groups) > 2) {
    # Perform ANOVA for three or more groups
    formula = as.formula(paste0(meas, " ~ ", facet))
    test_result <- aov(formula, data = df)
    p_value <- summary(test_result)[[1]][["Pr(>F)"]][1]
    return(data.frame(
      p_value = p_value,
      test = "ANOVA",
      mean_diff = NA
    ))
  } else {
    return(data.frame(p_value = NA, test = "Insufficient groups", mean_diff = NA))
  }
}
results <- data %>%
  group_by(interval) %>%
  group_split() %>%
  purrr::map_df(~ test_interval(.x) %>% mutate(interval = .x$interval[1]))

results <- results %>%
	mutate(time=((interval-1)*total_length+total_length/2)/60) # back to hours

   results <- results %>% mutate(adjusted_p = p.adjust(p_value, method="BH"), significant = ifelse(adjusted_p < 0.001, "*", NA))

   return(results)
}

################################################################################
#' add_windowed_plot
#' 
#' This function adds a windowed time plot
#' @param input 
#' @param output
#' @param session
#' @param global_data
#' @param true_metadata
#' @param metadatafile
#' @param df_to_plot
#' @param mylabel
#' @param offset
#' @param variable
################################################################################
add_windowed_plot <- function(input, output, session, global_data, true_metadata, metadatafile, df_to_plot, mylabel, offset, variable=NULL) {
		data <- df_to_plot
		data <- data %>% mutate(minutes=running_total.sec / 60)
		# User inputs
		total_length <- input$interval_length_for_window  # Total length (e.g., 30 minutes)
		step_size <- input$interval_steps_for_window      # Step size (e.g., 2 steps)

		# Calculate subintervals
		data <- data %>%
		dplyr::mutate(
			interval = ceiling(minutes / total_length), # Define each 30-minute interval
			sub_interval = (minutes %% total_length) %/% step_size + 1 # Subintervals
		)

      myvar <- input$myr
      if (!is.null(variable)) {
         myvar = variable
      }
      p_statistic_details <- add_windowed_plot_statistics(data, input, total_length, variable)

		#  TODO: need to left join the averages plot to get Genotype for facets
		# Group by Animals, Days, interval, and sub_interval, then calculate mean(Meas)
		averages <- data %>%
		dplyr::group_by(DayCount, Animals, interval, sub_interval) %>%
		dplyr::summarise(TEE = median(!!sym(myvar), na.rm = TRUE), .groups = "drop") 

		averages <- averages %>% rename(Days=DayCount)

		storeSession(session$token, "selected_indep_var", "Genotype", global_data)
      # TODO: this adds always the "Raw" panel which works currently, but legends are wrong then, need to fix this.
      # use the same way to add anova_Ancova_panel as in inc/visualization/*.R files.
		add_anova_ancova_panel(input, output, session, global_data, true_metadata, averages, metadatafile, mylabel, "Raw")

		# Calculate averages and SEM by interval
		plot_data <- averages %>%
		group_by(Days, Animals, interval) %>%
		summarise(
			avg_meas = median(TEE, na.rm = TRUE),
			sem = sd(TEE, na.rm = TRUE) / sqrt(n()),
			.groups = "drop"
		) %>%
		mutate(time=((interval-1)*total_length+total_length/2)/60) # back to hours

      plot_data2 <- NULL   
      if (!is.null(input$facets_by_data_one)) {
         if (input$facets_by_data_one != "Animals") {
            plot_data2 <- averages %>% left_join(true_metadata, by="Animals")
            plot_data2 <- plot_data2 %>%
            group_by(Days, !!sym(input$facets_by_data_one), interval) %>%
            summarise(
               avg_meas = median(TEE, na.rm = TRUE),
               sem = sd(TEE, na.rm = TRUE) / sqrt(n()),
               .groups = "drop"
            ) %>%
            mutate(time=((interval-1)*total_length+total_length/2)/60) # back to hours
         } else {
            plot_data2 <- plot_data
         }
      }

      plot_data <- plot_data %>% mutate(time = time + offset)
      if (!is.null(input$facets_by_data_one)) {
         plot_data2 <- plot_data2 %>% mutate(time = time + offset)
      }
      plot_data <- plot_data %>% left_join(true_metadata, by="Animals")
      plot_data2 <- averages

		if (input$boxplots_or_sem_plots == FALSE) {
			# Create the plot
			p2 <- ggplot(plot_data, aes(x = time, y = avg_meas, color = Animals, group = Animals)) +
			geom_line(size = 1) +  # Line plot for averages
			#geom_point(size = 3) +  # Points at each interval
			geom_errorbar(aes(ymin = avg_meas - sem, ymax = avg_meas + sem), width = 0.2) +  # Error bars
			labs(
				title = "Average measurement with SEM over intervals",
				x = "Zeitgeber time [h]",
				y = mylabel,
				color = "Animals"
			) +
			theme_minimal() +
			theme(
				legend.position = "right",
				plot.title = element_text(hjust = 0.5, size = 16)
			)
       
         if (input$facet_medians) {
            medians <- plot_data %>%
            group_by(!!sym(input$facets_by_data_one), time) %>%
            summarise(median_meas = median(avg_meas, na.rm = TRUE), sem=sd(avg_meas, na.rm=TRUE), .groups = "drop")
            p3 <- ggplot(medians, aes(x=time, y=median_meas, color = !!sym(input$facets_by_data_one), group=!!sym(input$facets_by_data_one)))  
            p3 <- p3 + geom_boxplot()#position=position_dodge(width=0.75)) 
            p3 <- p3 + geom_line(size=1) 
			   p3 <- p3 + geom_errorbar(aes(ymin = median_meas - sem, ymax = median_meas + sem), width = 0.2) 
            p2 <- p2 + p3
         }
		} else {
         # individual medians for animals
			medians <- plot_data %>%
			group_by(Animals, time) %>%
			summarise(median_meas = median(avg_meas, na.rm = TRUE), .groups = "drop")

			p2 <- ggplot(plot_data, aes(x = factor(time), y = avg_meas, fill = Animals)) +
			geom_boxplot()#position=position_dodge(width=0.75)) + 
			labs(
				title = "Measurement distribution by time and Animal",
				x = "Zeitgeber time [h]",
				y = mylabel,
				fill = "Animals"
			) +
			theme_minimal() +
			theme(
				legend.position = "right",
				plot.title = element_text(hjust = 0.5, size = 16)
			)

         # connect the invidiual medians for the animals
			if (input$connect_medians_of_boxplots == TRUE) {
				p2 <- p2 + geom_line(data=medians, aes(x=factor(time), y=median_meas, group=Animals, color=Animals), inherit.aes=FALSE, size=1)
			}

         # no boxplot here anymore for facets as for animals, just the median of facet and +- SEM lines
         if (input$facet_medians) {
            medians <- plot_data %>%
            group_by(!!sym(input$facets_by_data_one), time) %>%
            summarise(median_meas = median(avg_meas, na.rm = TRUE), sem=sd(avg_meas, na.rm=TRUE), .groups = "drop")
            p3 <- ggplot(medians, aes(x=time, y=median_meas, color = !!sym(input$facets_by_data_one), group=!!sym(input$facets_by_data_one)))  
            p3 <- p3 + geom_boxplot()#position=position_dodge(width=0.75)) 
            p3 <- p3 + geom_line(size=1) 
			   p3 <- p3 + geom_errorbar(aes(ymin = median_meas - sem, ymax = median_meas + sem), width = 0.2) 
            p2 <- p2 + p3
         }
		}

   annotations <- NULL
   if (input$facet_medians_statistics) {
      p_statistic_details$y = max(plot_data$avg_meas, na.rm=TRUE) 
      annotations <- geom_text(data=p_statistic_details, mapping=aes(x=time+offset, y=y, label=significant), inherit.aes = FALSE, linewidth=6, color = "black") # color="#39E639")
      p2 <- p2 + annotations
   }
   return(list("plot"=p2, "windowed_data"=plot_data, "annotations"=annotations))
}


################################################################################
#' add_anova_ancova_panel
#' 
#' This function adds the ANOVA/ANCOVA statistical panel
#' @param input
#' @param output
#' @param global_data
#' @param true_metadata
#' @param input_df
#' @param metadatafile
#' @param mylabel
#' @param dep_var
################################################################################
add_anova_ancova_panel <- function(input, output, session, global_data, true_metadata, input_df, metadatafile, mylabel, dep_var) {
   # cohort information
	choices = c(get_columns_with_at_least_two_levels(true_metadata), has_cohorts(input_df))
   # statistics start
	output$test <- renderUI({
		tagList(
			h4("Configuration"),
         uiOutput("formula_for_ancova_anova"),
			selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA", "1-way ANOVA", "2-way ANOVA"), selected = "1-way ANOVA"),
         checkboxInput("lm_or_glm", "Use generalized linear model", value=FALSE),
         conditionalPanel("input.lm_or_glm == true", selectInput("glm_family", "GLM Family", choices=c("Gaussian", "Binomial", "Poisson", "Gamma"), selected="Gaussian")),
         conditionalPanel("input.lm_or_glm == true", selectInput("glm_link_function", "Link function", choices=c("default"),  selected="default")),
         conditionalPanel("input.lm_or_glm == true", hr(style="width:30%")),
			selectInput("dep_var", "Dependent variable", choice = c(dep_var)),
			conditionalPanel("input.test_statistic == '1-way ANCOVA' || input.test_statistic == '2-way ANCOVA'", selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1')),
			selectInput("indep_var", "Independent grouping variable #1", choices = c(get_columns_with_at_least_two_levels(true_metadata), "Animals", has_cohorts(input_df)), selected = getSession(session$token, global_data)[["selected_indep_var"]]),
         conditionalPanel("(input.plot_type == 'TotalHeatProduction' || input.plot_type == 'RawMeasurement' || input.plot_type == 'FuelOxidation' || input.plot_type == 'HeatProduction' || input.plot_type == 'RestingMetabolicRate') && (input.test_statistic == '1-way ANOVA')", checkboxInput("average_days", "Average over days", value=FALSE)),
         conditionalPanel("input.test_statistic == '2-way ANCOVA' || input.test_statistic == '2-way ANOVA'", checkboxInput("connected_or_unconnected", "Repeated measurements", value=FALSE)),
			conditionalPanel("input.test_statistic == '1-way ANCOVA' || input.test_statistic == '2-way ANCOVA'", selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA' || input.test_statistic == '2-way ANOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", setdiff(get_columns_with_at_least_two_levels(true_metadata), input$indep_var)), selected = "Days")),
			conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
			conditionalPanel("input.test_statistic == '2-way ANOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
			conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
			conditionalPanel("input.test_statistic == '1-way ANCOVA' || input.test_statistic == '1-way ANOVA'",
			hr(style = "width: 50%"),
			h4("Advanced"),
			#checkboxInput("add_points_to_anova_or_ancova", "Add points"),
			selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman"), selected = "Sidak"),
			sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
			checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE)),
			hr(style = "width: 75%"),
			# here we fill the plot below with data
         h4("Raw data inspection"),
         checkboxInput("show_outliers_from_plot", "Show outliers", value = FALSE),
			plotlyOutput("plot_statistics_details"),
			hr(style = "width: 50%"),
			h4("Plotting control"),
         checkboxInput("sort_factors_alphabetically_decreasing", "Sort factors alphabetically", value = TRUE),
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


	output$plot_statistics_details <- renderPlotly({

      if (!is.null(input$average_days)) {
            if (input$average_days == TRUE) {
               if (dep_var == "RMR") {
                 mylabel = paste0("average RMR [", input$kj_or_kcal, " / day]")
                 input_df <- input_df %>% group_by(Animals) %>% summarize(TEE=sum(TEE) / n_distinct(Days), Days=n_distinct(Days))
               }
               if (dep_var == "TEE") {
                 mylabel = paste0("average THP [", input$kj_or_kcal, " / day]")
                 input_df <- input_df %>% group_by(Animals) %>% summarize(TEE=sum(TEE) / n_distinct(Days), Days=n_distinct(Days))
               } 
               if (dep_var == "EE") {
                 mylabel = paste0("average EE [", input$kj_or_kcal, " / day]")
                 input_df <- input_df %>% group_by(Animals) %>% summarize(EE=sum(EE) / n_distinct(Days), Days=n_distinct(Days))
               }
               if (mylabel == "GoxLox") {
                 input_df <- input_df %>% group_by(Animals) %>% summarize(GoxLox=sum(GoxLox) / n_distinct(Days), Days=n_distinct(Days))
               }
               if (dep_var == "Raw") {
                  if (input$windowed_plot == TRUE) {
                     input_df <- input_df %>% group_by(Animals) %>% summarize(TEE=sum(TEE) / n_distinct(Days) / n_distinct(interval), Days=n_distinct(Days))
                  } else {
                     input_df <- input_df %>% group_by(Animals) %>% summarize(TEE=sum(TEE) / n_distinct(Days), Days=n_distinct(Days))
                  }
               }
            } else {
               if (mylabel == "TEE") {
                  mylabel = paste0("TotalHeatProduction [", input$kj_or_kcal, " / day]")
               }
            }
         }

		ret <- do_ancova_alternative(input_df, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, dep_var, input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates, input$connected_or_unconnected, input$lm_or_glm, input$sort_factors_alphabetically_decreasing)
      p <- ret$plot_summary
      df <- ret$df

		if (input$test_statistic == '1-way ANOVA' || input$test_statistic == '2-way ANOVA') {
         if (input$test_statistic == '2-way ANOVA') {
            showTab(inputId = "additional_content", target = "Details")
         } else {
            # nothing to do
         }
			p <- p + xlab(pretty_print_label(input$depvar, metadatafile)) + ylab(pretty_print_variable(mylabel, metadatafile))
         if (input$test_statistic == '2-way ANOVA') {
			   p <- p + labs(color = input$indep_var2)
         } else {
			   p <- p + labs(color = input$indep_var)
         }
			if (input$test_statistic == '1-way ANOVA') {
			} else {
				p <- p + facet_wrap(as.formula(paste("~", input$indep_var2)))
			}
		} else {
            showTab(inputId = "additional_content", target = "Details")
			p <- p + xlab(pretty_print_label(input$covar, metadatafile)) + ylab(pretty_print_variable(mylabel, metadatafile))
		}

		if (input$test_statistic == '1-way ANCOVA' || input$test_statistic == '2-way ANCOVA') {
         if (input$test_statistic == '2-way ANCOVA') {
            showTab(inputId = "additional_content", target = "Details")
         } else {
            # nothing to do
         }

			if (!input$auto_scale_rmr_plot_limits_x) {
				p <- p + xlim(c(input$x_min_rmr_plot, input$x_max_rmr_plot))
			}

			if (!input$auto_scale_rmr_plot_limits_y) {
				p <- p + ylim(c(input$y_min_rmr_plot, input$y_max_rmr_plot))
			}
		}

		p <- p + ggtitle(input$study_description) 
		p <- ggplotly(p) %>% config(displaylogo = FALSE, 
				modeBarButtons = list(c("toImage", get_new_download_buttons("plot_statistics_details")), 
				list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), 
				list("hoverClosestCartesian", "hoverCompareCartesian")))


      # TODO: this works only for ANOVA and the outliers in the boxplot, think about how to improve this
		if (input$test_statistic != '1-way ANCOVA' && input$test_statistic != '2-way ANCOVA') {
      # Note: Data frame contains as dep var always TEE, so we need to modify this. 
      # TODO: Better: Construct data frame always with the correct dependent variable
      for (i in seq_along(p$x$data)) {
         if (!is.null(p$x$data[[i]]$text)) {
            p$x$data[[i]]$text <- gsub("TEE", pretty_print_label(mylabel, metadatafile), p$x$data[[i]]$text)
            p$x$data[[i]]$text <- gsub("Days", input$indep_var2, p$x$data[[i]]$text)
            p$x$data[[i]]$text <- gsub("Raw", pretty_print_label(mylabel, metadatafile), p$x$data[[i]]$text)
            if (!is.null(input$covar2)) {
              p$x$data[[i]]$text <- gsub("Weight2", input$covar2, p$x$data[[i]]$text)
            }
            if (!is.null(input$covar)) {
               p$x$data[[i]]$text <- gsub("Weight", input$covar, p$x$data[[i]]$text)
            }
        }
      }

      # plotly does not respect outlier aesthetics from geom_boxplot:
      # per default get rid of the outlier marks by geom_boxplot.
      # but let the user decide if he wishes to highlight the outliers
      number_levels <- length(levels(df$group))
      if (input$test_statistic == "2-way ANOVA") {
         number_levels <- length(levels(df$Days))
      }
      for (index in 1:number_levels) {
      if (input$show_outliers_from_plot == FALSE) {
         # TODO: Actually one should remove the outliers here manually
         #, i.e. in the lapply statement, for a specific point of y value
         # remove the point altogether. 
         # 1. Find index (i) of y-value with value e.g. of 33 in p$x$data[index]'s list x
         # 2. Remove this value in list x, also remove value at index i from list y
         p$x$data[index] <- lapply(p$x$data[index], function(x) {
            x$marker = list(opacity = 0)
            x$marker$line = list(color = "blue")
            return(x)
         })
      } else {
          p$x$data[index] <- lapply(p$x$data[index], function(x) {
            x$marker = list(opacity = 1)
            x$marker$line = list(color = "blue")
            return(x)
         })
      }
      }
      }
      # Use plotlyProxy(...) for the plot, and apply plotlyProxyInvoke("restyle", y=modified_y_values_without_value_33)
      # p$x$data <- p$x$data
      p
	})

	# required to keep the currently selected indep_var
	observeEvent(input$indep_var, {
		storeSession(session$token, "selected_indep_var", input$indep_var, global_data)
		updateSelectInput(session, "indep_var", choices=choices, selected = getSession(session$token, global_data)[["selected_indep_var"]])
	})

	output$plot_statistics_details2 <- renderPlotly({
		p <- do_ancova_alternative(input_df, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, dep_var, input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates, input$connected_or_unconnected, input$lm_or_glm, input$sort_factors_alphabetically_decreasing)$plot_summary2 
      p <- p + xlab(pretty_print_label(input$covar2, input$metadatafile)) 
      p <- p + ylab(pretty_print_variable(mylabel, input$metadatafile)) 
		p <- p + ggtitle(input$study_description)

		if (!input$auto_scale_rmr_plot_limits_x2) {
			p <- p + xlim(c(input$x_min_rmr_plot2, input$x_max_rmr_plot2))
		}

		if (!input$auto_scale_rmr_plot_limits_y2) {
			p <- p + ylim(c(input$y_min_rmr_plot2, input$y_max_rmr_plot2))
		}

		p <- p + ggtitle(input$study_description) 
		p <- ggplotly(p) %>% config(displaylogo = FALSE, 
				modeBarButtons = list(c("toImage", get_new_download_buttons("plot_statistics_details2")), 
				list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), 
				list("hoverClosestCartesian", "hoverCompareCartesian")))

      # Note: Data frame contains as dep var always TEE, so we need to modify this. 
      # TODO: Better: Construct data frame always with the correct dependent variable
      for (i in seq_along(p$x$data)) {
         if (!is.null(p$x$data[[i]]$text)) {
            p$x$data[[i]]$text <- gsub("TEE", pretty_print_label(mylabel, metadatafile), p$x$data[[i]]$text)
            p$x$data[[i]]$text <- gsub("Days", input$indep_var2, p$x$data[[i]]$text)
            p$x$data[[i]]$text <- gsub("Raw", pretty_print_label(mylabel, metadatafile), p$x$data[[i]]$text)
            if (!is.null(input$covar2)) {
              p$x$data[[i]]$text <- gsub("Weight2", input$covar2, p$x$data[[i]]$text)
            }
            if (!is.null(input$covar)) {
               p$x$data[[i]]$text <- gsub("Weight", input$covar, p$x$data[[i]]$text)
            }
         }
      }

      # plotly does not respect outlier aesthetics from geom_boxplot
      # outliers assumed on layer 1
      if (input$show_outliers_from_plot == TRUE) {
         p$x$data[1] <- lapply(p$x$data[1], function(x) {
            x$marker = list(opacity = 0)
            return(x)
         })
      } else {
          p$x$data[1] <- lapply(p$x$data[1], function(x) {
            x$marker = list(opacity = 1)
            return(x)
         })
      }
      p
	})

	output$details <- renderUI({
		results <- do_ancova_alternative(input_df, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, dep_var, input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
		tagList(
			h3("Post-hoc analysis"),
			plotlyOutput("post_hoc_plot"),
			hr(style = "width: 75%"),
			h4("Results of statistical testing"),
         checkboxInput("first_or_second_factor_for_2_way_analysis", "Use second factor", value=TRUE),
         tags$script(HTML("$('#first_or_second_factor_for_2_way_analysis').prop('disabled', true);")),
			tags$table(
				tags$thead(
					tags$tr(
					tags$th("p-value", style="width: 100px"),
					tags$th("p-value (adjusted)", style="width: 100px"),
					tags$th("significance level", style="width: 100px"),
					tags$th("degrees of freedom", style="width: 100px" ),
					tags$th("test statistic", style="width: 100px"),
					tags$th("group comparison", style="width: 100px")
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
					tags$th("Description", style="width:250px"),
					tags$th("Name of significance test", style="width:200px"),
					tags$th("p-value", style="width:100px"),
					tags$th("Status", style="width:200px")
					)
				),
				tags$tbody(
					tags$tr(
					tags$td("Homogeneity of variances", style="width:250px"),
					tags$td("Levene's test", style="width:200px"),
					tags$td(round(as.numeric(results$levene$p), digits=6), style="width:100px"),
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
					tags$td("Normality of residuals", style="width:250px"),
					tags$td("Shapiro-Wilk test", style="width:200px"),
					tags$td(round(as.numeric(results$shapiro$p.value), digits=6), style="width:100px"),
					tags$td(
						if (as.numeric(results$shapiro$p.value) < input$alpha_level) {
							icon("times")
						} else {
							icon("check")
						}
					,style="width: 200px"
					)
					),
               tags$tbody(
               tags$tr(
                  tags$td("Homogeneity of regression slopes", style="width:250px"),
                  tags$td("ANOVA on interaction terms", style="width:200px"),
                  tags$td(round(as.numeric(results$regression_slopes), digits=6), style="width:100px"),
                  tags$td(
                     if (as.numeric(results$regression_slopes) < input$alpha_level) {
                        icon("times")
                     } else {
                        icon("check")
                     }
                  ,style="width: 200px"
                  )
                  )
               )
            )
			)
		)
	})

	# FIXME: Optimization - results is calculated multiple times, in fact only once should be necessary... optimize this.
	output$post_hoc_plot <- renderPlotly({
		results <- do_ancova_alternative(input_df, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, dep_var, input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates, input$connected_or_unconnected)
		p <- results$plot_details + xlab(input$indep_var2) + ylab("estimated marginal mean") + labs(colour=input$indep_var)
      if (input$test_statistic == '1-way ANOVA' || input$test_statistic == '1-way ANCOVA') {
         p <- p + xlab(input$indep_var)
      }
		ggplotly(p) %>% config(displaylogo = FALSE, 
			modeBarButtons = list(c("toImage", get_new_download_buttons("post_hoc_plot")), 
			list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), 
			list("hoverClosestCartesian", "hoverCompareCartesian")))
	})
}


################################################################################
#' filter_for_days_and_animals
#' 
#' This methods adds UI elements for filtering for days and animals
#' @param input
#' @param session
#' @param output
#' @param df
#' @param global_data
################################################################################
add_filtering_for_days_and_animals <- function(input, session, output, df, global_data) {
	selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
	if (!is.null(selected_animals)) {
		df <- df %>% filter(`Animals` %in% selected_animals)
		updateSelectInput(session, "select_animal", selected=selected_animals)
	} else {
		output$select_animal <- renderUI({
			selectInput("select_animal", "Select animals(s):", choices = unique(df$Animals), selected = unique(df$Animals), multiple = TRUE)
		})
		storeSession(session$token, "selected_animals", unique(df$Animals), global_data)
	}

	selected_days <- getSession(session$token, global_data)[["selected_days"]]
	if (!is.null(selected_days)) {
		df <- df %>% filter(`Days` %in% selected_days)
		updateSelectInput(session, "select_day", selected=selected_days)
	} else {
		storeSession(session$token, "selected_days", unique(df$Days), global_data)
		output$select_day <- renderUI({
			selectInput("select_day", "Select days(s):", choices = unique(df$Days), selected = unique(df$Days), multiple = TRUE)
		})
	}
   return(df)
}

################################################################################
#' get_metadata datapath
#' 
#' This function gets the datapath (file) either from the UI by user input or
#' loads automatically the datapath from example data sets (also issued by the
#' user). Note that in Shiny R fileInput(...) elements are read-only, thus can't
#' be directly modified and we do have to workaround this. (Note that read-only
#' is intentional, since datapath are temporary files on disk in /tmp directory)
#' 
#' @param input shiny input
#' @param session shiny session
#' @param global_data global data
################################################################################
get_metadata_datapath <- function(input, session, global_data) {
   use_example_data <- getSession(session$token, global_data)[["use_example_data"]]
   datapath <- input$metadatafile$datapath
   if (use_example_data) {
      example_data_set <- getSession(session$token, global_data)[["example_data_single"]]
      if (example_data_set) {
         name_of_example_data <- getSession(session$token, global_data)[["example_data_single_name"]]
         if (name_of_example_data == "UCP1KO") {
            datapath <- paste(Sys.getenv(c("SHINY_DATA_FOLDER")), "example_data/UCP1KO/example_metadata_1.xlsx", sep="")
         } 

         if (name_of_example_data == "DAKO") {
            datapath <- paste(Sys.getenv(c("SHINY_DATA_FOLDER")), "example_data/DAKO/example_metadata_2.xlsx", sep="")
         } 

      }
   }
   return(datapath)
}

################################################################################
#' style_plot
#' 
#' This function is used to stylize the plotting result in the UI
#' @param p plot object
#' @param input shiny input
################################################################################
style_plot <- function(p, input) {
      # TODO: Configure the plot a-priori, i.e. before we convert to a plotly object with ggplotly
      # Usually it is easier to customize the ggplot object before converting it to a plotly object
      if (input$stylize_plot) {
         p <- p %>% layout(xaxis = list(title=input$stylize_plot_axes_x_axis_label, tickfont = list(size=input$stylize_plot_axes_x_axis_tickfont_size, color=input$stylize_plot_axes_x_axis_color), font = list(size=input$stylize_plot_axes_x_axis_font_size, color=input$stylize_plot_axes_x_ticks_color)))
         p <- p %>% layout(yaxis = list(title=input$stylize_plot_axes_y_axis_label, tickfont = list(size=input$stylize_plot_axes_y_axis_tickfont_size, color=input$stylize_plot_axes_y_axis_color), font = list(size=input$stylize_plot_axes_y_axis_font_size, color=input$stylize_plot_axes_y_ticks_color)))
         p <- p %>% layout(title = list(text=input$stylize_plot_theme_and_title_title_label, font = list(input$stylize_plot_theme_and_title_title_font_size, color=input$stylize_plot_theme_and_title_title_color)))
         p <- p %>% layout(font = list(family=input$stylize_plot_general_font_family, size=input$stylize_plot_theme_and_title_font_size))
         p <- p %>% layout(width = input$stylize_plot_general_width, height=input$stylize_plot_general_height)
      }
      return(p)
}

################################################################################
#' indicate_plot_rendered
#' 
#' Indicate if plot has been rendered already or not
#' @param p
#' @param output
################################################################################
indicate_plot_rendered <- function(p, output) {
      output$plotRendered <- reactive({
         !is.null(p)
      })
      outputOptions(output, "plotRendered", suspendWhenHidden = FALSE)
}

################################################################################
#' generate_statistical_table
#' 
#' This function generate statistical table in case we have multiple comparisons
#' @param results
################################################################################
generate_statistical_table <- function(results) {
   group_info <- results$statistics
   group_info <- group_info %>% mutate(comparison = paste0("(", group1, ", ", group2, ")"))
   if (length(results$statistics$p) == 1) { # only one comparison, e.g. WT vs KO, i.e. only one row in table
      return(tags$tr(
      tags$td(process_return_value_for_statistic(results$statistics$p, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$p.adj, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$p.adj.signif, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$df, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$statistic, FALSE), style="width: 100px"),
      tags$td(group_info$comparison)
      ))
   } else { # for multiple testing with ANCOVA, we need all (n over p) groups to be summarized in a table with multiple rows
      num_rows <- length(results$statistics$p)
      rows_p_value <- lapply(seq_along(results$statistics$p), function(i) {
         tags$tr(
            tags$td(process_return_value_for_statistic(results$statistics$p[i], FALSE)),
            tags$td(process_return_value_for_statistic(results$statistics$p.adj[i], FALSE)),
            tags$td(process_return_value_for_statistic(results$statistics$p.adj.signif[i], FALSE)),
            tags$td(process_return_value_for_statistic(results$statistics$df[i], FALSE)),
            tags$td(process_return_value_for_statistic(results$statistics$statistic[i], FALSE)),
            tags$td(paste0("(", results$statistics$group1[i], ", ", results$statistics$group2[i], ")"))
         )
      })
      return(rows_p_value)
   }
}

################################################################################
#' process_return_value_for_statistic
#' 
#' Helper method to return value for statistic after calculation, e.g. pretty
#' print the digits and round
#' @param value
#' @param as_ui
################################################################################
process_return_value_for_statistic <- function(value, as_ui=TRUE) {
   if (is.numeric(value) && length(value) == 1) {
      return(round(value, digits=6))
   } else {
      # create UI if requested
      if (is.numeric(value)) {
         if (as_ui) {
               rows <- lapply(seq_along(value), function(i) {
               tags$tr(
                  tags$td(value[i]))
            })
            return(rows) 
         } else {
           return(paste(round(value, digits=6), collapse=","))
         }
      } else {
         if (as_ui) {
            rows <- lapply(seq_along(value), function(i) {
               tags$tr(
                  tags$td(value[i]))
            })
            return(rows)
         } else {
            return(paste(value, collapse=","))
         }
      }
   }
}

################################################################################
# coarsening data sets
#' coarsen_data_sets
#' 
#' This function coarsens a data frame according to the numerical coarsening factor
#' 
#' @param df data frame
#' @param coarsening_factor coarsening factor
#' @examples 
#' coarsen_data_sets(values, 2)
#' @export 
################################################################################
coarsen_data_sets <- function(df, coarsening_factor) {
   # assumes that the data frame is sorted ascending in time, which should be the case
   df_filtered <- df %>% group_by(`Animal No._NA`) %>% mutate(row_num = row_number()) %>% filter(row_num == 1 | row_num %% coarsening_factor == 0) %>% select(-row_num)
   df_filtered <- df_filtered %>% mutate(diff.sec = diff.sec * coarsening_factor)
   return(df_filtered)
}

################################################################################
# remove zero values
#' remove_zero_values
#'
#' This function removes zero measurement values, based on epsilon threshold
#'
#' @param df data frame
#' @param eps epsilon threshold
#' @examples
#' remove_zero_values(values, 0.0001)
#' @export
################################################################################
remove_zero_values <- function(df, eps) {
   target_columns <- c("O2_[%]", "CO2_[%]", "VCO2_[ml/h]", "VO2_[ml/h]")
   available_columns = intersect(target_columns, colnames(df))
   df_filtered <- df
   if (length(available_columns) > 0) {
      df_filtered <- df %>% filter(if_any(all_of(available_columns), ~abs(.) > eps))
   }
   return(df_filtered)
}

################################################################################
# remove z score outliers
################################################################################
#' remove_z_score_outliers
#'
#' This function removes outliers based on multiples of the standard deviation
#' 
#' @param df data frame
#' @param sd multiple of the standard deviation (default=1)
#' @examples
#' remove_z_score_outliers(values, sd=2)
#' @export
remove_z_score_outliers <- function(df, sd=1) {
   target_columns <- c("O2_[%]", "CO2_[%]", "VCO2_[ml/h]", "VO2_[ml/h]")
   available_columns = intersect(target_columns, colnames(df))
   df_filtered <- df
   if (length(available_columns) > 0) {
      z_scores <- df %>% 
      select(all_of(available_columns)) %>%
      mutate(across(everything(), ~ scale(.) %>% as.vector()))
      df_with_z <- df %>% bind_cols(z_scores %>% setNames(paste0("z_", available_columns)))
      df_filtered <- df_with_z %>% filter(if_any(starts_with("z_"), ~abs(.) < sd))
      df_filtered <- df_filtered %>% select(-starts_with("z_"))
   } 
   return(df_filtered)
}

################################################################################
# enrich with metadata
#' enrich_with_metadata
#' 
#' Enrich data frame with metadata
#' 
#' @param finalC1 data frame
#' @param C1meta metadata from data files in finalC1
#' @param havemetadatafile boolean to indicate if we have structured metadata sheet
#' @param metadatafile structured metadata sheet
#' @examples 
#' enrich_with_metadata(values, metadata_basic, TRUE, "metadata.xlsx")
#' @export
################################################################################
enrich_with_metadata <- function(finalC1, C1meta, havemetadata, metadatafile) {
   df <- finalC1
   if (havemetadata) {
      metadata <- get_true_metadata(metadatafile)
      # fall back to TSE metadata
      if (is.null(metadata)) {
         return(enrich_with_metadata(finalC1, C1meta, FALSE, metadatafile))
      }

      # add some calculated metadata 
      delta_pairs <- list(
         c("bw_start", "bw_end", "delta_bw"),
         c("fm_start", "fm_end", "delta_fm"),
         c("lm_start", "lm_end", "delta_lm")
      )

      add_delta_columns <- function(df, column_pairs) {
         for (pair in column_pairs) {
            start_col <- pair[1]
            end_col <- pair[2]
            delta_col <- pair[3]
            if (all(c(start_col, end_col) %in% names(df)) && !(delta_col %in% names(df))) {
               df <- df %>%
               mutate(!!delta_col := as.numeric(!!sym(end_col)) - as.numeric(!!sym(start_col)))
            }
         }
         return(df)
      }

      metadata <- add_delta_columns(metadata, delta_pairs)
      # instead na.omit() we need to use select to remove columns which are all NA before joining, 
      # this might be because we have columns, e.g. dob which are not present at all in TSE files and also not in the metadata sheet as well
      # or also if not all cohort  files are used with the animal ids recorded in the metadata sheet, or: if
      # the metadata sheet contains nan values... for i.e. lm_start etc.
      # need to remove all nan columns because individual cohorts might not have the same columns
      df <- finalC1 %>% select(where(~ !all(is.na(.)))) %>% full_join(y = metadata, by = c("Animals")) 
   } else {
      empty_row_index <-which(apply(C1meta[,-1], 1, function(row) all(row == "")))
      rows_to_remove <- unique(c(empty_row_index, empty_row_index+1))
      C1meta <- C1meta[-rows_to_remove[rows_to_remove <= nrow(C1meta)], ]
      df_filtered <- C1meta[, colSums(is.na(C1meta)) == 0]
      df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
      df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
      colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
      if ("Animals" %in% colnames(finalC1)) {
         colnames(df_filtered)[colnames(df_filtered) == "Animal.No."] <- "Animals"
      } else {
         colnames(df_filtered)[colnames(df_filtered) == "Animal.No."] <- "Animal No._NA"
      }
      df <- NULL
      if ("Animals" %in% colnames(finalC1)) {
         df <- merge(finalC1, df_filtered, by = "Animals")
      } else {
         df <- merge(finalC1, df_filtered, by = "Animal No._NA")
      }
      colnames(df_filtered)[colnames(df_filtered) == "Animal No._NA"] <- "Animals"
      colnames(df_filtered)[colnames(df_filtered) == "Animal"] <- "Animals"
      df_filtered$Animals <- as.factor(df_filtered$Animals)
      for (col in colnames(df_filtered)) {
         if (col %in% c("Sex", "Diet", "Genotype", "Box", "Box_NA", "Dob")) { # factor columns from TSE standard header
            df_filtered[[col]] = as.factor(df_filtered[[col]])
         } # renaming columns are assumed to be numerical and used as covariates
      }
      metadata <- df_filtered 
      # FIXME: Figure out prime reason why sometimes doubly-joined, potential reason: Happens when
      # switching between panels occassionally? Data needs to be filtered because sometimes we
      # double merge the finalC1, creating duplicates?
      df <- df %>% select(-ends_with(".y")) %>% rename_with(~ sub("\\.x$", "", .), ends_with(".x"))
   }
   return(list("data"=df, "metadata"=metadata))
}


################################################################################
#' detect day night alternative
#' 
#' This method detects Day and Night
#' @param df
#' @param offset
################################################################################
detect_day_night <- function(df, offset) {
   df_day_night <- df
   day_label = "Day"
   night_label = "Night"
    if (offset > 0) {
      day_label = "Night"
      night_label = "Day"
   }

   df_day_night$NightDay <- ifelse(df_day_night$running_total.hrs < offset, day_label, NA)

   day_label = "Day"
   night_label = "Night"
   for (i in 1:nrow(df_day_night)) {
         if (is.na(df_day_night$NightDay[i]) && df_day_night$running_total.hrs[i] > offset) {
            interval_index <- floor((df_day_night$running_total.hrs[i] - offset) / 12)
            df_day_night$NightDay[i] <- ifelse(interval_index %% 2 == 0, day_label, night_label)
         }
   }
   return(df_day_night)
}

################################################################################
#' get global offset for day/night: when (hour) does the very first experiment start
#'
#' This function get's glboal day night offset
#' @param df 
################################################################################
get_global_offset_for_day_night <- function(df) {
   write.csv2(df, "before_getting_global_offset.csv")
   # Note: if this method or the method below is applied again to a filtered or
   # day/night selected data frame, we might not have any row with running_total.sec == 0.
   # thus, care if required when applying this method, make sure to shift the data frame
   # to 0 before, otherwise we will never find a row with running_total.sec == 0.
   offsets <- df %>% group_by(`Animal No._NA`) %>% filter(running_total.sec == 0) %>% select(Datetime, `Animal No._NA`) %>% as.data.frame()
   offsets <- offsets %>% mutate(offset = format(as.POSIXct(Datetime, format="%d/%m/%Y %H:%M"), "%H")) %>% select(offset, `Animal No._NA`)
   return(min(offsets$offset))
}

################################################################################
# convert df to zeitgeber zeit
#' zeitgeber_zeit
#' 
#' Converts the time to zeitgeber zeit
#' @param df data frame
#' @param light_on indicates when day starts (light on typically)
#' @examples 
#' zeitgeber_zeit(values, 8)
#' @export
################################################################################
zeitgeber_zeit <- function(df, light_on) {
   # TODO: v0.5.0 - this needs to be revised, if one want to select indvidual calendrical days,
   # because running_total.sec == 0 will not be foun, also for RMR the following df
   # is grouped and needs to be converted before writing. Why?
   write.csv2(apply(df, 2, as.character), "directly_before_offsets.csv")
   offsets <- df %>% group_by(`Animal No._NA`) %>% filter(running_total.sec == 0) %>% select(Datetime, `Animal No._NA`) %>% as.data.frame()
   offsets <- offsets %>% mutate(offset = format(as.POSIXct(Datetime, format="%d/%m/%Y %H:%M"), "%H")) %>% select(offset, `Animal No._NA`)
   offsets$`offset`  <- as.numeric(offsets$`offset`)
   offsets$offset2 <- offsets$offset - (light_on - offsets$offset)
   offsets$offset3 <- offsets$offset - light_on

   offsets <- offsets %>% unique()
   df_joined <- df %>% left_join(offsets, by = "Animal No._NA")
   df_joined <- df_joined %>% mutate(running_total.hrs = running_total.hrs + offset3)
   df_joined <- df_joined %>% mutate(running_total.hrs.halfhour = running_total.hrs.halfhour + offset3)
   return(df_joined)
}

################################################################################
#' pretty print variable 
#' @param equation
################################################################################
pretty_print_equation <- function(equation) {
   pretty_equation <- gsub("Heldmaier1", "Heldmaier #1", equation)
   pretty_equation <- gsub("Heldmaier2", "Heldmaier #2", pretty_equation)
   return(pretty_equation)
}


################################################################################
# 'pretty print variable 
#' @param variable
#' @param metadata
################################################################################
pretty_print_variable <- function(variable, metadata) {
   pretty_variable <- gsub("O2", "O<sub>2</sub>", variable)
   pretty_variable <- gsub("CO2", "CO<sub>2</sub>", pretty_variable)
   pretty_variable <- gsub("\\(3\\)", "", pretty_variable)
   return(pretty_variable)
}

################################################################################
#' pretty print label
#' @param label
#' @param metadata
#' @param ee_unit 
################################################################################
pretty_print_label <- function(label, metadata, ee_unit) {
   # remove underscores 
   pretty_label <- gsub("_", " ", label)
   # these units are fixed by convenience of plotting typically
   pretty_label <- gsub("TEE", paste0("TEE [kJ/day]"), pretty_label)
   pretty_label <- gsub("RMR", paste0("RMR [kJ/day]"), pretty_label)
   pretty_label <- gsub("Glucose oxidation", paste0("Glucose oxidation [ml/h]"), pretty_label)
   pretty_label <- gsub("Fat oxidation", paste0("Fat oxidation [ml/h]"), pretty_label)
   pretty_label <- gsub("Nitrogen oxidation", paste0("Nitrogen oxidation [ml/h]"), pretty_label)
   pretty_label <- gsub("Protein oxidation", paste0("Protein oxidation [ml/h]"), pretty_label)
   pretty_label <- gsub("HP", paste0("Energy expenditure [kJ/day]"), pretty_label)
   # get relevant data from metadata
   if (!is.null(metadata)) {
      metadata <- get_covariates_and_units(metadata)
      if (nrow(metadata) > 0) {
         pretty_label <- gsub("body_weight", paste0("body weight [", metadata %>% filter(covariates == "body weight") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("lean_mass", paste0("lean mass [", metadata %>% filter(covariates == "lean_mass") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("fat_mass", paste0("fat mass [", metadata %>% filter(covariates == "fat_mass") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("Age", paste0("Age [", metadata %>% filter(covariates == "age") %>% pull("units_values"), "]"), pretty_label)
      } else {
         # TSE header assumes body weight always in grams [g]
         pretty_label <- gsub("body weight", paste0("body weight [", "g", "]"), pretty_label)
         pretty_label <- gsub("lean mass", paste0("lean mass [", "g", "]"), pretty_label)
         pretty_label <- gsub("fat mass", paste0("fat mass [", "g", "]"), pretty_label)
      }
   } else {
      # TSE header assumes body weight always in grams [g]
      pretty_label <- gsub("body weight", paste0("body weight [", "g", "]"), pretty_label)
      pretty_label <- gsub("lean mass", paste0("lean mass [", "g", "]"), pretty_label)
      pretty_label <- gsub("fat mass", paste0("fat mass [", "g", "]"), pretty_label)
   }
   # from TSE header there might be Weight available, rename to have a consistent name
   pretty_label <- gsub("Weight..g", "Weight [g]", pretty_label)
   return(pretty_label)
}

################################################################################
# create annotations for Days on x-axis when using zeitgeber zeit
#' @param df
#' @param light_on
#' @param input_var
#' @param with_facets
################################################################################
annotate_zeitgeber_zeit <- function(df, light_on, input_var, with_facets=FALSE) {
   df_annotated <- df %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
   day_counts <- df_annotated %>% select(`Animal No._NA`, DayCount) %>% unique() %>% na.omit()

   print("there?")
   # we set for animals no ID since we are not interested for now only in the total days of recordings and want to select consecutive 3 days for instance
   annotations <- NULL
   # specify if we wish to use facets or not during annotation
   if (with_facets) {
      animals <- unique(day_counts$`Animal No._NA`)
      for (i in 1:length(animals)) {
         animal = animals[i]
         anno_for_animal = data.frame(Animals = rep(animal, length(sort(unique(day_counts$DayCount)))), x = seq(12+light_on, length(unique(day_counts$DayCount))*24, by=24), y=min(df[[input_var]], na.rm = TRUE), label = sapply(sort(unique(day_counts$DayCount)), function(x) paste0("Day #", x)))
         annotations <- rbind(annotations, anno_for_animal)
      }
   } else {
      annotations = data.frame(Animals = rep(NA, length(sort(unique(day_counts$DayCount)))), x = seq(12+light_on,length(unique(day_counts$DayCount))*24, by=24), y=min(df[[input_var]], na.rm = TRUE), label = sapply(sort(unique(day_counts$DayCount)), function(x) paste0("Day #", x)))
   }
   print("here")
   return(list(df_annotated=df_annotated, annotations=annotations))
}

################################################################################
# get number of days and sample ids to select from alternative
#' @param df
################################################################################
get_days_and_animals_for_select_alternative <- function(df) {
   day_counts <- df %>% pull(DayCount) %>% unique() %>% sort()
   animal_counts <- df %>% pull(`Animal No._NA`) %>% unique() %>% sort()
   return(list(days=day_counts, animals=animal_counts))
}

################################################################################
# get number of days and sample ids to select from in HeatProduction (Data curation)
#' @param df
################################################################################
get_days_and_animals_for_select <- function(df) {
   df_annotated <- df %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
   day_counts <- df_annotated %>% pull(DayCount) %>% unique() %>% sort()
   animal_counts <- df_annotated %>% pull(`Animal No._NA`) %>% unique() %>% sort()
   return(list(days=day_counts, animals=animal_counts))
}

################################################################################
# get factor columns
#' @param df
################################################################################
get_factor_columns <- function(df) {
	return(names(df)[sapply(df, is.factor)])
}

################################################################################
# get non-factor columns
#' @param df
################################################################################
get_non_factor_columns <- function(df) {
   return(names(df)[sapply(df, Negate(is.factor))])
}

################################################################################
# get columns with at least two levels
#' @param df
################################################################################
get_columns_with_at_least_two_levels <- function(df) {
   #columns_with_two_levels <- sapply(df, function(col) is.factor(col) || is.character(col) || length(unique(col)) >= 2)
   columns_with_two_levels_or_more <- names(df)[sapply(df, function(col) is.factor(col) && nlevels(col) >= 2)]
   return(columns_with_two_levels_or_more)
}

################################################################################
# check if at least two cohorts available
#' @param df
#' @param colname
################################################################################
has_cohorts <- function(df, colname="Cohort") {
   if (colname %in% colnames(df)) {
      if (length(unique(df[[colname]]))) {
         return(colname)
      }
   }
}


################################################################################
#' get_new_download_buttons
#' 
#' This function generate new download buttons to get plot as specified by div element
#' @param plot_div
################################################################################
get_new_download_buttons <- function(plot_div='') {
   new_buttons <- list(
      list(
      name = "Download plot as a SVG",
      icon = list(path = svg_path_svg, width=30, height=30),
      click = htmlwidgets::JS("
         function(gd) {
            Plotly.downloadImage(gd, {format: 'svg', filename: 'plot'});
         }
      ")
      ),
      list(
      name = "Download plot as a PDF",
      icon = list(path = svg_path_pdf, width=30, height=30, transform="matrix(0.59119871,0,0,0.48085484,21.328186,-59.600539)"),
      click = htmlwidgets::JS(glue("
         function(gd) {{
            var plotDiv;
            if ('' === '{plot_div}') {{
               // Main plot should always be visible, that is the first plot for each panel, if not specified a desired
               // plot, we will as a default pick the basic plot with plotlyOutput identifier 'plot'
               plotDiv = document.getElementById('plot');
            }} else {{
               plotDiv = document.getElementById('{plot_div}');
            }}
            
            /// Debug, remove in production, alert if plot is not visible with JS
            if (! (plotDiv.offsetWidth > 0 && plotDiv.offsetHeight > 0)) {{
               alert('The plot {plot_div} is not visible!')
            }}

            var width = plotDiv.offsetWidth;
            var height = plotDiv.offsetHeight;
            Plotly.toImage(gd, {{format: 'png', width: width, height: height, scale: 4, compression: 'NONE'}}).then(function(dataUrl) {{
               const {{ jsPDF }} = window.jspdf;
               const doc = new jsPDF('landscape');
               const ARimage = width/height;
               /// internal pagesize should be always around 297 (mm) since landscape a4 format
               var widthPDF = doc.internal.pageSize.getWidth(); // or: 280 and 
               var widthPDFoffset = 17; // to avoid image boundaries out of PDF size
               doc.addImage(dataUrl, 'eps', 10, 10, widthPDF-widthPDFoffset, Math.floor(widthPDF/ARimage)); // or: remove -17 and use 280 above
               doc.save('plot.pdf');
            }})
         }}
      "))
      )
   )
   return(new_buttons)
}
