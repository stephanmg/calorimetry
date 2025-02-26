library(patchwork)
library(RColorBrewer)
library(DT)
library(ggpubr)

# Note: library usage of car breaks RMR calculations from *this* file... Reason?
# Recode (from dplyr) hidden by car::recode and (from purr) hidden by car::some. 
# Thus we don't use of library(car), but we use double colon for absolute scoping

################################################################################
#' col_name_pretty
#' 
#' This function prettifies the column name of metadata
#' @param col_name
#' @examples
#' col_name("some_col_name")
#' @export
################################################################################
col_name_pretty <- function(col_name) {
	return(switch(col_name,
		lm_start = "Lean mass (Start) [g]",
		lm_end = "Lean mass (End) [g]",
		fm_start = "Fat mass (Start) [g]",
		fm_end = "Fat mass (End) [g]",
		bw_start = "Body weight (Start) [g]",
		bw_end = "Body weight (End) [g]",
		Animals = "Animal ID",
		Diet = "Diet",
		Genotype = "Genotype",
		Sex = "Sex",
		Age = "Age [wks]",
		delta_bw = "Delta Body weight [g]",
		delta_fm = "Delta Fat mass [g]",
		delta_lm = "Delta Lean mass [g]",
		col_name
	))
}

################################################################################
#' body_composition
#' 
#' This function generates and overview of body composition and other recorded metadata
#' 
#' @param body_comp_df input data
#' @param tse_metadata combined metadata
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param global_data dictionary to store variables session-based for users
#' @param scaleFactor used to scale energy expenditure units correctly
#' @examples 
#' body_composition(values, full_metadata, input, output, session, global_data, 1)
#' @export
################################################################################
body_composition <- function(body_comp_df, tse_metadata, input, output, session, global_data, scaleFactor) {
	# Enrich with metadata
	body_comp_df$Animals <- as.factor(`$`(body_comp_df, "Animal No._NA"))
	data_and_metadata <- enrich_with_metadata(body_comp_df, tse_metadata, input$havemetadata, get_metadata_datapath(input, session, global_data))
	true_metadata <- data_and_metadata$metadata
	body_comp_df <- true_metadata
	# Basic configuration: Ask for number of covariates and dynamically create selection for factors for each group
	output$test <- renderUI({
		tagList(
			h2("Configuration"),
			sliderInput("how_many_comparisons", "How many covariates to analyze", min=1, max=length(get_non_factor_columns(true_metadata)), value=1, step=1),
			selectInput("test_statistic", "Test statistic", choices=c("ANOVA", "Kruskal-Wallis rank sum test"), selected="ANOVA", multiple=FALSE),
			checkboxInput("check_validity_of_assumptions", "Check validity of test assumptions", value = FALSE),
			uiOutput("selection_sliders"),
			uiOutput("plotOutputs"),
			tags$style(HTML("
			table.dataTable tbody td {
				background-color: white !important;
				color: black !important
			}
			table.dataTable thead td {
				color: white !important;
			}

			table.dataTable thead tr {
				color: white !important;
			}

			table.dataTable tfoot th {
				color: white !important;
			}

			.dataTables_wrapper .dataTables_paginate .paginate_button {
				color: white !important;
			}

			.dataTables_wrapper .dataTables_info {
				color: white !improtant;
			}"
			)
			)
		)
	})

	# selection fields for factors for each group
	output$selection_sliders <- renderUI({
		n <- input$how_many_comparisons
		selectInputList <- lapply(1:n, function(i) {
			list(
				selectInput(inputId = paste0("select_group_", i), label = paste0("Select covariate #", i), selected = "Weight..g.", choices = get_non_factor_columns(true_metadata)),
				selectInput(inputId = paste0("how_many_for_anova_", i), label = paste0("Which groups?"), multiple=TRUE, selected = "Genotype", choices=get_factor_columns(true_metadata))
			)
		})
		do.call(tagList, selectInputList)
	})

	# Plot corresponding 1-way ANOVA and multi-way ANOVAs
	output$plotOutputs <- renderUI({
		n <- input$how_many_comparisons
		m <- length(input$how_many_for_anova)
		plotOutputList <- lapply(1:n, function(i) {
			m <- input[[paste0("how_many_for_anova_", i)]]
			n_way_anova <- list()

			if (length(m) > 1) {
				n_way_anova <- list(
					h4(paste0("Corresponding ", input[[paste0("stratification_for_anova_", i)]], "-way ANOVA")),
					plotOutput(outputId = paste0("n_way_anova_", i)),
					DTOutput(outputId = paste0("n_way_anova_summary_", i)),
					hr(style="width: 75%")
				)
			}

			list(
				h3(paste0("Covariate: ", input[[paste0("select_group_", i)]])),
				# Note: We fix this component, as it makes only sense to include all factors in combined interaction ANOVA
				sliderInput(inputId = paste0("stratification_for_anova_", i), label = paste0("n-way ANOVA"), min=1, step=1, max=length(m), value=length(m)),
				h4("Basic comparison of quantity"),
				lapply(1:length(m), function(j) {
					list(
						plotOutput(outputId = paste0("plot_factor_", i, "_group_", j)),
						h4("1-way ANOVA summary table"),
						DTOutput(outputId = paste0("plot_factor_summary_", i, "_group_", j)),
						conditionalPanel("input.check_validity_of_assumptions == true", 
							h4("Residuals vs Fitted"),
							plotOutput(outputId = paste0("plot_factor_assumption_normality_", i, "_group_", j))
						),
						hr(style="width: 50%"),
						conditionalPanel("input.check_validity_of_assumptions == true", 
							h4("Q-Q plot of residuals"),
							plotOutput(outputId = paste0("plot_factor_assumption_residuals_", i, "_group_", j))
						),
						hr(style="width: 50%")
					)
				}),
				n_way_anova
			)
		})

		do.call(tagList, plotOutputList)
	})
	
	# Observe changes in UI components and plot
	observe({
		n <- input$how_many_comparisons
		if (!is.null(n)) {
			lapply(1:n, function(i) {
				m <- input[[paste0("how_many_for_anova_", i)]]
				if (!is.null(m)) {
					lapply(1:length(m), function(j) {
						output[[paste0("plot_factor_", i, "_group_", j)]] <- renderPlot({
							my_var <- input[[paste0("select_group_", i)]]
							body_comp_df[[my_var]] <- as.numeric(body_comp_df[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=body_comp_df)
							} else {
								anova_result <- kruskal.test(formula, data=body_comp_df)
							}
							combinations <- combn(levels(body_comp_df[[input[[paste0("how_many_for_anova_", i)]][j]]]), m = 2, simplify = FALSE)
							ggplot(body_comp_df, aes_string(input[[paste0("how_many_for_anova_", i)]][j], input[[paste0("select_group_", i)]], fill=input[[paste0("how_many_for_anova_", i)]][j])) + geom_boxplot() + theme_minimal() + ggtitle(input[[paste0("how_many_for_anova_", i)]][j]) + stat_compare_means(comparisons = combinations, method="t.test", label="p.signif", bracket.size = 0.5, step.increase=0.1, tip.length=0.01, aes(label=paste0("p = ", ..p.format.., ", ", ..p.signif..)))
						})
						output[[paste0("plot_factor_summary_", i, "_group_", j)]] <- renderDT({
							my_var <- input[[paste0("select_group_", i)]]
							body_comp_df[[my_var]] <- as.numeric(body_comp_df[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=body_comp_df)
							} else {
								anova_result <- kruskal.test(formula, data=body_comp_df)
							}
							anova_summary <- as.data.frame(summary(anova_result)[[1]])
							datatable(anova_summary, options = list(pageLength = 5, autowidth = TRUE), rownames = TRUE) %>% formatStyle(columns=names(anova_summary), color="white", backgroundColor="black")
						})

						output[[paste0("plot_factor_assumption_normality_", i, "_group_", j)]] <- renderPlot({
							my_var <- input[[paste0("select_group_", i)]]
							body_comp_df[[my_var]] <- as.numeric(body_comp_df[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=body_comp_df)
							} else {
								anova_result <- kruskal.test(formula, data=body_comp_df)
							}

							residuals <- resid(anova_result)
							fitted <- fitted(anova_result)
							# FIXME - Shapiro can only take 5000 samples at maximum, replace with other test
							# Replace with nortest ad.test(data$variable) or 
							# Anderson Darling test (gives more weight to tails) or
							# Cramer von Mises (prefered since it asseses the entire distribution equally/comprehensively tails + centers) or
							# Lilliefors (a non-parametric approach). Maybe make this also option to let the user chose from.
							# For smaller data sets can use simple Shapiro or KS-test, note that Shapiro_Wilk is prefered here
							shapiro_result <- shapiro.test(residuals(anova_result)[0:5000]) 
							ggplot(data = data.frame(Fitted=fitted, Residuals = residuals), aes(x = Fitted, y=Residuals)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + labs(x="Fitted values", y = "Residuals", title = paste0("Shapiro-Wilk test for normality: p-value = ", shapiro_result$p.value))
						})

						output[[paste0("plot_factor_assumption_residuals_", i, "_group_", j)]] <- renderPlot({
							my_var <- input[[paste0("select_group_", i)]]
							body_comp_df[[my_var]] <- as.numeric(body_comp_df[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=body_comp_df)
							} else {
								anova_result <- kruskal.test(formula, data=body_comp_df)
							}

							standardized_residuals <- rstandard(anova_result)
							levene_result <- leveneTest(as.formula(paste0("residuals(anova_result)", "~", input[[paste0("how_many_for_anova_", i)]][j])), data=body_comp_df)
							ggplot(data = data.frame(StandardizedResiduals = standardized_residuals), aes(sample=StandardizedResiduals)) + stat_qq() + stat_qq_line() + labs(x = "Theoretical quantiles", y = "Standardized residuals", title=paste0("Levene's test for homogenity of variances: p-value = ", levene_result[["Pr(>F)"]][1]))
						})

					})
				}

				# Visualize also an n-way ANOVA in addition to the 1-way ANOVAs
				if (length(input[[paste0("how_many_for_anova_", i)]]) > 1) {
					output[[paste0("n_way_anova_", i)]] <- renderPlot({
						indep_vars <- input[[paste0("how_many_for_anova_", i)]]
						dep_var <- paste0(input[[paste0("select_group_", i)]])
						body_comp_df[[dep_var]] <- as.numeric(body_comp_df[[dep_var]])
						anova_formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "*")))
						anova_result <- aov(anova_formula, data=body_comp_df)

						# combinations: we need to first create all possible combinations of all levels of each factor
						combinations <- lapply(indep_vars, function(f) levels(body_comp_df[[f]]))
						combinations <- expand.grid(combinations)
						combinations$combined <- apply(combinations, 1, paste, collapse = ",")

						if (length(indep_vars) == 2) {
							# then we need to create for a 2-way ANOVA all pairwise grouped pairs, i.e. ("A,D" vs "B,C")
							body_comp_df$Combined <- interaction(body_comp_df[, indep_vars], sep=",")
							existing_combos <- unique(body_comp_df$Combined)
							available_combinations <- combinations[combinations$combined %in% existing_combos, ]

							combined_levels <- available_combinations$combined
							combinations <- combn(combined_levels, 2, simplify = FALSE)
							# Note: we need to use the Combined column from body_comp_df, since we did find only VALID pairs for the statistical annotation
							ggplot(body_comp_df, aes_string(x = "Combined", y = dep_var, fill=indep_vars[1])) + geom_boxplot()  + ggtitle(paste(indep_vars, collapse=",")) + stat_compare_means(comparisons = combinations, method="t.test", label="p.format", bracket.size = 0.5, step.increase=0.1, tip.length=0.01)
						} else if (length(indep_vars) == 3) {
							# FIXME - Add statistics, comparison groups need to be created differently then for 3-way ANOVA
							ggplot(body_comp_df, aes(x=indep_vars[2], y=dep_var, fill=indep_vars[1])) + geom_boxplot() + facet_grid(as.formula(paste0(". ~ ", indep_vars[3]))) + ggtitle(paste(indep_vars, collapse=",")) 
						} else if (length(indep_vars) == 4) {
							# FIXME - Add statistics, comparions groups need to be created differently then for 4-way ANOVA
							ggplot(body_comp_df, aes(x=indep_vars[2], y=dep_var, fill=indep_vars[1])) + geom_boxplot() + facet_grid(as.formula(paste0(indep_vars[4], " ~ ", indep_vars[3]))) + ggtitle(paste(indep_vars, collapse=",")) 
						} else { # general >= 5-way ANOVA, consider here to implement interaction plots rather than visualizations
							# Higher than 5-way ANOVA we will not be supported with visualizations other than interaction plots.
							# FIXME - Add visualization with estimated marginal means, aka interaction plots as in our ANCOVA statistics panel
							ggplot(body_comp_df, aes_string(x = paste("interaction(", paste(indep_vars, collapse=","), ")"), y = dep_var, fill=indep_vars[1])) + geom_boxplot()  + ggtitle(paste(indep_vars, collapse=",")) 
						}
					})

					# Editable table for n-way ANOVA
					output[[paste0("n_way_anova_summary_", i)]] <- renderDT({
						indep_vars <- input[[paste0("how_many_for_anova_", i)]]
						dep_var <- paste0(input[[paste0("select_group_", i)]])
						body_comp_df[[dep_var]] <- as.numeric(body_comp_df[[dep_var]])
						anova_formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "*")))
						anova_result <- aov(anova_formula, data=body_comp_df)
						anova_summary <- as.data.frame(summary(anova_result)[[1]])
						datatable(anova_summary, options = list(pageLength = 5, autowidth = TRUE), rownames = TRUE) %>% formatStyle(columns=names(anova_summary), color="white", backgroundColor="black")
					})
				}
			})
		}
	})

	colors = brewer.pal(ncol(true_metadata), "Set3") # has 12 colors, will need to extend otherwise
	if (ncol(true_metadata) > 12) { colors <- colorRampPalette(colors)(ncol(true_metadata)) }
	# Create overview of available metadata
	plots <- lapply(order(names(true_metadata)), function(i) {
		col_name <- names(true_metadata)[i]
		if (is.numeric(true_metadata[[col_name]])) {
			p <- ggplot(true_metadata, aes_string(y=col_name)) + geom_violin(aes_string(x=col_name), fill=colors[i]) + ylab("Density") + xlab(col_name)
		} else if (col_name == "lm_end" || col_name == "lm_start" || col_name == "fm_end" || col_name == "fm_start" || col_name == "bw_start" || col_name == "bw_end") {
			true_metadata[[col_name]] = as.numeric(true_metadata[[col_name]])
			p <- ggplot(true_metadata, aes_string(x=col_name)) + geom_density(fill=colors[i]) + ylab("Density") + xlab(col_name)
		} else {
			p <- ggplot(true_metadata, aes_string(x=col_name)) + geom_bar(fill=colors[i], color="black") 
		}
		p <- p + theme(axis.title.x = element_text(size=10, face="bold"))
		p <- p + theme(axis.title.y = element_text(size=10, face="bold"))
		p <- p + xlab(col_name_pretty(col_name))
		p <- ggplotly(p) 
	})

	combined_plot <- subplot(plots, nrows = as.integer(ncol(true_metadata)/3)+1, margin = 0.05, shareX = FALSE, shareY=FALSE, titleX = TRUE, titleY=TRUE) 
	combined_plot <- combined_plot %>% layout(height=1000)
	combined_plot <- ggplotly(combined_plot) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	storeSession(session$token, "plot_for_metadata", combined_plot, global_data)
	return(combined_plot)
}