library(patchwork)
library(DT)
library(ggpubr)
library(car)

body_composition <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
	# Enrich with metadata
	finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))
	data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
	true_metadata <- data_and_metadata$metadata
	# TODO: refactor to use better names
	finalC1 <- true_metadata

	# Basic configuration: Ask for number of covariates and dynamically create selection for factors for each group
	output$test <- renderUI({
		tagList(
			h2("Configuration"),
			sliderInput("how_many_comparisons", "How many covariates to analyze", min=1, max=length(get_non_factor_columns(true_metadata)), value=1, step=1),
			selectInput("test_statistic", "Test statistic", choices=c("ANOVA", "Kruskal-Wallis rank sum test"), selected="ANOVA", multiple=FALSE),
			checkboxInput("check_validity_of_assumptions", "Check validity of test assumptions", value = FALSE),
			uiOutput("selection_sliders"),
			uiOutput("plotOutputs")
		)
	})

	# TODO: should we only allow how_many_compariosns be restricted to groups which have at least 2 levels?
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
							finalC1[[my_var]] <- as.numeric(finalC1[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=finalC1)
							} else {
								anova_result <- kruskal.test(formula, data=finalC1)
							}
							ggplot(finalC1, aes_string(input[[paste0("how_many_for_anova_", i)]][j], input[[paste0("select_group_", i)]])) + geom_boxplot() + theme_minimal() + ggtitle(input[[paste0("how_many_for_anova_", i)]][j]) + stat_compare_means(method="anova", label="p.format")
						})
						output[[paste0("plot_factor_summary_", i, "_group_", j)]] <- renderDT({
							my_var <- input[[paste0("select_group_", i)]]
							finalC1[[my_var]] <- as.numeric(finalC1[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=finalC1)
							} else {
								anova_result <- kruskal.test(formula, data=finalC1)
							}
							anova_summary <- as.data.frame(summary(anova_result)[[1]])
							datatable(anova_summary, options = list(pageLength = 5, autowidth = TRUE), rownames = TRUE) %>% formatStyle(columns=names(anova_summary), color="white", backgroundColor="black")
						})

						output[[paste0("plot_factor_assumption_normality_", i, "_group_", j)]] <- renderPlot({
							my_var <- input[[paste0("select_group_", i)]]
							finalC1[[my_var]] <- as.numeric(finalC1[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=finalC1)
							} else {
								anova_result <- kruskal.test(formula, data=finalC1)
							}

							residuals <- resid(anova_result)
							fitted <- fitted(anova_result)
							# TODO: shapiro can only take 5000 samples maximum, replace with other test.
							shapiro_result <- shapiro.test(residuals(anova_result)[0:5000]) 
							ggplot(data = data.frame(Fitted=fitted, Residuals = residuals), aes(x = Fitted, y=Residuals)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + labs(x="Fitted values", y = "Residuals", title = paste0("Shapiro-Wilk test for normality: p-value = ", shapiro_result$p.value))
						})

						output[[paste0("plot_factor_assumption_residuals_", i, "_group_", j)]] <- renderPlot({
							my_var <- input[[paste0("select_group_", i)]]
							finalC1[[my_var]] <- as.numeric(finalC1[[my_var]])
							anova_result <- NULL
							formula <- as.formula(paste0(input[[paste0("select_group_", i)]], " ~ ", input[[paste0("how_many_for_anova_", i)]][j]))
							if (input$test_statistic == "ANOVA") {
								anova_result <- aov(formula, data=finalC1)
							} else {
								anova_result <- kruskal.test(formula, data=finalC1)
							}

							standardized_residuals <- rstandard(anova_result)
							levene_result <- leveneTest(as.formula(paste0("residuals(anova_result)", "~", input[[paste0("how_many_for_anova_", i)]][j])), data=finalC1)
							ggplot(data = data.frame(StandardizedResiduals = standardized_residuals), aes(sample=StandardizedResiduals)) + stat_qq() + stat_qq_line() + labs(x = "Theoretical quantiles", y = "Standardized residuals", title=paste0("Levene's test for homogenity of variances: p-value = ", levene_result[["Pr(>F)"]][1]))
						})

					})
				}

				# Visualize also an n-way ANOVA in addition to the 1-way ANOVAs
				if (length(input[[paste0("how_many_for_anova_", i)]]) > 1) {
					output[[paste0("n_way_anova_", i)]] <- renderPlot({
						indep_vars <- input[[paste0("how_many_for_anova_", i)]]
						dep_var <- paste0(input[[paste0("select_group_", i)]])
						finalC1[[dep_var]] <- as.numeric(finalC1[[dep_var]])
						anova_formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "*")))
						anova_result <- aov(anova_formula, data=finalC1)
						ggplot(finalC1, aes_string(x = paste("interaction(", paste(indep_vars, collapse=","), ")"), y = dep_var, fill=indep_vars[1])) + geom_boxplot()  + ggtitle(paste(indep_vars, collapse=",")) + stat_compare_means(method="anova", label="p.format")
					})


					# Editable table for n-way ANOVA
					output[[paste0("n_way_anova_summary_", i)]] <- renderDT({
						indep_vars <- input[[paste0("how_many_for_anova_", i)]]
						dep_var <- paste0(input[[paste0("select_group_", i)]])
						finalC1[[dep_var]] <- as.numeric(finalC1[[dep_var]])
						anova_formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "*")))
						anova_result <- aov(anova_formula, data=finalC1)
						anova_summary <- as.data.frame(summary(anova_result)[[1]])
						datatable(anova_summary, options = list(pageLength = 5, autowidth = TRUE), rownames = TRUE) %>% formatStyle(columns=names(anova_summary), color="white", backgroundColor="black")
					})
				}
			})
		}
	})
}