library(lme4)

################################################################################
# linear mixed effect modelling
################################################################################
model_effects <- function(df, dep_var, input) {
	fixed_effects <- sapply(1:(input$how_many_fixed_effects), function(i) input[[paste0("fixed_effect_variable_", i)]])
	random_effects <- sapply(1:(input$how_many_random_effects), function(i) input[[paste0("random_effect_variable_", i)]])
	fixed_effects <- fixed_effects[!is.null(fixed_effects) & fixed_effects != ""] 
	random_effects <- random_effects[!is.null(random_effects) & random_effects != ""] 
	fixed_effects_formula <- paste(fixed_effects, collapse = " + ") 
	random_effects_formula <- paste("(1 |", random_effects, ")", collapse = " + ") 
	formula_string <- paste(dep_var, "~", fixed_effects_formula, "+", random_effects_formula) 
	lmm <- lmer(as.formula(formula_string), data = df)
	fixed_effects_df <- as.data.frame(coef(summary(lmm)))
	random_effects_df <- as.data.frame(VarCorr(lmm))
	return(list("df1" = fixed_effects_df, "df2" = random_effects_df))
}

################################################################################
# visualizing the linear mixed effect modelling results
################################################################################
visualize_model_effects <- function(df, dep_var, input, output) {
	# set up LME model
	fixed_effects <- sapply(1:(input$how_many_fixed_effects), function(i) input[[paste0("fixed_effect_variable_", i)]])
	random_effects <- sapply(1:(input$how_many_random_effects), function(i) input[[paste0("random_effect_variable_", i)]])
	fixed_effects <- fixed_effects[!is.null(fixed_effects) & fixed_effects != ""] 
	random_effects <- random_effects[!is.null(random_effects) & random_effects != ""] 
	fixed_effects_formula <- paste(fixed_effects, collapse = " + ") 
	random_effects_formula <- paste("(1 |", random_effects, ")", collapse = " + ") 
	formula_string <- paste(dep_var, "~", fixed_effects_formula, "+", random_effects_formula) 
	lmm <- lmer(as.formula(formula_string), data = df)

	# calculation of metrics
	df$Fitted <- fitted(lmm)
	rss <- sum((df[[dep_var]] - df$Fitted)^2)
	tss <- sum((df[[dep_var]] - mean(df[[dep_var]]))^2)
	# traditional R-squared
	r2 <- 1 - (rss / tss)
	output$r_squared_output <- renderText(paste("R²: ", r2))
	# model selection criterions
	output$AIC_value <- renderText(paste("AIC: ", AIC(lmm)))
	output$BIC_value <- renderText(paste("BIC: ", BIC(lmm)))
	# marginal (fixed effect contributions) and conditional (random effect contributions)
	marginal_and_conditional <- performance::r2(lmm)
	marginal = marginal_and_conditional$R2_marginal
	conditional = marginal_and_conditional$R2_conditional
	marginal_and_conditional <- paste("Marginal R²: ", round(marginal, 4), "| Conditional R²:", round(conditional, 4))
	output$marginal_and_conditional_r_squared <- renderText(marginal_and_conditional)
	# diagnostic plot of fitted vs. dependent var
	p <- ggplot(df, aes_string(x="Fitted", y=dep_var)) + geom_point()
	p <- p + geom_abline(slope=1, intercept=0, linetype="dashed", color="red")
	return(p)
}

################################################################################
# create UI for modelling
################################################################################
create_lme_model_ui <- function(input, output, true_metadata, df_to_plot) {
	output$modelling <- renderUI({
		tagList(
			h4("Modelling Raw measurements with LME model"),
			sliderInput("how_many_fixed_effects", "How many fixed effect variables", min=1, max=length(get_factor_columns(true_metadata)), value=1, step=1),
			sliderInput("how_many_random_effects", "How many random effect variables", min=1, max=length(get_non_factor_columns(true_metadata)), value=1, step=1),
			uiOutput("selection_sliders_fixed"),
			uiOutput("selection_sliders_random"),
			renderPlot(visualize_model_effects(df_to_plot, input$myr, input, output)),
			verbatimTextOutput("r_squared_output"),
			h5("Additional metrics"),
			verbatimTextOutput("AIC_value"),
			verbatimTextOutput("BIC_value"),
			verbatimTextOutput("marginal_and_conditional_r_squared"),
			h5("Summary tables"),
			renderDT({
				datatable(model_effects(df_to_plot, input$myr, input)$df1, options=list(pageLength=5), caption = "Fixed effects") %>% formatStyle(columns=names(model_effects(df_to_plot, input$myr, input)$df1), color='white', backgroundColor = 'black')
			}),
			renderDT({
				datatable(model_effects(df_to_plot, input$myr, input)$df2, options=list(pageLength=5), caption = "Random effects") %>% formatStyle(columns=names(model_effects(df_to_plot, input$myr, input)$df2), color='white', backgroundColor = 'black')
			})
		)
	})

	output$selection_sliders_fixed <- renderUI({
		n <- input$how_many_fixed_effects
		selectInputList <- lapply(1:n, function(i) {
			list(
				selectInput(inputId = paste0("fixed_effect_variable_", i), label = paste0("Select fixed effect variable #", i), selected = "Weight..g.", choices = get_non_factor_columns(true_metadata))
			)
		})
		do.call(tagList, selectInputList)
	})

	output$selection_sliders_random <- renderUI({
		n <- input$how_many_random_effects
		selectInputList <- lapply(1:n, function(i) {
			list(
				selectInput(inputId = paste0("random_effect_variable_", i), label = paste0("Select random effect variable #", i), selected = "Weight..g.", choices = get_non_factor_columns(true_metadata))
			)
		})
		do.call(tagList, selectInputList)
	})
}