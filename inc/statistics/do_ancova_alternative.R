library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(emmeans)

################################################################################
# do_ancova_alternative
################################################################################
do_ancova_alternative <- function(df_data, df_metadata, indep_var, indep_var2, group, adjust_method = "bonferroni") {
  df <- df_data %>% full_join(y = df_metadata, by = c("Animals")) %>% na.omit()

  if (is.null(indep_var)) {
    indep_var <- "body_weight"
  }

  if (is.null(indep_var2)) {
    indep_var2 <- "body_weight"
  }

  if (is.null(group)) {
    group <- "genotype"
  }

  names(df)[names(df) == group] <- "group"
  names(df)[names(df) == indep_var] <- "Weight"
  names(df)[names(df) == indep_var] <- "Weight2"

  df <- df %>% select(c("Animals", "group", "Weight", "Weight2", "TEE"))
  df$Weight <- as.numeric(df$Weight)
  df$Weight2 <- as.numeric(df$Weight2)
  df$TEE <- as.numeric(df$TEE)

  p2 <- ggscatter(df, x = "Weight", y = "TEE", color = "group", add = "reg.line")
  p2 <- p2 + stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group))
  res.aov <- df %>% anova_test(TEE ~ Weight + group)
  pwc <- df %>%
    emmeans_test(
      TEE ~ group, covariate = c(Weight),
      p.adjust.method = adjust_method
    )

  # Visualization of assumptions
  pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
  p <- ggline(get_emmeans(pwc), x = "group", y = "emmean") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )

  # Fit the model, the covariate goes first
  model <- lm(TEE ~ Weight + group, data = df)
  model.metrics <- augment(model)
  shapiro <- shapiro_test(model.metrics$.resid)
  levene <- model.metrics %>% levene_test(.resid ~ group)
  return(list("plot_details" = p, "plot_summary" = p2, "statistics" = pwc, "shapiro" = shapiro, "levene" = levene))
}
