library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(emmeans)

################################################################################
# do_ancova
################################################################################
do_ancova <- function(df_data, df_metadata) {
  colnames(df_metadata) <- c("Weight", "group", "Animals")

  df <- na.omit(merge(df_data, df_metadata, by = "Animals", all = TRUE))
  df <- df[-grep("HP2", df$Equation), ]
  df <- df %>% select(c("Animals", "group", "Weight", "TEE"))

  df$Animals <- seq(1, length(df$Animals))
  write.csv2(df, "final_data_and_meta.csv")
  df <- read.csv2("final_data_and_meta.csv")

  p2 <- ggscatter(df, x = "Weight", y = "TEE", color = "group", add = "reg.line") 
  p2 <- p2 + stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group))

  res.aov <- df %>% anova_test(TEE ~ Weight + group)
  pwc <- df %>%
    emmeans_test(
      TEE ~ group, covariate = Weight,
      p.adjust.method = "bonferroni"
    )

  pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
  p <- ggline(get_emmeans(pwc), x = "group", y = "emmean") 
  p <- p + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2)
  p <- p + stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE)
  p <- p + labs(subtitle = get_test_label(res.aov, detailed = TRUE), caption = get_pwc_label(pwc))
  return(list("plot_details" = p, "plot_summary" = p2, "statistics" = pwc))
}
