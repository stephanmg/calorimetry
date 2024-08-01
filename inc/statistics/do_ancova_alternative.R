library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(emmeans)

################################################################################
# Get r squared clean for plotly
################################################################################
get_r_squared_clean <- function(rvalue) {
  r_squared_value <- sub(".*italic\\(R\\)\\^2\\s=\\s(-?[0-9.]+).*", "\\1", rvalue)
  return(as.numeric(r_squared_value))
}

################################################################################
# Calculate statistic based on provided method
################################################################################
calculate_statistic <- function(data, method) {
  switch(method,
    mean = mean(data),
    median = median(data),
    mean(data))
}

################################################################################
# do_ancova_alternative
################################################################################

do_ancova_alternative <- function(df_data, df_metadata, indep_var, indep_var2, group, group2, dep_var, test_type, adjust_method = "bonferroni") {
  df <- df_data %>% full_join(y = df_metadata, by = c("Animals")) %>% na.omit() 
  # Might be necessary, check carefully, if not, remove later
  if (! "Genotype" %in% names(df)) {
    if ("Genotype.x" %in% names(df)) {
      df <- df %>% rename(Genotype = `Genotype.x`)
    }
  }

  if (is.null(indep_var)) {
    indep_var <- "body_weight"
  }
  # TODO: First covariate, rename Weight -> Covariate1
  names(df)[names(df) == indep_var] <- "Weight"

  # TODO: Add ANCOVA which uses multiple covariate, rename Weight2 -> Covariate2
  if (!is.null(indep_var2)) {
    names(df)[names(df) == indep_var2] <- "Weight2"
  }

  # first grouping variable, set some sensible default, Genotype should always be available
  if (is.null(group)) {
    group <- "Genotype"
  }

  # second grouping variable, set some sensible default, Days should always be available
  if (is.null(group2)) {
    group2 <- "Days"
  }

  names(df)[names(df) == group] <- "group"

  # for DayNight activity, 2nd grouping variable Genotype or Diet renamed to Days, needs to be changed, see TODOs below
  if (dep_var == "HP") {
    df <- df %>% select(-Days)
    names(df)[names(df) == group2] <- "Days"
  }

  # TODO: Rename TEE -> DependentVariable to generalize/cleanup the implementation
  if (dep_var == "TEE") {
    df <- df %>% select(c("Animals", "group", "Weight", "TEE", "Days"))
  } else if (dep_var == "GoxLox") {
    names(df)[names(df) == dep_var] <- "TEE"
    df <- df %>% select(c("Animals", "group", "Weight", "TEE", "Days"))
  } else if (dep_var == "HP") {
    names(df)[names(df) == dep_var] <- "TEE"
    df <- df %>% select(c("Animals", "group", "Weight", "TEE", "Days"))
  } else if (dep_var == "Raw") {
    names(df)[names(df) == dep_var] <- "TEE"
    df <- df %>% select(c("Animals", "group", "Weight", "TEE", "Days"))
  } else if (dep_var == "RMR") {
    names(df)[names(df) == dep_var] <- "TEE" 
    df <- df %>% select(c("Animals", "group", "Weight", "TEE"))
  } else { # other quantities are supported only by 1-way ANCOVA
    df <- df %>% select(c("Animals", "group", "Weight", "TEE"))
  }

  df$Weight <- as.numeric(df$Weight)
  df$TEE <- as.numeric(df$TEE)

  if (test_type == "1-way ANCOVA") {
    if (dep_var == "TEE") {
      df = df %>% group_by(Animals) %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first))
    } else if (dep_var == "GoxLox") {
      df = df %>% group_by(Animals) %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first))
    } else if (dep_var == "HP") {
      df = df %>% group_by(group, Animals) %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first)) %>% ungroup()
    } else if (dep_var == "Raw") {
      df = df %>% group_by(Animals) %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first))
    } else if (dep_var == "RMR") {
      df = df %>% group_by(Animals) %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first))
    }
  } 

  if (test_type == "2-way ANCOVA") {
    if (dep_var == "HP") {
      df = as.data.frame(df) %>% select(c("Animals", "group", "Weight", "TEE", "Days")) %>% distinct()
    }
  }

  p2 <- NULL
  if (dep_var == "TEE") {
    p2 <- ggscatter(df, x = "Weight", y = "TEE", color = "group", add = "reg.line")
    p2 <- p2 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE)+2, max(df$TEE)+8), geom="text", output.type = "text", parse=FALSE)
  } else {
    p2 <- ggscatter(df, x = "Weight", y = "TEE", color = "group", add = "reg.line")
    p2 <- p2 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE)+2, max(df$TEE)+8), geom="text", output.type = "text", parse=FALSE)
  }

  p2 <- p2 + labs(colour=group)

  # 1-way ANCOVA based on user input grouping variable
  res.aov <- NULL
  if (test_type == "1-way ANCOVA") {
   res.aov <- df %>% anova_test(TEE ~ Weight + group)
  }

  if (test_type == "2-way ANCOVA") {
    # 2-way ANCOVA for now uses Days as second group always
    res.aov <- df %>% anova_test(TEE ~ Weight + group * Days)
  }

  p <- NULL
  pwc <- NULL
  if (test_type == "1-way ANCOVA") {
    pwc <- df %>%
      emmeans_test(
        TEE ~ group, covariate = Weight,
        p.adjust.method = adjust_method
      )

  # Visualization of estimated marginal means for 1-way ancova
  pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
  p <- ggline(get_emmeans(pwc), x = "group", y = "emmean") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  }

  if (test_type == "2-way ANCOVA") {
    pwc <- df %>% group_by(group) %>% emmeans_test(TEE ~ Days, covariate=Weight)
    pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
    p <- ggline(get_emmeans(pwc), x = "group", y="emmean", color="Days") 
    p <- p + geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color=Days), width=0.1)
    p <- p + stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
    # TODO: get all statistics into table for 2-way ANCOVA, more than one comparison of course, 
    # thus multiple to report in table format, for a 1-way ANCOVA we simply report the first()
    print(pwc)
    pwc <- pwc %>% first()
  }


  # Fit the model, the covariate goes first
  model <- lm(TEE ~ Weight + group, data = df)
  if (test_type == "2-way ANCOVA") {
    model <- lm(TEE ~ Weight + group * Days, data = df)
  }

  # Check test assumptions met in general
  model.metrics <- augment(model)
  shapiro <- shapiro_test(model.metrics$.resid)
  levene <- model.metrics %>% levene_test(.resid ~ group)
  return(list("plot_details" = p, "plot_summary" = p2, "statistics" = pwc, "shapiro" = shapiro, "levene" = levene))
}
