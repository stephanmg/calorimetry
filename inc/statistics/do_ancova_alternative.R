library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(emmeans)

################################################################################
#' brunner_munzel
#' 
#' This function get's the p-value for the Brunner Munzel test (statistic)
brunner_munzel <- function(df) {
  return(lawsat::brunner.munzel.test(as.numeric(df$TEE) ~ data$group)$p.value)
}

################################################################################
#' get_r_squared_clean
#' 
#' This function get's the R-squared value in a clean way for plotly
################################################################################
get_r_squared_clean <- function(rvalue) {
  r_squared_value <- sub(".*italic\\(R\\)\\^2\\s=\\s(-?[0-9.]+).*", "\\1", rvalue)
  return(as.numeric(r_squared_value))
}

################################################################################
#' calculate_statistic
#' 
#' This function calculate statistic based on provided method: mean or median
################################################################################
calculate_statistic <- function(data, method) {
  switch(method,
    mean = mean(data),
    median = median(data),
    mean(data))
}

################################################################################
#' do_ancova_alternative
#' 
#' This function performs multi-way ANCOVA or ANOVA analysis
################################################################################
do_ancova_alternative <- function(df_data, df_metadata, indep_var, indep_var2, group, group2, dep_var, test_type, adjust_method = "bonferroni", connected_or_independent_ancova=FALSE, num_covariates=1, repeated_measurements=FALSE) {
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
  # TODO: Rename covariates ANCOVA
  # First covariate, rename Weight -> Covariate1
  # Second covariate, rename Weight2 -> Covariate2
  names(df)[names(df) == indep_var] <- "Weight"

  # ANCOVA which uses multiple covariate
  if (num_covariates > 1) {
    if (is.null(indep_var2)) {
      indep_var2 <- "lean_mass"
    }
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

  to_select_columns = c("Animals", "group", "Weight", "TEE", "Days")
  if (num_covariates > 1) {
    to_select_columns = c("Animals", "group", "Weight", "Weight2", "TEE", "Days")
  }


  # TODO: Rename TEE for ANCOVA 
  # -> DependentVariable to generalize/cleanup the naming of variables in this statistics module
  if (dep_var == "TEE") {
    df <- df %>% select(all_of(to_select_columns))
  } else if (dep_var == "GoxLox") {
    names(df)[names(df) == dep_var] <- "TEE"
    df <- df %>% select(all_of(to_select_columns))
  } else if (dep_var == "HP") {
    names(df)[names(df) == dep_var] <- "TEE"
    df <- df %>% select(all_of(to_select_columns))
  } else if (dep_var == "Raw") {
    names(df)[names(df) == dep_var] <- "TEE"
    df <- df %>% select(all_of(to_select_columns))
  } else if (dep_var == "RMR") { # RMR makes only sense to have 1-way ANCOVA currently (without Days)
    names(df)[names(df) == dep_var] <- "TEE" 
    if (num_covariates > 1) {
      df <- df %>% select(c("Animals", "group", "Weight", "Weight2", "TEE"))
    } else {
      df <- df %>% select(c("Animals", "group", "Weight", "TEE"))
    }
  } else if (dep_var == "EE") {
    names(df)[names(df) == dep_var] <- "TEE"
    if (num_covariates > 1) { # EE makes only sense to have 1-way ANCOVA currently (without Days)
      df <- df %>% select(c("Animals", "group", "Weight", "Weight2", "TEE"))
    } else {
      df <- df %>% select(c("Animals", "group", "Weight", "TEE"))
    }
  } else { # other quantities are supported only by 1-way ANCOVA with either 1 or 2 covariates
    if (num_covariates > 1) {
      df <- df %>% select(c("Animals", "group", "Weight", "Weight2", "TEE"))
    } else {
      df <- df %>% select(c("Animals", "group", "Weight", "TEE"))
    }
  }

  # covariates need to be always numeric, tse header must be prepared better, see util methods to allow only numerical non-factor columns?
  df$Weight <- as.numeric(df$Weight)
  if (num_covariates > 1) {
    print(df)
    print(colnames(df))
    df$Weight2 <- as.numeric(df$Weight2)
  }
  df$TEE <- as.numeric(df$TEE)

  print(levels(df$group))
  print(levels(df$Animals))
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
      df = df %>% group_by(Animal)  %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first))
    } else if (dep_var == "EE") {
      df = df %>% group_by(Animals) %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first))
    }
  } 

  if (test_type == "2-way ANCOVA" || test_type == "2-way ANOVA") {
    if (dep_var == "HP") {
      if (num_covariates > 1) {
        df = as.data.frame(df) %>% select(c("Animals", "group", "Weight", "Weight2", "TEE", "Days")) %>% distinct()
      } else {
        df = as.data.frame(df) %>% select(c("Animals", "group", "Weight", "TEE", "Days")) %>% distinct()
      }
    }
  }

  p2 <- NULL
  p3 <- NULL
  if (dep_var == "TEE") {
    p2 <- ggscatter(df, x = "Weight", y = "TEE", color = "group", add = "reg.line", alpha=0) 
    p2 <- p2 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE)+2, max(df$TEE)+8), geom="text", output.type = "text", parse=FALSE)
    p2 <- p2 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", Days), color=group), label = "", alpha=1)

    if (num_covariates > 1) {
     p3 <- ggscatter(df, x = "Weight2", y = "TEE", color = "group", add = "reg.line")
     p3 <- p3 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE)+2, max(df$TEE)+8), geom="text", output.type = "text", parse=FALSE)
     p3 <- p3 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", Days), color=group), label = "", alpha=1)
    }

  } else {
    label_y <- c(max(df$TEE)+2, max(df$TEE)+8)
    df <- df %>% rename(!!dep_var := TEE)
    p2 <- ggscatter(df, x = "Weight", y = dep_var, color = "group", add = "reg.line", alpha=0)
    p2 <- p2 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=label_y, geom="text", output.type = "text", parse=FALSE)
    p2 <- p2 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", Days), color=group), label = "", alpha=1)
    df <- df %>% rename(TEE := !!dep_var)
    if (num_covariates > 1) {
      df <- df %>% rename(!!dep_var := TEE)
      p3 <- ggscatter(df, x = "Weight2", y = dep_var, color = "group", add = "reg.line")
      p3 <- p3 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE)+2, max(df$TEE)+8), geom="text", output.type = "text", parse=FALSE)
      p3 <- p3 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", Days), color=group), label = "", alpha=1)
      df <- df %>% rename(TEE := !!dep_var)
    }
  }

  p2 <- p2 + labs(colour=group)
  if (num_covariates > 1) {
    p3 <- p3 + labs(colour=group)
  }

  # 1-way ANCOVA based on user input grouping variable
  res.aov <- NULL
  if (test_type == "1-way ANCOVA") {
    if (num_covariates > 1) {
      res.aov <- df %>% anova_test(TEE ~ Weight * Weight2 + group)
    } else {
      res.aov <- df %>% anova_test(TEE ~ Weight + group)
    }
  }

  if (test_type == "2-way ANCOVA") {
    # 2-way ANCOVA for now uses Days as second group always
    if (connected_or_independent_ancova) { # interaction, group and Days are independent categorial grouping variables
        if (num_covariates > 1) {
          res.aov <- df %>% anova_test(TEE ~ Weight + Weight2 + group * Days)
        } else {
          res.aov <- df %>% anova_test(TEE ~ Weight + group * Days)
        }
    } else { # otherwise assume that there is no interaction
      if (num_covariates > 1) {
        res.aov <- df %>% anova_test(TEE ~ Weight:Weight2:group:Days)
      } else {
        res.aov <- df %>% anova_test(TEE ~ Weight:group:Days)
      }
    }
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
  }

  # Fit the model, the covariate goes first
  model <- lm(TEE ~ Weight + group, data = df)
  # TODO: adjust to use both covariates first, and then also both covariates for repeated measurement anova
  if (test_type == "2-way ANCOVA") {
     model <- lm(TEE ~ Weight + group * Days, data = df)
  }

 # TODO: use this to report in the statistics panel (Details)
  if (test_type == "2-way ANOVA") {
    if (repeated_measurements) {
    } else {
      model <- lm(TEE ~ group * Days, data = df)
    }
  }

  # Check test assumptions met in general
  model.metrics <- augment(model)
  shapiro <- shapiro_test(model.metrics$.resid[0:5000]) # TODO: shapiro can only handle 5000 samples max
  levene <- model.metrics %>% levene_test(.resid ~ group)

  if (test_type == "2-way ANOVA" || test_type == "2-way ANCOVA") {
      df$Days <- as.factor(df$Days)
      model = lm(TEE ~ group * Days, data=df)
      if (repeated_measurements) {
        model = nlme::lme(TEE ~ group * Days, random=~1|Animals, data=df)
      }

      if (test_type == "2-way ANCOVA") {
        model = lm(TEE ~ group * Days + Weight, data=df)
        if (repeated_measurements) {
          model = nlme::lme(TEE ~ group * Days + Weight, random=~1|Animals, data=df)
        }
        if (num_covariates > 1) {
          model = lm(TEE ~ group * Days + Weight + Weight2, data=df)
          if (repeated_measurements) {
            model = nlme::lme(TEE ~ group * Days + Weight + Weight2, random=~1|Animals, data=df)
          }
        }
      } 

      print("data df:")
      print(df)
      emm = emmeans(model, ~Days | group)
      emm_df <- as.data.frame(emm)
      print("emm_df:")
      print(emm_df)
      p <- ggplot(emm_df, aes(x=Days, y=emmean, group=group, color=group)) + geom_line() + geom_point()
      p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2) 

      pairwise_raw <- contrast(emm, method="pairwise", by="group") %>% as.data.frame()
      pairwise <- contrast(emm, method="pairwise", by="group", adjust="tukey") %>% as.data.frame() %>% mutate(
        significance=case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      ) %>% rename(group1=contrast) %>% rename(group2=group) %>% rename(statistic=t.ratio) %>% rename(p=p.value) %>% rename(p.adj.signif=significance)
      pairwise <- pairwise %>% mutate(p.adj = pairwise_raw$p.value)
      mean_p_value <- mean(pairwise$p.adj)

      p <- p + geom_text(aes(x = levels(emm_df$Days)[1], y = max(emm_df$emmean)), label=paste0("p-value: ", mean_p_value))

      print("pairwise")
      print(colnames(pairwise))
      #print("summary:")
      #print(summary(pairwise))
      pwc <- pairwise 
#    }
  } 

  # for ANOVAs report statistics directly in panel Statistical Testing, no Details section required.
  if (test_type == "1-way ANOVA") {
    p2 <- NULL
    p2 <- ggplot(df, aes(x = group, y = TEE, color = group)) + geom_boxplot(outlier.shape=15, outlier.size=0, outlier.color="red") # outlier.shape=NA)  
    p2 <- p2 + geom_jitter(aes(text=paste0("ID: ", Animals, "<br>", "Group: ", group, "<br>Day: ", Days)), size=3, width=0.2, alpha=0.6)
    p2 <- p2 + stat_compare_means()
  }

  if (test_type == "2-way ANOVA") {
    df$Days <- as.factor(df$Days)
    if (connected_or_independent_ancova) {
       p2 <- ggboxplot(df, "group", "TEE", color = "Days")
       p2 <- p2 + geom_jitter(aes(text=paste0("ID: ", Animals, "<br>", "Group: ", group, "<br>Day: ", Days), color=Days), size=3, width=0.2, alpha=0.6)
       p2_old <- p2
       result <- try({
        p2 <<- p2 + stat_compare_means(
        aes(group = Days), method="anova", label="p.format")
       }, silent=TRUE)
       if (inherits(result, "try-error")) {
        p2 <<- p2_old + stat_compare_means()
       } 
    } else {
       p2 <- ggboxplot(df, "group", "TEE", color = "Days")
       p2 <- p2 + geom_jitter(aes(text=paste0("ID: ", Animals, "<br>", "Group: ", group, "<br>Day: ", Days), color=Days), size=3, width=0.2, alpha=0.6)
       p2 <- p2 + stat_compare_means()
    }
  }


  regression_slopes <- summary(aov(TEE ~ Weight:group, data = df))
  regression_slopes <- regression_slopes[[1]]["Weight:group", "Pr(>F)"]

  return(list(
    "plot_details" = p, # Details plot
    "plot_summary" = p2, # first covariate in Statistical Testing panel
    "plot_summary2" = p3,  # second covariate Statistical Testing panel
    "statistics" = pwc, # Statistics table below Details plot
    "shapiro" = shapiro, # ...
    "levene" = levene, # ...
    "regression_slopes" = regression_slopes, ### Statistics table below Details plot
    "df"=df))
}
