library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(emmeans)

################################################################################
#' brunner_munzel
#' 
#' This function get's the p-value for the Brunner Munzel test (statistic)
#' @param df
brunner_munzel <- function(df) {
  return(lawsat::brunner.munzel.test(as.numeric(df$TEE) ~ data$group)$p.value)
}

################################################################################
#' get_r_squared_clean
#' 
#' This function get's the R-squared value in a clean way for plotly
#' @param rvalue
################################################################################
get_r_squared_clean <- function(rvalue) {
  r_squared_value <- sub(".*italic\\(R\\)\\^2\\s=\\s(-?[0-9.]+).*", "\\1", rvalue)
  return(as.numeric(r_squared_value))
}

################################################################################
#' calculate_statistic
#' 
#' This function calculate statistic based on provided method: mean or median
#' @param data
#' @param method
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
#' @param df_data
#' @param df_metadata
#' @param indep_var
#' @param indep_var2
#' @param group
#' @param group2
#' @param dep_var
#' @param test_type
#' @param adjust_method
#' @param connected_or_independent_anova
#' @param num_covariates
#' @param repeated_measurements
#' @param lm_or_glm
################################################################################
# TODO: Add possibility to let the user choose the glm family and link function 
# via the inputs (input$glm_family and input$link_function) - not only defaults
do_ancova_alternative <- function(df_data, df_metadata, indep_var, indep_var2, group, group2, dep_var, test_type, adjust_method = "bonferroni", connected_or_independent_ancova=FALSE, num_covariates=1, repeated_measurements=FALSE, lm_or_glm=FALSE, sort_factors_alphabetically_decreasing=TRUE, sort_factors_by_custom_sorting=NULL) {
  df <- df_data %>% full_join(y = df_metadata, by = c("Animals")) %>% na.omit() 
  # Might not be necessary, does no harm, can be removed if no adverse effects revealed during testing
  if (! "Genotype" %in% names(df)) {
    if ("Genotype.x" %in% names(df)) {
      df <- df %>% rename(Genotype = `Genotype.x`)
    }
  }

  if (is.null(indep_var)) {
    indep_var <- "body_weight"
  }

  # TODO: Rename covariates for ANCOVA
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

  # second grouping variable, set some sensible default, group2 should always be available
  if (is.null(group2)) {
    group2 <- "group2"
  }

  names(df)[names(df) == group] <- "group"
  names(df)[names(df) == group2] <- "group2"

  if (dep_var == "HP") {
    df <- df %>% select(-group2)
    # TODO: For DayNight activity, 2nd grouping variable Genotype or Diet renamed to group2, needs to be changed to a generic name, e.g. group2
    names(df)[names(df) == group2] <- "group2"
  }


  to_select_columns = c("Animals", "group", "Weight", "TEE", "group2")
  if (num_covariates > 1) {
    to_select_columns = c("Animals", "group", "Weight", "Weight2", "TEE", "group2")
  }

  # TODO: Rename TEE for ANCOVA 
  # -> DependentVariable to generalize/cleanup the naming of variables in this statistics module
  tryCatch({
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
  } else if (dep_var == "RMR") { 
    names(df)[names(df) == dep_var] <- "TEE" 
    if (num_covariates > 1) {
      df <- df %>% select(c("Animals", "group", "Weight", "Weight2", "TEE", "group2"))
    } else {
      df <- df %>% select(c("Animals", "group", "Weight", "TEE", "group2"))
    }
  } else if (dep_var == "EE") {
    names(df)[names(df) == dep_var] <- "TEE"
    if (num_covariates > 1) {
      df <- df %>% select(c("Animals", "group", "Weight", "Weight2", "TEE", "group2"))
    } else {
      df <- df %>% select(c("Animals", "group", "Weight", "TEE", "group2"))
    }
  } else { # other quantities are supported only by 1-way ANCOVA with either 1 or 2 covariates
    if (num_covariates > 1) {
      df <- df %>% select(c("Animals", "group", "Weight", "Weight2", "TEE"))
    } else {
      df <- df %>% select(c("Animals", "group", "Weight", "TEE"))
    }
  }

  print("raw df:")
  print(df)
  }, error = function(e) {
    shinyalert("Error", "No metadata attached - statistical comparisons are futile. Please start from scratch.")
    delay(10000, shinyjs::runjs("history.go(0);"))
  })

  # covariates need to be always numeric, tse header must be prepared better, see util methods to allow only numerical non-factor columns?
  df$Weight <- as.numeric(df$Weight)
  if (num_covariates > 1) {
    df$Weight2 <- as.numeric(df$Weight2)
  }
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
      df = df %>% group_by(Animals)  %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first))
    } else if (dep_var == "EE") {
      df = df %>% group_by(group, Animals) %>% summarize(TEE=mean(TEE, na.rm=TRUE), across(-TEE, first)) %>% ungroup()
    }
  } 

  if (test_type == "2-way ANCOVA" || test_type == "2-way ANOVA") {
    if (dep_var == "HP" || dep_var == "EE" || dep_var == "RMR") {
      if (num_covariates > 1) {
        df = as.data.frame(df) %>% select(c("Animals", "group", "Weight", "Weight2", "TEE", "group2")) %>% distinct()
      } else {
        df = as.data.frame(df) %>% select(c("Animals", "group", "Weight", "TEE", "group2")) %>% distinct()
      }
    }
  }

   

  # sort factors decreasing or not
  if (sort_factors_alphabetically_decreasing == TRUE) {
    df$group <- factor(df$group, levels=sort(unique(df$group), decreasing=FALSE))
  } else {
     if (!is.null(sort_factors_by_custom_sorting)) {
      print("not sorting alphabetically!")
      print("sorting with custom sorting!")
      df$group <- factor(df$group, levels=sort_factors_by_custom_sorting)
     }
     #df$group <- factor(df$group, levels=input$factors_custom_sorting)
  }

  p2 <- NULL
  p3 <- NULL
  if (dep_var == "TEE") {

    p2 <- ggscatter(df, x = "Weight", y = "TEE", color = "group", add = "reg.line", alpha=0) 
    p2 <- p2 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE), max(df$TEE)), label.x=c(min(df$Weight), min(df$Weight)+1), geom="text", output.type = "text", parse=FALSE)
    p2 <- p2 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", group2), color=group), label = "", alpha=1)

    if (num_covariates > 1) {
     p3 <- ggscatter(df, x = "Weight2", y = "TEE", color = "group", add = "reg.line")
     p3 <- p3 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE), max(df$TEE)), label.x=c(min(df$Weight2), min(df$Weight2)+1), geom="text", output.type = "text", parse=FALSE)
     p3 <- p3 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", group2), color=group), label = "", alpha=1)
    }

  } else {
    group_count <- length(unique(df$group))
    y_max <- max(df$TEE)
    y_range <- max(df$TEE) - min(df$TEE)
    label_x <- rep(min(df$Weight), group_count)
    label_y <- seq(y_max, y_max+((y_range * 0.05) * (group_count - 1)), length.out = group_count)
    df <- df %>% rename(!!dep_var := TEE)
    p2 <- ggscatter(df, x = "Weight", y = dep_var, color = "group", add = "reg.line", alpha=0)
    p2 <- p2 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=label_y, label.x=label_x, geom="text", output.type = "text", parse=FALSE)
    if (dep_var == "RMR" || dep_var == "EE") {
      # Note: RMR and EE comes already averaged per group2
      p2 <- p2 + geom_point(aes(text=paste0("ID: ", Animals), color=group), label = "", alpha=1)
    } else {
      p2 <- p2 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", group2), color=group), label = "", alpha=1)
    }
    df <- df %>% rename(TEE := !!dep_var)
    if (num_covariates > 1) {
      df <- df %>% rename(!!dep_var := TEE)
      p3 <- ggscatter(df, x = "Weight2", y = dep_var, color = "group", add = "reg.line")
      p3 <- p3 + stat_regline_equation(aes(label = after_stat(rr.label), color = group), label.y=c(max(df$TEE), max(df$TEE)), label.x=c(min(df$Weight2), min(df$Weight2)+1.0), geom="text", output.type = "text", parse=FALSE)
     if (dep_var == "RMR" || dep_var == "EE") {
        # Note: RMR and EE comes already averaged per group2
      p3 <- p3 + geom_point(aes(text=paste0("ID: ", Animals), color=group), label = "", alpha=1)
     } else {
      p3 <- p3 + geom_point(aes(text=paste0("ID: ", Animals, "<br>Day: ", group2), color=group), label = "", alpha=1)
     }
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
    # 2-way ANCOVA for now uses group2 as second group always
    if (connected_or_independent_ancova) { # interaction, group and group2 are independent categorial grouping variables
        if (num_covariates > 1) {
          res.aov <- df %>% anova_test(TEE ~ Weight + Weight2 + group * group2)
        } else {
          res.aov <- df %>% anova_test(TEE ~ Weight + group * group2)
        }
    } else { # otherwise assume that there is no interaction
      if (num_covariates > 1) {
        res.aov <- df %>% anova_test(TEE ~ Weight:Weight2:group:group2)
      } else {
        res.aov <- df %>% anova_test(TEE ~ Weight:group:group2)
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

  mean_p_value = mean(pwc$p.adj)
  # Visualization of estimated marginal means for 1-way ancova
  pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
  p <- ggline(get_emmeans(pwc), x = "group", y = "emmean", color="group", group="group") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=group), width = 0.1) 
    p <- p + annotate("text", x  = 1.0, y = max(get_emmeans(pwc)$emmean) + 1, label = paste0("p-value: ", round(mean_p_value, 6)), size = 4)

  }

  if (test_type == "2-way ANCOVA") {
    df$group2 = as.factor(df$group2)
    pwc <- df %>% group_by(group) %>% emmeans_test(TEE ~ group2, covariate=Weight)
    pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
    p <- ggline(get_emmeans(pwc), x = "group", y="emmean", color="group2") 
    p <- p + geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color=group2), width=0.1)
    p <- p + stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  }

  # Fit the model, the covariate goes first
  model <- lm(TEE ~ Weight + group, data = df)

  ##############################################################################
  # TODO: Refactor this
  if (test_type == "2-way ANCOVA") {
     model <- lm(TEE ~ Weight + group * group2, data = df)
  }

  if (test_type == "2-way ANOVA") {
    if (repeated_measurements) {
    } else {
      model <- lm(TEE ~ group * group2, data = df)
    }
  }
  ##############################################################################

  # Check test assumptions met in general
  model.metrics <- augment(model)
  # FIXME: Shapiro can only handle 5000 samples max
  shapiro <- shapiro_test(model.metrics$.resid[0:5000])
  levene <- model.metrics %>% levene_test(.resid ~ group)

  if (test_type == "2-way ANOVA" || test_type == "2-way ANCOVA") {
      df$group2 <- as.factor(df$group2)
      model = lm(TEE ~ group * group2, data=df)
      if (lm_or_glm == TRUE) {
        model = glm(TEE ~ group * group2, data=df)
      }

      if (repeated_measurements) {
        model = nlme::lme(TEE ~ group * group2, random=~1|Animals, data=df)
      }

      if (test_type == "2-way ANCOVA") {
        model = lm(TEE ~ group * group2 + Weight, data=df)
        if (lm_or_glm == TRUE) {
          model = glm(TEE ~ group * group2 + Weight, data=df)
        }
        if (repeated_measurements) {
          model = nlme::lme(TEE ~ group * group2 + Weight, random=~1|Animals, data=df)
        }
        if (num_covariates > 1) {
          model = lm(TEE ~ group * group2 + Weight + Weight2, data=df)
          if (lm_or_glm == TRUE) {
            model = glm(TEE ~ group * group2 + Weight + Weight2, data=df)
          }

          if (repeated_measurements) {
            model = nlme::lme(TEE ~ group * group2 + Weight + Weight2, random=~1|Animals, data=df)
          }
        }
      } 

      emm = emmeans(model, ~group2 | group)
      emm_df <- as.data.frame(emm)
      p <- ggplot(emm_df, aes(x=group2, y=emmean, group=group, color=group)) + geom_line() + geom_point()
      p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2) 

      pairwise_raw <- contrast(emm, method="pairwise", by="group2") %>% as.data.frame()
      pairwise <- contrast(emm, method="pairwise", by="group2", adjust="tukey") %>% as.data.frame() %>% mutate(
        significance=case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      ) %>% rename(group1=contrast) %>% rename(group2=group2) %>% rename(statistic=t.ratio) %>% rename(p=p.value) %>% rename(p.adj.signif=significance)
      pairwise <- pairwise %>% mutate(p.adj = pairwise_raw$p.value)
      mean_p_value <- mean(pairwise$p.adj)
      p <- p + annotate("text", x = levels(emm_df$group2)[1], y = min(emm_df$emmean) -1, label=paste0("p-value: ", round(mean_p_value, 6)))
      pwc <- pairwise 
  } 

  # for ANOVAs report statistics directly in panel Statistical Testing, no Details section required.
  if (test_type == "1-way ANOVA") {
    # FIXME: Plotly does not support comparisons=pairs in stat_compare_means()
    #df$group <- as.character(df$group)
    #pairs <- combn(unique(df$group), 2, simplify=FALSE)
    p2 <- ggplot(df, aes(x = group, y = TEE, color = group)) + geom_boxplot(outlier.shape=15, outlier.size=0, outlier.color="red") # outlier.shape=NA)  
    #if (dep_var == "RMR" || dep_var == "EE") {
    #  p2 <- p2 + geom_jitter(aes(text=paste0("ID: ", Animals, "<br>", "Group: ", group)), size=3, width=0.2, alpha=0.6)
    #  p2 <- p2 + stat_compare_means()
    #} else {
      p2 <- p2 + geom_jitter(aes(text=paste0("ID: ", Animals, "<br>", "Group: ", group, "<br>Day: ", group2)), size=3, width=0.2, alpha=0.6)
      p2 <- p2 + stat_compare_means()
    #}
  }

  if (test_type == "2-way ANOVA") {
    df$group2 <- as.factor(df$group2)
    if (connected_or_independent_ancova) {
       p2 <- ggboxplot(df, "group", "TEE", color = "group2")
       p2 <- p2 + geom_jitter(aes(text=paste0("ID: ", Animals, "<br>", "Group: ", group, "<br>Day: ", group2), color=group2), size=3, width=0.2, alpha=0.6)
       p2_old <- p2
       result <- try({
        p2 <<- p2 + stat_compare_means(
        aes(group = group2), method="anova", label="p.format")
       }, silent=TRUE)
       if (inherits(result, "try-error")) {
        p2 <<- p2_old + stat_compare_means()
       } 
    } else {
       p2 <- ggboxplot(df, "group", "TEE", color = "group2")
       p2 <- p2 + geom_jitter(aes(text=paste0("ID: ", Animals, "<br>", "Group: ", group, "<br>Day: ", group2), color=group2), size=3, width=0.2, alpha=0.6)
       p2 <- p2 + stat_compare_means()
    }
  }

  if (test_type == '1-way ANOVA' || test_type == '1-way GLM') {
    model = lm(TEE ~ group, data=df)
    if (lm_or_glm == TRUE) { # use GLM
      model = glm(TEE ~ group, data=df)
    }
    emm = emmeans(model, ~ group)
    emm_df <- as.data.frame(emm)

     pairwise_raw <- contrast(emm, method="pairwise") %>% as.data.frame()
      pairwise <- contrast(emm, method="pairwise", adjust="tukey") %>% as.data.frame() %>% mutate(
        significance=case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      ) 
      pairwise <- pairwise %>% rename(group1=contrast) %>% rename(statistic=t.ratio) %>% rename(p=p.value) %>% rename(p.adj.signif=significance)
      pairwise <- pairwise %>% mutate(p.adj = pairwise_raw$p.value)
      pairwise$group2 <- length(unique(df$group2))
      mean_p_value <- mean(pairwise$p.adj)

      p <- ggplot(emm_df, aes(x=group, y=emmean, group=group, color=group)) + geom_line() + geom_point()
      p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.1) 
      p <- p + annotate("text", x  = 1.0, y = max(emm_df$emmean) + 1, label = paste0("p-value: ", round(mean_p_value, 6)), size = 4)
      pwc <- pairwise 
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
    "df"=df, # data frame
    "dep_var"=dep_var))
}
