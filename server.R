library(shiny)
library(tidyr) # for unite
library(ggplot2)
library(data.table) # for filtering with %like%
library(readxl)
library(writexl)
library(plotly)
library(zoo) # for running average methods
library(ggpubr)
library(viridis)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(htmlwidgets)
library(fs)
library(hash)
require(tidyverse)
library(tools)
library(shinyalert)
library(shinyjs)

source("inc/util.R") # general utility methods

source("inc/rmr/helper.R") # rmr helper methods
source("inc/rmr/extract_rmr.R") # rmr extraction
source("inc/rmr/extract_rmr_helper.R") # rmr extraction helper

source("inc/importers/import_promethion_helper.R") # import for SABLE/Promethion data sets
source("inc/importers/import_pheno_v8_helper.R") # import for PhenoMaster V8 data sets
source("inc/importers/import_cosmed_helper.R") # import for COSMED data sets

source("inc/locomotion/locomotion.R") # for locomotion probability heatmap
source("inc/locomotion/locomotion_budget.R") # for locomotion budget

source("inc/annotations/timeline.R") # for colorizing timeline by day/night rhythm
source("inc/annotations/guide.R") # for guide

source("inc/statistics/do_ancova.R") # for ancova without metadata
source("inc/statistics/do_ancova_alternative.R") # for ancova with metadata

source("inc/metadata/read_metadata.R") # for metadata sheet handling

source("inc/exporters/default_exporter.R") # for data export

time_diff <- 5
time_start_end <- NULL
start_date <- "1970-01-01"
end_date <- Sys.Date()
################################################################################
# Helper functions
################################################################################
convert <- function(x) {
    splitted <- strsplit(as.character(x), " ")
    paste(splitted[[1]][2], ":00", sep = "")
}

################################################################################
# Create plotly plot
################################################################################
do_plotting <- function(file, input, exclusion, output) { # nolint: cyclocomp_linter.
   #############################################################################
   # Configure base plot look and feel with ggpubr
   #############################################################################
   theme_pubr_update <- theme_pubr(base_size = 8.5) +
   theme(legend.key.size = unit(0.3, "cm")) +
   theme(strip.background = element_blank()) +
   theme(strip.text = element_text(hjust = 0)) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
   theme_set(theme_pubr_update)

   #############################################################################
   # Detect file type
   #############################################################################
   detectFileType <- function(filename) {
      file_ext(filename)
   }

   #############################################################################
   # Data read-in
   #############################################################################
   fileFormatTSE <- FALSE
   finalC1 <- c()
   finalC1meta <- data.frame(matrix(nrow = 0, ncol = 6))
   # Supported basic metadata fields from TSE LabMaster/PhenoMaster
   colnames(finalC1meta) <- c("Animal.No.", "Diet", "Genotype", "Box", "Sex", "Weight..g.")
   for (i in 1:input$nFiles) {
      file <- input[[paste0("File", i)]]
      file <- file$datapath
      con <- file(file)
      line <- readLines(con, n = 2)
   if (i == 1) {
      fileFormatTSE <- line[2]
      studyDescription <- line[1]
      output$study_description <- renderText(paste("Study description: ", gsub("[;]", "", studyDescription), sep = " "))
      output$file_type_detected <- renderText(paste("Input file type detected:", gsub("[;,]", "", line[2]), sep = " "))
   }
   #########################################################################################################
   # Detect data type (TSE v6/v7, v5 (Akim/Dominik) or v8 (Jan/Tabea)) or Promethion/Sable (.xlsx) (Jenny))
   #########################################################################################################
   detectData <- function(filename) {
      con <- file(filename, "r")
      lineNo <- 1
      while (TRUE) {
         lineNo <- lineNo + 1
         line <- readLines(con, n = 1)
         # lines with either only ; or , or combination as spacer between metadata and data per convention in TSE files
         if (length(line) == 0 || length(grep("^[;,]+$", line) != 0) ||
          line == "") {
            return(lineNo)
         }
      }
   }

   # Skip metadata before data
   toSkip <- detectData(file)

   # time diff (interval) or recordings
   time_diff <<- 5

   # check file extension
   fileExtension <- detectFileType(file)

   # default
   sep <- ";"
   dec <- ","

   scaleFactor <- 1
   # Promethion Live/Sable input needs scale factor of 60 (1 unit is 60 seconds)
   if (fileExtension == "xlsx") {
      output$study_description <- renderText("")
      tmp_file <- tempfile()
      if (length(excel_sheets(file)) == 2) {
        if (check_for_cosmed(file)) {
            output$file_type_detected <- renderText("Input file type detected as: COSMED")
            import_cosmed(file, tmp_file)
        } else {
            output$file_type_detected <- renderText("Unknown file type detected from Excel")
        }
      } else {
        output$file_type_detected <- renderText("Input file type detected as: Promethion/Sable")
        import_promethion(file, tmp_file)
      }
      file <- tmp_file
      toSkip <- detectData(file)
      scaleFactor <- 60
   }

   # LabMaster V5 (horizontal format)
   if (grepl("V5", fileFormatTSE)) {
      sep <- ";"
      dec <- "."
   }

   # LabMaster V6
   if (grepl("V6", fileFormatTSE)) {
      sep <- ";"
      dec <- ","
   }

   # Phenomaster V7: Date separated via /, Time Hour:Minutes, decimal separator ., field separator ,
   if (grepl("V7", fileFormatTSE)) {
      sep <- ","
      dec <- "."
   }

   # Phenomaster V8
   if (grepl("V8", fileFormatTSE)) {
      tmp_file <- tempfile()
      import_pheno_v8(file, tmp_file)
      file <- tmp_file
      toSkip <- detectData(file)
   }

   # File encoding matters: Shiny apps crashes due to undefined character entity
   C1 <- read.table(file, header = FALSE, skip = toSkip + 1,
      na.strings = c("-", "NA"), fileEncoding = "ISO-8859-1", sep = sep, dec = dec)


   # Note: We will keep the basic metadata informatiom from TSE files
   C1meta <- read.table(file, header = TRUE, skip = 2, nrows = toSkip + 1 - 4,
      na.strings = c("-", "NA"), fileEncoding = "ISO-8859-1", sep = sep, dec = dec)
   #############################################################################
   # Curate data frame
   #############################################################################
   C1.head <- read.table(file, # input file
                        header = FALSE, # no header
                        skip = toSkip - 1, # skip headers up to first animal
                        nrows = 2, # read only two rows (variable name + unit)
                        na.strings = c("", "NA"), #transform missing units to NA
                        as.is = TRUE, # avoid transformation character->vector
                        check.names = FALSE, # set the decimal separator
                        fileEncoding = "ISO-8859-1", # file encoding to ISO-8559-1
                        sep = sep, # separator for columns
                        dec = dec) # decimal separator
   names(C1) <- paste(C1.head[1, ], C1.head[2, ], sep = "_")
   # unite data sets
   C1 <- C1 %>%
   unite(Datetime, # name of the final column
         c(Date_NA, Time_NA), # columns to be combined
         sep = " ") # separator set to blank
   C1$Datetime <- gsub(".", "/", C1$Datetime, fixed = TRUE)
   # transform into time format appropriate to experimenters
   C1$Datetime2 <- as.POSIXct(C1$Datetime, format = "%d/%m/%Y %H:%M")
   C1$hour <- hour(C1$Datetime2)
   C1$minutes <- minute(C1$Datetime2)

   if (!is.null(time_start_end)) {
      start_date <<- time_start_end$date_start
      end_date <<- time_start_end$date_end
   }

   if (!input$do_select_date_range) {
      start_date <<- "1970-01-01"
      end_date <<- Sys.Date()
   }
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>%
   arrange(Datetime2) %>%
   filter(Datetime2 >= start_date & # From
            Datetime2 <= end_date) %>% # To
   mutate(MeasPoint = row_number())
   C1 <- C1[!is.na(C1$MeasPoint), ]
   # Step #1 - calculate the difference between consecutive dates
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # subtract the next value from first value and safe as variable "diff.sec)
   mutate(diff.sec = Datetime2 - lag(Datetime2, default = first(Datetime2)))
   C1$diff.sec <- as.numeric(C1$diff.sec) # change format from difftime->numeric
   # Step #2 - calc the cumulative time difference between consecutive dates
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # calculate the cumulative sum of the running time and
   # save as variable "running_total.sec"
   mutate(running_total.sec = cumsum(diff.sec))
   # Step #3 - transfers time into hours (1 h = 3600 s)
   C1$running_total.hrs <- round(C1$running_total.sec / 3600, 1)
   # Step #4 - round hours downwards to get "full" hours
   C1$running_total.hrs.round <- floor(C1$running_total.hrs)
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # subtract the next value from the first value. Safe as variable diff.sec)
   mutate(diff.sec = Datetime2 - lag(Datetime2, default = first(Datetime2)))
   C1$diff.sec <- as.numeric(C1$diff.sec) # change format from difftime->numeric
   # Step #6 - calculate cumulative time difference between consecutive dates
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # calculate cumulative sum of running time. Save in var. "running_total.sec"
   mutate(running_total.sec = cumsum(diff.sec))
   # Step #7 - transfers time into hours (1 h = 3600 s)
   C1$running_total.hrs <- round(C1$running_total.sec / 3600, 1)
   # Step #8 - round hours downwards to get "full" hours
   C1$running_total.hrs.round <- floor(C1$running_total.hrs)


   #############################################################################
   # Consistency check: Negative values
   #############################################################################
   if (input$negative_values) {
      C1_test <- C1
      if ("Ref.TD_[°C]" %in% colnames(C1)) {
         C1_test <- C1_test %>% select(!`Ref.TD_[°C]`)
      }
      if ("TD_[°C]" %in% colnames(C1)) {
         C1_test <- C1_test %>% select(!`TD_[°C]`)
      }

      invalid_data <- nrow(C1_test %>% filter(if_any(everything(), ~.x < 0)))
      if (invalid_data > 0) {
         shinyalert("Error", paste("Negative values encountered in measurements", invalid_data, ".Check your input data.", sep = ""), type = "warning", showCancelButton = TRUE)
      }
   }

   #############################################################################
   # Consistency check: Highly fluctuating measurements
   #############################################################################
   if (input$highly_varying_measurements) {
      invalid_data <- nrow(C1 %>% mutate(col_diff = abs(`VO2(3)_[ml/h]` - lag(`VO2(3)_[ml/h]`) / max(`VO2(3)_[ml/h]`, lag(`VO2(3)_[ml/h]`), na.rm = TRUE))) %>% select(col_diff) %>% filter(col_diff > input$threshold_for_highly_varying_measurements))
      if (invalid_data > 0) {
         shinyalert("Error", paste("Highly varying input measurements detected in O2 signal: ", invalid_data, ". Check your input data", sep = ""), type = "warning", showCancelButton = TRUE)
      }

      invalid_data <- nrow(C1 %>% mutate(col_diff = abs(`VCO2(3)_[ml/h]` - lag(`VCO2(3)_[ml/h]`) / max(`VCO2(3)_[ml/h]`, lag(`VCO2(3)_[ml/h]`), na.rm = TRUE))) %>% select(col_diff) %>% filter(col_diff > input$threshold_for_highly_varying_measurements))
      if (invalid_data > 0) {
         shinyalert("Error", paste("Highly varying input measurements detected in CO2 signal: ", invalid_data, ". Check cour input data", sep = ""), type = "warning", showCancelButton = TRUE)
      }
   }

   # Step #9 - define 1/n-hours steps
   if (input$averaging == 10) { # 1/6 hours
      C1 <- C1 %>%
      mutate(timeintervalinmin = case_when(minutes <= 10 ~ 0,
                                    minutes <= 20 ~ (1 / 6),
                                    minutes <= 30 ~ (2 / 6),
                                    minutes <= 40 ~ (3 / 6),
                                    minutes <= 50 ~ (4 / 6),
                                    minutes > 50 ~ (5 / 6)))
   } else if (input$averaging == 20) { # 1/3 hours
      C1 <- C1 %>%
      mutate(timeintervalinmin = case_when(minutes <= 20 ~ 0,
                                    minutes <= 40 ~ 0.3,
                                    minutes > 40 ~ 0.6))
   } else if (input$averaging == 30) { # 1/2 hours
      C1 <- C1 %>%
      mutate(timeintervalinmin = case_when(minutes <= 30 ~ 0,
                                 minutes > 30 ~ 0.5))
   } else { # no averaging at all
      C1 <- C1 %>% mutate(timeintervalinmin = minutes)
   }
   # Step #10 - create a running total with half hour intervals by adding
   # the thirty min to the full hours
   C1$running_total.hrs.halfhour <- C1$running_total.hrs.round + C1$timeintervalinmin
   f1 <- input$variable1
   f2 <- input$variable2

   #############################################################################
   # Heat production formula #1
   #############################################################################
   C1 <- calc_heat_production(f2, C1, "HP", scaleFactor)

   #############################################################################
   # Heat production formula #2 (for comparison in scatter plots)
   #############################################################################
   C1 <- calc_heat_production(f2, C1, "HP2", scaleFactor)

   # step 11 means
   C1.mean.hours <- do.call(data.frame, aggregate(list(HP2 = C1$HP2, # calculate mean of HP2
                                       VO2 = C1$`VO2(3)_[ml/h]`, # calculate mean of VO2
                                       VCO2 = C1$`VCO2(3)_[ml/h]`, # calculate mean of VCO2
                                       RER = C1$RER_NA), # calculate mean of RER
                     by = list(
                              Animal = C1$`Animal No._NA`, # groups by Animal ID
                              Time = C1$running_total.hrs.round), # groups by total rounded running hour
                        FUN = function(x) c(mean = mean(x), sd = sd(x)))) # calculates mean and standard deviation

   # step 12 (debugging: save cohort means)
   write.csv2(C1.mean.hours, file = paste0(tools::file_path_sans_ext(file), "-cohort_means.csv"))

   # exclude animals (outliers) from data sets
   if (! is.null(exclusion)) {
      for (i in exclusion) {
         C1 <- C1 %>% filter(`Animal No._NA` != as.numeric(i))
      }
   }

   # compile final measurement frame
   finalC1 <- rbind(C1, finalC1)
   common_cols <- intersect(colnames(finalC1meta), colnames(C1meta))
   finalC1meta <- rbind(subset(finalC1meta, select = common_cols), subset(C1meta, select = common_cols))
   }
   # step 13 (debugging: save all cohort means)
   write.csv2(C1.mean.hours, file = paste0("all-cohorts_means.csv"))
   C1meta <- finalC1meta

   # rescale to kj or kcal
   if (input$kj_or_kcal == "kcal") {
      finalC1$HP <- finalC1$HP / 4.184 # kcal to kj
      finalC1$HP2 <- finalC1$HP2 / 4.184 # kcal to kj
   }

   # override light cycle configuration from metadata 
   if (input$override_metadata_light_cycle) {
      # Force override by user from data files if demanded (not all TSE files have light cycle configuration data)
      if (! is.null(input$light_cycle)) {
         if ("LightC_[%]" %in% colnames(finalC1)) {
            `$`(finalC1, "LightC_[%]") <- as.numeric(`$`(finalC1, "LightC_[%]"))
            # filter finalC1 by light cycle
            if (input$light_cycle == "Night") {
               finalC1 <- mutate(`LightC_[%]` = as.numeric(`LightC_[%]`)) %>% filter(finalC1, `LightC_[%]` < input$threshold_light_day)
            } else {
               finalC1 <- mutate(`LightC_[%]` = as.numeric(`LightC_[%]`)) %>% filter(finalC1, `LightC_[%]` > input$threshold_light_day)
            }
         }
      }
   }

   # time interval diff for finalC1
   time_diff <<- get_time_diff(finalC1)

   # set the time date ranges once for the final data frame
   if (is.null(time_start_end)) {
      time_start_end <<- get_date_range(finalC1)
      output$daterange <- renderUI(dateRangeInput("daterange", "Date", start = time_start_end$date_start, end = time_start_end$date_end))
   }

   # gender choice
   output$checkboxgroup_gender <- renderUI(
      checkboxGroupInput(inputId = "checkboxgroup_gender", label = "Chose gender",
      choices = list("male" = "male", "female" = "female"), selected = c("male", "female")))

   #####################################################################################################################
   # Plotting and data output for downstream debugging
   #####################################################################################################################
   plotType <- input$plot_type
   write.csv2(C1, file = "all_data.csv")
   write.csv2(finalC1, file = "finalC1.csv")

   # filter out whole days with given threshold
   # TODO: Seems not to work correctly to filter out non-full days
   if (input$only_full_days) {
      time_diff <- get_time_diff(finalC1)
      finalC1 <- filter_full_days(finalC1, time_diff, input$full_days_threshold)
   }

   # curate data if desired
   if (input$curate) {
      finalC1 <- trim_front_end(finalC1, input$exclusion_end, input$exclusion_start)
   }

   switch(plotType,
   #####################################################################################################################
   # CompareHeatProductionFormulas
   #####################################################################################################################
   CompareHeatProductionFormulas = {
      p <- ggplot(data = finalC1, aes_string(x = "HP", y = "HP2"))
      p <- p + geom_point() + stat_smooth(method = "lm")
      p <- p + stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
      p <- p + ggtitle("Pearson-correlation between energy expenditures as computed by two different formulas")
   },

   #####################################################################################################################
   # GoxLox
   #####################################################################################################################
   GoxLox = {
      # Metadata from TSE file header should be enough, want to see oxidation of substrates by animals
      C1meta_tmp <- C1meta
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")

      # MK formulas
      if (input$goxlox == "Glucose oxidation") {
         df_to_plot$GoxLox <- scaleFactor * 4.55 * df_to_plot$`VO2(3)_[ml/h]` - scaleFactor * 3.21 * df_to_plot$`VCO2(3)_[ml/h]`
      } else if (input$goxlox == "Lipid oxidation" || input$goxlox == "Fat oxidation") {
         df_to_plot$GoxLox <- scaleFactor * 1.67 * df_to_plot$`VO2(3)_[ml/h]` - scaleFactor * 1.67 * df_to_plot$`VCO2(3)_[ml/h]`
      # Turku formulas
      } else if (input$goxlox == "Nitrogen oxidation" || input$goxlox == "Protein oxidation") {
         df_to_plot$GoxLox <- 6.25 # this is constant 6.25 g N per minute
      }

      # df to plot prepared for a line plot over time, next few lines sum over whole day
      colors <- as.factor(`$`(df_to_plot, "Animal No._NA"))
      df_to_plot$Animals <- colors

      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][1], "", sep = "")
      }

      # df to plot now contains the summed oxidation over individual days   
      df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))
      GoxLox <- aggregate(df_to_plot$GoxLox, by = list(Animals = df_to_plot$Animals, Days = df_to_plot$Datetime), FUN = sum)
      GoxLox <- GoxLox %>% rename(GoxLox = x)

    if (input$havemetadata) {
         true_metadata <- get_true_metadata(input$metadatafile$datapath)
         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("GoxLox")),
               selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test)$plot_summary + xlab(input$covar) + ylab(input$dep_var) + ggtitle(input$study_description))
            )
         })

         # FIXME: Add back analysis without metadata sheet for TEE calculations

         output$details <- renderUI({
            results <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var)),
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
                     tags$tr(
                        tags$td(round(as.numeric(results$statistics$p), digits=6), style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$p.adj), digits=6), style="width: 100px"),
                        tags$td(results$statistics$p.adj.signif, style="width: 100px"),
                        tags$td(results$statistics$df, style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$statistic), digits=6), style="width: 100px")
                     )
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
    }


      p <- ggplot(data = df_to_plot, aes_string(y = "GoxLox", x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
      p <- p + ylab(paste(input$goxlox, "[ml/h]", sep = " ")) + xlab("Time [h]") + ggtitle(input$goxlox)
   },
   #####################################################################################################################
   ### Energy Expenditure
   #####################################################################################################################
   # This is an example on how to use the metadata (sheet) in different functions.
   ## The metadata from the data files (e.g. TSE) could be joined directly with the metadata sheet. For compability, we 
   # currently require a valid TSE metadata header corresponding with entries in the columns of metadata sheet, issue #62.
   EnergyExpenditure = {
      # colors for plotting as factor
      finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))
      if (input$havemetadata) {
         true_metadata <- get_true_metadata(input$metadatafile$datapath)
         finalC1 <- finalC1 %>% full_join(y = true_metadata, by = c("Animals")) %>% na.omit()
      } else {
         df_filtered <- C1meta[, colSums(is.na(C1meta)) == 0]
         df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
         df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
         colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
         colnames(df_filtered)[colnames(df_filtered) == "Animal.No."] <- "Animal No._NA"
         finalC1 <- merge(finalC1, df_filtered, by = "Animal No._NA")
      }

      # filter conditions
      if (input$with_grouping) {
         if (!input$havemetadata) {
            if (!is.null(input$select_data_by) && !is.null(input$condition_type)) {
               my_var <- input$condition_type
               colnames(finalC1)[colnames(finalC1) == "Animal No._NA"] <- "Animal.No."
               colnames(finalC1)[colnames(finalC1) == "Box_NA.x"] <- "Box"
               finalC1 <- finalC1 %>% filter((!!sym(my_var)) == input$select_data_by)
               colnames(finalC1)[colnames(finalC1) == "Animal.No."] <- "Animal.No._NA"
               colnames(finalC1)[colnames(finalC1) == "Box"] <- "Box_NA.x"
            }
         } else {
            if (!is.null(input$condition_type) && !is.null(input$select_data_by)) {
               my_var <- input$condition_type
               finalC1 <- finalC1 %>% filter((!!sym(my_var)) == input$select_data_by)
            }
         }
      }

      # prepare for facet usage when no metadata available
      if (input$with_facets) {
         if (!input$havemetadata) {
            colnames(finalC1)[colnames(finalC1) == "Animal No._NA"] <- "Animal.No."
            colnames(finalC1)[colnames(finalC1) == "Box_NA.x"] <- "Box"
         }
      }

      # calculate running averages
      if (input$running_average > 0) {
         p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = "HP2", color = "Animals", group = "Animals"))
         if (input$running_average_method == "Mean") {
            p <- p + geom_line(aes(y = rollmean(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
         } else if (input$running_average_method == "Max") {
            p <- p + geom_line(aes(y = rollmax(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
         } else if (input$running_average_method == "Median") {
            p <- p + geom_line(aes(y = rollmedian(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
         } else if (input$running_average_method == "Sum") {
            p <- p + geom_line(aes(y = rollsum(HP2, input$running_average, na.pad = TRUE)), group = "Animals")
         } else {
            p <- p + geom_line(aes(y = HP2), group = "Animals")
         }
      } else {
         p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = "HP2", color = "Animals", group = "Animals"))
         p <- p + geom_line()
      }


      # add means
      if (input$wmeans) {
         p <- p + geom_smooth(method = input$wmeans_choice)
      }

      # add stats
      if (input$wstats) {
         p <- p + stat_cor(method = input$wmethod)
      }

      # axis labels
      p <- p + xlab("Time [h]")
      p <- p + ylab(paste("Energy expenditure [", input$kj_or_kcal, "/ h]", sep = " "))

     # add light cycle annotation
     lights <- data.frame(x = finalC1["running_total.hrs.halfhour"], y = finalC1["HP2"])
     colnames(lights) <- c("x", "y")
     
     if (input$timeline) {
      my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, 0, input$light_cycle_day_color, input$light_cycle_night_color)
      p <- p + my_lights
     }

     # add title
     p <- p + ggtitle(paste("Energy expenditure [", input$kj_or_kcal, "/ h]", " using equation ", input$myp))

     # group with group from metadata
     if (input$with_facets) {
        if (!is.null(input$facets_by_data_one)) {
           if (input$orientation == "Horizontal") {
              p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
          } else {
              p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
           }
        }
     }
     p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
   },
   #####################################################################################################################
   ### RestingMetabolicRate
   #####################################################################################################################
   RestingMetabolicRate = {
      component2 <- ""
      if (length(input$cvs) == 2) {
         component <- input$cvs[[1]]
         component2 <- input$cvs[[2]]
      } else {
         component <- input$cvs
         component2 <- component
      }
      component <- paste("V", component, "(3)_[ml/h]", sep = "")
      component2 <- paste("V", component2, "(3)_[ml/h]", sep = "")

      if (length(input$cvs) == 0) {
         component <- "HP2"
         component2 <- "HP"
      }

      # first component, typically O2
      df <- data.frame(Values = finalC1[[component]],
         Group = `$`(finalC1, "Animal No._NA"),
         Values2 = finalC1$HP)

      df_new <- partition(df)
      print(df_new)
      write.csv2(df_new, "df_new_before.csv")
      # Note that this might introduce NAs by coercion through cv(...) method.
      ## This is correct, since the time length of recordings per sample are not identical typically
      df_new <- cv(df_new, input$window)
      write.csv2(df_new, "df_new_after.csv")
      df_new <- reformat(df_new) %>% na.omit()
      write.csv2(df_new, "df_new_after_reformat.csv")

      write.csv2(df_new, "df_new.csv")

      # second component, typically CO2
      df2 <- data.frame(Values = finalC1[[component2]],
         Group = `$`(finalC1, "Animal No._NA"),
         Values2 = finalC1$HP)
      df_new2 <- partition(df2)
      # Note that this might introduce NAs by coercion through cv(...) method.
      ## This is correct, since the time length of recordings per sample are not identical typically
      df_new2 <- cv(df_new2, input$window)
      df_new2 <- reformat(df_new2) %>% na.omit()

      finalC1$Datetime <- lapply(finalC1$Datetime, convert)

      # if coefficient of variation is used in analysis, we might end up with 1 less timepoint
      # thus we need to make sure to always take the minimum of these three dataframes or pad accordingly each sample
      ## Note that this happens when input$window 
      do_select_n <- min(nrow(finalC1), nrow(df_new), nrow(df_new2))
      to_pad <- nrow(finalC1) - nrow(df_new) # difference is exactly the number of samples as only 1 point misses 
      df_new <- padding_helper(df_new) # pads by replicating the last value for each sample in timeline and inserting a new row after the last row for each sample
      df_new2 <- padding_helper(df_new2) # padas by replicting the last value for each sample in timeline and inserting a new row after the last row for each sample

      finalC1 <- finalC1 %>%  slice(1:(do_select_n-1)) # removes the last point for each of the samples available, since we use averaging, note that finalC1 is grouped by animals seemingly
      df_new <- df_new %>%  slice(1:(do_select_n+to_pad)) 
      df_new2 <- df_new2 %>%  slice(1:(do_select_n+to_pad))

      df_to_plot <- cbind(df_new, `$`(finalC1, "running_total.hrs.halfhour"))
      df_to_plot2 <- cbind(df_new2, `$`(finalC1, "running_total.hrs.halfhour"))
      df_to_plot$Group <- as.factor(df_to_plot$Group)
      df_to_plot2$Group <- as.factor(df_to_plot$Group)

      write.csv2(df_new, file = "df_new.csv")
      colnames(df_to_plot) <- c("RestingMetabolicRate", "Animal", "Time")
      colnames(df_to_plot2) <- c("RestingMetabolicRate2", "Animal", "Time")
      write.csv2(df_to_plot, file = "df_to_plot.csv")

      df_for_cov_analysis <- cbind(df_to_plot, `$`(finalC1, "VO2(3)_[ml/h]"),
         `$`(finalC1, "VCO2(3)_[ml/h]"), `$`(finalC1, "HP"),
         df_to_plot2$RestingMetabolicRate2)
      df_for_cov_analysis$Group <- df_to_plot$Group
      colnames(df_for_cov_analysis) <- c("CoV1", "Animal", "Time", "O2", "CO2", "HP", "CoV2")
      write.csv2(df_for_cov_analysis, file = "df_for_cov_analysis.csv")

      M <- input$window
      PERCENTAGE <- input$percentage_best
      INTERVAL_LENGTH <- time_diff
      # TODO: 1, 1 seems a reasonable choice to reconstruct RMR, but needs to be validated
      df_plot_total <- extract_rmr_helper(INTERVAL_LENGTH, 1, 1)
      write.csv2(df_plot_total, file = "df_for_comparison_with_calimera.csv")
      df_plot_total$HP <- as.numeric(df_plot_total$HP) * 1000
      df_plot_total$Time <- as.numeric(df_plot_total$Time)

      # TODO: This seems problematic when using TEE to compare with, why? Only one component really needed.
      # df_plot_total <- df_plot_total %>% filter(Component %in% input$cvs)

      p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, group = Component,
      color = Component)) + geom_line() + facet_wrap(~Animal)
      p <- p + ylab(paste("RMR [", input$kj_or_kcal, "/ h]", "(equation: ", input$myp, ")", sep = " "))
      p <- p + xlab("Time [minutes]")
      p <- p + ggtitle("Resting metabolic rates")
      finalC1 <- df_plot_total
   },
   #####################################################################################################################
   ### Weight vs Energy Expenditure
   #####################################################################################################################
   WeightVsEnergyExpenditure = {
      C1meta_tmp <- C1meta
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")
      df_to_plot["HP"] <- df_to_plot["HP"] / 24
      p <- ggplot(df_to_plot, aes(x = `Weight..g.`, y = `HP`, color = `Animal No._NA`))
      p <- p + geom_violin(trim = FALSE) + geom_jitter(position = position_jitter(0.2))
      if (! is.null(input$statistics)) {
         if (input$statistics == "mean_sdl") {
         } else {
            p <- p + stat_summary(fun.y = input$statistics, color = "red")
         }
      }

   if (input$with_facets) {
      if (!is.null(input$facets_by_data_one)) {
         if (input$orientation == "Horizontal") {
            p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
         } else {
            p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
         }
      }
   }

   p <- p + ggtitle("Weight vs Energy Expenditure")
   p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
   },
   #####################################################################################################################
   ### Day Night Activity
   #####################################################################################################################
   DayNightActivity = {
      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][2], ":00", sep = "")
      }
      C1meta_tmp <- C1meta
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")
      df_to_plot$Animals <- as.factor(`$`(df_to_plot, "Animal No._NA"))

      df_to_plot$Datetime2 <- lapply(df_to_plot$Datetime, convert)
      df_to_plot$Datetime <- lapply(df_to_plot$Datetime, convert)
      light_on <- 720
      if (input$havemetadata) {
         light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
      }

      if (input$override_metadata_light_cycle) {
         light_on <- 60 * input$light_cycle_start
      }

      # df already prepared to be day and night summed activities
      df_to_plot$NightDay <- ifelse(hour(hms(df_to_plot$Datetime2)) * 60 + minute(hms(df_to_plot$Datetime2)) < light_on, "am", "pm")
      df_to_plot$NightDay <- as.factor(df_to_plot$NightDay)

      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][1], "", sep = "")
      }

      # insert day to group by different days in ANCOVA
      df_to_plot$Days <- day(dmy(lapply(df_to_plot$Datetime, convert)))

 if (input$havemetadata) {
         true_metadata <- get_true_metadata(input$metadatafile$datapath)
         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               # TODO: needs potentially a 3-way ANCOVA, on Days, Genotype and NightDay
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA", "3-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("HP")),
               selectInput("indep_var", "Independent grouping variable #1", choices = "NightDay", selected = "NightDay"),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               conditionalPanel("input.test_statistic == '3-way ANCOVA'", selectInput("indep_var3", "Independent grouping variable #3", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(df_to_plot, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "HP", input$test_statistic, input$post_hoc_test)$plot_summary + xlab(input$covar) + ylab(input$dep_var) + ggtitle(input$study_description))
            )
         })
 }

         # FIXME: Add back analysis without metadata sheet for TEE calculations

output$details <- renderUI({
            results <- do_ancova_alternative(df_to_plot, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "HP", input$test_statistic, input$post_hoc_test)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var)),
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
                     tags$tr(
                        tags$td(round(as.numeric(results$statistics$p), digits=6), style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$p.adj), digits=6), style="width: 100px"),
                        tags$td(results$statistics$p.adj.signif, style="width: 100px"),
                        tags$td(results$statistics$df, style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$statistic), digits=6), style="width: 100px")
                     )
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




      if (input$day_only && input$night_only) {
         # nothing to do we keep both night and day
      } else if (input$night_only) {
         df_to_plot <- df_to_plot %>% filter(NightDay == "pm")
      } else if (input$day_only) {
         df_to_plot <- df_to_plot %>% filter(NightDay == "pm")
      } else {
         df_to_plot <- NULL
      }

      p <- ggplot(df_to_plot, aes(x = Animals, y = HP, fill = NightDay)) + geom_violin()
      p <- p + ggtitle("Day Night Activity")


      if (input$with_facets) {
            if (!is.null(input$facets_by_data_one)) {
               if (input$orientation == "Horizontal") {
                  p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
               } else {
                  p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
               }
            }
      }

      p <- p + ylab(paste("Energy expenditure [", input$kj_or_kcal, "/ h]", "(equation: ", input$myp, ")", sep = " "))
      if (input$with_facets) {
         p <- ggplotly(p) %>% layout(boxmode = "group") %>% # nolint: pipe_continuation_linter.
         config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
      } else {

         p <- ggplotly(p) %>% layout(boxmode = "group") %>% # nolint: pipe_continuation_linter.
         config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
      }
   },
   #####################################################################################################################
   ### Locomotion
   #####################################################################################################################
   Locomotion = {
      file <- input[[paste0("File", 1)]]
      if (input$have_box_coordinates) {
         p <- plot_locomotion(file$datapath, input$x_min_food, input$x_max_food, input$y_min_food, input$y_max_food, input$x_min_scale, input$x_max_scale, input$y_min_scale, input$y_max_scale, input$x_min_bottle, input$x_max_bottle, input$y_min_bottle, input$y_max_bottle)
      } else {
         p <- plot_locomotion(file$datapath)
      }
      p <- p + ggtitle("Probability density map of locomotion")
      p
   },

   #####################################################################################################################
   ### Locomotion Budget
   #####################################################################################################################
   LocomotionBudget = {
      file <- input[[paste0("File", 1)]]
      p <- plot_locomotion_budget(file$datapath)
      p <- p + ggtitle("Locomotional budget")
      p
   },
   #####################################################################################################################
   ### Estimate RMR for COSMED
   #####################################################################################################################
   EstimateRMRforCOSMED = {
      C1meta_tmp <- C1meta
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")
      endtime <- input$rmr_method_begin # in minutes for ss methods
      stepwidth <- input$rmr_method_frequency # in seconds
      endindex <- endtime * 60 / stepwidth
      n <- nrow(df_to_plot) / endindex

      CV_RER_MAX <- input$SS_method_RER # in %
      CV_VO2_MAX <- input$SS_method_VO2 # in %
      CV_VCO2_MAX <- input$SS_method_VCO2 # in %

      indices_to_keep <-  c()
      indices_to_discard <- c()
      for (i in 1:n) {
         indices <- seq((i - 1) + (endindex - 1) * (i - 1), i * (endindex) - 1)
         values <- df_to_plot[indices, "RER_NA"]
         cv_rer <- 100 * (sd(values) / mean(values))
         values <- df_to_plot[indices, "VO2(3)_[ml/h]"]
         cv_vo2 <- 100 * (sd(values) / mean(values))
         values <- df_to_plot[indices, "VCO2(3)_[ml/h]"]
         cv_vco2 <- 100 * (sd(values) / mean(values))
         if ((cv_rer < CV_RER_MAX) && (cv_vo2 < CV_VO2_MAX) && (cv_vco2 < CV_VCO2_MAX)) {
            indices_to_keep <- append(indices_to_keep, i)
         } else {
            indices_to_discard <- append(indices_to_keep, i)
         }
      }

      total_indices <- c()
      for (i in indices_to_keep) {
         indices <- seq((i - 1) + (endindex - 1) * (i - 1), i * (endindex) - 1)
         total_indices <- append(total_indices, indices)
      }

      if (input$rmr_method == "SS") {
         min_rer <- min(df_to_plot[total_indices, "RER_NA"])
         min_vco2 <- min(df_to_plot[total_indices, "VCO2(3)_[ml/h]"])
         min_vo2 <- min(df_to_plot[total_indices, "VO2(3)_[ml/h]"])
      } else {
         min_rer <- min(df_to_plot[c(indices_to_discard, indices_to_keep), "RER_NA"])
         min_vco2 <- min(df_to_plot[c(indices_to_discard, indices_to_keep), "VCO2(3)_[ml/h]"])
         min_vo2 <- min(df_to_plot[c(indices_to_discard, indices_to_keep), "VO2(3)_[ml/h]"])
      }

      # weir formula (for test with COSMED data)
      rmr <- 1440 * (3.9 * min_vo2 / 1000 + 1.1 * min_vco2 / 1000)
      # plot RMR as histogram
      df <- data.frame(c(rmr))
      names(df) <- rmr
      p <- ggplot(df, aes(x = rmr)) + geom_histogram(fill = "green")
      p <- p + ggtitle("Resting metabolic rate for COSMED")
      ggplotly(p)
   },
   #####################################################################################################################
   ### Raw
   #####################################################################################################################
   Raw = {
      C1meta_tmp <- C1meta
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")

      write.csv2(df_to_plot, file = "finalC1.csv")
      colors <- as.factor(`$`(df_to_plot, "Animal No._NA"))
      df_to_plot$Animals <- colors
      mylabel <- paste0(input$myr, sep = "", "_[%]")
      myvar <- input$myr
      if (startsWith(input$myr, "V")) {
         mylabel <- paste0(input$myr, sep = "", "(3)_[ml/h]")
         names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
      }

      if (startsWith(input$myr, "Temp")) {
         mylabel <- paste0(input$myr, sep = "", "_C")
         names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
      }

      if (startsWith(input$myr, "RER")) {
         mylabel <- "RER"
      }

      names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
      names(df_to_plot)[names(df_to_plot) == "RER_NA"] <- "RER"

      p <- ggplot(data = df_to_plot, aes_string(y = input$myr, x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
      mylabel <- gsub("_", " ", mylabel)

      lights <- data.frame(x = df_to_plot["running_total.hrs.halfhour"], y = df_to_plot[input$myr])
      colnames(lights) <- c("x", "y")
      if (input$timeline) {
            my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, 0, input$light_cycle_day_color, input$light_cycle_night_color)
            p <- p + my_lights
      }

      p <- p + ylab(mylabel)
      p <- p + xlab("Time [h]")

     convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][1], "", sep = "")
      }

      # df to plot now contains the summed oxidation over individual days   
      df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))
      df_to_plot$GoxLox = df_to_plot[input$myr]
      GoxLox <- aggregate(df_to_plot$GoxLox, by = list(Animals = df_to_plot$Animals, Days = df_to_plot$Datetime), FUN = sum) %>% rename("Raw" = input$myr)
      

    if (input$havemetadata) {
         true_metadata <- get_true_metadata(input$metadatafile$datapath)
         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("Raw")),
               selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test)$plot_summary + xlab(input$covar) + ylab(input$dep_var) + ggtitle(input$study_description))
            )
         })

         # FIXME: Add back analysis without metadata sheet for TEE calculations

         output$details <- renderUI({
            results <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var)),
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
                     tags$tr(
                        tags$td(round(as.numeric(results$statistics$p), digits=6), style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$p.adj), digits=6), style="width: 100px"),
                        tags$td(results$statistics$p.adj.signif, style="width: 100px"),
                        tags$td(results$statistics$df, style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$statistic), digits=6), style="width: 100px")
                     )
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
    }


      if (input$with_facets) {
         if (!is.null(input$facets_by_data_one)) {
            if (input$orientation == "Horizontal") {
               p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
            } else {
               p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
            }
         }
      }

      p <- p + ggtitle(paste("Raw measurement: ", mylabel, sep = ""))
      p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
   },
   #####################################################################################################################
   ### Total Energy Expenditure
   #####################################################################################################################
   TotalEnergyExpenditure = {
      colors <- as.factor(`$`(finalC1, "Animal No._NA"))
      finalC1$Animals <- colors

      C1meta_tmp <- C1meta
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      finalC1 <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")

      print("meta columns:")
      print(colnames(finalC1))

      convert2 <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][2], ":00", sep = "")
      }

      finalC1$Datetime2 <- lapply(finalC1$Datetime, convert2)
      light_on <- 720
      if (input$havemetadata) {
         light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
      }

      if (input$override_metadata_light_cycle) {
         light_on <- 60 * input$light_cycle_start
      }

      finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime2)) * 60 + minute(hms(finalC1$Datetime2)) < light_on, "am", "pm")

      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][1], "", sep = "")
      }

      time_diff <<- get_time_diff(finalC1)
      finalC1$Datetime <- day(dmy(lapply(finalC1$Datetime, convert)))
      finalC1$HP <- finalC1$HP / time_diff
      finalC1$HP2 <- finalC1$HP2 / time_diff

      if (input$day_only && input$night_only) {
         # nothing to do we keep both night and day
      } else if (input$night_only) {
         finalC1 <- finalC1 %>% filter(NightDay == "pm")
      } else if (input$day_only) {
         finalC1 <- finalC1 %>% filter(NightDay == "pm")
      } else {
         finalC1 <- NULL
      }

      TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$Datetime), FUN = sum)
      TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$Datetime), FUN = sum)

      if (input$with_facets) {
         if (input$facets_by_data_one %in% names(finalC1)) {
            TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$Datetime, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum)
            TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$Datetime, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum)
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
      TEE <- TEE %>% filter(Equation == input$variable1)

       convert <- function(x) {
                 splitted <- strsplit(as.character(x), " ")
                paste(splitted[[1]][1], "", sep = "")
                }
                # df to plot now contains the summed oxidation over individual days   
               df_unique_days <- TEE %>% group_by(Animals) %>% summarize(unique_days = n_distinct(Days))
         TEE <- left_join(TEE, df_unique_days, by = "Animals")
         # calculate averages of TEE over number of given days
         TEE <- TEE %>% mutate(TEE = TEE / unique_days)

      write.csv2(TEE, "tee_after_mutate.csv")



      p <- ggplot(data = TEE, aes(x = Animals, y = TEE, label = Days)) + geom_point() + geom_violin(fill="grey80", colour="#3366FF") # position = position_jitterdodge())
      p <- p + geom_text(check_overlap = TRUE, aes(label = Days),  position = position_jitter(width = 0.15))
      p <- p + ylab(paste("TEE [", input$kj_or_kcal, "/day]", sep = ""))
      if (input$with_facets) {
         if (!is.null(input$facets_by_data_one)) {
            if (input$orientation == "Horizontal") {
            p <- p + facet_grid(as.formula(".~Facet"))
            } else {
            p <- p + facet_grid(as.formula("Facet~."))
            }
         }
      }

      if (input$havemetadata) {
         true_metadata <- get_true_metadata(input$metadatafile$datapath)
         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("TEE")),
               selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test)$plot_summary + xlab(input$covar) + ylab(input$dep_var) + ggtitle(input$study_description))
            )
         })

         # FIXME: Add back analysis without metadata sheet for TEE calculations

         output$details <- renderUI({
            results <- do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var)),
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
                     tags$tr(
                        tags$td(round(as.numeric(results$statistics$p), digits=6), style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$p.adj), digits=6), style="width: 100px"),
                        tags$td(results$statistics$p.adj.signif, style="width: 100px"),
                        tags$td(results$statistics$df, style="width: 100px"),
                        tags$td(round(as.numeric(results$statistics$statistic), digits=6), style="width: 100px")
                     )
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
      }
      p <- p + ggtitle(paste("Total energy expenditure (days=", length(levels(TEE$Days)), ") using equation ", input$variable1, sep = ""))
      p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
      },
      {
         # all other options
      }
   )
   # return data to UI
   list("plot" = p, "animals" = `$`(finalC1, "Animal No._NA"), "data" = finalC1, "metadata" = C1meta)
}

#####################################################################################################################
# Create server
#####################################################################################################################
server <- function(input, output, session) {
   # observer helpers
   observe_helpers()

   # observer metadata field
   output$metadatafile <- renderUI({
      html_ui <- " "
      html_ui <- paste0(html_ui,
         fileInput("metadatafile",
         label = "Metadata file"))
      HTML(html_ui)
   })

   # if no data available, no download button
   output$downloadData <- downloadHandler(
      filename = function() {
      if (! input$export_file_name == "") {
         paste(input$export_file_name, ".csv", sep = "")
      } else {
         extension <- ".csv"
         if (input$export_format == "Excel") {
            extension <- ".xlsx"
         }
         paste("data-", Sys.Date(), input$export_format, extension, sep = "")
      }
      },
         content = function(file) {
            status_okay <- do_export_alternative(input$export_format, input, output, file, do_plotting)
      }
   )

   # Dynamically create fileInput fields by the number of requested files of the user
   output$fileInputs <- renderUI({
      html_ui <- " "
      for (i in 1:input$nFiles) {
         html_ui <- paste0(html_ui, fileInput(paste0("File", i),
            label = paste0("Cohort ", i)))
         }
      HTML(html_ui)
      })

   #####################################################################################################################
   # Observe heat production formula 1 and forumula 2 and print formular to label
   #####################################################################################################################
   observeEvent(c(input$variable1, input$variable2, input$plot_type), {
         text1 <- ""
         text2 <- ""
         switch(input$variable1,
         Weir = {
            text1 <- "$$ \\tag{1} 16.3 \\times VO2[\\frac{ml}{h}] + 4.57 \\times RER $$"
         },
         HP = {
            text1 <- "$$ \\tag{1} VO2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         HP2 = {
            text1 <- "$$ \\tag{1} (4.44 + 1.43 \\times RER) + VO2[\\frac{ml}{h}] $$"
         },
         Lusk = {
            text1 <- "$$ \\tag{1} 15.79 \\times VO2[\\frac{ml}{h}] + 5.09 \\times RER $$"
         },
         Elia = {
            text1 <- "$$ \\tag{1} 15.8 \\times VO2[\\frac{ml}{h}] + 5.18 \\times RER $$"
         },
         Brouwer = {
            text1 <- "$$ \\tag{1} 16.07 \\times VO2[\\frac{ml}{h}]+ 4.69 \\times RER $$"
         },
         Ferrannini = {
            text1 <- "$$ \\tag{1} 16.37117 \\times VO2[\\frac{ml}{h}] + 4.6057 \\times RER $$"
         },
         {
         }
         )

         switch(input$variable2,
         Weir = {
            text2 <- "$$ \\tag{2} 16.3 \\times VO2[\\frac{ml}{h}] + 4.57 \\times RER $$"
         },
         HP = {
            text2 <- "$$ \\tag{2} VO2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         HP2 = {
            text2 <- "$$ \\tag{2} (4.44 + 1.43 \\times RER) + VO2[\\frac{ml}{h}] $$"
         },
         Lusk = {
            text2 <- "$$ \\tag{2} 15.79 \\times VO2[\\frac{ml}{h}] + 5.09 \\times RER $$"
         },
         Elia = {
            text2 <- "$$ \\tag{2} 15.8 \\times VO2[\\frac{ml}{h}] + 5.18 \\times RER $$"
         },
         Brouwer = {
            text2 <- "$$ \\tag{2} 16.07 \\times VO2[\\frac{ml}{h}] + 4.69 \\times RER $$"
         },
         Ferrannini = {
            text2 <- "$$ \\tag{2} 16.37117 \\times VO2[\\frac{ml}{h}] + 4.6057 \\times RER $$"
         },
         {
         }
         )

         if (input$plot_type == "CompareHeatProductionFormulas") {
            output$heat_production_equations <- renderUI(
               tagList(
               withMathJax(),
               div("Chosen equations for calculation of heat production"),
               div(text1),
               div(text2)
               )
            )
         } else {
            output$heat_production_equations <- renderUI(
               tagList(
               withMathJax(),
               div("Chosen equation for calculation of heat production"),
               div(text1)
               )
            )
         }
   })
   #####################################################################################################################
   # Observe plot type
   #####################################################################################################################
   observeEvent(input$plot_type, {
            output$myp <- renderUI(
               selectInput(inputId = "myp",
               label = "Chose prefered method for calculating caloric equivalent over time",
               choices = c(input$variable1, input$variable2), selected = input$variable1))
         })

   observeEvent(input$plot_type, {
            output$checkboxgroup_gender <- renderUI(
               checkboxGroupInput(inputId = "checkboxgroup_gender", label = "Chose gender",
               choices = list("male" = "male", "female" = "female"), selected = c("male", "female")))
         })

   observeEvent(input$plot_type, {
      output$myr <- renderUI(
         selectInput(inputId = "myr", label = "Chose raw data to plot", choices = c("O2", "CO2", "RER", "VO2", "VCO2", "Temp")))
    })

   observeEvent(input$plot_type, {
            output$wmeans <- renderUI(
               checkboxInput(inputId = "wmeans", label = "Display means"))

            output$wmeans_choice <- renderUI(
               selectInput(inputId = "wmeans_choice", label = "Method", choices = c("lm", "glm", "gam", "loess")))
         })

   observeEvent(input$plot_type, {
            output$wstats <- renderUI(
               checkboxInput(inputId = "wstats", label = "Display statistics"))
         })

   observeEvent(input$plot_type, {
            output$wmethod <- renderUI(
               selectInput(inputId = "wmethod", label = "Statistic", choices = c("pearson", "kendall", "spearman")))
         })

   #####################################################################################################################
   # Observe export events
   #####################################################################################################################
   observeEvent(input$export_folder, {
       output$folder_name_export <- renderUI(
            renderText(paste(path_home(), input$export_folder$path[[2]], sep = "/")))
      })

   observeEvent(input$downloadData, {
      # CalR
      if (input$export_format == "CalR") {
           status_okay <- do_export("CalR", input, output, do_plotting)
           if (!status_okay) {
             output$message <- renderText("Error during data export to CalR, check logs")
           } else {
             output$message <- renderText(paste("Consolidated data exported to format >>",
             input$export_format, "<<", sep = " "))
           }
      }
      # Excel
      if (input$export_format == "Excel") {
           status_okay <- do_export_alternative("Excel", input, output, do_plotting)
          if (!status_okay) {
             output$message <- renderText("Error during data export to Excel, check logs")
           } else {
             output$message <- renderText(paste("Consolidated data exported to format >>",
             input$export_format, "<<", sep = " "))
           }
       }
     })

   #############################################################################
   # Refresh plot (action button's action)
   #############################################################################
   observeEvent(c(input$replotting, input$daterange), {
           output$plot <- renderPlotly({
           file <- input$File1
           real_data <- do_plotting(file$datapath, input, exclusion = input$sick, output)
           time_start_end <<- get_date_range(real_data$data)
           real_data$plot
           })
   })

   #############################################################################
   # Observe validate plotting result
   #############################################################################
   observeEvent(input$plottingvalidation, {
         output$plotvalidation <- renderPlotly({
            ref <- read.csv2(input$rerself$name, na.strings = c("-", "NA"), header = FALSE, sep = ";")
            calr <- read.csv2(input$rercalr$name, na.strings = c("-", "NA"), header = FALSE, sep = ";")
            df_comparison <- data.frame(ref = as.numeric(gsub(",", ".", gsub("\\.", "", ref$V11))),
               calr = as.numeric(gsub(",", ".", gsub("\\.", "", calr$V11))))
            p <- ggscatter(df_comparison, x = "ref", y = "calr", add = "reg.line",
            add.params = list(color = "black", fill = "lightgray")) +
            stat_cor(method = "pearson") +
            theme(axis.text.x = element_text(angle = 90)) +
            xlab("RER self") +
            ylab("RER CalR") +
            ggtitle(input$plotTitle)
            p
         })
   })

   real_data <- NULL
   #############################################################################
   # Show plot (action button's action)
   #############################################################################
   observeEvent(input$plotting, {
      output$plot <- renderPlotly({
         if (is.null(input$File1)) {
            output$message <- renderText("Not any cohort data given. Need at least one data set.")
         } else {
           file <- input$File1
           real_data <- do_plotting(file$datapath, input, input$sick, output)

           if (! is.null(real_data$status)) {
               if (real_data$status == FALSE) {
                  output$message <- renderText("Input data incompatible, make sure you either supply only TSE or Sable system files not a combination of both file types.") #nolint
               } else {
                  output$message <- renderText(real_data$status)
               }
            }

            #############################################################################
            # Outlier
            #############################################################################
            if ((! is.null(real_data$animals)) && is.null(input$sick)) {
              output$sick <- renderUI(
              multiInput(inputId = "sick", label = "Remove outliers (sick animals, etc.) ", selected = "", choices = unique(real_data$animals)))
            }

            #############################################################################
            # Facets
            #############################################################################
            if ((! is.null(real_data$animals)) && is.null(input$facets_by_data_one)) {
               if (input$havemetadata) {
                  true_metadata <- get_true_metadata(input$metadatafile$datapath)
                  output$facets_by_data_one <- renderUI(
                  selectInput(inputId = "facets_by_data_one", label = "Choose facet",
                  selected = "Animals", choices = colnames(true_metadata)))
                } else {
                  df_filtered <- real_data$metadata[, colSums(is.na(real_data$metadata)) == 0]
                  df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
                  df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
                  colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
                  our_group_names <- unique(colnames(df_filtered))

                  output$facets_by_data_one <- renderUI(
                     selectInput(inputId = "facets_by_data_one", label = "Choose facet",
                     selected = "Animals", choices = our_group_names))
                  }
             }  

            #############################################################################
            # Initializing
            #############################################################################
            if (input$havemetadata) {
               if (is.null(input$condition_type)) {
                  true_metadata <- get_true_metadata(input$metadatafile$datapath)
                  output$condition_type <- renderUI(selectInput(inputId = "condition_type", colnames(true_metadata), label = "Condition"))
               }
            } else {
               if (! is.null(real_data$animals)) {
                  metadata <- real_data$metadata
                  output$condition_type <- renderUI(selectInput(inputId = "condition_type", colnames(metadata), label = "Condition"))
               }
            }

            #############################################################################
            # condition type and select data by
            #############################################################################
            observeEvent(input$condition_type, {
            if (input$havemetadata) {
               true_metadata <- get_true_metadata(input$metadatafile$datapath)
               output$select_data_by <- renderUI(selectInput("select_data_by", "Filter by", choices = unique(true_metadata[[input$condition_type]]), selected = input$select_data_by))
            } else {
               metadata <- real_data$metadata
               my_var <- input$condition_type
               diets <- c()
               if (is.null(my_var)) {
                  diets <- unique(metadata %>% select(1) %>% pull())
               } else {
                  diets <- unique(metadata[[my_var]])
               }
               output$select_data_by <- renderUI(
               selectInput("select_data_by", "Filter by", choices = diets, selected = input$select_data_by))
            }
            })

            if (input$plot_type == "RestingMetabolicRate") {
              showTab(inputId = "additional_content", target = "Summary statistics")
               write.csv2(real_data$data, "before_rmr_written.csv")
               # bar plot rmr vs non-rmr (we filter out nans just in case to be sure - might come from covariance analysis above)
               df_filtered <- real_data$data %>%
                  filter(Component != input$cvs) %>%
                  select(!Component) %>%
                  group_by(Animal) %>%
                  na.omit() %>%
                  summarize(Value = min(HP), cgroups = c(Animal))
               write.csv2(df_filtered, "rmr.csv")

               df <- real_data$data
               df$Animal <- as.factor(df$Animal)
               df$Component <- as.factor(df$Component)
               p <- ggplot(df, aes(x = Animal, y = HP, color = Animal)) + geom_violin() + geom_point(position = position_jitter(0.1))
               p <- p + xlab("Animal") + ylab(paste("RMR [", input$kj_or_kcal, "/h]", sep = ""))
               if (!input$havemetadata) {
                  output$test <- renderUI(renderPlotly(p))
               }

            # explanation of plot
            output$explanation <- renderUI({
               str1 <- "<h3> RMR estimation </h3>"
               str2 <- "Resting metabolic rate (RMR) has been estimated based on the coefficient of variation of the O2 and CO2 signal."
               str3 <- "Bar plots show minimum over whole recorded time (experiment), see Summary statistics tab."
               str4 <- "<hr/>"
               str5 <- "Time traces are available in the Plot tab, displaying RMR over time for each animal in cohort(s)."
               HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
            })

            # summary of plot
            output$summary <- renderPlotly(ggplotly(p))

            # if we have metadata, check time diff again to be consistent
            # TODO: should always be double checked, not only if metadata available
            time_diff <- 5
            df_diff <- read.csv2("finalC1.csv")
            if (input$havemetadata) {
               names(df_diff)[names(df_diff) == "Animal.No._NA"] <- "Animal No._NA"
               time_diff <- get_time_diff(df_diff, 1, 3)
               if (time_diff == 0) {
                  time_diff <- 5
               }
              
            }
          convert <- function(x) {
                 splitted <- strsplit(as.character(x), " ")
                paste(splitted[[1]][1], "", sep = "")
                }
                # df to plot now contains the summed oxidation over individual days   
               df_diff$Datetime <- day(dmy(lapply(df_diff$Datetime, convert)))
               df_unique_days <- df_diff %>% group_by(`Animal No._NA`) %>% summarize(unique_days = n_distinct(Datetime)) %>% rename(Animals = `Animal No._NA`)
               write.csv2(df_unique_days, "before_averaging_for_rmr.csv")




         df1 <- read.csv2("rmr.csv")
         df2 <- read.csv2("tee.csv")

         print(names(df_unique_days))

         df1 <- rename(df1, Animals = Animal)
         print(names(df1))
         how_many_days <- length(levels(as.factor(df2$Days)))
         df1$Animals <- as.factor(df1$Animals)
         df_unique_days$Animals = as.factor(df_unique_days$Animals)
         df2$Animals <- as.factor(df2$Animals)
         # time interval is determined by diff_time from data (not always fixed time interval in TSE systems)
         # Note: TEE over day might contain NANs in case we have not only FULL days in recordings of calorimetry data
         df1 <- df1 %>% group_by(Animals) %>% summarize(EE = sum(Value, na.rm = TRUE) / time_diff)
         df2 <- df2 %>% group_by(Animals) %>% summarize(EE = sum(TEE, na.rm = TRUE) / time_diff)

         df1 <- left_join(df1, df_unique_days, by = "Animals")
         df2 <- left_join(df2, df_unique_days, by = "Animals")

         # calculate averages of RMR over number of given days
         df1 <- df1 %>% mutate(EE = EE / unique_days)
         df2 <- df2 %>% mutate(EE = EE / unique_days)

         df1$TEE <- as.factor(rep("non-RMR", nrow(df1)))
         df2$TEE <- as.factor(rep("RMR", nrow(df2)))

         df_total <- rbind(df1, df2)
         df_total$Animals <- as.factor(df_total$Animals)

         p2 <- ggplot(data = df_total, aes(factor(Animals), EE, fill = TEE)) + geom_bar(stat = "identity")
         p2 <- p2 + xlab("Animal") + ylab(paste("EE [", input$kj_or_kcal, "/day]"))
         p2 <- p2 + ggtitle(paste("Energy expenditure (over a maximum of ", how_many_days, " days)", sep = ""))
         output$summary <- renderPlotly(ggplotly(p2))
         df_total <- df_total %>% filter(TEE == "RMR") %>% rename(RMR=EE)
         write.csv2(df_total, "test_for_rmr.csv")
         if (input$havemetadata) {
               true_metadata <- get_true_metadata(input$metadatafile$datapath)
               output$test <- renderUI({
                  tagList(
                     h4("Configuration"),
                     selectInput("test_statistic", "Test", choices = c("1-way ANCOVA")),
                     selectInput("dep_var", "Dependent variable", choice = c("RMR")),
                     selectInput("indep_var", "Independent grouping variable", choices = get_factor_columns(true_metadata), selected = "Genotype"),
                     selectInput("covar", "Covariate", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
                     conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
                     hr(style = "width: 50%"),
                     h4("Advanced"),
                     selectInput("post_hoc_test", "Post-hoc test", choices = c("bonferroni", "tukey", "spearman")),
                     sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
                     checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
                     hr(style = "width: 75%"),
                     renderPlotly(do_ancova_alternative(df_total, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "RMR", input$test_statistic, input$post_hoc_test)$plot_summary + xlab(input$covar) + ylab(input$dep_var))
                  )
                  })

               # FIXME: Add back analysis without metadata sheet for RMR calculations

               output$details <- renderUI({
                  results <- do_ancova_alternative(df_total, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "RMR", input$test_statistic, input$post_hoc_test)
                  tagList(
                     h3("Post-hoc analysis"),
                     renderPlotly(results$plot_details + xlab(input$indep_var)),
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
                           tags$tr(
                              tags$td(round(as.numeric(results$statistics$p), digits=6), style="width: 100px"),
                              tags$td(round(as.numeric(results$statistics$p.adj), digits=6), style="width: 100px"),
                              tags$td(results$statistics$p.adj.signif, style="width: 100px"),
                              tags$td(results$statistics$df, style="width: 100px"),
                              tags$td(round(as.numeric(results$statistics$statistic), digits=6), style="width: 100px")
                           )
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
                                       if (as.numeric(results$levene$p) < as.numeric(input$alpha_level)) {
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
                     )
                  )
               })
              }
           } else if (input$plot_type == "CompareHeatProductionFormulas") {
            output$explanation <- renderUI({
            str1 <- "<h3> Comparison of heat production formulas </h3>"
            str2 <- "Two heat production formulas can be compared via a simple scatter plot and plotting into the plot a regression line (Pearson-Product-Moment correlation coefficient r)" #nolint
            str3 <- "p-value reported, HP and HP2 correspond to the formulas displayed in the sidebar on the left"
            str4 <- "<hr/>"
            str5 <- "When heat production formulas agree mostly, so there should visually not be too many large residuals from a line of slope 1 be apparent in the plot." #nolint
            HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target = "Summary statistics")
               hideTab(inputId = "additional_content", target = "Details")
               hideTab(inputId = "additional_content", target = "Statistical testing")
           } else if (input$plot_type == "GoxLox") {
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Details")
               showTab(inputId = "additional_content", target = "Statistical testing")
            output$explanation <- renderUI({
               str1 <- "<h3> Glucose, lipid and protein oxidation </h3>"
                  str2 <- "Displays the glucose, lipid and protein oxidation by means of respiratory gas exchange measurements during indirect calorimetry"
               HTML(paste(str1, str2, sep = "<br/>"))
            })
           } else if (input$plot_type == "EnergyExpenditure") {
             output$explanation <- renderUI({
            str1 <- "<h3> Caloric Equivalent / heat production over time </h3>"
            str2 <- "According to a heat production formula the energy expenditure is calculated from indirect calorimetry data"
            str3 <- "Note that in case no metadata available to specify day and night, a single violin plot will be displayed per animal ID."
            str4 <- "<hr/>"
            str5 <- "Cohorts are usually stratified by animal ID by default"
            HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target = "Summary statistics")
               hideTab(inputId = "additional_content", target = "Statistical testing")
               hideTab(inputId = "additional_content", target = "Details")
           } else if (input$plot_type == "DayNightActivity") {
              output$explanation <- renderUI({
               str1 <- "<h3> Day and night (average) energy expenditure of animals in cohorts </h3>"
               str2 <- "According to a heat production formula the energy expenditure is calculated from indirect calorimetry data"
               str3 <- "<hr/>"
               str4 <- "Cohorts are usually stratified by animal ID and day night activity by default"
               HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
               })
               showTab(inputId = "additional_content", target = "Statistical testing")
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Details")
           } else if (input$plot_type == "Raw") {
            output$explanation <- renderUI({
               str1 <- "<h3> Raw measurements and derived quantities </h3>"
               str2 <- "The values of the raw measurement recorded over time during an indirect calorimetry experiment are displayed. Each line graphs respresents the raw measurement for an animal identified through either the ID reported in the metadata sheet, lab book or in the header of the raw data files. <hr/>"
               str3 <- "Note that oxygen consumption, carbondioxide production as well as derived quantities like the RER (respiratory exchange ratio) can be plotted by the drop-down menu on the left-hand side."
            HTML(paste(str1, str2, str3, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Statistical testing")
               showTab(inputId = "additional_content", target = "Details")
           } else if (input$plot_type == "TotalEnergyExpenditure") {
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Statistical testing")
               showTab(inputId = "additional_content", target = "Details")
           } else {
            output$summary <- renderPlotly(NULL)
            hideTab(inputId = "additional_content", target = "Explanation")
           }
           # plot
           real_data$plot
        }
      })

      # scroll to top after click on plot
      shinyjs::runjs("window.scrollTo(0,50);")
    })
 
   #############################################################################
   # Helpers to hide/show components
   #############################################################################
   lapply(
      X = c("DC", "HP", "DE", "PC"),
      FUN = function(i) {
         c(
         observeEvent(input[[paste0("hideTab", i)]], {
            hideTab(inputId = paste0("tabs", i), target = i)
         }),
         observeEvent(input[[paste0("showTab", i)]], {
            showTab(inputId = paste0("tabs", i), target = i, select = TRUE)
         })
         )
      }
   )


   #############################################################################
   # Hide certain components on startup
   #############################################################################
   lapply(
      X = c("DE", "PC"),
      FUN = function(i) {
         hideTab(inputId = paste0("tabs", i), target = i)
      }
   )

   #############################################################################
   # Reset session
   #############################################################################
   observeEvent(input$reset, {
      session$reload()
   })

   #############################################################################
   # Start guide
   #############################################################################
   observeEvent(input$guide, {
      # for guide, we need to see all components
      lapply(
         X = c("DC", "DE", "PC"),
         FUN = function(i) {
            showTab(inputId = paste0("tabs", i), target = i, select = TRUE)
         }
      )
      guide$init()$start()
  })

   #############################################################################
   # Observe guide
   #############################################################################
   observeEvent(input$guide_cicerone_next, {
      if (!input$guide_cicerone_next$has_next) {
         lapply(
            X = c("DC", "DE", "PC"),
            FUN = function(i) {
               hideTab(inputId = paste0("tabs", i), target = i)
            }
         )
      }
   })
}