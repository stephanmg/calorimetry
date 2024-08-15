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

source("inc/session_management.R") # for session management

# TODO: these global variables are not safe for multi-user scenario, and,
# select date start and end is obsolete, refactor and delete dead code next
time_start_end <- NULL
start_date <- "1970-01-01"
end_date <- Sys.Date()

global_data <- new.env()

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
do_plotting <- function(file, input, exclusion, output, session) { # nolint: cyclocomp_linter.
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
   finalC1meta <- data.frame(matrix(nrow = 0, ncol = 7))
   # Supported basic metadata fields from TSE LabMaster/PhenoMaster (these are defined manually by the user exporting the TSE files)
   colnames(finalC1meta) <- c("Animal.No.", "Diet", "Genotype", "Box", "Sex", "Weight..g.", "Dob")
   interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
   for (i in 1:input$nFiles) {
      file <- input[[paste0("File", i)]]
      file <- file$datapath
      con <- file(file)
      line <- readLines(con, n = 2)
   if (i == 1) {
      fileFormatTSE <- line[2]
      studyDescription <- line[1]
      if  (input$havemetadata) {
         output$study_description <- renderText(paste0("Study description: ", get_study_description_from_metadata(input$metadatafile$datapath)))
      } else {
         output$study_description <- renderText(paste("Study description: ", gsub("[;]", "", studyDescription), sep = " "))
      }
      output$file_type_detected <- renderText(paste("Input file type: ", gsub("[;,]", "", line[2]), sep = " "))
      output$additional_information <- renderText("Additional informations")
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

   # time diff (interval) or recordings, default if no time diff otherwise found
   time_diff <- getSession(session$token, global_data)[["time_diff"]]

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
      for (to_exclude_from_list in exclusion) {
         C1 <- C1 %>% filter(`Animal No._NA` != as.numeric(to_exclude_from_list))
      }
   }

   # coarsen data set (might need to average later than)
   if (input$coarsen_data_sets) {
      C1 <- coarsen_data_sets(C1, input$coarsening_factor)
   }

   # add interval info for each data frame / cohort separately
   interval_length_list[[paste0("Cohort #", i)]] <- list(values=c(unique(C1$`Animal No._NA`)), interval_length=get_time_diff(C1, 2, 3, input$detect_nonconstant_measurement_intervals))

   # compile final measurement frame
   finalC1 <- rbind(C1, finalC1)
   common_cols <- intersect(colnames(finalC1meta), colnames(C1meta))
   finalC1meta <- rbind(subset(finalC1meta, select = common_cols), subset(C1meta, select = common_cols))
   }

   # print master list for interval lengths
   storeSession(session$token, "interval_length_list", interval_length_list, global_data)
   pretty_print_interval_length_list(interval_length_list)


   # remove z-score outliers
   if (input$z_score_removal_of_outliers) {
     finalC1 <- remove_z_score_outliers(finalC1, input$sds)
   }

   # remove zero values
   if (input$remove_zero_values) {
      finalC1 <- remove_z_values(finalC1, input$eps)
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
   storeSession(session$token, "time_diff", get_time_diff(finalC1, 2, 3, input$detect_nonconstant_measurement_intervals), global_data)

   # set the time date ranges once for the final data frame
   if (is.null(time_start_end)) {
      time_start_end <<- get_date_range(finalC1)
      output$daterange <- renderUI(dateRangeInput("daterange", "Date", start = time_start_end$date_start, end = time_start_end$date_end))
   }


   #####################################################################################################################
   # Plotting and data output for downstream debugging
   #####################################################################################################################
   plotType <- input$plot_type
   write.csv2(C1, file = "all_data.csv")
   write.csv2(finalC1, file = "finalC1.csv")

   # filter out whole days with given threshold
   if (input$only_full_days) {
      storeSession(session$token, "time_diff", get_time_diff(finalC1, 2, 3, input$detect_nonconstant_measurement_intervals), global_data)
      print("int val length list:")
      print(interval_length_list)
      finalC1 <- filter_full_days_alternative(finalC1, input$full_days_threshold, interval_length_list)
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
      # colors for plotting as factor
      finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))

      # Select sexes
      if (!is.null(input$checkboxgroup_gender)) {
         if ("Sex" %in% names(finalC1)) {
           finalC1 <- finalC1 %>% filter(Sex %in% c(input$checkboxgroup_gender))
         }
      }

      # get metadata from tse header only
      data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
      finalC1 <- data_and_metadata$data
      true_metadata <- data_and_metadata$metadata

      # filter conditions
      # TODO: add this filtering to all panels.
      if (input$with_grouping) {
         my_var <- input$condition_type
         if (!is.null(input$select_data_by) && !is.null(input$condition_type)) {
            finalC1 <- finalC1 %>% filter((!!sym(my_var)) == input$select_data_by)
         }
      }

      # find light cycle start by metadata, or override from UI, or use default from UI
      light_on <- input$light_cycle_start * 60
      if (input$havemetadata) {
         light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
      }

      if (input$override_metadata_light_cycle) {
         light_on <- 60 * input$light_cycle_start
      }

      # TODO: Note light on is from metadata sheet, if metadata sheet is missing the light_on, this is problematic
      finalC1 <- zeitgeber_zeit(finalC1, light_on)

      # annotate days and animals (Already shifted by above correction, thus light_on is now 0)
      day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, "HP2", input$with_facets)
      finalC1 <- day_annotations$df_annotated
   
      # create input select fields for animals and days
      days_and_animals_for_select <- get_days_and_animals_for_select(finalC1)

      selected_days <- getSession(session$token, global_data)[["selected_days"]]
      selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
     if (is.null(selected_days)) {

     output$select_day <- renderUI({
      selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = days_and_animals_for_select$days, multiple = TRUE)
     })
     selected_days = days_and_animals_for_select$days
      storeSession(session$token, "selected_days", selected_days, global_data)
     } else {
      output$select_day <- renderUI({
      selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = selected_days, multiple = TRUE)
     })
     }

   if (is.null(selected_animals)) {
      output$select_animal <- renderUI({
      selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = days_and_animals_for_select$animals, multiple = TRUE)
     })
     selected_animals = days_and_animals_for_select$animals
     storeSession(session$token, "selected_animals", selected_animals, global_data)
   } else {
      output$select_animal <- renderUI({
      selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = selected_animals, multiple = TRUE)
      })
   }

      selected_days <- getSession(session$token, global_data)[["selected_days"]]
      selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
      # filter for selected days and animals in data set
      finalC1 <- finalC1 %>% filter(DayCount %in% selected_days)
      finalC1 <- finalC1 %>% filter(`Animal No._NA` %in% selected_animals)

      # trim times from end and beginning of measurements (obsolete)
      if (input$curate) {
         finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour >= input$exclusion_start, running_total.hrs.halfhour <= (max(finalC1$running_total.hrs.halfhour) - input$exclusion_end))
      }

      # Metadata from TSE file header should be enough, want to see oxidation of substrates by animals
      #C1meta_tmp <- C1meta
      #colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      #df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")

      df_to_plot <- finalC1
      # if we do not have metadata, this comes from some not-clean TSE headers
      if (!input$havemetadata) {
         df_to_plot$`Animal.No.` <- df_to_plot$Animals
      }

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
      write.csv2(df_to_plot, "goxLox_without_metadata.csv")
      GoxLox <- aggregate(df_to_plot$GoxLox, by = list(Animals = df_to_plot$Animals, Days = df_to_plot$Datetime), FUN = sum)
      GoxLox <- GoxLox %>% rename(GoxLox = x)

         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("GoxLox")),
               selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
               selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)$plot_summary + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)),
               hr(style = "width: 75%"),
               conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)))
            )
         })

         output$details <- renderUI({
            results <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "GoxLox", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")),
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
 
      p <- ggplot(data = df_to_plot, aes_string(y = "GoxLox", x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()

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
   
      light_offset <- 0
     # add day annotations and indicators vertical lines
     p <- p + geom_text(data=day_annotations$annotations, aes(x = x, y = y, label=label), vjust=1.5, hjust=0.5, size=4, color="black")
     # indicate new day
     p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
     # indicate night start
     p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
     # re-center at 0
     p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df_to_plot$running_total.hrs.halfhour), max(df_to_plot$running_total.hrs.halfhour)))
     # legends
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
      print(finalC1)
      write.csv2(finalC1, "in_ennergy_expenditure.csv")

      # join either metadata from sheet or tse supported header columns (see above) to measurement data
      # enrich with metadata from TSE header (C1meta) or from metadata sheet (input$metadatafile)
      data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
      finalC1 <- data_and_metadata$data
      true_metadata <- data_and_metadata$metadata

      # Select sexes
      if (!is.null(input$checkboxgroup_gender)) {
         if ("Sex" %in% names(finalC1)) {
           finalC1 <- finalC1 %>% filter(Sex %in% c(input$checkboxgroup_gender))
         }
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

      light_on <- input$light_cycle_start * 60

      if (input$havemetadata) {
         light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
      }

       if (input$override_metadata_light_cycle) {
          light_on <- 60 * input$light_cycle_start
         }

      # display zeitgeber zeit
      write.csv2(finalC1, "before_zeitgeber_zeit.csv")
        finalC1 <- zeitgeber_zeit(finalC1, input$light_cycle_start)
        write.csv2(finalC1, "zeitgeber_zeit_ee.csv")

      # already shifted by zeitgeber zeit above, so light_on is now 0
      day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, "HP2", input$with_facets)
      finalC1 <- day_annotations$df_annotated
   
      # create input select fields for animals and days
      days_and_animals_for_select <- get_days_and_animals_for_select(finalC1)

      selected_days <- getSession(session$token, global_data)[["selected_days"]]
      selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
     if (is.null(selected_days)) {
     output$select_day <- renderUI({
      selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = days_and_animals_for_select$days, multiple = TRUE)
     })
     selected_days = days_and_animals_for_select$days
      storeSession(session$token, "selected_days", selected_days, global_data)
     } else {
      output$select_day <- renderUI({
      selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = selected_days, multiple = TRUE)
     })
     }

   if (is.null(selected_animals)) {
      output$select_animal <- renderUI({
      selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = days_and_animals_for_select$animals, multiple = TRUE)
     })
     selected_animals = days_and_animals_for_select$animals
      storeSession(session$token, "selected_animals", selected_animals, global_data)
   } else {
      output$select_animal <- renderUI({
      selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = selected_animals, multiple = TRUE)
      })
   }

      selected_days <- getSession(session$token, global_data)[["selected_days"]]
      selected_animals <- getSession(session$token, global_data)[["selected_animals"]]

      # filter for selected days and animals in data set
      finalC1 <- finalC1 %>% filter(DayCount %in% selected_days)
      finalC1 <- finalC1 %>% filter(`Animal No._NA` %in% selected_animals)

      # trim times from end and beginning of measurements   
      if (input$curate) {
         finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour >= input$exclusion_start, running_total.hrs.halfhour <= (max(finalC1$running_total.hrs.halfhour) - input$exclusion_end))
      }

      # if we do not have metadata, this comes from some not-clean TSE headers
      if (!input$havemetadata) {
         finalC1$`Animal.No.` <- finalC1$Animals
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

         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("EE")),
               selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
               selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
               conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly({
                  if (!getSession(session$token, global_data)[["is_RMR_calculated"]]) {
                     shinyalert("Error:", "Resting metabolic rate needs to be calculated before!")
                     return()
                  }

                  EE <- getSession(session$token, global_data)[["TEE_and_RMR"]]
                  EE <- EE %>% filter(TEE == "non-RMR")
                  EE$Animals <- as.factor(EE$Animals)

                  p <- do_ancova_alternative(EE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "EE", input$test_statistic, input$post_hoc_test,input$connected_or_independent_ancova)$plot_summary 
                  p <- p + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_variable("EE", input$metadatafile$datapath)) + ggtitle(input$study_description)
                  ggplotly(p)
               }),
               hr(style = "width: 75%"),
               conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(EE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_variable("EE", input$metadatafile$datapath)) + ggtitle(input$study_description)))
            )
         })

         output$details <- renderUI({
            EE <- getSession(session$token, global_data)[["TEE_and_RMR"]]
            EE <- EE %>% filter(TEE == "non-RMR")
            EE$Animals <- as.factor(EE$Animals)
            results <- do_ancova_alternative(EE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "EE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")),
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
     
     light_offset <- 0
     if (input$timeline) {
       # this is already corrected with zeitgeber zeit above (shifted towards the beginning of the light cycle, then re-centered at 0)
       my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, light_offset, input$light_cycle_day_color, input$light_cycle_night_color)
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

     # add day annotations and indicators vertical lines
     p <- p + geom_text(data=day_annotations$annotations, aes(x = x, y = y, label=label), vjust=1.5, hjust=0.5, size=4, color="black")
     # indicate new day
     p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
     # indicate night start
     p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
     # re-center at 0
     p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(lights$x), max(lights$x)))
     print(max(lights$x))
     #p <- p + scale_y_continuous(expand = c(0, 0), limits = c(min(lights$y), max(lights$y)))
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

      light_on <- 720
      if (input$havemetadata) {
         light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
      }

      if (input$override_metadata_light_cycle) {
         light_on <- 60 * input$light_cycle_start
      }

      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][2], ":00", sep = "")
      }
      finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)

      # df already prepared to be day and night summed activities
      finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime2)) * 60 + minute(hms(finalC1$Datetime2)) < light_on, "Day", "Night")
      finalC1$NightDay <- as.factor(finalC1$NightDay)
      finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)

      # first component, typically O2
      df <- data.frame(Values = finalC1[[component]],
         Group = `$`(finalC1, "Animal No._NA"),
         Values2 = finalC1$HP)

      df_new <- partition(df)
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

      # if coefficient of variation is used in analysis, we might end up with 1 or more less timepoint (depending on averaging window!)
      # thus we need to make sure to always take the minimum of these three dataframes or pad accordingly the df_new and df_new2 data frames for each sample
      ## Note that this happens when input$window 
      do_select_n <- min(nrow(finalC1), nrow(df_new), nrow(df_new2))
      #to_pad <- nrow(finalC1) - nrow(df_new) # difference between energy expenditure data frame and RMR
      #df_new <- padding_helper(df_new) # pads by replicating the last value for each sample in timeline and inserting a new row after the last row for each sample
      #df_new2 <- padding_helper(df_new2) # pads by replicting the last value for each sample in timeline and inserting a new row after the last row for each sample
      write.csv2(df_new, "df_new_after_padding_before_join.csv")

      finalC1 <- finalC1 %>% ungroup() %>% slice(1:do_select_n) %>% group_by(`Animal No._NA`)
      df_new <- df_new %>% slice(1:nrow(finalC1))
      df_new2 <- df_new2 %>% slice(1:nrow(finalC1))

      #df_to_plot <- cbind(df_new, `$`(finalC1, "running_total.hrs.halfhour"))
      my_order <- unique(df_new$Group)
      df_sorted <- finalC1
      df_sorted$`Animal No._NA` = as.factor(df_sorted$`Animal No._NA`)
      df_sorted$`Animal No._NA` = factor(df_sorted$`Animal No._NA`, levels=my_order)
      df_sorted <- df_sorted %>% arrange(`Animal No._NA`)
      write.csv2(apply(df_sorted, 2, as.character), "df_sorted_new.csv")
      df_to_plot <- cbind(df_new, `$`(df_sorted, "running_total.hrs.halfhour"), `$`(df_sorted, "Animal No._NA"))

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

      # TODO: Check RMR params: mean interval length of cohorts, 1, 1, 5, seems to be a robust choice
      # to reconstruct reliably RMR, but needs to be validated with additional analysis, e.g. BA analysis
      interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
      AVERAGE_INTERVAL_LENGTH <- mean(sapply(interval_length_list, function(x) x$interval_length))
      SLIDING_WINDOW_SIZE_M <- input$window
      PERCENTAGE_BEST <- input$percentage_best
      AVERAGING_WIDTH <- input$rmr_averaging
      df_plot_total <- extract_rmr_helper(AVERAGE_INTERVAL_LENGTH, PERCENTAGE_BEST, AVERAGING_WIDTH)
      write.csv2(df_plot_total, file = "df_for_comparison_with_calimera.csv")
      df_plot_total$HP <- as.numeric(df_plot_total$HP) 
      df_plot_total$Time <- as.numeric(df_plot_total$Time)
      df_plot_total$Type <- sapply(df_plot_total$Animal, lookup_interval_length, interval_length_list_per_cohort_and_animals=interval_length_list)
      df_plot_total$Time <- df_plot_total$Time * df_plot_total$Type
      df_plot_total$Cohort <- sapply(df_plot_total$Animal, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)

      df_plot_total$Animals = df_plot_total$Animal
      # since NAs might be introduced to to un-even measurement lengths in the not full days case, we need to remove NAs here (overhang)
      df_plot_total <- enrich_with_metadata(df_plot_total, finalC1meta, input$havemetadata, input$metadatafile)$data %>% na.omit()
      write.csv2(df_plot_total, "after_enriching_again.csv")

      # we have O2 and CO2 components, but as  they are pretty similar we instead color RMR traces of samples by membership in cohorts
      # p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, group = Component, color = Component)) + geom_line() + facet_wrap(~Animal)
      p <- NULL 
      print(colnames(df_plot_total))

      # group with group from metadata
      if (input$with_facets) {
         p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, color=Animal)) + geom_line()
         if (!is.null(input$facets_by_data_one)) {
            if (input$orientation == "Horizontal") {
               p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
            } else {
               p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
            }
         }
      } else {
         p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, color=Cohort)) + geom_line()
         p <- p + facet_wrap(~Animal)
      }



      write.csv2(df_plot_total, "before_anno_rmr.csv")
      df_annos <- annotate_rmr_days(df_plot_total) %>% na.omit()
      write.csv2(df_annos, "before_annos.csv")
      print("annos:")
      print(df_annos)
      p <- p + geom_text(data = df_annos, aes(x=Time, y = 0, label = Label), vjust = 1.5, hjust = 0.5, size = 3, color='black')

      day_length <- 24
      # if selected either Day or Night, the day length is assumed to be 12 hours
      if (length(input$light_cycle) != 2) {
         day_length = 12
      }

      light_offset <- 0
      p <- p + geom_vline(xintercept = as.numeric(seq(day_length*60, max(df_plot_total$Time, na.rm = TRUE), day_length*60)), linetype="dashed", color="black")
      p <- p + geom_vline(xintercept = as.numeric(seq((day_length/2)*60, max(df_plot_total$Time, na.rm = TRUE), day_length*60)), linetype="dashed", color="gray")

      p <- p + ylab(paste0("RMR [", input$kj_or_kcal, "/ h]"))
      p <- p + xlab("Time [minutes]")
      p <- p + ggtitle("Resting metabolic rates")

      p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df_plot_total$Time), max(df_plot_total$Time)))
      p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
      storeSession(session$token, "is_RMR_calculated", TRUE, global_data)
      finalC1 <- df_plot_total
   },
   #####################################################################################################################
   ### Day Night Activity
   #####################################################################################################################
   DayNightActivity = {
      # colors for plotting
      finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))

      # get metadata from tse header only
      data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
      finalC1 <- data_and_metadata$data
      true_metadata <- data_and_metadata$metadata

      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][2], ":00", sep = "")
      }

      # if we do not have metadata, this comes from some not-clean TSE headers
      if (!input$havemetadata) {
         finalC1$`Animal.No.` <- finalC1$Animals
      }

      df_to_plot <- finalC1
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
      df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))

      #DayNight <- df_to_plot %>% group_by(NightDay, Animals) %>% summarize(HP=sum(HP, na.rm=TRUE), unique_days = n_distinct(Days), Days=Days) %>% na.omit()
      DayNight <- df_to_plot %>% group_by(NightDay, Animals) %>% summarize(HP=sum(HP, na.rm=TRUE), Days=Days) %>% na.omit()
      df_unique_days <- df_to_plot %>% group_by(Animals) %>% summarize(unique_days = n_distinct(Datetime))
      DayNight <- left_join(DayNight, df_unique_days, by = "Animals")
      DayNight <- DayNight %>% mutate(HP = HP / unique_days) 
      DayNight$NightDay <- as.factor(DayNight$NightDay)
      write.csv2(apply(DayNight, 2, as.character), "test_before_day_night.csv")

         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("HP")),
               selectInput("indep_var", "Independent grouping variable #1", choices = "NightDay", selected = "NightDay"),
               selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Genotype", get_factor_columns(true_metadata)), selected = "Days")),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(DayNight, true_metadata, input$covar, input$covar2, "NightDay", input$indep_var2, "HP", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)$plot_summary + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)),
               hr(style = "width: 75%"),
               conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(DayNight, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "HP", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)))
            )
         })


output$details <- renderUI({
            results <- do_ancova_alternative(DayNight, true_metadata, input$covar, input$covar2, "NightDay", input$indep_var2, "HP", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")),
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

      p <- p + ylab(paste0("Energy expenditure [", input$kj_or_kcal, "/ h]"))
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
      # colors for plotting as factor
      finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))
      write.csv2(finalC1, "before_enrich_with_metadata.csv")
      # get metadata from tse header only
      data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, input$metadatafile)
      finalC1 <- data_and_metadata$data
      true_metadata <- data_and_metadata$metadata
      print("true metadata")
      print(colnames(true_metadata))

      # TODO: need to use light_on from metadata sheet if metadata sheet provided, take care of the correct units minutes vs. hours!
      # default light on from UI
      light_on <- input$light_cycle_start * 60

      # in case we have metadata, override with values from sheet
      if (input$havemetadata) {
         light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
      }

      # in case no information in metadata sheet, override light cycle manually
      if (input$override_metadata_light_cycle) {
        light_on <- 60 * input$light_cycle_start
      }

      # display zeitgeber zeit
      write.csv2(finalC1, "before_to_zeitgeber.csv")
      finalC1 <- zeitgeber_zeit(finalC1, input$light_cycle_start)
      write.csv2(finalC1, "to_zeitgeber.csv")

      # format variable from UI to compatible TSE format
      mylabel <- paste0(input$myr, sep = "", "_[%]")
      if (startsWith(input$myr, "V")) {
         mylabel <- paste0(input$myr, sep = "", "(3)_[ml/h]")
      }

      if (startsWith(input$myr, "Temp")) {
         mylabel <- paste0(input$myr, sep = "", "_C")
      }

      if (startsWith(input$myr, "RER")) {
         mylabel <- "RER"
      }

      # annotate days and animals (Already shifted by above correction)
      day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, mylabel, input$with_facets)
      finalC1 <- day_annotations$df_annotated
   
      # create input select fields for animals and days
      days_and_animals_for_select <- get_days_and_animals_for_select(finalC1)
      selected_days <- getSession(session$token, global_data)[["selected_days"]]
      selected_animals <- getSession(session$token, global_data)[["selected_animals"]]

      # set default for animals and selected days: typically all selected at the beginning
      if (is.null(selected_days)) {
         output$select_day <- renderUI({
            selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = days_and_animals_for_select$days, multiple = TRUE)
         })
         selected_days = days_and_animals_for_select$days
         storeSession(session$token, "selected_days", selected_days, global_data)
      } else {
         output$select_day <- renderUI({
            selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = selected_days, multiple = TRUE)
         })
      }

      if (is.null(selected_animals)) {
         output$select_animal <- renderUI({
            selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = days_and_animals_for_select$animals, multiple = TRUE)
         })
         selected_animals = days_and_animals_for_select$animals
         storeSession(session$token, "selected_animals", selected_animals, global_data)
      } else {
         output$select_animal <- renderUI({
            selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = selected_animals, multiple = TRUE)
         })
      }

      # filter for selected days and animals in data set
      selected_days <- getSession(session$token, global_data)[["selected_days"]]
      selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
      finalC1 <- finalC1 %>% filter(DayCount %in% selected_days)
      finalC1 <- finalC1 %>% filter(`Animal No._NA` %in% selected_animals)

      # trim times from end and beginning of measurements (obsolete)
      if (input$curate) {
         finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour >= input$exclusion_start, running_total.hrs.halfhour <= (max(finalC1$running_total.hrs.halfhour) - input$exclusion_end))
      }

      df_to_plot <- finalC1
      # if we do not have metadata, this comes from some not-clean TSE headers
      if (!input$havemetadata) {
         df_to_plot$`Animal.No.` <- df_to_plot$Animals
      }

      # format labels for plot
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

      # plot basic plot
      write.csv2(df_to_plot, "df_to_plot_failing.csv")

      if (!is.null(getSession(session$token, global_data)[["reactive_data"]])) {
         df_to_plot <- getSession(session$token, global_data)[["reactive_data"]]()
         print("replotting done?!")
         print(nrow(df_to_plot))
      }

     if (is.null(getSession(session$token, global_data)[["reactive_data"]])) {
        storeSession(session$token, "reactive_data", reactiveVal(df_to_plot), global_data)
     }

      p <- ggplot(data = df_to_plot, aes_string(y = input$myr, x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
      mylabel <- gsub("_", " ", mylabel)

      # annotate timeline
      # TODO: Raw does not work df_to_plot does not contain only full days, but finalC1 does, see GOxLox and EE works too with finalC1, change here too
      lights <- data.frame(x = df_to_plot["running_total.hrs.halfhour"], y = df_to_plot[input$myr])
      colnames(lights) <- c("x", "y")
      if (input$timeline) {
            my_lights <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, 0, input$light_cycle_day_color, input$light_cycle_night_color)
            p <- p + my_lights
      }

      p <- p + ylab(pretty_print_variable(mylabel, input$metadatafile$datapath))
      p <- p + xlab("Time [h]")

      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][1], "", sep = "")
      }

      # df to plot now contains the summed oxidation over individual days   
      df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))
      df_to_plot$GoxLox = df_to_plot[input$myr]
      GoxLox <- aggregate(df_to_plot$GoxLox, by = list(Animals = df_to_plot$Animals, Days = df_to_plot$Datetime), FUN = sum) %>% rename("Raw" = input$myr)

         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("Raw")),
               selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
               selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
               conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test,input$connected_or_independent_ancova)$plot_summary + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_variable(mylabel, input$metadatafile$datapath)) + ggtitle(input$study_description)),
               hr(style = "width: 75%"),
               conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_variable(mylabel, input$metadatafile$datapath)) + ggtitle(input$study_description)))
            )
         })

         output$details <- renderUI({
            results <- do_ancova_alternative(GoxLox, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "Raw", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")),
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

      light_offset <- 0
      if (input$with_facets) {
         if (!is.null(input$facets_by_data_one)) {
            if (input$orientation == "Horizontal") {
               p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
            } else {
               p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
            }
         }
      }

     # add day annotations and indicators vertical lines
     p <- p + geom_text(data=day_annotations$annotations, aes(x = x, y = y, label=label), vjust=1.5, hjust=0.5, size=4, color="black")
     # indicate new day
     p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
     # indicate night start
     p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
     # set title and display buttons
     p <- p + ggtitle(paste0("Raw measurement: ", pretty_print_variable(mylabel, input$metadatafile$datapath))) 
      # add points only if toggle outliers
     if (input$toggle_outliers) {
      p <- p + geom_point()
     }
     # center x axis
     #p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df_to_plot$running_total.hrs.halfhour), max(df_to_plot$running_total.hrs.halfhour)))
     p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(finalC1$running_total.hrs.halfhour), max(finalC1$running_total.hrs.halfhour)))
     print(min(lights$x))
     print(max(lights$x))
     # basic plotly config
     # toggle outliers
     if (input$toggle_outliers) {
      exceed_indices <- which(df_to_plot[[input$myr]] > input$threshold_toggle_outliers)
      p <- ggplotly(p)
      for (i in seq_along(exceed_indices)) {
         p <- p %>% add_segments(x = df_to_plot$running_total.hrs.halfhour[exceed_indices[i]]-0.25, xend = df_to_plot$running_total.hrs.halfhour[exceed_indices[i]]+0.25, y = input$threshold_toggle_outliers, yend = input$threshold_toggle_outliers, line = list(color="red", width=8), name = paste("Outlier #", i)) # showlegend=FALSE
      }
     }
     # store number of total curves already present in plotly
     storeSession(session$token, "all_curves_plotly", length(plotly_build(p)$x$data), global_data)
     p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
     p <- ggplotly(p)
   },
   #####################################################################################################################
   ### Total Energy Expenditure
   #####################################################################################################################
   TotalEnergyExpenditure = {
      # assign colors based on animals
      colors <- as.factor(`$`(finalC1, "Animal No._NA"))
      finalC1$Animals <- colors

      # enrich with metadata
      data_and_metadata <- enrich_with_metadata(finalC1, C1meta, input$havemetadata, input$metadatafile)
      finalC1 <- data_and_metadata$data
      true_metadata <- data_and_metadata$metadata

      light_on <- 720
      if (input$havemetadata) {
         light_on <- 60 * as.integer(get_constants(input$metadatafile$datapath) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
      }

      if (input$override_metadata_light_cycle) {
         light_on <- 60 * input$light_cycle_start
      }

      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][2], ":00", sep = "")
      }

      finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)

      # df already prepared to be day and night summed activities
      finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime2)) * 60 + minute(hms(finalC1$Datetime2)) < light_on, "Day", "Night")
      finalC1$NightDay <- as.factor(finalC1$NightDay)
      finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)

      # Create unique days for each animals sorted ascending based by Datetime
      finalC1 <- finalC1 %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()

      colors <- as.factor(`$`(finalC1, "Animal No._NA"))
      finalC1$Animals <- colors

      #C1meta_tmp <- C1meta
      #colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      #finalC1 <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")

      # if we do not have metadata, this comes from some not-clean TSE headers
      if (!input$havemetadata) {
         finalC1$`Animal.No.` <- finalC1$Animals
      }


      convert2 <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][2], ":00", sep = "")
      }

      finalC1$Datetime2 <- lapply(finalC1$Datetime, convert2)
      convert <- function(x) {
         splitted <- strsplit(as.character(x), " ")
         paste(splitted[[1]][1], "", sep = "")
      }

      interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
      pretty_print_interval_length_list(interval_length_list)
      finalC1$CohortTimeDiff <- sapply(finalC1$Animals, lookup_interval_length, interval_length_list_per_cohort_and_animals=interval_length_list)

      finalC1 <- finalC1 %>% mutate(HP = (HP/60) * CohortTimeDiff)
      finalC1 <- finalC1 %>% mutate(HP2 = (HP2/60) * CohortTimeDiff)
      finalC1$Datetime <- day(dmy(lapply(finalC1$Datetime, convert)))

      TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount), FUN = sum, na.rm = T)
      TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount), FUN = sum, na.rm = T) 

      if (input$with_facets) {
         if (input$facets_by_data_one %in% names(finalC1)) {
            TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum, na.rm = T)
            TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$DayCount, Facet = finalC1[[input$facets_by_data_one]]), FUN = sum, na.rm = T) 
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
      storeSession(session$token, "TEE", TEE, global_data)
      TEE <- TEE %>% filter(Equation == input$variable1)

      interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
      TEE$Cohort <- sapply(TEE$Animals, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)

      p <- ggplot(data = TEE, aes(x = Animals, y = TEE, label = Days, color=Cohort)) 
      p <- p + geom_point() + geom_violin(fill="grey80", colour="#3366FF", alpha=0.3) 
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

         output$test <- renderUI({
            tagList(
               h4("Configuration"),
               selectInput("test_statistic", "Test", choices = c("1-way ANCOVA", "2-way ANCOVA")),
               selectInput("dep_var", "Dependent variable", choice = c("TEE")),
               selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
               selectInput("indep_var", "Independent grouping variable #1", choices = get_factor_columns(true_metadata), selected = "Genotype"),
               selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
               conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Days", get_factor_columns(true_metadata)), selected = "Days")),
               conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
               hr(style = "width: 50%"),
               h4("Advanced"),
               selectInput("post_hoc_test", "Post-hoc test", choices = c("Bonferonni", "Tukey", "Sidak", "Spearman")),
               sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
               checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
               hr(style = "width: 75%"),
               renderPlotly(do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)),
               hr(style = "width: 75%"),
               conditionalPanel("input.num_covariates == '2'", renderPlotly(do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) + ggtitle(input$study_description)))
            )
         })

         output$details <- renderUI({
            results <- do_ancova_alternative(TEE, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "TEE", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)
            tagList(
               h3("Post-hoc analysis"),
               renderPlotly(results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")),
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

      p <- p + ggtitle(paste("Total energy expenditure (days=", length(levels(TEE$Days)), ") using equation ", input$variable1, sep = ""))
      p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
      storeSession(session$token, "is_TEE_calculated", TRUE, global_data)
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
   # save session id
   output$session_id <- renderText(paste0("Global session ID: ", session$token))

   # store globally the data per session
   storeSession(session$token, "time_diff", 5, global_data)
   storeSession(session$token, "time_start_end", NULL, global_data)
   storeSession(session$token, "start_date", "1970-01-01", global_data)
   storeSession(session$token, "end_date", Sys.Date(), global_data)
   storeSession(session$token, "selected_days", NULL, global_data)
   storeSession(session$token, "selected_animals", NULL, global_data)
   storeSession(session$token, "interval_length_list", list(), global_data)
   storeSession(session$token, "is_TEE_calculated", FALSE, global_data)
   storeSession(session$token, "is_RMR_calculated", FALSE, global_data)

   # gender choice = all
   output$checkboxgroup_gender <- renderUI(
      checkboxGroupInput(inputId = "checkboxgroup_gender", label = "Chosen sexes",
      choices = list("male" = "male", "female" = "female"), selected = c("male", "female")))

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
            status_okay <- do_export_alternative(input$export_format, input, output, session, file, do_plotting)
      }
   )

  output$downloadPlottingData <- downloadHandler(
      filename = function() {
      if (! input$export_file_name2 == "") {
         paste(input$export_file_name2, ".csv", sep = "")
      } else {
         extension <- ".csv"
         paste("plotting_data-", Sys.Date(), extension, sep = "")
      }
      },
         content = function(file) {
            status_okay <- do_export_plotting_data(input, output, session, file, do_plotting, global_data)
      }
   )

   output$downloadAllData <- downloadHandler(
         filename = function() {
            paste0("all_data-", Sys.Date(), ".zip")
         },
         content = function(file) {
            zip_file = do_export_all_data(input, output, session, file, do_plotting, global_data)
            file.copy(zip_file, file)
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
   # Observer plotly_click (mouse left-click)
   #####################################################################################################################
   observeEvent(event_data("plotly_click"), {
      click_data <- event_data("plotly_click")
      if (!is.null(click_data)) {
         data <- getSession(session$token, global_data)[["reactive_data"]]()
         nearest_idx <- which.min(abs(data$running_total.hrs.halfhour - click_data$x))
         # highlight the point
         isolate({
            p <- plotlyProxy("plot", session) %>% plotlyProxyInvoke("restyle", list(marker=list(color = "red")), list(nearest_idx))
         })

         isolate({
            session$sendCustomMessage(type = "selected_point", nearest_idx)
         })
      }
   })


   #####################################################################################################################
   # Observer plotly_selected (lasso or rectangle)
   #####################################################################################################################
   observeEvent(event_data("plotly_selected"), {
      selected_data <- event_data("plotly_selected")
      if (!is.null(selected_data)) {
         data <- getSession(session$token, global_data)[["reactive_data"]]()
         # Animal grouping is added last, thus we have an offset of number of traces already in plot - number of unique levels in factor
         # TODO: Generalize this that it is useable in other plots as well.
         all_curves_plotly <- getSession(session$token, global_data)[["all_curves_plotly"]] - length(levels(data$Animals))
         curve_to_sampleid_mapping <- levels(data$Animals)
         selected_sampleid <- curve_to_sampleid_mapping[selected_data$curveNumber+1-all_curves_plotly]

         selected_indices <- which(
            data$Animals %in% selected_sampleid &
            data$running_total.hrs.halfhour %in% selected_data$x,
            data[[input$myr]] %in% selected_data$y
         )

         isolate({
            p <- plotlyProxy("plot", session) %>% plotlyProxyInvoke("restyle", list(marker=list(color = "green")), list(selected_indices))
         })

         isolate({
             session$sendCustomMessage(type = "selected_points", selected_indices)
         })
      }
   })

   #####################################################################################################################
   # Observer remvove_lasso_points
   #####################################################################################################################
   observeEvent(input$remove_lasso_points, {
      selected_data <- event_data("plotly_selected")
      if (!is.null(selected_data)) {
         data <- getSession(session$token, global_data)[["reactive_data"]]()

         all_curves_plotly <- getSession(session$token, global_data)[["all_curves_plotly"]] - length(levels(data$Animals))
         curve_to_sampleid_mapping <- levels(data$Animals)
         selected_sampleid <- curve_to_sampleid_mapping[selected_data$curveNumber+1-all_curves_plotly]

         selected_indices <- which(
            data$Animals %in% selected_sampleid &
            data$running_total.hrs.halfhour %in% selected_data$x,
            data[[input$myr]] %in% selected_data$y
         )

         updated_data <- data[-selected_indices, ]
         getSession(session$token, global_data)[["reactive_data"]](updated_data)
         click("plotting")
      }
   })


   #####################################################################################################################
   # Observe heat production formula 1 and forumula 2 and print formular to label
   #####################################################################################################################
   observeEvent(c(input$variable1, input$variable2, input$plot_type), {
         text1 <- ""
         text2 <- ""
         switch(input$variable1,
         Weir = {
            text1 <- "$$ \\tag{1} 16.3 \\times \\dot{V}O_2[\\frac{ml}{h}] + 4.57 \\times \\dot{V}CO_2[\\frac{ml}{h}] $$"
         },
         HP = {
            text1 <- "$$ \\tag{1} \\dot{V}O_2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         HP2 = {
            text1 <- "$$ \\tag{2} (4.44 + 1.43 \\times RER) + \\dot{V}O_2[\\frac{ml}{h}] $$"
         },
         Lusk = {
            text1 <- "$$ \\tag{4} 15.79 \\times \\dot{V}O_2[\\frac{ml}{h}] + 5.09 \\times RER $$"
         },
         Elia = {
            text1 <- "$$ \\tag{5} 15.8 \\times \\dot{V}O_2[\\frac{ml}{h}] + 5.18 \\times RER $$"
         },
         Brouwer = {
            text1 <- "$$ \\tag{6} 16.07 \\times \\dot{V}O_2[\\frac{ml}{h}]+ 4.69 \\times RER $$"
         },
         Ferrannini = {
            text1 <- "$$ \\tag{3} 16.37117 \\times \\dot{V}O_2[\\frac{ml}{h}] + 4.6057 \\times \\dot{V} CO_2 [\\frac{ml}{h}] $$"
         },
         {
         }
         )

         switch(input$variable2,
         Weir = {
            text2 <- "$$ \\tag{1} 16.3 \\times \\dot{V}O_2[\\frac{ml}{h}] + 4.57 \\times \\dot{V}CO_2[\\frac{ml}{h}] $$"
         },
         HP = {
            text2 <- "$$ \\tag{1} \\dot{V}O_2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         HP2 = {
            text2 <- "$$ \\tag{2} (4.44 + 1.43 \\times RER) + \\dot{V}O_2[\\frac{ml}{h}] $$"
         },
         Lusk = {
            text2 <- "$$ \\tag{4} 15.79 \\times \\dot{V}O2[\\frac{ml}{h}] + 5.09 \\times RER $$"
         },
         Elia = {
            text2 <- "$$ \\tag{5} 15.8 \\times \\dot{V}O2[\\frac{ml}{h}] + 5.18 \\times RER $$"
         },
         Brouwer = {
            text2 <- "$$ \\tag{6} 16.07 \\times \\dot{V}O2[\\frac{ml}{h}] + 4.69 \\times RER $$"
         },
         Ferrannini = {
            text2 <- "$$ \\tag{3} 16.37117 \\times \\dot{V}O_2[\\frac{ml}{h}] + 4.6057 \\times \\dot{V} CO_2 [\\frac{ml}{h}] $$"
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
               checkboxGroupInput(inputId = "checkboxgroup_gender", label = "Chosen sexes",
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
           status_okay <- do_export("CalR", input, output, session, do_plotting)
           if (!status_okay) {
             output$message <- renderText("Error during data export to CalR, check logs")
           } else {
             output$message <- renderText(paste("Consolidated data exported to format >>",
             input$export_format, "<<", sep = " "))
           }
      }
      # Excel
      if (input$export_format == "Excel") {
           status_okay <- do_export_alternative("Excel", input, output, session, do_plotting)
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
      # No error messages if no input file given or no excel given for metadata 
      # req(input$File1)
      # req(grepl("\\.xls$", input$metadatafile$datafilepath, ignore.case = TRUE) || grepl("\\.xlsx$", input$metadatafile$datafilepath, ignore.case=TRUE))

      output$plot <- renderPlotly({
         if (is.null(input$File1)) {
            output$message <- renderText("Not any cohort data given. Need at least one data set.")
         } else {
           file <- input$File1
           real_data <- do_plotting(file$datapath, input, input$sick, output, session)

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
              multiInput(inputId = "sick", label = "Remove outliers", selected = "", choices = unique(real_data$animals)))
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
               if (is.null(input$condition_type)) {
                  tse_metadata <- enrich_with_metadata(real_data$data, real_data$metadata, FALSE, FALSE)$metadata
                  output$condition_type <- renderUI(selectInput(inputId = "condition_type", colnames(tse_metadata), label = "Condition"))
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
               tse_metadata <- enrich_with_metadata(real_data$data, real_data$metadata, FALSE, FALSE)$metadata
               output$select_data_by <- renderUI(selectInput("select_data_by", "Filter by",  choices = levels(tse_metadata[[input$condition_type]]), selected = input$select_data_by))
            }
            })

            if (input$plot_type == "RestingMetabolicRate") {
                if (!getSession(session$token, global_data)[["is_TEE_calculated"]]) {
                     shinyalert("Error:", "Total energy expenditure needs to be calculated before!")
                     return()
                  }


               showTab(inputId = "additional_content", target = "Summary statistics")
               write.csv2(real_data$data, "before_rmr_written.csv")
               # bar plot rmr vs non-rmr (we filter out nans just in case to be sure - might come from covariance analysis above)
               df_filtered <- real_data$data %>%
                  filter(Component != input$cvs) %>%
                  select(!Component) %>%
                  group_by(Animal) %>%
                  na.omit() %>%
                  summarize(Value = HP, cgroups = c(Animal))
                  # summarize(Value = min(HP), cgroups = c(Animal))
               write.csv2(df_filtered, "rmr.csv")
               storeSession(session$token, "RMR", df_filtered, global_data)

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

            # if we have metadata, check time diff again to be consistent with metadata sheet
            time_diff <- getSession(session$token, global_data)[["time_diff"]]
            df_diff <- read.csv2("finalC1.csv")
            if (input$havemetadata) {
               names(df_diff)[names(df_diff) == "Animal.No._NA"] <- "Animal No._NA"
               time_diff <- get_time_diff(df_diff, 1, 3, input$detect_nonconstant_measurement_intervals)
               if (time_diff == 0) {
                  time_diff <- 5
               }
               storeSession(session$token, "time_diff", time_diff, global_data)
            }

         df1 <- getSession(session$token, global_data)[["RMR"]]
         df2 <- getSession(session$token, global_data)[["TEE"]]

         df1 <- rename(df1, Animals = Animal)
         how_many_days <- length(levels(as.factor(df2$Days)))
         df1$Animals <- as.factor(df1$Animals)
         df2$Animals <- as.factor(df2$Animals)

         # unique days
         unique_days_tee <- df2 %>% group_by(Animals) %>% summarize(unique_days = n_distinct(Days)) %>% as.data.frame()

         # RMR has not been scaled before to minutes and interval length, required to be compared with TEE which has been previously scaled already.
         interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
         df1$CohortTimeDiff <- sapply(df1$Animals, lookup_interval_length, interval_length_list_per_cohort_and_animals=interval_length_list)
         df1 <- df1 %>% mutate(Value = (Value / 60) * CohortTimeDiff)

         # time interval is determined by diff_time from data (not always fixed time interval in TSE systems)
         # Note: TEE over day might contain NANs in case we have not only FULL days in recordings of calorimetry data
         df1 <- df1 %>% group_by(Animals) %>% summarize(EE = sum(Value, na.rm = TRUE))
         df2 <- df2 %>% filter(Equation == "HP") %>% group_by(Animals) %>% summarize(EE = sum(TEE, na.rm = TRUE))

         df1 <- left_join(df1, unique_days_tee, by = "Animals")
         df2 <- left_join(df2, unique_days_tee, by = "Animals")

         # calculate averages of RMR over number of given days
         df1 <- df1 %>% mutate(EE = EE / unique_days)
         df2 <- df2 %>% mutate(EE = EE / unique_days)

         df1$TEE <- as.factor(rep("RMR", nrow(df1)))
         df2$TEE <- as.factor(rep("non-RMR", nrow(df2)))
         df2$EE <- df2$EE - df1$EE

         df_total <- rbind(df1, df2)
         df_total$Animals <- as.factor(df_total$Animals)
         df_total$TEE <- factor(df_total$TEE, levels=c("non-RMR", "RMR"))

         p2 <- ggplot(data = df_total, aes(factor(Animals), EE, fill = TEE)) + geom_bar(stat = "identity")
         p2 <- p2 + xlab("Animal") + ylab(paste("EE [", input$kj_or_kcal, "/day]"))
         p2 <- p2 + scale_fill_manual(values=c("non-RMR" = "#B2DF8A", "RMR" = "#CAB2D6"))
         p2 <- p2 + ggtitle(paste("Energy expenditure (over a maximum of ", how_many_days, " days)", sep = ""))
         output$summary <- renderPlotly(
            ggplotly(p2) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
         )
         
         # write.csv2(df_total, "tee_and_rmr.csv")
         storeSession(session$token, "TEE_and_RMR", df_total, global_data)
         df_total <- df_total %>% filter(TEE == "RMR") %>% rename(RMR=EE)
         write.csv2(df_total, "test_for_rmr.csv")

      data_and_metadata <- enrich_with_metadata(df_total, real_data$metadata, input$havemetadata, input$metadatafile)
      #df_total <- data_and_metadata$data
      true_metadata <- data_and_metadata$metadata
      print("true_metadata")
      print(true_metadata)



         if (input$havemetadata || !input$havemetadata) {
               # true_metadata <- get_true_metadata(input$metadatafile$datapath)
               output$test <- renderUI({
                  tagList(
                     h4("Configuration"),
                     selectInput("test_statistic", "Test", choices = c("1-way ANCOVA")),
                     selectInput("dep_var", "Dependent variable", choice = c("RMR")),
                     selectInput("indep_var", "Independent grouping variable", choices = get_factor_columns(true_metadata), selected = "Genotype"),
                     selectInput("num_covariates", "Number of covariates", choices=c('1', '2'), selected='1'),
                     selectInput("covar", "Covariate #1", choices = get_non_factor_columns(true_metadata), selected = "body_weight"),
                     conditionalPanel("input.num_covariates == '2'", selectInput("covar2", "Covariate #2", choices = get_non_factor_columns(true_metadata), selected = "lean_mass")),
                     conditionalPanel("input.test_statistic == '2-way ANCOVA'", selectInput("indep_var2", "Independent grouping variable #2", choices = c("Diet", get_factor_columns(true_metadata)), selected = "Days")),
                     conditionalPanel("input.test_statistic == '2-way ANCOVA'", checkboxInput("connected_or_independent_ancova", "Interaction term", value = FALSE)),
                     hr(style = "width: 50%"),
                     h4("Advanced"),
                     selectInput("post_hoc_test", "Post-hoc test", choices = c("bonferroni", "tukey", "spearman")),
                     sliderInput("alpha_level", "Alpha-level", 0.001, 0.05, 0.05, step = 0.001),
                     checkboxInput("check_test_assumptions", "Check test assumptions?", value = TRUE),
                     hr(style = "width: 75%"),
                     renderPlotly({
                        p <- do_ancova_alternative(df_total, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "RMR", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary 
                        p <- p + xlab(pretty_print_label(input$covar, input$metadatafile$datapath)) 
                        p <- p + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) 
                        # TODO: use this auto-scale feature also in all other plots
                        if (!input$auto_scale_rmr_plot_limits_x) {
                           p <- p + xlim(c(input$x_min_rmr_plot, input$x_max_rmr_plot))
                        }

                        if (!input$auto_scale_rmr_plot_limits_y) {
                           p <- p + ylim(c(input$y_min_rmr_plot, input$y_max_rmr_plot))
                        }

                        ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
                     }),
                        hr(style = "width: 50%"),
                        h4("Plotting control"),
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
                     conditionalPanel("input.num_covariates == '2'", renderPlotly({
                        p <- do_ancova_alternative(df_total, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "RMR", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova, input$num_covariates)$plot_summary2 
                        p <- p + xlab(pretty_print_label(input$covar2, input$metadatafile$datapath)) 
                        p <- p + ylab(pretty_print_label(input$dep_var, input$metadatafile$datapath)) 
                        if (!input$auto_scale_rmr_plot_limits_x2) {
                           p <- p + xlim(c(input$x_min_rmr_plot2, input$x_max_rmr_plot2))
                        }

                        if (!input$auto_scale_rmr_plot_limits_y2) {
                           p <- p + ylim(c(input$y_min_rmr_plot2, input$y_max_rmr_plot2))
                        }
                        ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
                     })),
                     conditionalPanel("input.num_covariates == '2'", 
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
                     )
                  )
                  })


               output$details <- renderUI({
                  results <- do_ancova_alternative(df_total, true_metadata, input$covar, input$covar2, input$indep_var, input$indep_var2, "RMR", input$test_statistic, input$post_hoc_test, input$connected_or_independent_ancova)
                  tagList(
                     h3("Post-hoc analysis"),
                     renderPlotly(results$plot_details + xlab(input$indep_var) + ylab("estimated marginal mean")),
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
       highlight_style <- "background-color: #FFB3BA;"
         # Function to create a table row with optional highlighting
         create_row <- function(name, equation, unit, reference, highlight = FALSE) {
            style <- if (highlight) highlight_style else ""
            paste0(
            '<tr style="', style, '">',
            '<td style="border:1px solid black; padding: 5px;">', name, '</td>',
            '<td style="border:1px solid black; padding: 5px;">\\(', equation, '\\)</td>',
            '<td style="border:1px solid black; padding: 5px;">\\(', unit, '\\)</td>',
            '<td style="border:1px solid black; padding: 5px;">[', reference, ']</td>',
            '</tr>'
            )
         }

         # List of table rows
         rows <- list(
            '<h3> Energy expenditure equations and references </h3>',
            create_row("Heldmaier's first", "(4.44 + 1.43 \\times RER) + \\dot{V}O_2", "\\frac{ml}{h}", "1", input$variable1 == "HP"),
            create_row("Heldmaier's second", "\\dot{V}O_2 \\times (6 + RER + 15.3) \\times 0.278", "\\frac{ml}{h}", "1", input$variable1 == "HP2"),
            create_row("Weir", "16.3 \\times \\dot{V}O_2 + 4.57 \\times RER", "\\frac{ml}{h}", "2", input$variable1 == "Weir"),
            create_row("Ferrannini", "16.37117 \\times \\dot{V}O_2 + 4.6057 \\times RER", "\\frac{ml}{h}", "3", input$variable1 == "Ferrannini"),
            create_row("Lusk", "15.79 \\times \\dot{V}O_2 + 5.09 \\times RER", "\\frac{ml}{h}", "4", input$variable1 == "Lusk"),
            create_row("Elia", "15.8 \\times \\dot{V}O_2 + 5.18 \\times RER", "\\frac{ml}{h}", "5", input$variable1 == "Elia"),
            create_row("Brouwer", "16.07 \\times \\dot{V}O_2 + 4.69 \\times RER", "\\frac{ml}{h}", "6", input$variable1 == "Brouwer")
         )

         # Combine rows into a single HTML string
         table_html <- paste0(
            '<table style="width:100%; border:1px solid black; border-collapse: collapse;">',
            '<tr>',
            '<th style="border:1px solid black; padding: 5px;"><strong>Name</strong></th>',
            '<th style="border:1px solid black; padding: 5px;"><strong>Equation</strong></th>',
            '<th style="border:1px solid black; padding: 5px;"><strong>Unit</strong></th>',
            '<th style="border:1px solid black; padding: 5px;"><strong>Reference</strong></th>',
            '</tr>',
            paste(rows, collapse = ""),
            '</table>'
         )
             
             output$explanation <- renderUI({
            str1 <- "<h3> Caloric Equivalent / heat production over time </h3>"
            str2 <- "Chose one of the established heat equations for calculating of heat production respectively energy expenditure. Note that the abbreviations HP and HP2 refer to Heldmaier's equations as reported in the publication <i>J Comp Physiol B 102, 115–122 (1975)</i>."
            str3 <- "According to a heat production formula the energy expenditure is calculated from indirect calorimetry data"
            str4 <- "Note that in case no metadata available to specify day and night, a single violin plot will be displayed per animal ID."
            str5 <- "Cohorts are usually stratified by animal ID by default"
            str6 <- "<hr/>"
            str7 <- "<h3> References </h3>"
            str8 <- "[1] G. Heldmaier and S. Steinlechner. Seasonal pattern and energetics of short daily torpor in the djungarian hamster, phodopus sungorus. Oecologia, 48:265––270, 1981."
            str9 <- "[2] J. B. d. V. Weir. New methods for calculating metabolic rate with special reference to protein metabolism. The Journal of Physiology, 109(1-2):1–9, 194"
            str10 <- "[3] E. Ferrannini. The theoretical bases of indirect calorimetry: A review. Metabolism, 37(3):287–301, 1988"
            str11 <- "[4] G. Lusk. The Elements of the Science of Nutrition. Sanders, Philadelphia, PA, 1928."
            str12 <- "[5] M. Elia and G. Livesey. Energy Expenditure and Fuel Selection in Biological Systems: The Theory and Practice of Calculations Based on Indirect Calorimetry and Tracer Methods. In Metabolic Control of Eating, Energy Expenditure and the Bioenergetics of Obesity. S.Karger AG, 09 1992."
            str13 <- "[6] E. Brouwer. Report of sub-committee on constant and factors. Energy metabolism, 11:441–443, 1965"
            str14 <- "[7] Seep, L., Grein, S., Splichalova, I. et al. From Planning Stage Towards FAIR Data: A Practical Metadatasheet For Biomedical Scientists. Sci Data 11, 524 (2024). https://doi.org/10.1038/s41597-024-03349-2"
            str15 <- "<h3> Workflow of indirect calorimetry analysis </h3>"
            str16 <- "<ol> <li> First inspect raw data (O2, CO2 and RER) for inconsistencies </li> <li> Calculate energy expenditures according to a heat production equation from the drop-down menu </li> <li> Calculate the total energy expenditure (TEE) and resting metabolic rate (RMR) and contrast genotype and or diet effects between or within TEE respectively RMR. </li> <li> (Optional) Analyse recorded locomotional data, e.g. compare budgets and probability density maps. </li> <li> Export compiled data sets and calculated quantities into Excel or CalR-compatible files </li> <li> Export figures as publication-ready vector or raster graphics </li></ol>" 
            str17 <- "Note The default indirect calorimetry functions should be enough for most analysis, if working with COSMED platform or the Sable/Promethion chose accordingly for additional plotting functions."

                withMathJax(HTML(paste(str1, str2, str3, str4, str5, str6, table_html, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, sep = "<br/>")))
            })
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Statistical testing")
               showTab(inputId = "additional_content", target = "Details")
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
               str3 <- "Note that oxygen consumption, carbon dioxide production as well as derived quantities like the RER (respiratory exchange ratio) can be plotted by selection the corresponding label in the the drop-down menu on the left-hand side window."
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
      # scroll to top after click on plot only
      #shinyjs::runjs("window.scrollTo(0, 50);")
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
      X = c("DE", "PC", "DC"),
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

   #############################################################################
   # Observe select_day input
   #############################################################################
   observeEvent(input$select_day, {
      click("plotting")
      selected_days <<- input$select_day
      storeSession(session$token, "selected_days", input$select_day, global_data)
   })


   #############################################################################
   # Observe select_animal input
   #############################################################################
   observeEvent(input$select_animal, {
      click("plotting")
      selected_animals <<- input$select_animal
      storeSession(session$token, "selected_animals", input$select_animal, global_data)
   })
}