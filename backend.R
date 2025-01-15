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

################################################################################
# General utilities and plotting utilities
################################################################################
source("inc/util.R") 
source("inc/plotting/util.R")

################################################################################
# RMR functions
################################################################################
source("inc/rmr/helper.R") # rmr helper methods
source("inc/rmr/extract_rmr.R") # rmr extraction
source("inc/rmr/extract_rmr_helper.R") # rmr extraction helper

################################################################################
# Importers
################################################################################
source("inc/importers/import_promethion_helper.R") # import for SABLE/Promethion data sets
source("inc/importers/import_pheno_v8_helper.R") # import for PhenoMaster V8 data sets
source("inc/importers/import_cosmed_helper.R") # import for COSMED data sets
source("inc/importers/import_example_data_sets_helper.R") # for example data sets
source("inc/importers/util.R") # for consistency checks of columns

################################################################################
# Locomotion helpers
################################################################################
source("inc/locomotion/locomotion.R") # for locomotion probability heatmap
source("inc/locomotion/locomotion_budget.R") # for locomotion budget

################################################################################
# UI guide and timeline coloring
################################################################################
source("inc/annotations/timeline.R") # for colorizing timeline by day/night rhythm
source("inc/annotations/guide.R") # for guide
source("inc/annotations/style.R") # for styling of basic plots

################################################################################
# Statistics
################################################################################
source("inc/statistics/do_ancova_alternative.R") # for ancova with metadata

################################################################################
# Metadata handling
################################################################################
source("inc/metadata/read_metadata.R") 

################################################################################
# Export functionality
################################################################################
source("inc/exporters/default_exporter.R") 

################################################################################
# User session management
################################################################################
source("inc/session_management.R") 

################################################################################
# Versioning
################################################################################
source("inc/versioning/git_info.R") 

################################################################################
# Visualizations
################################################################################
source("inc/visualizations/goxlox.R") # for goxlox
source("inc/visualizations/energy_expenditure.R") # for energy expenditure
source("inc/visualizations/raw_measurement.R") # for raw measurements
source("inc/visualizations/total_energy_expenditure.R") # for total energy expenditure
source("inc/visualizations/resting_metabolic_rate.R") # for resting metabolic rate
source("inc/visualizations/day_night_activity.R") # for day night activity
source("inc/visualizations/estimate_rmr_for_cosmed.R") # for COSMED-based RMR estimation
source("inc/visualizations/body_composition.R") # for body composition

################################################################################
# Selection of calendrical dates, currently not implemented/thus obsolete
################################################################################
time_start_end <- NULL
start_date <- "1970-01-01"
end_date <- Sys.Date()

################################################################################
# Session global environment to hold user data
################################################################################
global_data <- new.env()

################################################################################
# Configure base plot look and feel with ggpubr
################################################################################
configure_default_plot_look_and_feel <- function() {
   theme_pubr_update <- theme_pubr(base_size = 8.5) +
   theme(legend.key.size = unit(0.3, "cm")) +
   theme(strip.background = element_blank()) +
   theme(strip.text = element_text(hjust = 0)) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
   theme_set(theme_pubr_update)
}

################################################################################
# Load data
################################################################################
load_data <- function(file, input, exclusion, output, session) {
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

   # check if we need to use example data or not
   use_example_data <- getSession(session$token, global_data)[["use_example_data"]]
   if (is.null(use_example_data)) {
      storeSession(session$token, "use_example_data", FALSE, global_data)
   }
   use_example_data <- getSession(session$token, global_data)[["use_example_data"]]

   use_example_data_set <- getSession(session$token, global_data)[["example_data_single"]]
   num_files <- getSession(session$token, global_data)[["example_data_single_length"]]

   if (!use_example_data) {
      num_files <- input$nFiles
   } 
   interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]

	metadatafile <- get_metadata_datapath(input, session, global_data)

   for (i in 1:num_files) {
      file <- input[[paste0("File", i)]]
      file <- file$datapath
      if (use_example_data) {
         if (use_example_data_set) {
            example_data_set_name <- getSession(session$token, global_data)[["example_data_single_name"]]
            if (example_data_set_name == "UCP1KO") {
               file <- paste(Sys.getenv(c("SHINY_DATA_FOLDER")), paste0("example_data/UCP1KO/", "example_data_", i, ".csv"), sep = "")
            }
            if (example_data_set_name == "DAKO") {
               file <- paste(Sys.getenv(c("SHINY_DATA_FOLDER")), paste0("example_data/DAKO/", "example_data_", i, ".csv"), sep = "")
            }
         }
      }
      con <- file(file)
      line <- readLines(con, n = 2)
   if (i == 1) {
      fileFormatTSE <- line[2]
      studyDescription <- line[1]
      if  (input$havemetadata) {
         output$study_description <- renderText(paste0("Study description: ", get_study_description_from_metadata(metadatafile)))
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

   print("what?")

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
            storeSession(session$token, "input_file_type", "COSMED", global_data)
            import_cosmed(file, tmp_file)
        } else {
            output$file_type_detected <- renderText("Unknown file type detected from Excel")
            storeSession(session$token, "input_file_type", "Unknown", global_data)
        }
      } else {
        output$file_type_detected <- renderText("Input file type detected as: Promethion/Sable")
        updateSelectInput(session, "myr", choices = c("VO2", "VCO2", "RER"))
        storeSession(session$token, "input_file_type", "Sable", global_data)
        import_promethion(file, tmp_file)
      }
      file <- tmp_file
      toSkip <- detectData(file)
      scaleFactor <- 60
   }

   # LabMaster V5 (horizontal format)
   if (grepl("V5", fileFormatTSE)) {
      storeSession(session$token, "input_file_type", "LabMaster/V5", global_data)
      sep <- ";"
      dec <- "."
   }

   # LabMaster V6
   if (grepl("V6", fileFormatTSE)) {
      storeSession(session$token, "input_file_type", "LabMaster/V6", global_data)
      sep <- ";"
      dec <- ","
   }

   # PhenoMaster V7: Date separated via /, Time Hour:Minutes, decimal separator ., field separator ,
   if (grepl("V7", fileFormatTSE)) {
      storeSession(session$token, "input_file_type", "PhenoMaster/V7", global_data)
      sep <- ","
      dec <- "."
   }

   # PhenoMaster V8
   if (grepl("V8", fileFormatTSE)) {
      # V8 seems to export sloppy, i.e. non-consistent CSV files, check for this before importing data sets
      is_consistent <- check_column_consistency(file, sep=sep)
      if (!is_consistent) {
         shinyalert("Error", paste("Input data file has different number of columns for rows. Inconsistent format."), showCancelButton=TRUE)
         return()
      }

      storeSession(session$token, "input_file_type", "PhenoMaster/V8", global_data)
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

   # Debug C1meta (TSE metadata)
   print(C1meta)

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
   # Step #9 - recalculate RER
   if (input$recalculate_RER) {
     C1$RER_NA = C1$`VCO2(3)_[ml/h]` / C1$`VO2(3)_[ml/h]`
   }

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
   print("f1:")
   print(f1)
   f2 <- input$variable2

   #############################################################################
   # Heat production formula #1
   #############################################################################
   current_cohort_time_diff = 1.0
   if (input$kj_or_kcal == "mW") {
     # 1000 because from Watts to milli Watts
     1000 * current_cohort_time_diff <- get_time_diff(C1, 2, 3, input$detect_nonconstant_measurement_intervals)
   } 
   C1 <- calc_heat_production(f1, C1, "HP", scaleFactor * (1.0 / current_cohort_time_diff))

   #############################################################################
   # Heat production formula #2 (for comparison in scatter plots)
   #############################################################################
   if (!is.null(input$variable2)) {
      C1 <- calc_heat_production(f2, C1, "HP2", scaleFactor * (1.0 / current_cohort_time_diff))
   } else {
      C1 <- calc_heat_production(f1, C1, "HP2", scaleFactor * (1.0 / current_cohort_time_diff))
   }

   # step 11 means
   C1.mean.hours <- do.call(data.frame, aggregate(list(HP2 = C1$HP2, # calculate mean of HP2
                                       VO2 = C1$`VO2(3)_[ml/h]`, # calculate mean of VO2
                                       VCO2 = C1$`VCO2(3)_[ml/h]`, # calculate mean of VCO2
                                       RER = C1$`VCO2(3)_[ml/h]` / C1$`VO2(3)_[ml/h]`), # calculate mean of RER
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
      finalC1 <- remove_zero_values(finalC1, input$eps)
   }

    # step 13 (debugging: save all cohort means)
   write.csv2(C1.mean.hours, file = paste0("all-cohorts_means.csv"))
   C1meta <- finalC1meta

   # rescale to kcal from kj
   if (input$kj_or_kcal == "kcal") {
      finalC1$HP <- finalC1$HP / 4.184 # to kj
      finalC1$HP2 <- finalC1$HP2 / 4.184 # to kj
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

   write.csv2(C1, file = "all_data.csv")
   write.csv2(finalC1, file = "finalC1.csv")

   storeSession(session$token, "finalC1", finalC1, global_data)
   storeSession(session$token, "finalC1meta", finalC1meta, global_data)
   storeSession(session$token, "C1meta", C1meta, global_data)
   storeSession(session$token, "scaleFactor", scaleFactor, global_data)
   storeSession(session$token, "finalC1cols", colnames(finalC1), global_data)
}


################################################################################
# Create plotly plot
################################################################################
do_plotting <- function(file, input, exclusion, output, session) { # nolint: cyclocomp_linter.
   # if data is not loaded, load the data
   if (is.null(getSession(session$token, global_data)[["data_loaded"]])) {
      print("Loading data")
      load_data(file, input, exclusion, output, session)
      storeSession(session$token, "data_loaded", TRUE, global_data)
   } else {
      print("not loading data!")
      # load_data(file, input, exclusion, output, session)
   }

   # load default plotting style
   if (input$use_default_plot_style) {
      configure_default_plot_look_and_feel()
   }

   # get stored data so far
   finalC1 = getSession(session$token, global_data)[["finalC1"]]
   print("finalC1:")
   print(finalC1)
   finalC1meta = getSession(session$token, global_data)[["finalC1meta"]]
   C1meta = getSession(session$token, global_data)[["C1meta"]]
   scaleFactor = getSession(session$token, global_data)[["scaleFactor"]]

   # Filter out additionally whole calendrical days with given percentage threshold
   # However this is currently imcompatible with zeitgeber time utility, which shifts
   # based on the assumption that running_total.secs == 0 exists in the data frame to 
   # find the shift offset with respect to the start of the light cycle (this also means
   # that running_total.secs might be negative). If we filter out non-full calendricald
   # dates, we might not have running_total.secs == 0 true in our data frame, so the
   # zeitgeber_time function in util.R fails accordingly.
   # We use && ! input$use_zeitgeber_time to exclude this for now - also in ui.R we 
   # do not offer the selection of calendrical days if zeitgeber time is used.
   # TODO: Remove the if statement once zeitgeber_time function in util.R has been adapted
   if (input$only_full_days && !input$use_zeitgeber_time) {
      interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
      storeSession(session$token, "time_diff", get_time_diff(finalC1, 2, 3, input$detect_nonconstant_measurement_intervals), global_data)
      print("int val length list:")
      print(interval_length_list)
      finalC1 <- filter_full_days_alternative(finalC1, input$full_days_threshold, interval_length_list)
      write.csv2(finalC1, "after_fitlering.csv")
   }

   #####################################################################################################################
   # Plotting and data output for downstream debugging
   #####################################################################################################################
   plotType <- input$plot_type
  
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
   # FuelOxidation
   #####################################################################################################################
   FuelOxidation = {
      p <- goxlox(finalC1, finalC1meta, input, output, session, global_data, scaleFactor)
      p_window = p$window_plot
      p <- p$plot

      # indicate if plot available
      indicate_plot_rendered(p, output)

      # style plot
      p <- style_plot(p, input)

      if (!is.null(p_window)) {
         output$windowPlot <- renderPlotly(p_window)
      }

   },
   #####################################################################################################################
   ### Energy Expenditure
   #####################################################################################################################
   HeatProduction = {
      p <- energy_expenditure(finalC1, finalC1meta, input, output, session, global_data, scaleFactor)
      p_window <- p$window_plot
      p <- p$plot

      # indicate if plot available
      indicate_plot_rendered(p, output)

      # style plot
      p <- style_plot(p, input)

      if (!is.null(p_window)) {
         output$windowPlot <- renderPlotly(p_window)
      }
   },
   #####################################################################################################################
   ### RestingMetabolicRate
   #####################################################################################################################
   RestingMetabolicRate = {
      # Check first if RMR can be calculated
      if (!getSession(session$token, global_data)[["is_TEE_calculated"]]) {
        shinyalert("Error:", "Total energy expenditure needs to be calculated before!")
        return()
      }

      df_returned <- resting_metabolic_rate(finalC1, finalC1meta, input, output, session, global_data, scaleFactor, true_metadata)
      finalC1 <- df_returned$finalC1

      p <- df_returned$plot
      p_window <- df_returned$window_plot

      # indicate if plot available
      indicate_plot_rendered(p, output)

      # style plot
      p <- style_plot(p, input)

      if (!is.null(p_window)) {
         output$windowPlot <- renderPlotly(p_window)
      }

   },
   #####################################################################################################################
   ### Day Night Activity
   #####################################################################################################################
   DayNightActivity = {
      p <- day_night_activity(finalC1, finalC1meta, input, output, session, global_data, scaleFactor)

      # indicate if plot available
      indicate_plot_rendered(p, output)

      # style plot
      p <- style_plot(p, input)
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
   },
   #####################################################################################################################
   ### Locomotion Budget
   #####################################################################################################################
   LocomotionBudget = {
      file <- input[[paste0("File", 1)]]
      p <- plot_locomotion_budget(file$datapath)
      p <- p + ggtitle("Locomotional budget")
   },
   #####################################################################################################################
   ### Estimate RMR for COSMED
   #####################################################################################################################
   EstimateRMRforCOSMED = {
      p <- estimate_rmr_for_cosmed(finalC1, finalC1meta, input, output, session, global_data, scaleFactor)

      # indicate if plot available
      indicate_plot_rendered(p, output)

      # style plot
      p <- style_plot(p, input)
   },
   #####################################################################################################################
   ### RawMeasurement
   #####################################################################################################################
   RawMeasurement = {
      p <- raw_measurement(finalC1, finalC1meta, input, output, session, global_data, scaleFactor)
      p_window <- p$window_plot
      p <- p$plot

      # indicate if plot available
      indicate_plot_rendered(p, output)

      # style plot
      p <- style_plot(p, input)

      if (!is.null(p_window)) {
         output$windowPlot <- renderPlotly(p_window)
      }
   },
   #####################################################################################################################
   ### Total Energy Expenditure
   #####################################################################################################################
   TotalHeatProduction = {
      p <- total_energy_expenditure(finalC1, C1meta, finalC1meta, input, output, session, global_data, scaleFactor)
      p_time <- p$time_trace
      p_window <- p$window_plot
      p <- p$plot

      # indicate if main plot available
      indicate_plot_rendered(p, output)

      # style main plot
      p <- style_plot(p, input)
      
      # add time plot as well
      if (!is.null(p_time)) {
         output$timeTrace <- renderPlotly(p_time)
      }

      # and windowed plot
      if (!is.null(p_window)) {
         output$windowPlot <- renderPlotly(p_window)
      }
   },
   #####################################################################################################################
   ### Metadata
   #####################################################################################################################
   Metadata = {
      p <- body_composition(finalC1, finalC1meta, input, output, session, global_data, scaleFactor)

      # indicate if plot available
      indicate_plot_rendered(p, output)

      # increase plot size
      p %>% layout(height=1000)
   },
   #####################################################################################################################
   ### Other options
   #####################################################################################################################
   {
      # all other options which should come
   }
   )
   # return data to UI
   list("plot" = p, "animals" = `$`(finalC1, "Animal No._NA"), "data" = finalC1, "metadata" = C1meta)
}

#####################################################################################################################
# Create server
#####################################################################################################################
server <- function(input, output, session) {
   # stylize plot
   output$stylize_plot_plotting_control <- renderUI({
       if (input$stylize_plot) {
         add_stylize_plot_plotting_control()
       } else {
         NULL
       }
   })

   # git info
   output$git_info <- renderText({
      get_git_information_from_repository()
   })

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

   # download data as csv or xlsx. if no data available, no download button is displayed
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

  # Download handler to get current data frame of displayed plot
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

   # Draw initial number of files -> typically one file
   output$nFiles <- renderUI(numericInput("nFiles", "Number of data files", value = 1, min = 1, step = 1))

   # Download handler for all data download as zip
   output$downloadAllData <- downloadHandler(
         filename = function() {
            paste0("all_data-", Sys.Date(), ".zip")
         },
         content = function(file) {
            print("Here?")
            zip_file = do_export_all_data(input, output, session, file, do_plotting, global_data)
            file.copy(zip_file, file)
         }
    )

   # Dynamically create fileInput fields by the number of requested files of the user
   observeEvent(input$nFiles, {
      output$fileInputs <- renderUI({
         html_ui <- " "
         for (i in 1:input$nFiles) {
            html_ui <- paste0(html_ui, fileInput(paste0("File", i),
               label = paste0("Cohort ", i)))
            }
         HTML(html_ui)
         })
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
         # Animal grouping is added last, thus we have an offset of number of traces already in the plot object - number of unique levels in factor
         # TODO: Generalize this that it is re-useable in other plots as well. Currently only used in panel RawMeasurement for outlier removal.
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
   # Observer remove_lasso_points
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
         # trigger re-plotting, but will take the modified data in the RawMeasurement panel now (reactive_data)
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
            text1 <- "$$ \\tag{2} 16.3 \\times \\dot{V}O_2[\\frac{ml}{h}] + 4.57 \\times \\dot{V}CO_2[\\frac{ml}{h}] $$"
         },
         Heldmaier1 = {
            text1 <- "$$ \\tag{1} \\dot{V}O_2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         Heldmaier2 = {
            text1 <- "$$ \\tag{1} (4.44 + 1.43 \\times RER) + \\dot{V}O_2[\\frac{ml}{h}] $$"
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
            text2 <- "$$ \\tag{2} 16.3 \\times \\dot{V}O_2[\\frac{ml}{h}] + 4.57 \\times \\dot{V}CO_2[\\frac{ml}{h}] $$"
         },
         Heldmaier1 = {
            text2 <- "$$ \\tag{1} \\dot{V}O_2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         Heldmaier2 = {
            text2 <- "$$ \\tag{1} (4.44 + 1.43 \\times RER) + \\dot{V}O_2[\\frac{ml}{h}] $$"
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
   # FIXME: Check that this is in fact the problematic code which leads to the HP / HP2 issue in RMR
   # on server side, but it appears it is already fixed by now - see note about old bug in ui.R too.
   observeEvent(input$plot_type, {
            output$myp <- renderUI(
               selectInput(inputId = "myp",
               label = "Chosen prefered method for calculating caloric equivalent over time",
               choices = c(input$variable1), selected = input$variable1))
         })

   observeEvent(input$plot_type, {
            output$checkboxgroup_gender <- renderUI(
               checkboxGroupInput(inputId = "checkboxgroup_gender", label = "Chosen sexes",
               choices = list("male" = "male", "female" = "female"), selected = c("male", "female")))
         })

   # TODO: here we need to then filter choices based on actual data we have read in with one of the importers,
   # update this myr plot once when data has been loaded by load_data with column names of finalC1 data frame,
   # use colnames(finalC1) for this and take intersection with choices reported here...
   observeEvent(input$plot_type, {
      raw_cols <- getSession(session$token, global_data)[["finalC1cols"]]
      choices = c("O2", "CO2", "RER", "VO2", "VCO2", "Temp", "WeightBody")
      output$myr <- renderUI(
         selectInput(inputId = "myr", label = "Chosen raw data to plot", choices = intersect(unlist(choices), unlist(choices))))
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
             storeSession(session$token, "real_data", real_data, global_data)
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

   #############################################################################
   # Show plot (action button's action)
   #############################################################################
   observeEvent(input$plotting, {
      # No error messages if no input file given or no excel given for metadata 
      # req(input$File1)
      # req(grepl("\\.xls$", input$metadatafile$datafilepath, ignore.case = TRUE) || grepl("\\.xlsx$", input$metadatafile$datafilepath, ignore.case=TRUE))
      output$plot <- renderPlotly({
         use_example_data <- getSession(session$token, global_data)[["use_example_data"]]
         print("use_example_data")
         print(use_example_data)
         if (is.null(use_example_data)) {
            print("is null:")
            use_example_data = FALSE
         } else {
            use_example_data = TRUE
         }

         print("use_example_data")
         print(use_example_data)
         if (is.null(input$File1) && !use_example_data) {
            print("Here?")
            output$message <- renderText("Not any cohort data given. Need at least one data set.")
            shinyalert("Not any data given", "Did you forget to click on an example data set or upload own data sets?")
            return()
         } else {
           file <- input$File1
           real_data <- do_plotting(file$datapath, input, input$sick, output, session)
           storeSession(session$token, "real_data", real_data, global_data)

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
	               metadatafile <- get_metadata_datapath(input, session, global_data)
                  true_metadata <- get_true_metadata(metadatafile)
                  output$facets_by_data_one <- renderUI(
                  selectInput(inputId = "facets_by_data_one", label = "Chosen facet",
                  selected = "Animals", choices = colnames(true_metadata)))
                } else {
                  df_filtered <- real_data$metadata[, colSums(is.na(real_data$metadata)) == 0]
                  df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
                  df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
                  colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
                  our_group_names <- unique(colnames(df_filtered))

                  output$facets_by_data_one <- renderUI(
                     selectInput(inputId = "facets_by_data_one", label = "Chosen facet",
                     selected = "Animals", choices = our_group_names))
                  }
             }  

            #############################################################################
            # Initializing
            #############################################################################
            if (input$havemetadata) {
               if (is.null(input$condition_type)) {
	               metadatafile <- get_metadata_datapath(input, session, global_data)
                  true_metadata <- get_true_metadata(metadatafile, FALSE)
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
               true_metadata <- get_true_metadata(metadatafile, FALSE)
               output$select_data_by <- renderUI(selectInput("select_data_by", "Filter by", choices = unique(true_metadata[[input$condition_type]]), selected = input$select_data_by))
            } else {
               tse_metadata <- enrich_with_metadata(real_data$data, real_data$metadata, FALSE, FALSE)$metadata
               output$select_data_by <- renderUI(selectInput("select_data_by", "Filter by",  choices = levels(tse_metadata[[input$condition_type]]), selected = input$select_data_by))
            }
            })

            # Main plot needs to be always visible
            showTab(inputId = "additional_content", target = "Main plot")

            ## TODO: Resting Metabolic Rate is still handled differently, i.e. in backend.R,
            ## should be move to inc/visualizations/resting_metabolic_rate.R

            if (input$plot_type == "RestingMetabolicRate") {
               
	            metadatafile <- get_metadata_datapath(input, session, global_data)
               showTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Statistical model")

               # RMR time trace required in TotalEnergyExpenditure
               storeSession(session$token, "RMR_time_trace", real_data$data, global_data)

               # bar plot rmr vs non-rmr (we filter out nans just in case to be sure - might come from covariance analysis above)
               print("col names from real_data$data")
               print(colnames(real_data$data))
               df_filtered <- real_data$data %>%
                  select(-which(is.na(names(real_data$data)))) %>%
                  filter(Component != input$cvs) %>%
                  select(!Component) %>%
                  group_by(Animal) %>%
                  na.omit() %>%
                  summarize(Value = HP, cgroups = c(Animal), Days=floor(running_total.sec / (3600*24))+1)
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


            write.csv2(df1, "only_df1.csv")
            write.csv2(df2, "only_df2.csv")

            # TODO: Here RMR and EE get averaged per day already... need to change this if required.
            # time interval is determined by diff_time from data (not always fixed time interval in TSE systems)
            # Note: TEE over day might contain NANs in case we have not only FULL days in recordings of calorimetry data
            ## This also creates the bar plots, we need to create grouped by day bar plots here instead? or we need to add the option
            ## TODO: use uiOutput for summary bar plot and add checkbox grouped by individual days or average per  day...
            df1 <- df1 %>% group_by(Animals) %>% summarize(EE = sum(Value, na.rm = TRUE)) %>% arrange(Animals)
            df2 <- df2 %>% filter(Equation == input$variable1) %>% group_by(Animals) %>% summarize(EE = sum(TEE, na.rm = TRUE)) %>% arrange(Animals)


            df1 <- left_join(df1, unique_days_tee, by = "Animals")
            df2 <- left_join(df2, unique_days_tee, by = "Animals")

            # calculate averages of RMR over number of given days
            df1 <- df1 %>% mutate(EE = EE / unique_days)
            df2 <- df2 %>% mutate(EE = EE / unique_days)

            df1$TEE <- as.factor(rep("RMR", nrow(df1)))
            df2$TEE <- as.factor(rep("non-RMR", nrow(df2)))
            df1 <- df1 %>% group_by(Animals) %>% arrange(Animals)
            df2 <- df2 %>% group_by(Animals) %>% arrange(Animals)

            # Verify whether correct or not, but this should be correct... depending on the quality of RMR estimation we ovre or under estimate the RMR contribution,
            # thus we need  to check for negative values in difference as well ... can happen because the one data sets has very large measurement intervals: 30 minutes! 
            # so the rmr is severely overestimates, and larger than TEE, we need to visually correct for this...
            df1 <- df1 %>% group_by(Animals) %>% arrange(Animals)
            df2 <- df2 %>% group_by(Animals) %>% arrange(Animals)

            combined_df <- inner_join(df1, df2, by = "Animals", suffix = c("_a", "_b"))
            combined_df <- combined_df %>% mutate(EE = EE_b - EE_a)
            combined_df$EE <- pmax(combined_df$EE, 0) # zeros instead for RMR (might be due to RMR method not estimating as good, thus overestimating...)
            write.csv2(combined_df, "combined_fine_df.csv")

            df_total <- rbind(df1, df2)
            df_total$Animals <- as.factor(df_total$Animals)
            df_total$TEE <- factor(df_total$TEE, levels=c("non-RMR", "RMR"))

            combined_df <- combined_df %>% select(Animals, EE_a, EE, unique_days_a) %>% pivot_longer(cols=c(EE_a, EE), names_to="TEE", values_to="EE") %>% mutate(TEE = recode(TEE, "EE_a" = "non-RMR", "EE"="RMR", "unique_days_a"="Days"))
            write.csv2(df_total, "df_total_verify_plot.csv")
            df_total <- combined_df
            p2 <- ggplot(data = df_total, aes(factor(Animals), EE, fill = TEE)) + geom_bar(stat = "identity")
            p2 <- p2 + xlab("Animal") + ylab(paste("EE [", input$kj_or_kcal, "/day]"))
            p2 <- p2 + scale_fill_manual(values=c("non-RMR" = "#B2DF8A", "RMR" = "#CAB2D6"))
            p2 <- p2 + ggtitle(paste("Energy expenditure (over a maximum of ", how_many_days, " days)", sep = ""))
            output$summary <- renderPlotly(
               ggplotly(p2) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian"))) 
            )
            
            # write.csv2(df_total, "tee_and_rmr.csv")
            # TODO: TEE is used in EnergyExpenditure too, so if it condtians Days again, we can also allow 2-way analayis for EE again, see energy_expenditure (before add_anova_ancova panel)
            storeSession(session$token, "TEE_and_RMR", df_total %>% rename(Days=unique_days_a), global_data)
            write.csv2(df_total, "test_for_rmr.csv")
            df_total <- df_total %>% filter(TEE == "RMR") %>% select(-TEE) %>% rename(TEE=EE)

            data_and_metadata <- enrich_with_metadata(df_total, real_data$metadata, input$havemetadata, metadatafile)
            true_metadata <- data_and_metadata$metadata
            print("true_metadata")
            print(true_metadata)

            # add statistics panel here
            add_anova_ancova_panel(input, output, session, global_data, true_metadata, df_total %>% rename(Days=unique_days_a), metadatafile, paste0("RMR [", input$kcal, "/ day]"), "RMR")

            # create LME model UI
            RMR_for_model <- getSession(session$token, global_data)[["TEE_and_RMR"]]
            RMR_for_model <- RMR_for_model %>% filter(TEE == "RMR") %>% select(-TEE) %>% rename(RMR=EE)
            if (!is.null(RMR_for_model)) {
               RMR_for_model <- RMR_for_model %>% full_join(y = true_metadata, by = c("Animals")) %>% na.omit() 
               write.csv2(RMR_for_model, "tee_before_lme_model.csv")
               create_lme_model_ui(input, output, true_metadata, RMR_for_model, "RMR", session, global_data)
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
           } else if (input$plot_type == "FuelOxidation") {
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Details")
               showTab(inputId = "additional_content", target = "Statistical testing")
               showTab(inputId = "additional_content", target = "Statistical model")
            output$explanation <- renderUI({
               str1 <- "<h3> Glucose, lipid and protein oxidation </h3>"
                  str2 <- "Displays the glucose, lipid and protein oxidation by means of respiratory gas exchange measurements during indirect calorimetry"
               HTML(paste(str1, str2, sep = "<br/>"))
            })
           } else if (input$plot_type == "HeatProduction") {
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
            create_row("Heldmaier's first", "(4.44 + 1.43 \\times RER) + \\dot{V}O_2", "\\frac{ml}{h}", "1", input$variable1 == "Heldmaier1"),
            create_row("Heldmaier's second", "\\dot{V}O_2 \\times (6 + RER + 15.3) \\times 0.278", "\\frac{ml}{h}", "1", input$variable1 == "Heldmaier2"),
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
               showTab(inputId = "additional_content", target = "Statistical model")
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
               hideTab(inputId = "additional_content", target = "Statistical model")
           } else if (input$plot_type == "RawMeasurement") {
            output$explanation <- renderUI({
               str1 <- "<h3> RawMeasurement measurements and derived quantities </h3>"
               str2 <- "The values of the raw measurement recorded over time during an indirect calorimetry experiment are displayed. Each line graphs respresents the raw measurement for an animal identified through either the ID reported in the metadata sheet, lab book or in the header of the raw data files. <hr/>"
               str3 <- "Note that oxygen consumption, carbon dioxide production as well as derived quantities like the RER (respiratory exchange ratio) can be plotted by selection the corresponding label in the the drop-down menu on the left-hand side window."
            HTML(paste(str1, str2, str3, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Statistical testing")
               showTab(inputId = "additional_content", target = "Details")
               showTab(inputId = "additional_content", target = "Explanation")
           } else if (input$plot_type == "TotalHeatProduction") {
               hideTab(inputId = "additional_content", target = "Summary statistics")
               showTab(inputId = "additional_content", target = "Statistical testing")
               showTab(inputId = "additional_content", target = "Details")
               showTab(inputId = "additional_content", target = "Statistical model")
           } else if (input$plot_type == "Metadata") {
               hideTab(inputId = "additional_content", target = "Summary statistics")
               hideTab(inputId = "additional_content", target = "Details")
               hideTab(inputId = "additional_content", target = "Explanation")
               hideTab(inputId = "additional_content", target = "Statistical model")
           } else {
            output$summary <- renderPlotly(NULL)
            hideTab(inputId = "additional_content", target = "Explanation")
           }

           # Main plot needs to be always visible
           showTab(inputId = "additional_content", target = "Main plot")

           # plot
           real_data$plot
        }
      })
      # scroll to top after click on plot only
      #shinyjs::runjs("window.scrollTo(0, 50);")
    })

   #############################################################################
   # Add example data sets
   #############################################################################
   add_example_data_sets(input, session, output, global_data)
 
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
      X = c("DE", "PC", "DC", "HP"),
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
         X = c("DC", "DE", "PC", "HP"),
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

   #############################################################################
   # Refresh
   #############################################################################
   observeEvent(input$refresh, {
     if (is.null(input$file)) {
         use_example_data <- getSession(session$token, global_data)[["use_example_data"]]
         if (is.null(use_example_data)) {
            shinyalert("Nothing to refresh", "Did you forgot to upload data or use an example data set and press load?")
            return()
         }
      }

      storeSession(session$token, "data_loaded", NULL, global_data)
      storeSession(session$token, "is_FuelOxidation_calculated", NULL, global_data)
      click("plotting")
   })

   #############################################################################
   # Load data
   #############################################################################
   observeEvent(input$load_data, {
      if (!is.null(input$File1)) {
           load_data(file$File1$datapath, input, input$sick, output, session)
           storeSession(session$token, "data_loaded", TRUE, global_data)
      } else {
           shinyalert("No data files given", "Upload at least one data file (Number of data files > 1)")
           return()
      }
   })
}
