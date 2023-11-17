library(shiny)
library(tidyr)
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
library(fs)
library(hash)
require(tidyverse)
library(tools)
library(shinyalert)

source("inc/helper.R") # helper methods
source("inc/extract_rmr.R") # rmr extraction
source("inc/extract_rmr_helper.R") # rmr extraction helper
source("inc/import_promethion_helper.R") # import for SABLE/Promethion data sets
source("inc/import_pheno_v8_helper.R") # import for PhenoMaster V8 data sets
source("inc/import_cosmed_helper.R") # import for COSMED data sets
source("inc/locomotion.R") # for locomotion probability heatmap
source("inc/timeline.R") # for colorizing timeline by day/night rhythm
source("inc/locomotion_budget.R") # for locomotion budget
source("inc/guide.R") # for guide




################################################################################
# Helper functions
################################################################################
convert <- function(x) {
    splitted <- strsplit(as.character(x), " ")
    paste(splitted[[1]][2], ":00", sep = "")
}

################################################################################
# Export to CalR compatible file format and Excel
################################################################################
do_export2 <- function(format, input, output, file_output) {
      # exports for now only the first data set to CalR data format
      file <- input$File1
      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file <- input$File1
         real_data <- do_plotting(file$datapath, input, input$sick, output)
         h <- hash()

         # Specific mapping of column names from TSE to CalR to produce
         # a compatible .csv file
         h[["Datetime"]] <- "Date_Time"
         h[["VCO2(3)_[ml/h]"]] <- "RT_VCO2_3"
         h[["VO2(3)_[ml/h]"]] <- "RT_VO2_3"
         # caloric equivalent by 1st formula (HeatProduction formula 1)
         h[["HP"]] <- "RT_Kcal_hr_1"
         # caloric equivalent by 2nd formula (HeatProduction formula 2)
         h[["HP2"]] <- "RT_Kcal_hr_2"
         h[["Animal No._NA"]] <- "Animal"
         # animal
         # rename data
         for (v in ls(h)) {
            names(real_data$data)[names(real_data$data) == v] <- h[[v]]
         }
         if (format == "CalR") {
         write.csv(real_data$data[values(h)], file = file_output, row.names = FALSE)
         writeLines(gsub(pattern = '"', replace = "", x = readLines(file_output)), con = file_output)
         }

         if (format == "Excel") {
            tmp <- cbind(real_data$data[values(h)], real_data$animals)
           write_xlsx(tmp, path = file_output)
         }
      }
   }

do_export <- function(format, input, output) {
   if (format == "CalR") {
      file <- input$File1

      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file <- input$File1
         real_data <- do_plotting(file$datapath, input, input$sick, output)
         h <- hash()

         # Specific mapping of column names from TSE to CalR to produce
         # a compatible .csv file
         h[["Datetime"]] <- "Date_Time"
         h[["VCO2(3)_[ml/h]"]] <- "RT_VCO2_3"
         h[["VO2(3)_[ml/h]"]] <- "RT_VO2_3"
         # caloric equivalent by 1st formula (HeatProduction formula 1)
         h[["HP"]] <- "RT_Kcal_hr_1"
         # caloric equivalent by 2nd formula (HeatProduction formula 2)
         h[["HP2"]] <- "RT_Kcal_hr_2"
         # rename data
         for (v in ls(h)) {
            names(real_data$data)[names(real_data$data) == v] <- h[[v]]
         }
         fname <- paste(paste(path_home(), input$export_folder$path[[2]],
            sep <- "/"), input$export_file_name, sep <- "/")
         write.csv(real_data$data[values(h)], file = fname, row.names = FALSE)
         writeLines(gsub(pattern = '"', replace = "", x = readLines(fname)),
          con = fname)
      }
   }
}

################################################################################
# Create plotly plot
################################################################################
do_plotting <- function(file, input, exclusion, output) {
   # plot look and feel configuration
   theme_pubr_update <- theme_pubr(base_size = 8.5) +
   theme(legend.key.size = unit(0.3, "cm")) +
   theme(strip.background = element_blank()) +
   theme(strip.text = element_text(hjust = 0)) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
   theme_set(theme_pubr_update)

   detectFileType <- function(filename) {
      file_ext(filename)
   }
   # data read-in
   fileFormatTSE <- FALSE
   finalC1 <- c()
   finalC1meta <- data.frame(matrix(nrow = 0, ncol = 6))
   # Supported metadata fields from TSE LabMaster/PhenoMaster (Sable/Promethion must should go through metadata sheet (TODO))
   colnames(finalC1meta) <- c("Animal.No.", "Diet", "Genotype", "Box", "Sex", "Weight..g.")
   for (i in 1:input$nFiles) {
      file <- input[[paste0("File", i)]]
      file <- file$datapath
      con <- file(file)
      line <- readLines(con, n = 2)
   if (i == 1) {
      fileFormatTSE <- line[2]
      studyDescription <- line[1]
      output$study_description <- renderText(paste("Study description: ", gsub("[;]", "", studyDescription), sep= " "))
      output$file_type_detected <- renderText(paste("Input file type detected:", gsub("[;,]", "", line[2]), sep = " "))
   }
   #########################################################################################################
   # Detect data type (TSE v6/v7, v5 (akim, dominik) or v8 (jan, tabea) or promethion/sable (.xlsx) (jenny))
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

   # check file extension
   fileExtension <- detectFileType(file)

   # default
   sep <- ";"
   dec <- ","

   scaleFactor <- 1
   # Promethion live/Sable input
   if (fileExtension == "xlsx") {
      output$study_description <- renderText("")
      tmp_file <- tempfile()
      if (length(excel_sheets(file)) == 2) { # TODO: Improve this check
        output$file_type_detected <- renderText("Input file type detected as: COSMED")
        import_cosmed(file, tmp_file)
      } else {
        output$file_type_detected <- renderText("Input file type detected as: Promethion/Sable")
        import_promethion(file, tmp_file)
      }
      file <- tmp_file
      toSkip <- detectData(file)
      scaleFactor <- 60
   }

   # LabMaster V5 (horizontal format?)
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

   # Phenomaster V8 (needs to be treated differently)
   if (grepl("V8", fileFormatTSE)) {
      tmp_file <- tempfile()
      import_pheno_v8(file, tmp_file)
      file <- tmp_file
      toSkip <- detectData(file)
   }

   # Note: replaces read.csv with read table
   # File encoding matters: Shiny apps crashes due to undefined character entity
   C1 <- read.table(file, header = FALSE, skip = toSkip + 1,
      na.strings = c("-", "NA"), fileEncoding = "ISO-8859-1", sep = sep, dec = dec)

   # TODO: C1meta obsolete when using metadata sheet, implement/use Lea's metadata sheet
   C1meta <- read.table(file, header = TRUE, skip = 2, nrows = toSkip + 1 - 4,
      na.strings = c("-", "NA"), fileEncoding = "ISO-8859-1", sep = sep, dec = dec)
   # Curate data frame
   C1.head <- read.table(file, # input file
                        header = FALSE, # no header
                        skip = toSkip - 1, # skip headers up to first animal
                        nrows = 2, # read only two rows (variable name + unit)
                        na.strings = c("", "NA"), #transform missing units to NA
                        as.is = TRUE, # avoid transformation character->vector
                        check.names = FALSE, # set the decimal separator
                        fileEncoding = "ISO-8859-1",
                        sep = sep,
                        dec = dec)
   names(C1) <- paste(C1.head[1, ], C1.head[2, ], sep = "_")


   # unite data sets (unite in tidyverse package)
   # print(C1)
   C1 <- C1 %>%
   unite(Datetime, # name of the final column
         c(Date_NA, Time_NA), # columns to be combined
         sep = " ") # separator set to blank

   # substitute "." by "/"
   C1$Datetime <- gsub(".", "/", C1$Datetime, fixed = TRUE)

   # transform into time format appropriate to experimenters
   C1$Datetime2 <- as.POSIXct(C1$Datetime, format = "%d/%m/%Y %H:%M")
   C1$hour <- hour(C1$Datetime2)
   C1$minutes <- minute(C1$Datetime2)
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>%
   arrange(Datetime2) %>%
   filter(Datetime2 >= input$daterange[[1]] & # From
            Datetime2 <= input$daterange[[2]]) %>% # To
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


   if (input$negative_values) {
      if (!(nrow(C1 %>% select(where(~any(. < 0)))) == 0)) {
         shinyalert("Oops!", "Negative values encountered in measurements. Check your input data.", type = "error")
      }
   }


   if (input$highly_varying_measurements) {
      if (any(C1 %>% mutate(col_diff = `VO2(3)_[ml/h]` - lag(`VO2(3)_[ml/h]`)) > 0.5)) {
         shinyalert("Oops!", "Highly varying input measurements detected in O2 signal", type = "error")
      }

      if (any(C1 %>% mutate(col_diff = `CO2(3)_[ml/h]` - lag(`CO2(3)_[ml/h]`)) > 0.5)) {
         shinyalert("Oops!", "Highly varying input measurements detected in CO2 signal", type = "error")
      }
   }

   # Step #9 - define 1/n-hours steps
   # TODO: Check that this implementation is actually correct as taken from provided code
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
   switch(f1,
   Lusk = {
      C1$HP <- 15.79 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 5.09 * C1$RER_NA / 1000
   },
   HP = {
      C1$HP <- scaleFactor * C1$`VO2(3)_[ml/h]` * (6 * C1$RER_NA + 15.3) * 0.278 / 1000 * (3600/1000)
   },
   HP2 = {
      C1$HP <- (4.44 + 1.43 * C1$RER_NA) * scaleFactor * C1$`VO2(3)_[ml/h]` * (3600/1000) / 1000
   },
   Weir = {
      C1$HP <- 16.3 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.57 * C1$`VCO2(3)_[ml/h]` / 1000
   },
   Elia = {
      C1$HP <- 15.8 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 5.18 * C1$RER_NA / 1000
   },
   Brower = {
      C1$HP <- 16.07 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.69 * C1$RER_NA / 1000
   },
   Ferrannini = {
      C1$HP <- 16.37117 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.6057 * C1$RER_NA / 1000
   },
   {

   }
   )
   #############################################################################
   # Heat production formula #2
   #############################################################################
   switch(f2,
   Lusk = {
      C1$HP2 <- scaleFactor * 15.79 * C1$`VO2(3)_[ml/h]` / 1000 + 5.09 * C1$RER_NA / 1000
   },
   HP = {
      C1$HP2 <- scaleFactor * C1$`VO2(3)_[ml/h]` * (6 * C1$RER_NA + 15.3) * 0.278 / 1000 * (3600/1000)
   },
   HP2 = {
      C1$HP2 <- (4.44 + 1.43 * C1$RER_NA) * scaleFactor * C1$`VO2(3)_[ml/h]` * (3600/1000) / 1000
   },
   Weir = {
      C1$HP2 <- 16.3 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.57 * C1$`VCO2(3)_[ml/h]` / 1000
   },
   Elia = {
      C1$HP2 <- scaleFactor * 15.8 * C1$`VO2(3)_[ml/h]` / 1000 + 5.18 * C1$RER_NA / 1000
   },
   Brower = {
      C1$HP2 <- scaleFactor * 16.07 * C1$`VO2(3)_[ml/h]` / 1000 + 4.69 * C1$RER_NA / 1000
   },
   Ferrannini = {
      C1$HP2 <- scaleFactor * 16.37117 * C1$`VO2(3)_[ml/h]` / 1000 + 4.6057 * C1$RER_NA / 1000
   },
   {

   }
   )


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

   # TODO: Put filtering for light cycle back in (useful for RMR calculation)
   # print("before filtering!")
   # print(colnames(finalC1))
   if (! is.null(input$light_cycle)) {
      if ("LightC_[%]" %in% colnames(finalC1)) {
         `$`(finalC1, "LightC_[%]") <- as.numeric(`$`(finalC1, "LightC_[%]"))
         # filter finalC1 by light cycle
         if (input$light_cycle == "Night") {
         #   finalC1 <- filter(finalC1, `LightC_[%]` == "0")
         } else {
          #  finalC1 <- filter(finalC1, `LightC_[%]` == "49")
         }
      }
   }


   #############################################################################
   # Plotting
   #############################################################################
   plotType <- input$plot_type
   write.csv2(C1, file = "all_data.csv")
   write.csv2(finalC1, file = "finalC1.csv")

   switch(plotType,
   CompareHeatProductionFormulas = {

   p <- ggplot(data = finalC1, aes_string(x = "HP", y = "HP2")) +
   geom_point() +
   stat_smooth(method = "lm") + # add regression line and pearson product moment correlation
   stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
   },
   GoxLox = {

      C1meta_tmp <- C1meta
      #print(C1meta_tmp)
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      #print("colnames metadata:")
      #print(colnames(C1meta_tmp))
      #print("colnames fin1lC1")
      #print(colnames(finalC1))
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")


      if (input$goxlox == "Glucose oxidation") {
         df_to_plot$GoxLox <- scaleFactor * 4.55 * df_to_plot$`VO2(3)_[ml/h]` - scaleFactor * 3.21 * df_to_plot$`VCO2(3)_[ml/h]`
      } else {
         df_to_plot$GoxLox <- scaleFactor * 1.67 * df_to_plot$`VO2(3)_[ml/h]` - scaleFactor * 1.67 * df_to_plot$`VCO2(3)_[ml/h]`
      }
      colors <- as.factor(`$`(df_to_plot, "Animal No._NA"))
      df_to_plot$Animals <- colors


   p <- ggplot(data = df_to_plot, aes_string(y = "GoxLox", x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
   p <- p + ylab("GoxLox quantity")
   p <- p + xlab("Time [h]")

   },
   EnergyExpenditure = {
   colors <- as.factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals <- colors

   # get metadata from TSE header (should in fact use C1meta metadata from metadata sheet)
   df_filtered <- C1meta[, colSums(is.na(C1meta)) == 0]
   df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
   df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
   colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
   colnames(df_filtered)[colnames(df_filtered) == "Animal.No."] <- "Animal No._NA"
   ##finalC1 <- merge(finalC1, df_filtered, by = "Box_NA")
   # Note: merge should be done on unique Animal No!
   finalC1 <- merge(finalC1, df_filtered, by = "Animal No._NA")

   if (input$with_grouping) {
      if (!is.null(input$select_data_by)) {
         my_var <- input$condition_type
         finalC1 <- finalC1[finalC1[[my_var]] == input$select_data_by, ]
     }
  }

   # TODO: This filters out first recordings on each day, probably not desired
   # finalC1$Datetime <- lapply(finalC1$Datetime, convert)
   # finalC1$TimeInHours <- hour(hms(finalC1$Datetime))*60+minute(hms(finalC1$Datetime))
   # finalC1 = filter(finalC1, TimeInHours > (60*as.numeric(input$exclusion)))
   # print(finalC1$TimeInHours)
   # p <- ggplot(data = finalC1, aes_string(x = C1$running_total.hrs.round, y = "HP2", color=finalC1$`Animal No._NA`, group=finalC1$`Animal No._NA`, color=finalC1$`Animal No._NA`)) + geom_point() #nolint
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
   p <- p + scale_fill_brewer(palette = "Spectral")

   if (input$wmeans) {
      p <- p + geom_smooth(method = input$wmeans_choice)
   }

   if (input$wstats) {
      p <- p + stat_cor(method = "pearson")
   }


  lights <- data.frame(x = finalC1["running_total.hrs.halfhour"], y = finalC1["HP2"])
  colnames(lights) <- c("x", "y")
  if (input$timeline) {
   p <- draw_day_night_rectangles(lights, p, input$light_cycle_start, input$light_cycle_stop, 0)
  }

   p <- p + xlab("Time [h]")
   # p <- p + ylab(paste("Caloric equivalent [", input$myp, "]"))
   p <- p + ylab(paste("Energy expenditure [", input$kj_or_kcal, "/ h]", "(equation: ", input$myp, ")", sep = " "))
   # Note this excludes only at beginning of measurement experiment currently in the visualization
   #p <- p + scale_x_continuous(limits = c(input$exclusion, NA))

   if (input$with_facets) {
      if (!is.null(input$facets_by_data_one)) {
         if (input$orientation == "Horizontal") {
            p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
         } else {
            p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
         }
      }
   }
   p <- ggplotly(p) %>% config( toImageButtonOptions = list(
      format = "svg",
      width = 1200,
      height = 600
    )
  )
   },
   # TODO: fix issues with multiple files
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

      #print("from RMR:")
      #print(colnames(finalC1))
      # first component, typically O2
      df <- data.frame(Values = finalC1[[component]],
         Group = `$`(finalC1, "Animal No._NA"),
         Values2 = finalC1$HP)
      df_new <- partition(df)
      df_new <- cv(df_new, input$window)
      df_new <- reformat(df_new)

      # second component, typically CO2
      df2 <- data.frame(Values = finalC1[[component2]],
         Group = `$`(finalC1, "Animal No._NA"),
         Values2 = finalC1$HP)
      df_new2 <- partition(df2)
      df_new2 <- cv(df_new2, input$window)
      df_new2 <- reformat(df_new2)

      finalC1$Datetime <- lapply(finalC1$Datetime, convert)

      # FIXME: Code works in this way only if NO averaging present done for RMR
      # depending on calculation of finalC1$HP values we have different number
      # of rows for df_new and finalC1, then fails.
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

      p2 <- ggplot(data = df_for_cov_analysis, aes(x = Time, y = CoV1, group = Animal))
      p3 <- ggplot(data = df_for_cov_analysis, aes(x = Time, y = CoV2, group = Animal))

      # TODO: Use actual user input values (The following has been
      # used for testing purposes and have full control over data)
      M <- 1
      PERCENTAGE <- 1
      df_plot_total <- extract_rmr_helper()
      write.csv2(df_plot_total, file = "df_for_comparison_with_calimera.csv")
      df_plot_total$HP <- as.numeric(df_plot_total$HP) * 1000
      df_plot_total$Time <- as.numeric(df_plot_total$Time)
      p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, group = Component,
      color = Component)) + geom_line() + facet_wrap(~Animal)
      p <- p + ylab(paste("Energy expenditure [", input$kj_or_kcal, "/ h]", "(equation: ", input$myp, ")", sep = " "))
      p <- p + xlab("Time [minutes]")
      finalC1 <- df_plot_total
   },

   WeightVsEnergyExpenditure = {

      #C1meta_tmp <- read.csv2("metadata_now.csv", sep=";")
      C1meta_tmp <- C1meta
      #print(C1meta_tmp)
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      #print("colnames metadata:")
      #print(colnames(C1meta_tmp))
      #print("colnames fin1lC1")
      #print(colnames(finalC1))
      # Animal No. NA would be correct, but not updated in user interface (old value Animal.No.) 
      # how to force gui update before? TODO
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


             p <- ggplotly(p) %>% config( toImageButtonOptions = list(
      format = "svg",
      width = 1200,
      height = 600
    )
  )
   },
   DayNightActivity = {

   convert <- function(x) {
      splitted <- strsplit(as.character(x), " ")
      paste(splitted[[1]][2], ":00", sep = "")
   }
      C1meta_tmp <- C1meta
      #print(C1meta_tmp)
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      #print("colnames metadata:")
      #print(colnames(C1meta_tmp))
      #print("colnames fin1lC1")
      #print(colnames(finalC1))
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")

   df_to_plot$Datetime <- lapply(df_to_plot$Datetime, convert)
   df_to_plot$NightDay <- ifelse(hour(hms(df_to_plot$Datetime)) * 60 + minute(hms(df_to_plot$Datetime)) < 720, "am", "pm")
   df_to_plot$Animals <- as.factor(`$`(df_to_plot, "Animal No._NA"))
   p <- ggplot(df_to_plot, aes(x = Animals, y = HP, fill = NightDay)) + geom_boxplot()

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
      p <- ggplotly(p) %>% layout(boxmode = "group") %>% config( toImageButtonOptions = list(
      format = "svg",
      width = 1200,
      height = 600
    )
      )
   } else {
      p <- ggplotly(p) %>% layout(boxmode = "group") %>% config( toImageButtonOptions = list(
      format = "svg",
      width = 1200,
      height = 600
    )
  )
   }
   },
   ANCOVA = {
   # fall back covariate weight
   # (TODO: can be used from xlsx sheet not from the TSE file)
   my_covariate <- "Weight..g."
   for (c in colnames(C1meta)) {
      if (c %like% input$covariates) {
         my_covariate <- "Weight..g."
         break
      }
   }

   # Use the metadata from the provided TSE file header
   # metadata <- data.frame(`$`(C1meta, foo), `$`(C1meta, "Animal.No."))
   # metadata <- data.frame(C1meta[my_covariate], `$`(C1meta, "Animal.No."))
   # names(metadata) <- c("Weight", "Animal No._NA")
   # print(metadata)
   finalC1[, "Weight"] <- NA
   finalC1[, "Gender"] <- NA

   # The metadata from the excel spreadsheet
   my_data <- read_excel(input$metadatafile$datapath)

   df <- tbl_df(my_data) # TODO: use as_tibble (tbl_df deprecated)
   # TODO: based on covariate extract a different line, weight is in line 26
   # lean in 27 and fat in 28 line... (this data comes from the Excel metadata sheet)
   a <- df[21, ] %>% slice(1) %>% unlist(., use.names = FALSE)
   c <- df[23, ] %>% slice(1) %>% unlist(., use.names = FALSE)
   b <- df[26, ] %>% slice(1) %>% unlist(., use.names = FALSE)
   metadata <- data.frame(a, b, c)
   metadata <- na.omit(metadata)
   names(metadata) <- c("Weight", "Animal No._NA", "Gender")
   metadata <- metadata[seq(2, nrow(metadata)), ]
   #print(metadata$Weight)

   # TODO: Make for loop more efficient somehow, perhaps with the following statement:
   # by(finalC1, seq_len(nrow(finalC1)), function(row) row["Weight"] = which(`$`(metadata, "Animal No._NA") == row["Animal No._NA"] %>% pull("Animal No._NA")))
   for (i in 1:nrow(finalC1)) {
      js <- which(`$`(metadata, "Animal No._NA") == finalC1[i, "Animal No._NA"] %>% pull("Animal No._NA"))
      if (length(js) > 0) {
         w <- metadata[js, "Weight"]
         w2 <- metadata[js, "Gender"]
         finalC1[i, "Weight"] <- as.numeric(w)
         finalC1[i, "Gender"] <- w2
      }
   }
   # filter df by indexing
   if (length(input$checkboxgroup_gender) == 0) {
      finalC1 <- NA
   } else {
      if (! length(input$checkboxgroup_gender) == 2) {
          finalC1 <- finalC1[finalC1$Gender == input$checkboxgroup_gender, ]
    }
   }
   names(finalC1)[names(finalC1) == "Animal No._NA"] <- "Animal"
   finalC1$Animal <- as.factor(finalC1$Animal)
   write.csv2(finalC1, file = "finalC1.csv")
   finalC1 <- drop_na(finalC1)
   finalC1 <- aggregate(finalC1, by = list(AnimalGroup = finalC1$Animal), FUN = mean)

   p <- ggscatter(finalC1, x = "Weight", y = "HP", add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"), conf.int = TRUE)
   p <- p + xlab("Weight [g]") + ylab("Mean heat production")
     p <- ggplotly(p) %>% config( toImageButtonOptions = list(
      format = "svg",
      width = 1200,
      height = 600
    )
  )
   # TOOD: add summary statistics to plot
   #print(summary(lm(HP ~ Weight, finalC1)))
   message <- NULL
   if (sum(is.na(finalC1)) > 0) {
      message <- paste("# ", sum(is.na(finalC1)),
      " entries do not have metadata attached, Check metadata file.")
   }
   return(list("plot" = p, status = message, metadata = metadata))
   },
   Locomotion = {
      # TODO: Implement / Fix for plotly plotting not supported yet of the given graph type
      file <- input[[paste0("File", 1)]]
      p <- plot_locomotion(file$datapath, input$x_min_food, input$x_max_food, input$y_min_food, input$y_max_food, input$x_min_scale, input$x_max_scale, input$y_min_scale, input$y_max_scale, input$x_min_bottle, input$x_max_bottle, input$y_min_bottle, input$y_max_bottle)
      p
   },
   LocomotionBudget = {
      file <- input[[paste0("File", 1)]]
      p <- plot_locomotion_budget(file$datapath)
      p
   },
   EstimateRMR = {
      C1meta_tmp <- C1meta
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      df_to_plot <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")
      endtime <- input$rmr_method_begin # in minutes for ss methods
      stepwidth <- input$rmr_method_frequency # in seconds (fixed for COSMED?!)
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
         if ( (cv_rer < CV_RER_MAX) && (cv_vo2 < CV_VO2_MAX) && (cv_vco2 < CV_VCO2_MAX)) {
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
      p <- ggplot(df, aes(x = rmr)) + geom_histogram()
      ggplotly(p)
   },
   Raw = {
      C1meta_tmp <- C1meta
      #print(C1meta_tmp)
      colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
      #print("colnames metadata:")
      #print(colnames(C1meta_tmp))
      #print("colnames fin1lC1")
      #print(colnames(finalC1))
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

   #print("to plot names")
   #print(names(df_to_plot))
   names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
   names(df_to_plot)[names(df_to_plot) == "RER_NA"] <- "RER"
   p <- ggplot(data = df_to_plot, aes_string(y = input$myr, x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
   mylabel <- gsub("_", " ", mylabel)
   p <- p + ylab(mylabel)
   p <- p + xlab("Time [h]")

  if (input$with_facets) {
      if (!is.null(input$facets_by_data_one)) {
         if (input$orientation == "Horizontal") {
            p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
         } else {
            p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
         }
      }
   }
     p <- ggplotly(p) %>% config( toImageButtonOptions = list(
      format = "svg",
      width = 1200,
      height = 600
    )
  )
   },
   TotalEnergyExpenditure = {
   colors <- as.factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals <- colors

   C1meta_tmp <- C1meta
   colnames(C1meta_tmp)[colnames(C1meta_tmp) == "Animal.No."] <- "Animal No._NA"
   finalC1 <- merge(C1meta_tmp, finalC1, by = "Animal No._NA")
   convert <- function(x) {
      splitted <- strsplit(as.character(x), " ")
      paste(splitted[[1]][1], "", sep = "")
   }

   finalC1$Datetime <- day(dmy(lapply(finalC1$Datetime, convert)))
   # TODO: get time interval for rescaling 60 / 15 minutes = 6
   finalC1$HP <- finalC1$HP / 6
   finalC1$HP2 <- finalC1$HP2 / 6

   # TODO: use input$only_full_days to filter out here no full days...
   # TODO if diet then group plot by diet
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

  p <- ggplot(data = TEE, aes(x = Animals, y = TEE, fill = Equation, label = Days)) + geom_boxplot() + geom_point() # position = position_jitterdodge())
  p <- p +  geom_text(check_overlap = TRUE, aes(label=Days),  position = position_jitter(width = 0.15))
  p <- p + ylab(paste("TEE [", input$kj_or_kcal, "/day]", sep = ""))
  if (input$with_facets) {
      if (!is.null(input$facets_by_data_one)) {
         if (input$orientation == "Horizontal") {
           p <- p + facet_grid(as.formula(".~Facet"))
          #  p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
         } else {
           # p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
            p <- p + facet_grid(as.formula("Facet~."))
         }
      }
   }

 # if (input$with_facets) {
 #     if (!is.null(input$facets_by_data_one)) {
 #        if (input$orientation == "Horizontal") {
 #           p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
 #        } else {
 #           p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
 #        }
 #     }
#
 #     p <- ggplotly(p)
 #  } else {
      p <- ggplotly(p) %>% layout(boxmode = "group") %>%
      config(toImageButtonOptions = list(
      format = "svg",
      width = 1200,
      height = 600
    )
  )
#  }

   # TODO: group by condition/diet with facets
   },
   {

   }
   )

   list("plot" = p, "animals" = `$`(finalC1, "Animal No._NA"), "data" = finalC1, "metadata" = C1meta)
}

################################################################################
# Create server
################################################################################
server <- function(input, output, session) {
   observe_helpers()

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
            status_okay <- do_export2(input$export_format, input, output, file)
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

   #############################################################################
   # Observe heat production formula 1 and forumula 2
   #############################################################################
   observeEvent(c(input$variable1, input$variable2), {
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
         Brower = {
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
         Brower = {
            text2 <- "$$ \\tag{2} 16.07 \\times VO2[\\frac{ml}{h}] + 4.69 \\times RER $$"
         },
         Ferrannini = {
            text2 <- "$$ \\tag{2} 16.37117 \\times VO2[\\frac{ml}{h}] + 4.6057 \\times RER $$"
         },
         {
         }
         )

         output$heat_production_equations <- renderUI(
            tagList(
            withMathJax(),
            div("Chosen first (1) and second (2) equation for plotting"),
            div(text1),
            div(text2)
            )
         )
   })
   #############################################################################
   # Observe plot type
   #############################################################################
   observeEvent(input$plot_type, {
            output$myp <- renderUI(
               selectInput(inputId = "myp",
               label = "Chose prefered method for calculating caloric equivalent over time",
               selected = input$variable1, choices = c(input$variable1, input$variable2)))
         })

   observeEvent(input$plot_type, {
            output$checkboxgroup_gender <- renderUI(
               checkboxGroupInput(inputId = "checkboxgroup_gender", label = "Chose gender",
               choices = list("male" = "male", "female" = "female"), selected = c("malw", "female")))
         })

   observeEvent(input$plot_type, {
      output$myr <- renderUI(
         selectInput(inputId = "myr", label = "Chose raw data to plot", choices = c("O2", "CO2", "RER", "VO2", "VCO2", "Temp")))
    })

   observeEvent(input$plot_type, {
            output$wmeans <- renderUI(
               checkboxInput(inputId = "wmeans", label = "Display means"))

            output$wmeans_choice <- renderUI(
               selectInput(inputId = "wmeans_choice", label="Method", choices = c("lm", "glm", "gam", "loess")))
         })

   observeEvent(input$plot_type, {
            output$wstats <- renderUI(
               checkboxInput(inputId = "wstats", label = "Display statistics"))
         })

   observeEvent(input$plot_type, {
      output$covariates <- renderUI(
            selectInput(inputId = "covariates", label = "Chose a covariate",
            selected = "Weight", choices = c("Weight", "Lean mass", "Fat mass")))
   })


   #############################################################################
   # Observe export events
   #############################################################################
   observeEvent(input$export_folder, {
       output$folder_name_export <- renderUI(
            renderText(paste(path_home(), input$export_folder$path[[2]], sep = "/")))
      })

   observeEvent(input$downloadData, {
       if (input$export_format == "CalR") {
            status_okay <- do_export("CalR", input, output)
            if (!status_okay) {
              output$message <- renderText("Error during data export to CalR, check logs")
            } else {
              output$message <- renderText(paste("Consolidated data exported to format >>",
              input$export_format, "<<", sep = " "))
            }
       }
       if (input$export_format == "Excel") {
            status_okay <- do_export2("Excel", input, output)
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
   observeEvent(input$replotting, {
           output$plot <- renderPlotly({
           file <- input$File1
           real_data <- do_plotting(file$datapath, input, exclusion = input$sick, output)
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
            # print("No cohort data given!")
            output$message <- renderText("Not any cohort data given")
         } else {
           file <- input$File1
           # print(file$datapath)
           real_data <- do_plotting(file$datapath, input, input$sick, output)

           # print(real_data$status)
           if (! is.null(real_data$status)) {
               if (real_data$status == FALSE) {
                  output$message <- renderText("Input data incompatible, make sure you either supply only TSE or Sable system files not a combination of both file types.") #nolint
               } else {
                  output$message <- renderText(real_data$status)
               }
            }

           # Caused an error before on deployment, need to check for null always.
            if (!is.null(input$covariates)) {
               if (! any(colnames(real_data$metadata) %like% input$covariates)) {
                  output$message <- renderText("Covariate not present in (all) data sets, fallback to default (Weight [g])")
               }
            }

           if ((! is.null(real_data$animals)) && is.null(input$sick)) {
              output$sick <- renderUI(
              multiInput(inputId = "sick", label = "Remove outliers (sick animals, etc.) ", selected = "", choices = unique(real_data$animals)))
           }

           if ((! is.null(real_data$animals)) && is.null(input$facets_by_data_one)) {
            df_filtered <- real_data$metadata[, colSums(is.na(real_data$metadata)) == 0]
            df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
            df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
            colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
            our_group_names <- unique(colnames(df_filtered))

            output$facets_by_data_one <- renderUI(
               selectInput(inputId = "facets_by_data_one", label = "Choose facet",
               selected = "Animals", choices = our_group_names))

           # TODO: add this back to filter any group by a certain condition... is this useful at all?
           # if (! is.null(input$condition_type)) {
           # output$condition_type <- renderUI(
           #    selectInput(inputId="condition_type", label="Group", selected="Diet", choices=our_group_names))
           # }
           }
           observeEvent(input$condition_type, {
            metadata <- real_data$metadata
            my_var <- input$condition_type
            diets <- unique(metadata[[my_var]])
            output$select_data_by <- renderUI(
               selectInput("select_data_by", "Condition", choices = diets, selected=input$select_data_by)
            )
           }
           )
        if ((! is.null(real_data$animals)) && is.null(input$select_data_by)) {
            metadata <- real_data$metadata
            my_var <- input$condition_type
            diets <- unique(metadata[[my_var]])
            output$select_data_by <- renderUI(
               selectInput("select_data_by", "Condition", choices = diets)
            )
           }

               if (input$plot_type == "RestingMetabolicRate") {
            # old bar plot
            df_filtered <- real_data$data %>% group_by(Animal, Component) %>% summarize(Value = min(HP), cgroups = c(Animal, Component))
            write.csv2(df_filtered, "rmr.csv")
            #p <- ggplot(df_filtered, aes(factor(Animal), Value, fill = Component))
            #p <- p + geom_bar(stat = "identity", position = "dodge") + scale_fill_brewer(palette = "Set1")
            #p <- p + xlab("Animal") + ylab("RMR (min) [kcal/day]")
            df <- real_data$data
            df$Animal <- as.factor(df$Animal)
            df$Component <- as.factor(df$Component)
            p <- ggplot(df, aes(x=Animal, y=HP, color=Animal)) + geom_boxplot() + geom_point(position=position_jitter(0.1))
            p <- p + xlab("Animal") + ylab(paste("RMR [", input$kj_or_kcal, "/h]", sep=""))


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

            df1 <- read.csv2("rmr.csv")
df2 <- read.csv2("tee.csv")
df1 <- rename(df1, Animals=Animal)
#df1 <- aggregate(Value~Component, df1, sum)
#df1 <- df1 %>% distinct(Component, Value, cgroups, .keep_all=TRUE)
#print(head(df1))
df1 <- df1 %>% group_by(Animals) %>% summarize(EE = sum(Value)/15) # TODO fixme: divide by days and measuring interval
df2 <- df2 %>% group_by(Animals) %>% summarize(EE = sum(TEE)/15)

df1$TEE <- as.factor(rep("non-RMR", nrow(df1)))
df2$TEE <- as.factor(rep("RMR", nrow(df2)))

df_total <- rbind(df1, df2)
df_total$Animals <- as.factor(df_total$Animals)

#inner_join(df1, df2, by='Animals')

#totalB <- aggregate(Value~Component, df1, sum)
#print(totalB)
#total_df <- inner_join(df1, df2, by='Animals')


p2 <- ggplot(data = df_total, aes(factor(Animals), EE, fill=TEE)) + geom_bar(stat="identity")
p2 <- p2 + xlab("Animal") + ylab(paste("EE [", input$kj_or_kcal, "/day]"))

            output$details <- renderPlotly(ggplotly(p2))
           } else if (input$plot_type == "CompareHeatProductionFormulas") {
            output$explanation <- renderUI({
            str1 <- "<h3> Comparison of heat production formulas </h3>"
            str2 <- "Two heat production formulas can be compared via a simple scatter plot and plotting into the plot a regression line (Pearson-Product-Moment correlation coefficient r)" #nolint
            str3 <- "p-value reported, HP and HP2 correspond to the formulas displayed in the sidebar on the left"
            str4 <- "<hr/>"
            str5 <- "When heat production formulas agree mostly, so there should visually not be too many large residuals from a line of slope 1 be apparent in the plot." #nolint
            HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target="Summary statistics")
               hideTab(inputId = "additional_content", target="Details")
           } else if (input$plot_type == "EnergyExpenditure") {
             output$explanation <- renderUI({
            str1 <- "<h3> Caloric Equivalent / heat production over time </h3>"
            str2 <- "According to a heat production formula the energy expenditure is calculated from indirect calorimetry data"
            str3 <- "<hr/>"
            str4 <- "Cohorts are usually stratified by animal ID by default"
            HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target="Summary statistics")
               hideTab(inputId = "additional_content", target="Details")
           } else if (input$plot_type == "DayNightActivity") {
              output$explanation <- renderUI({
            str1 <- "<h3> Day and night (average) energy expenditure of animals in cohorts </h3>"
            str2 <- "According to a heat production formula the energy expenditure is calculated from indirect calorimetry data"
            str3 <- "<hr/>"
            str4 <- "Cohorts are usually stratified by animal ID and day night activity by default"
            HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target="Summary statistics")
               hideTab(inputId = "additional_content", target="Details")
           } else if (input$plot_type == "Raw") {
            output$explanation <- renderUI({
               str1 <- "<h3> Raw data values are plotted </h3>"
               str2 <- "According to the recorded data, line graphs are displayed"
               str3 <- "<hr/>"
               str4 <- "Cohorts are usually strafified by animal ID by default"
            HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target="Summary statistics")
               hideTab(inputId = "additional_content", target="Details")
           } else if (input$plot_type == "TotalEnergyExpenditure") {
            output$explanation <- renderUI({
               str1 <- "<h3> Total energy expenditure (TEE) for animal per day is displayed </h3>"
               str2 <- "Depending on the heat production formulas chosen (HP and HP2)"
               str3 <- "<hr/>"
               str4 <- "Usually there is no large discrepancy between TEEs calculated from different heat production formulas"
            HTML(paste(str1, sep = "<br/>"))
            })
               hideTab(inputId = "additional_content", target="Summary statistics")
               hideTab(inputId = "additional_content", target="Details")
           } else {
            output$summary <- renderPlotly(NULL)
            output$explanation <- renderUI({
               HTML("No information available yet.")
            })
           }
           # hide tabs as we do not have Summary statistics or Details as of now
           hideTab(inputId = "additional_content", target="Summary statistics")
           hideTab(inputId = "additional_content", target="Details")
           # plot
           real_data$plot
        }
      })
    })

   # hide and show components
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

   # on startup, hide irrelevant components
   lapply(
      X = c("DC", "DE", "PC"),
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
   # Guide
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
