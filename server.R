# Libraries
library(shiny)
library(tidyr)
library(ggplot2)
library(data.table) # for filtering with %like%
library(readxl)
library(plotly)
library(zoo) # running average methods
library(ggpubr)
library(viridis)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(fs)
library(hash)
require(tidyverse)

# TODO: Cleanup file names and code in included source files
source("helper.R") # helpers
source("new_feature.R") # new feature
source("new_feature2.R") # new feature2

################################################################################
# Helper functions
################################################################################
convert <- function(x) {
    splitted <- strsplit(as.character(x), " ")
    paste(splitted[[1]][2], ":00", sep = "")
}

################################################################################
# Export to CalR and Sable format
################################################################################
do_export2 <- function(format, input, output, file_output) {
      # exports for now only the first data set to CalR data format
      # TODO: Use combined data set (all_data.csv), reformat for xlsx/Sable
      file <- input$File1
      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file <- input$File1
         real_data <- do_plotting(file$datapath, input, input$sick)
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
         write.csv(real_data$data[values(h)], file = file_output, row.names = FALSE)
         writeLines(gsub(pattern = '"', replace = "", x = readLines(file_output)), con = file_output)
      }
   }

do_export <- function(format, input, output) {
   if (format == "CalR") {
      file <- input$File1

      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file <- input$File1
         real_data <- do_plotting(file$datapath, input, input$sick)
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

   if (format == "Sable") {
      # TODO: Implement Sable system data import
      output$message <- renderText("Sable system export not yet implemented!")
      FALSE
   }
   FALSE
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

   # data read-in
   fileFormatTSE <- FALSE
   finalC1 <- c()
   for (i in 1:input$nFiles) {
      file <- input[[paste0("File", i)]]
      file <- file$datapath
      con <- file(file)
      line <- readLines(con, n = 2)
   if (i == 1) {
      fileFormatTSE <- grepl("TSE", line[2])
   } else {
      if (grepl("TSE", line[2]) != fileFormatTSE) {
         return(list("plot" = NULL, "animals" = NULL, "status" = FALSE))
      }
      # TODO: need to support also Jennifers and Tabeas dataformat
   }
   #############################################################################
   # Detect data type (TSE or Sable)
   #############################################################################
   detectData <- function(filename) {
      con <- file(filename, "r")
      lineNo <- 1
      while (TRUE) {
         lineNo <- lineNo + 1
         line <- readLines(con, n = 1)
         print(length(line))
         if (length(line) == 0 || length(grep("^;+$", line) != 0) ||
          line == "") {
            return(lineNo)
         }
      }
   }

   # Skip metadata before data
   toSkip <- detectData(file)
   # File encoding matters: Shiny apps crashes due to undefined character entity
   C1 <- read.csv2(file, header = FALSE, skip = toSkip + 1,
      na.strings = c("-", "NA"), fileEncoding = "ISO-8859-1", sep = ";")
   # TODO: C1meta obsolete when using metadata sheet, implement/use the sheet
   C1meta <- read.csv2(file, header = TRUE, skip = 2, nrows = toSkip + 1 - 4,
      na.strings = c("-", "NA"), fileEncoding = "ISO-8859-1")
   # Curate data frame
   C1.head <- read.csv2(file, # input file
                        header = FALSE, # no header
                        skip = toSkip - 1, # skip headers up to first animal
                        nrows = 2, # read only two rows (variable name + unit)
                        na.strings = c("", "NA"), #transform missing units to NA
                        as.is = TRUE, # avoid transformation character->vector
                        check.names = FALSE, # set the decimal separator
                        fileEncoding = "ISO-8859-1")
   names(C1) <- paste(C1.head[1, ], C1.head[2, ], sep = "_")

   # unite data sets (unite in tidyverse package)
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
      C1$HP <- 15.79 * C1$`VO2(3)_[ml/h]` + 5.09 * C1$RER_NA
   },
   HP = {
      C1$HP <- C1$`VO2(3)_[ml/h]` * (6 * C1$RER_NA + 15.3) * 0.278
   },
   HP2 = {
      C1$HP <- (4.44 + 1.43 * C1$RER_NA) * C1$`VO2(3)_[ml/h]`
   },
   Weir = {
      C1$HP <- 16.3 * C1$`VO2(3)_[ml/h]` + 4.57 * C1$RER_NA
   },
   Elia = {
      C1$HP <- 15.8 * C1$`VO2(3)_[ml/h]` + 5.18 * C1$RER_NA
   },
   Brower = {
      C1$HP <- 16.07 * C1$`VO2(3)_[ml/h]` + 4.69 * C1$RER_NA
   },
   Ferrannini = {
      C1$HP <- 16.37117 * C1$`VO2(3)_[ml/h]` + 4.6057 * C1$RER_NA
   },
   {

   }
   )

   #############################################################################
   # Heat production formula #2
   #############################################################################
   switch(f2,
   Lusk = {
      C1$HP2 <- 15.79 * C1$`VO2(3)_[ml/h]` + 5.09 * C1$RER_NA
   },
   HP = {
      C1$HP2 <- C1$`VO2(3)_[ml/h]` * (6 * C1$RER_NA + 15.3) * 0.278
   },
   HP2 = {
      C1$HP2 <- (4.44 + 1.43 * C1$RER_NA) * C1$`VO2(3)_[ml/h]`
   },
   Weir = {
      C1$HP2 <- 16.3 * C1$`VO2(3)_[ml/h]` + 4.57 * C1$RER_NA
   },
   Elia = {
      C1$HP2 <- 15.8 * C1$`VO2(3)_[ml/h]` + 5.18 * C1$RER_NA
   },
   Brower = {
      C1$HP2 <- 16.07 * C1$`VO2(3)_[ml/h]` + 4.69 * C1$RER_NA
   },
   Ferrannini = {
      C1$HP2 <- 16.37117 * C1$`VO2(3)_[ml/h]` + 4.6057 * C1$RER_NA
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
   }
   # step 13 (debugging: save all cohort means)
   write.csv2(C1.mean.hours, file = paste0("all-cohorts_means.csv"))

   #############################################################################
   # Plotting
   #############################################################################
   plotType <- input$plot_type
   write.csv2(C1, file = "all_data.csv")
   write.csv2(finalC1, file = "finalC1.csv")

   # rescale to kj or kcal
   if (input$kj_or_kcal == "kj") {
      finalC1$HP <- finalC1$HP * 4184 # kcal to kj
      finalC1$HP2 <- finalC1$HP2 * 4184 # kcal to kj
   }

   switch(plotType,
   CompareHeatProductionFormulas = {

   p <- ggplot(data = finalC1, aes_string(x = "HP", y = "HP2")) +
   geom_point() +
   stat_smooth(method = "lm") + # add regression line and pearson product moment correlation
   stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
   },
   CaloricEquivalentOverTime = {
   colors <- as_factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals <- colors

   # get metadata from TSE header (should in fact use C1meta metadata from metadata sheet)
   df_filtered <- C1meta[, colSums(is.na(C1meta)) == 0]
   df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
   df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
   colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
   finalC1 <- merge(finalC1, df_filtered, by = "Box_NA")

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
      p <- p + geom_smooth(method = "lm")
   }

   if (input$wstats) {
      p <- p + stat_cor(method = "pearson")
   }

   p <- p + xlab("Time [h]")
   p <- p + ylab(paste("Caloric equivalent [", input$myp, "]"))
   # Note this excludes only at beginning of measurement experiment currently in the visualization
   p <- p + scale_x_continuous(limits = c(input$exclusion, NA))

   if (input$with_facets) {
      if (!is.null(input$facets_by_data_one)) {
         if (input$orientation == "Horizontal") {
            p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)))
         } else {
            p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")))
         }
      }
   }
   p <- ggplotly(p)
   },
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
      df_to_plot$Group <- as_factor(df_to_plot$Group)
      df_to_plot2$Group <- as_factor(df_to_plot$Group)

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
      df_plot_total$HP <- as.numeric(df_plot_total$HP)
      df_plot_total$Time <- as.numeric(df_plot_total$Time)
      p <- ggplot(data = df_plot_total, aes(x = Time, y = HP, group = Component,
      color = Component)) + geom_line() + facet_wrap(~Animal)
      finalC1 <- df_plot_total
   },
   DayNightActivity = {

   convert <- function(x) {
      splitted <- strsplit(as.character(x), " ")
      paste(splitted[[1]][2], ":00", sep = "")
   }

   finalC1$Datetime <- lapply(finalC1$Datetime, convert)
   finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime)) * 60 + minute(hms(finalC1$Datetime)) < 720, "am", "pm")
   finalC1$Animals <- as_factor(`$`(finalC1, "Animal No._NA"))
   p <- ggplot(finalC1, aes(x = Animals, y = HP, fill = NightDay)) + geom_boxplot()
   p <- ggplotly(p) %>% layout(boxmode = "group")
   },
   StackedBarPlotForRMRandNonRMR = {
      # TODO: Implement stacked bar plot for RMR and non-RMR
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
   print(metadata$Weight)

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
   p <- ggplotly(p)
   # TOOD: add summary statistics to plot
   print(summary(lm(HP ~ Weight, finalC1)))
   message <- NULL
   if (sum(is.na(finalC1)) > 0) {
      message <- paste("# ", sum(is.na(finalC1)),
      " entries do not have metadata attached, Check metadata file.")
   }
   return(list("plot" = p, status = message, metadata = metadata))
   },
   Raw = {
   write.csv2(finalC1, file = "finalC1.csv")
   colors <- as_factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals <- colors
   mylabel <- paste0(input$myr, sep = "", "_[%]")
   names(finalC1)[names(finalC1) == mylabel] <- input$myr
   names(finalC1)[names(finalC1) == "RER_NA"] <- "RER"
   p <- ggplot(data = finalC1, aes_string(y = input$myr, x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
   p <- ggplotly(p)
   },
   TotalOverDay = {
   colors <- as_factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals <- colors

   convert <- function(x) {
      splitted <- strsplit(as.character(x), " ")
      paste(splitted[[1]][1], "", sep = "")
   }

   finalC1$Datetime <- day(dmy(lapply(finalC1$Datetime, convert)))
   TEE1 <- aggregate(finalC1$HP, by = list(Animals = finalC1$Animals, Days = finalC1$Datetime), FUN = sum)
   TEE2 <- aggregate(finalC1$HP2, by = list(Animals = finalC1$Animals, Days = finalC1$Datetime), FUN = sum)
   TEE <- rbind(TEE1, TEE2)
   names(TEE)[names(TEE) == "x"] <- "TEE"
   TEE$Equation <- as_factor(c(rep(input$variable1, nrow(TEE1)), rep(input$variable2, nrow(TEE2))))
   TEE$Days <- as_factor(TEE$Days)
   TEE$Animals <- as_factor(TEE$Animals)
   p <- ggplot(data = TEE, aes(x = Animals, y = TEE, fill = Equation)) + geom_boxplot() + geom_point(position = position_jitterdodge())
   p <- ggplotly(p) %>% layout(boxmode = "group")
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
         paste("data-", Sys.Date(), input$export_format, sep = "")
      }
      },
         content = function(file) {
            status_okay <- do_export2("CalR", input, output, file)
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
         selectInput(inputId = "myr", label = "Chose raw data to plot", choices = c("O2", "CO2", "RER")))
    })

   observeEvent(input$plot_type, {
            output$wmeans <- renderUI(
               checkboxInput(inputId = "wmeans", label = "Display means"))
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

   observeEvent(input$export, {
       if (input$export_format == "CalR") {
            status_okay <- do_export("CalR", input, output)
            if (!status_okay) {
              output$message <- renderText("Error during data export, check logs")
            } else {
              output$message <- renderText(paste("Consolidated data exported to format >>",
              input$export_format, "<<", sep = " "))
            }
       }
       if (input$export_format == "Sable") {
           output$message <- renderText("Sable system export not yet implemented!")
       }
       if (input$export_format == "XLSX") {
           output$message <- renderText("XLSX not yet implemented!")
        }
      })

   #############################################################################
   # Refresh plot (action button's action)
   #############################################################################
   observeEvent(input$replotting, {
           output$plot <- renderPlotly({
           file <- input$File1
           real_data <- do_plotting(file$datapath, input, exclusion = input$sick)
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
            print("No cohort data given!")
            output$message <- renderText("Not any cohort data given")
         } else {
           file <- input$File1
           print(file$datapath)
           real_data <- do_plotting(file$datapath, input, input$sick)

           print(real_data$status)
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

        if ((! is.null(real_data$animals)) && is.null(input$select_data_by)) {
            metadata <- real_data$metadata
            my_var <- input$condition_type
            diets <- unique(metadata[[my_var]])
            output$select_data_by <- renderUI(
               selectInput("select_data_by", "Condition", choices = diets)
            )
           }

         if (input$plot_type == "RestingMetabolicRate") {
            # plot
            df_filtered <- real_data$data %>% group_by(Animal, Component) %>% summarize(Value = min(HP), cgroups = c(Animal, Component))
            p <- ggplot(df_filtered, aes(factor(Animal), Value, fill = Component))
            p <- p + geom_bar(stat = "identity", position = "dodge") + scale_fill_brewer(palette = "Set1")
            p <- p + xlab("Animal") + ylab("RMR (min) [kcal/day]")

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
           } else if (input$plot_type == "CompareHeatProductionFormulas") {
            output$explanation <- renderUI({
            str1 <- "<h3> Comparison of heat production formulas </h3>"
            str2 <- "Two heat production formulas can be compared via a simple scatter plot and plotting into the plot a regression line (Pearson-Product-Moment correlation coefficient r)" #nolint
            str3 <- "p-value reported, HP and HP2 correspond to the formulas displayed in the sidebar on the left"
            str4 <- "<hr/>"
            str5 <- "When heat production formulas agree mostly, so there should visually not be too many large residuals from a line of slope 1 be apparent in the plot." #nolint
            HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
            })
           } else if (input$plot_type == "CaloricEquivalentOverTime") {
             output$explanation <- renderUI({
            str1 <- "<h3> Caloric Equivalent / heat production over time </h3>"
            str2 <- "According to a heat production formula the energy expenditure is calculated from indirect calorimetry data"
            str3 <- "<hr/>"
            str4 <- "Cohorts are usually stratified by animal ID by default"
            HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
            })
           } else if (input$plot_type == "DayNightActivity") {
              output$explanation <- renderUI({
            str1 <- "<h3> Day and night (average) energy expenditure of animals in cohorts </h3>"
            str2 <- "According to a heat production formula the energy expenditure is calculated from indirect calorimetry data"
            str3 <- "<hr/>"
            str4 <- "Cohorts are usually stratified by animal ID and day night activity by default"
            HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
            })
           } else if (input$plot_type == "Raw") {
            output$explanation <- renderUI({
               str1 <- "<h3> Raw data values are plotted </h3>"
               str2 <- "According to the recorded data, line graphs are displayed"
               str3 <- "<hr/>"
               str4 <- "Cohorts are usually strafified by animal ID by default"
            HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
            })
           } else if (input$plot_type == "TotalOverDay") {
            output$explanation <- renderUI({
               str1 <- "<h3> Total energy expenditure (TEE) for animal per day is displayed </h3>"
               str2 <- "Depending on the heat production formulas chosen (HP and HP2)"
               str3 <- "<hr/>"
               str4 <- "Usually there is no large discrepancy between TEEs calculated from different heat production formulas"
            HTML(paste(str1, sep = "<br/>"))
            })
           } else {
            output$summary <- renderPlotly(NULL)
            output$explanation <- renderUI({
               HTML("No information available yet.")
            })
           }
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
}
