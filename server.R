# required libraries 
library(ggplot2)
library(data.table) # %like% 
library(readxl) # to read .xlsx/.xlsxm files
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
source("helper.R") # helpers 

################################################################################
# Export to CalR and Sable format
################################################################################
do_export <- function(format, input, output) {
   if (format == "CalR") {
      file = input$File1

      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file = input$File1
         real_data <- do_plotting(file$datapath, input, input$sick)
         h = hash()

         # Specific mapping of column names from TSE to CalR to produce a compatible .csv file
         h[["Datetime"]] <- "Date_Time"
         h[["VCO2(3)_[ml/h]"]] <- "RT_VCO2_3"
         h[["VO2(3)_[ml/h]"]] <- "RT_VO2_3"
         h[["HP"]] <- "RT_Kcal_hr_1" # caloric equivalent by 1st formula (HeatProduction formula 1)
         h[["HP2"]] <- "RT_Kcal_hr_2" # caloric equivalent by 2nd formula (HeatProduction formula 2)
         for (v in ls(h)) {
            names(real_data$data)[names(real_data$data) == v] = h[[v]]
         }

         fname = paste(paste(path_home(), input$export_folder$path[[2]], sep="/"), 
            input$export_file_name, sep="/")
         write.csv(real_data$data[values(h)], file=fname, row.names = FALSE)
         writeLines(gsub(pattern='"', replace="", x=readLines(fname)), con=fname)
      }
   }

   if (format == "Sable") {
      # TODO: Implement Sable system import
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
   cbPalette <- viridis(3, option = "cividis", begin = 0.1, end = 0.8, alpha = 1)
   cbPalette2 <- cbPalette[c(1,3)]
   theme_pubr_update <- theme_pubr(base_size = 8.5) +
   theme(legend.key.size = unit(0.3, "cm")) +
   theme(strip.background = element_blank()) +
   theme(strip.text = element_text(hjust = 0)) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
   theme_set(theme_pubr_update)

   # data read-in
   fileFormatTSE = FALSE
   finalC1 <- c()
   for (i in 1:input$nFiles) {
   file = input[[paste0("File", i)]]
   file = file$datapath
   con =  file(file)
   line = readLines(con, n = 2)
   if (i == 1) {
      fileFormatTSE = grepl("TSE", line[2])
   } else {
      if (grepl("TSE", line[2]) != fileFormatTSE) {
         return (list("plot"=NULL, "animals"=NULL, "status"=FALSE))
      }
   }
   #############################################################################
   # Detect data type (TSE or Sable)
   #############################################################################
   detectData = function(filename) {
      con = file(filename, "r")
      lineNo = 1
      while (TRUE) {
         lineNo = lineNo + 1
         line = readLines(con, n=1)
         print(length(line))
         if (length(line) == 0 || length(grep("^;+$", line) != 0) || line == "") {
            return(lineNo)
         }
      }
   }

   # Skip metadata 
   toSkip = detectData(file)
   # File encoding important, otherwise Shiny apps crashes due to undefined character entity
   C1 <- read.csv2(file, header = F, skip = toSkip+1, na.strings = c("-","NA"), fileEncoding="ISO-8859-1", sep=";")
   # TODO: C1meta obsolete when using metadata sheet
   C1meta <- read.csv2(file, header=T, skip=2, nrows=toSkip+1-4, na.strings = c("-", "NA"), fileEncoding="ISO-8859-1")

   # Curate data frame
   C1.head <- read.csv2(file, 
                        header = F,
                        skip = toSkip-1, # skip rows until the first header line indicating the variable
                        nrows = 2, # read only two rows (variable name + unit)
                        na.strings = c("","NA"), #transform missing units to NA
                        as.is = TRUE, #avoid transformation of character to vector
                        check.names = FALSE, #set the decimal separator of the csv file
                        fileEncoding = "ISO-8859-1")
   names(C1) <- paste(C1.head[1, ], C1.head[2, ], sep = "_")

   # unite data sets
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
   arrange(Datetime2)  %>% 
   filter(Datetime2 >= input$daterange[[1]] & # From 
            Datetime2 <= input$daterange[[2]]) %>% # To
   mutate(MeasPoint = row_number())
   C1 <- C1[!is.na(C1$MeasPoint),]

   # Step #1 - calculate the difference between consecutive dates
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # subtract the next value from the first value and safe as variable "diff.sec)
   mutate(diff.sec = Datetime2 - lag(Datetime2, default = first(Datetime2))) 
   C1$diff.sec <- as.numeric(C1$diff.sec) # change format from difftime to numeric

   # Step #2 - calculate the cumulative time difference between consecutive dates  
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # calculate the cumulative sum of the running time and save as variable "running_total.sec"
   mutate(running_total.sec = cumsum(diff.sec)) 

   # Step #3 - transfers time into hours (1 h = 3600 s)
   C1$running_total.hrs <- round(C1$running_total.sec / 3600, 1)

   # Step #4 - round hours downwards to get "full" hours
   C1$running_total.hrs.round <- floor(C1$running_total.hrs)
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # subtract the next value from the first value and safe as variable "diff.sec)
   mutate(diff.sec = Datetime2 - lag(Datetime2, default = first(Datetime2))) 
   C1$diff.sec <- as.numeric(C1$diff.sec) # change format from difftime to numeric

   # Step #6 - calculate the cumulative time difference between consecutive dates  
   C1 <- C1 %>%
   group_by(`Animal No._NA`) %>% # group by Animal ID
   arrange(Datetime2) %>% # sort by Datetime2
   # calculate the cumulative sum of the running time and save as variable "running_total.sec"
   mutate(running_total.sec = cumsum(diff.sec)) 

   # Step #7 - transfers time into hours (1 h = 3600 s)
   C1$running_total.hrs <- round(C1$running_total.sec / 3600, 1)

   # Step #8 - round hours downwards to get "full" hours
   C1$running_total.hrs.round <- floor(C1$running_total.hrs)

   # Step #9 - define 1/n-hours steps 
   # TODO: Check that this implementation is actually correct
   if (input$averaging == 10) { # 1/6 hours
   C1 <- C1 %>%
      mutate(timeintervalinmin = case_when(minutes <= 10 ~ 0,
                                    minutes <= 20 ~ (1/6),
                                    minutes <= 30 ~ (2/6),
                                    minutes <= 40 ~ (3/6),
                                    minutes <= 50 ~ (4/6),
                                    minutes > 50 ~ (5/6)))
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
   C1 <- C1 %>%
      mutate(timeintervalinmin = case_when(minutes <= 2 ~ 0))
   }

   # Step #10 - create a running total with half hour intervals by adding the thirty min to the full hours
   C1$running_total.hrs.halfhour <- C1$running_total.hrs.round + C1$timeintervalinmin
   f1 = input$variable1
   f2 = input$variable2

   #############################################################################
   # Heat production formula #1
   #############################################################################
   switch(f1,
   Lusk={
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
   Lusk={
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
      print(i)
         C1 <- C1 %>% 
            filter(`Animal No._NA` != as.numeric(i))
      }
   }
   finalC1 <- rbind(C1, finalC1)
   }
   # step 13 (debugging: save all cohort means)
   write.csv2(C1.mean.hours, file = paste0("all-cohorts_means.csv"))

   #############################################################################
   # Plotting
   #############################################################################
   plotType=input$plot_type
   write.csv2(C1, file="all_data.csv")
   write.csv2(finalC1, file="finalC1.csv")

   switch(plotType,
   CompareHeatProductionFormulas={
   p <- ggplot(data = finalC1, aes_string(x = "HP", y = "HP2")) +
   geom_point() +
   stat_smooth(method = "lm") + # adds regression line
   stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) # adds correlation coefficient
   },
   CaloricEquivalentOverTime={
   colors=as_factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals=colors

   convert <- function(x) {
   # print(x[[1]])
      splitted = strsplit(as.character(x), " ")
      #splitted = strsplit(as.character(data[1,"Datetime"]), ' ')
      paste(splitted[[1]][2], ":00", sep="")
   }

   ### TODO: Check the implementation below
   ### Note: This filters out first recordings on each day, probably not desired
   #finalC1$Datetime <- lapply(finalC1$Datetime, convert)
   #finalC1$TimeInHours <- hour(hms(finalC1$Datetime))*60+minute(hms(finalC1$Datetime))
   #finalC1 = filter(finalC1, TimeInHours > (60*as.numeric(input$exclusion)))
   #print(finalC1$TimeInHours)
   #p <- ggplot(data = finalC1, aes_string(x = C1$running_total.hrs.round, y = "HP2", color=finalC1$`Animal No._NA`, group=finalC1$`Animal No._NA`, color=finalC1$`Animal No._NA`)) + geom_point()

   if (input$running_average > 0) {
      p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = "HP2", color="Animals", group="Animals")) 
      # TODO: Add switch statement for running_average_method
      p <- p + geom_line(aes(y=rollmean(HP2, input$running_average, na.pad=TRUE)), group="Animals")
   } else {
   p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = "HP2", color="Animals", group="Animals")) 
   p <- p + geom_line()
   }
   p <- p + scale_fill_brewer(palette="Spectral")

   # p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = "CO_2[%]", color="Animals", group="Animals")) + 
   #  scale_fill_brewer(palette="Spectral") + geom_line() 

   if (input$wmeans) {
      p <- p + geom_smooth(method="lm") 
   }

   if (input$wstats) {
      p <- p + stat_cor(method="pearson")
   }

   p <- p + xlab("Time [h]")
   p <- p + ylab(paste("Caloric equivalent [", input$myp, "]"))
   # Note this excludes only at beginning of measurement experiment
   p <- p + scale_x_continuous(limits=c(input$exclusion, NA))

   p <- ggplotly(p)
   # p <- ggplotly(p) %>%layout(boxmode = "group") # %>% config(displayModeBar = FALSE)
   # p <- p %>% layout(dragmode = "pan") # %>% config(displayModeBar = FALSE)

   #  stat_smooth(method = "lm") + # adds regression line
   #  stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) # adds correlation coefficient
   },
   CoefficientOfVariation={
      print(rollmean(finalC1$HP2, input$running_average, na.pad=TRUE))
      df <- data.frame(Values=finalC1$HP2, Group=`$`(finalC1, "Animal No._NA"), Values2=finalC1$HP)
      df_new = partition(df)
      df_new = cv(df_new, input$window)
      df_new = reformat(df_new)

      convert <- function(x) {
        splitted = strsplit(as.character(x), " ")
        paste(splitted[[1]][2], ":00", sep="")
     }

      finalC1$Datetime <- lapply(finalC1$Datetime, convert)
      print("nrows ndew")
      print(nrow(df_new))
      print("finalc rows")
      print(nrow(finalC1))
      df_to_plot = cbind(df_new, rep(unique(`$`(finalC1, "running_total.hrs.halfhour")), length(unique(`$`(finalC1, 'Animal No._NA')))))
      # finalC1 = finalC1 %>% arrange(desc(`Animal No._NA`))
      # df_to_plot = cbind(df_new, `$`(finalC1, "running_total.hrs.halfhour"))

      df_to_plot$Group = as_factor(df_to_plot$Group)
      write.csv2(df_new, file="df_new.csv")
      colnames(df_to_plot) = c("CoefficientOfVariation", "Animal", "Time")
      print("to plot:")
      print(df_to_plot)
      write.csv2(df_to_plot, file="df_to_plot.csv")

       p <- ggplot(df_to_plot, aes(x=Time, y=CoefficientOfVariation, color=Animal)) + geom_line()
       p <- p + xlab("Time [h]")
       p <- p + ylab("Coefficient of variation")
       p <- ggplotly(p) 
   },
   DayNightActivity={

   convert <- function(x) {
      splitted = strsplit(as.character(x), " ")
      paste(splitted[[1]][2], ":00", sep="")
   }

   finalC1$Datetime <- lapply(finalC1$Datetime, convert)
   finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime))*60+minute(hms(finalC1$Datetime)) < 720, 'am', 'pm')
   finalC1$Animals = as_factor(`$`(finalC1, "Animal No._NA"))
   p <- ggplot(finalC1, aes(x=Animals, y=HP, fill=NightDay)) + geom_boxplot()
   p <- ggplotly(p) %>%layout(boxmode = "group") # %>% config(displayModeBar = FALSE)

   },
   StackedBarPlotForRMRandNonRMR={
      # TODO: Implement stacked bar plot for RMR and non-RMR
   },
   ANCOVA={
   # fall back covariate (TODO: can be used from xlsx sheet not from csv input measurement file)
   my_covariate = "Weight..g."
   for (c in colnames(C1meta)) {
      if (c %like% input$covariates) {
         my_covariate = "Weight..g."
         break
      }
   }

   # metadata <- data.frame(`$`(C1meta, foo), `$`(C1meta, "Animal.No."))
   # metadata <- data.frame(C1meta[my_covariate], `$`(C1meta, "Animal.No."))
   # names(metadata) <- c("Weight", "Animal No._NA")
   # print(metadata)
   finalC1[, "Weight"] <- NA
   finalC1[, "Gender"] <- NA

   # The metadata from the excel spreadsheet
   my_data <- read_excel(input$metadatafile$datapath)

   df <- tbl_df(my_data) # use as_tibble (tbl_df deprecated)
   # TODO: based on covariate extract a different line, weight is in line 26
   # lean in 27 and fat in 28 line...
   a = df[21,] %>% slice(1) %>% unlist(., use.names=FALSE)
   c = df[23,] %>% slice(1) %>% unlist(., use.names=FALSE)
   b = df[26,] %>% slice(1) %>% unlist(., use.names=FALSE)
   metadata <- data.frame(a, b, c)
   metadata <- na.omit(metadata)
   names(metadata) <- c("Weight", "Animal No._NA", "Gender")
   metadata = metadata[seq(2, nrow(metadata)),]
   print(metadata$Weight)

   #print(by(finalC1, seq_len(nrow(finalC1)), function(row) row["Weight"] = 10))
   #by(finalC1, seq_len(nrow(finalC1)), function(row) row["Weight"] = which(`$`(metadata, "Animal No._NA") == row["Animal No._NA"] %>% pull("Animal No._NA")))
   # TODO: Make for loop more efficient somehow
   for (i in 1:nrow(finalC1)) 
   {
      js = which(`$`(metadata, "Animal No._NA") == finalC1[i, "Animal No._NA"] %>% pull("Animal No._NA"))
      if(length(js) > 0) {
         w = metadata[js, "Weight"]
         w2 = metadata[js, "Gender"]
         finalC1[i, "Weight"] = as.numeric(w) # as.numeric()
         finalC1[i, "Gender"] = w2
      }
   }
      
   print("before filter")
   # filter df by indexing
   if (length(input$checkboxgroup_gender) == 0) {
      finalC1 <- NA
   } else {
      if (! length(input$checkboxgroup_gender) == 2) {
          print(input$checkboxgroup_gender)
          finalC1 = finalC1[finalC1$Gender == input$checkboxgroup_gender,]
          print(finalC1)
    }
   }
   print("after filter")
   

   names(finalC1)[names(finalC1) == "Animal No._NA"] <- "Animal"
   finalC1$Animal <- as.factor(finalC1$Animal)

   write.csv2(finalC1, file="finalC1.csv")
   finalC1 <- drop_na(finalC1)
   finalC1 <- aggregate(finalC1, by=list(AnimalGroup=finalC1$Animal), FUN=mean)
   print(finalC1)
   print(names(finalC1))

   p <- ggscatter(finalC1, x="Weight", y="HP", add = "reg.line", add.params = list(color="blue", fill="lightgray"), conf.int = TRUE)
   p <- p + xlab("Weight [g]") + ylab("Mean heat production")
   p <- ggplotly(p)
   # TOOD: add summary statistics to plot
   print(summary(lm(HP ~ Weight, finalC1)))
   message <- NULL
   if (sum(is.na(finalC1)) > 0) { 
      message <- paste("# ", sum(is.na(finalC1)), " entries do not have metadata attached, Check metadata file.")
   }
   return (list("plot"=p, status=message, metadata=metadata))
   },
   RAW={
   write.csv2(finalC1, file="finalC1.csv")
   colors=as_factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals=colors
   mylabel=paste0(input$myr, sep="", "_[%]")
   names(finalC1)[names(finalC1) == mylabel] <- input$myr
   names(finalC1)[names(finalC1) == 'RER_NA'] <- 'RER'
   p <- ggplot(data = finalC1, aes_string(y = input$myr, x="running_total.hrs.halfhour", color="Animals", group="Animals")) + geom_line()
   # p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = `$`(finalC1, mylabbel), color="Animals", group="Animals")) 
   p <- ggplotly(p)
   },
   TotalOverDay={
   colors=as_factor(`$`(finalC1, "Animal No._NA"))
   finalC1$Animals=colors

   convert <- function(x) {
      splitted = strsplit(as.character(x), " ")
      paste(splitted[[1]][1], "",sep="")
   }

   finalC1$Datetime <- day(dmy(lapply(finalC1$Datetime, convert)))

   TEE1=aggregate(finalC1$HP, by=list(Animals=finalC1$Animals, Days=finalC1$Datetime), FUN=sum)
   TEE2=aggregate(finalC1$HP2, by=list(Animals=finalC1$Animals, Days=finalC1$Datetime), FUN=sum)
   TEE=rbind(TEE1, TEE2)
   names(TEE)[names(TEE) == 'x'] <- 'TEE'
   TEE$Equation = as_factor(c(rep(input$variable1, nrow(TEE1)), rep(input$variable2, nrow(TEE2))))
   TEE$Days = as_factor(TEE$Days)
   TEE$Animals = as_factor(TEE$Animals)
   # p <- ggplot(data=TEE, aes(x=Equation, y=TEE, fill=Animals)) + geom_boxplot() # geom_point(aes(fill=Animals), size = 1, shape = 21, position=position_jitterdodge())
   p <- ggplot(data=TEE, aes(x=Animals, y=TEE, fill=Equation)) + geom_boxplot() + geom_point(position=position_jitterdodge()) # geom_point(aes(fill=Animals), size = 1, shape = 21, position=position_jitterdodge())
   p <- ggplotly(p) %>%layout(boxmode = "group") # %>% config(displayModeBar = FALSE)
   },
   {
   }
   )
   list("plot"=p, "animals"=`$`(C1, "Animal No._NA"), "data"=finalC1,"metadata"=C1meta)
}

################################################################################
# Create server 
################################################################################
server <- function(input, output, session) {
   shinyDirChoose(input, 'export_folder', roots=c(wd=path_home()), filetypes=c('', 'txt'))

   output$metadatafile=renderUI({
      html_ui = " "
      html_ui <- paste0(html_ui, fileInput("metadatafile", label="Metadata file"))
      HTML(html_ui)
   })

   # Dynamically create fileInput fields by the number of requested files of the user
   output$fileInputs=renderUI({
      html_ui = " "
      for (i in 1:input$nFiles) {
         html_ui <- paste0(html_ui, fileInput(paste0("File", i), label=paste0("Cohort ",i)))
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
         Weir={
            text1 = "$$ \\tag{1} 16.3 \\times VO2[\\frac{ml}{h}] + 4.57 \\times RER $$"
         },
         HP={
            text1 = "$$ \\tag{1} VO2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         HP2={
            text1 = "$$ \\tag{1} (4.44 + 1.43 \\times RER) + VO2[\\frac{ml}{h}] $$"
         },
         Lusk={
            text1 = "$$ \\tag{1} 15.79 \\times VO2[\\frac{ml}{h}] + 5.09 \\times RER $$"
         },
         Elia={
            text1 = "$$ \\tag{1} 15.8 \\times VO2[\\frac{ml}{h}] + 5.18 \\times RER $$"
         },
         Brower={
            text1 = "$$ \\tag{1} 16.07 \\times VO2[\\frac{ml}{h}]+ 4.69 \\times RER $$"
         },
         Ferrannini={
            text1 = "$$ \\tag{1} 16.37117 \\times VO2[\\frac{ml}{h}] + 4.6057 \\times RER $$"
         },
         {
         }
         )

         switch(input$variable2,
         Weir={
            text2 = "$$ \\tag{2} 16.3 \\times VO2[\\frac{ml}{h}] + 4.57 \\times RER $$"
         },
         HP={
            text2 = "$$ \\tag{2} VO2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"
         },
         HP2={
            text2 = "$$ \\tag{2} (4.44 + 1.43 \\times RER) + VO2[\\frac{ml}{h}] $$"
         },
         Lusk={
            text2 = "$$ \\tag{2} 15.79 \\times VO2[\\frac{ml}{h}] + 5.09 \\times RER $$"
         },
         Elia={
            text2 = "$$ \\tag{2} 15.8 \\times VO2[\\frac{ml}{h}] + 5.18 \\times RER $$"
         },
         Brower={
            text2 = "$$ \\tag{2} 16.07 \\times VO2[\\frac{ml}{h}] + 4.69 \\times RER $$"
         },
         Ferrannini={
            text2 = "$$ \\tag{2} 16.37117 \\times VO2[\\frac{ml}{h}] + 4.6057 \\times RER $$"
         },
         {
         }
         )

         output$heat_production_equations = renderUI(
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
            output$myp = renderUI(
               selectInput(inputId="myp", label="Chose prefered method for calculating caloric equivalent over time", selected=input$variable1, choices=c(input$variable1, input$variable2)))
         })

   observeEvent(input$plot_type, {
            output$checkboxgroup_gender = renderUI(
               checkboxGroupInput(inputId="checkboxgroup_gender", label="Chose gender", choices=list("male"="male", "female"="female"), selected=c("malw","female")))
         })

   observeEvent(input$plot_type, {
      output$myr = renderUI(
         selectInput(inputId="myr", label="Chose raw data to plot", choices=c("O2", "CO2", "RER")))
    })

   observeEvent(input$plot_type, {
            output$wmeans = renderUI(
               checkboxInput(inputId="wmeans", label="Display means"))
         })

   observeEvent(input$plot_type, {
            output$wstats = renderUI(
               checkboxInput(inputId="wstats", label="Display statistics"))
         })

   observeEvent(input$plot_type, {
      output$covariates = renderUI(
            selectInput(inputId="covariates", label="Chose a covariate", selected="Weight", choices=c("Weight", "Lean mass", "Fat mass")))
   })


   #############################################################################
   # Observe export events
   #############################################################################
   observeEvent(input$export_folder, {
       output$folder_name_export = renderUI(
            renderText(paste(path_home(), input$export_folder$path[[2]], sep="/")))
      })

   observeEvent(input$export, {
       if (input$export_format == "CalR") {
            status_okay <- do_export("CalR", input, output)
            if (!status_okay) {
              output$message <- renderText("Error during data export, check logs")
            } else {
              output$message <- renderText(paste("Consolidated data exported to format >>", input$export_format, "<<", sep=" "))
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
           file = input$File1
           real_data <- do_plotting(file$datapath, input, exclusion=input$sick)
           real_data$plot
           })
   })

   #############################################################################
   # Observe validate plotting result
   #############################################################################
   observeEvent(input$plottingvalidation, {
         output$plotvalidation <- renderPlotly({
            ref <- read.csv2(input$rerself$name, na.strings = c("-","NA"), header=FALSE, sep=";")
            calr <- read.csv2(input$rercalr$name, na.strings = c("-","NA"), header=FALSE, sep=";")
            df_comparison <- data.frame(ref=as.numeric(gsub(",", ".", gsub("\\.", "", ref$V11))), calr=as.numeric(gsub(",", ".", gsub("\\.", "", calr$V11))))
            p <- ggscatter(df_comparison, x="ref", y="calr", add = "reg.line", add.params = list(color="black", fill="lightgray")) +
            stat_cor(method="pearson") +
            theme(axis.text.x = element_text(angle=90)) + 
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
            print("No cohort data given!");
            output$message <- renderText("Not any cohort data given")
         } else {
           file = input$File1
           print(file$datapath)
           real_data <- do_plotting(file$datapath, input, input$sick)
           
           print(real_data$status)
           if (! is.null(real_data$status)) {
               if (real_data$status == FALSE) {
                  output$message <- renderText("Input data incompatible, make sure you either supply only TSE or Sable system files not a combination of both file types.")
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
              output$sick = renderUI(
              multiInput(inputId="sick", label="Remove outliers (sick animals, etc.) ", selected="", choices=unique(real_data$animals)))
           }
            
           real_data$plot
        }
      })
    })

   #############################################################################
   # Reset session
   #############################################################################
   observeEvent(input$reset, {
      session$reload()
   })
}
