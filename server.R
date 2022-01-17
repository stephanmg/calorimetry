# libraries
library(ggplot2)
library(ggpubr)
library(viridis)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(fs)
library(hash)
require(tidyverse)

do_export <- function(format, input, output) {
   if (format == "CalR") {
      file = input$File1

      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given")
      } else {
         file = input$File1
         real_data <- do_plotting(file$datapath, input, input$sick)
         h = hash()
         # Specific mapping of colum names from TSE to CalR compatible .csv file
         h[["Datetime"]] <- "Date_Time"
         h[["VCO2(3)_[ml/h]"]] <- "RT_VCO2_3"
         h[["VO2(3)_[ml/h]"]] <- "RT_VO2_3"
         h[["HP"]] <- "RT_Kcal_hr_1" # caloric equivalent by 1st formula
         h[["HP2"]] <- "RT_Kcal_hr_2" # caloric equivalent by 2nd formula
         for (v in ls(h)) {
            names(real_data$data)[names(real_data$data) == v] = h[[v]]
         }

         fname = paste(paste(path_home(), input$export_folder$path[[2]], sep="/"), input$export_file_name, sep="/")
         write.csv(real_data$data[values(h)], file=fname, row.names = FALSE)
         writeLines(gsub(pattern='"', replace="", x=readLines(fname)), con=fname)
      }
   }

   if (format == "Sable") {
      output$message <- renderText("Sable system export not yet implemented!")
      FALSE
   }
   FALSE
}

do_plotting <- function(file, input, exclusion) {

cbPalette <- viridis(3, option = "cividis", begin = 0.1, end = 0.8, alpha = 1)
cbPalette2 <- cbPalette[c(1,3)]

theme_pubr_update <- theme_pubr(base_size = 8.5) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_text(hjust = 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
theme_set(theme_pubr_update)


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


C1.raw <- read.csv2(file, na.strings = c("-","NA"))
C1.raw[1:12,1:6]
C1 <- read.csv2(file, header = F, skip = 10, na.strings = c("-","NA"))
C1.head <- read.csv2(file, 
                     header = F,
                     skip = 8, #skip rows until the first header line indicating the variable
                     nrows = 2, #read only two rows (variable name + unit)
                     na.strings = c("","NA"), #transform missing units to NA
                     as.is = TRUE, #avoid transformation of character to vector
                     check.names = FALSE) #set the decimal separator of the csv file
names(C1) <- paste(C1.head[1, ], C1.head[2, ], sep = "_")
head(C1)
print(head(C1))
C1 <- C1 %>%
    mutate(Genotype = case_when(`Animal No._NA` == 2262 ~ 'wt',
                                `Animal No._NA` == 2263 ~ 'ko',
                                `Animal No._NA` == 2265 ~ 'ko',
                                `Animal No._NA` == 2195 ~ 'wt'))

C1 <- C1 %>%
  unite(Datetime, # name of the final column
        c(Date_NA, Time_NA), # columns to be combined
        sep = " ") # separator set to blank

C1$Datetime <- gsub(".", "/", C1$Datetime, fixed = TRUE) #substitute "." by "/"

C1$Datetime2 <- as.POSIXct(C1$Datetime, 
                                 format = "%d/%m/%Y %H:%M") #transform into time format
C1$hour <- hour(C1$Datetime2)
C1$minutes <- minute(C1$Datetime2)
C1 <- C1 %>% 
  group_by(`Animal No._NA`) %>% 
  arrange(Datetime2)  %>% 
  filter(Datetime2 >= input$daterange[[1]] & # 2020-05-08 12:02:00
         Datetime2 <= input$daterange[[2]]) %>% # 2020-05-19 11:32:00
  mutate(MeasPoint = row_number())

C1 <- C1[!is.na(C1$MeasPoint),]

# Step #1 - calculate the difference between consecutive dates
C1 <- C1 %>%
  group_by(`Animal No._NA`) %>% # group by Animal ID
  arrange(Datetime2) %>% # sort by Datetime2
  mutate(diff.sec = Datetime2 - lag(Datetime2, default = first(Datetime2))) # subtract the next value from the first value and safe as variable "diff.sec)

C1$diff.sec <- as.numeric(C1$diff.sec) # change format from difftime to numeric

# Step #2 - calculate the cumulative time difference between consecutive dates  
C1 <- C1 %>%
  group_by(`Animal No._NA`) %>% # group by Animal ID
  arrange(Datetime2) %>% # sort by Datetime2
  mutate(running_total.sec = cumsum(diff.sec)) # calculate the cumulative sum of the running time and save as variable "running_total.sec"

# Step #3 - transfers time into hours (1 h = 3600 s)
C1$running_total.hrs <- round(C1$running_total.sec / 3600, 1)

# Step #4 - round hours downwards to get "full" hours
C1$running_total.hrs.round <- floor(C1$running_total.hrs)

C1 <- C1 %>%
  group_by(`Animal No._NA`) %>% # group by Animal ID
  arrange(Datetime2) %>% # sort by Datetime2
  mutate(diff.sec = Datetime2 - lag(Datetime2, default = first(Datetime2))) # subtract the next value from the first value and safe as variable "diff.sec)

C1$diff.sec <- as.numeric(C1$diff.sec) # change format from difftime to numeric

# Step #2 - calculate the cumulative time difference between consecutive dates  
C1 <- C1 %>%
  group_by(`Animal No._NA`) %>% # group by Animal ID
  arrange(Datetime2) %>% # sort by Datetime2
  mutate(running_total.sec = cumsum(diff.sec)) # calculate the cumulative sum of the running time and save as variable "running_total.sec"

# Step #3 - transfers time into hours (1 h = 3600 s)
C1$running_total.hrs <- round(C1$running_total.sec / 3600, 1)

# Step #4 - round hours downwards to get "full" hours
C1$running_total.hrs.round <- floor(C1$running_total.hrs)

# Step #1 - define 1/n-hours steps 
# TODO: Check if this is really correct ...
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
} else { # no averaging
#C1 <- C1 %>%
#    mutate(timeintervalinmin = case_when(minutes <= 60 ~ 0))
}

# Step #2 - create a running total with half hour intervals by adding the thirty min to the full hours
C1$running_total.hrs.halfhour <- C1$running_total.hrs.round + C1$timeintervalinmin

f1 = input$variable1
f2 = input$variable2

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



C1.mean.hours <- do.call(data.frame, aggregate(list(HP2 = C1$HP2, # calculate mean of HP2
                                    VO2 = C1$`VO2(3)_[ml/h]`, # calculate mean of VO2
                                    VCO2 = C1$`VCO2(3)_[ml/h]`, # calculate mean of VCO2
                                    RER = C1$RER_NA), # calculate mean of RER
                   by = list(Genotype = C1$Genotype,
                             Animal = C1$`Animal No._NA`, # groups by Animal ID
                             Time = C1$running_total.hrs.round), # groups by total rounded running hour
                      FUN = function(x) c(mean = mean(x), sd = sd(x)))) # calculates mean and standard deviation

write.csv2(C1.mean.hours, file = paste0(tools::file_path_sans_ext(file), "-cohort_means.csv"))

if (! is.null(exclusion)) {
   for (i in exclusion) {
     print(i)
      C1 <- C1 %>% 
         filter(`Animal No._NA` != as.numeric(i))
   }
}

   finalC1 <- rbind(C1, finalC1)
}

write.csv2(C1.mean.hours, file = paste0("all-cohorts_means.csv"))


plotType=input$plot_type
print(plotType)

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
#p <- ggplot(data = finalC1, aes_string(x = C1$running_total.hrs.round, y = "HP2", color=finalC1$`Animal No._NA`, group=finalC1$`Animal No._NA`)) +
p <- ggplot(data = finalC1, aes_string(x = "running_total.hrs.halfhour", y = input$myp, color="Animals", group="Animals")) + 
  geom_point() + scale_fill_brewer(palette="Spectral") 

if (input$wmeans) {
   p <- p + geom_smooth(method="lm") 
}

if (input$wstats) {
   p <- p + stat_cor(method="pearson")
}

p <- p + xlab("Time [h]")
p <- p + ylab(paste("Caloric equivalent [", input$myp, "]"))

#  stat_smooth(method = "lm") + # adds regression line
#  stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) # adds correlation coefficient
},
{
}
)

#write.csv2(finalC1, file="test.csv")

list("plot"=p, "animals"=`$`(C1, "Animal No._NA"), "data"=finalC1)
}


# Create server -------------------------------------------------------
server <- function(input, output, session) {
   shinyDirChoose(input, 'export_folder', roots=c(wd=path_home()), filetypes=c('', 'txt'))

   # Dynamically create fileInput fields by the number of requested files of the user
   output$fileInputs=renderUI({
      html_ui = " "
      for (i in 1:input$nFiles) {
         html_ui <- paste0(html_ui, fileInput(paste0("File", i), label=paste0("Cohort ",i)))
         }
      HTML(html_ui)
      })

   # TODO: here decide which plot_type is used.
   observeEvent(input$plot_type, {
            output$myp = renderUI(
               selectInput(inputId="myp", label="Chose prefered method for calculating caloric equivalent over time", selected=input$variable1, choices=c(input$variable1, input$variable2)))
         })

   observeEvent(input$plot_type, {
            output$wmeans = renderUI(
               checkboxInput(inputId="wmeans", label="Display means"))
         })

   observeEvent(input$plot_type, {
            output$wstats = renderUI(
               checkboxInput(inputId="wstats", label="Display statistics"))
         })


     observeEvent(input$export_folder, {
         print(input$export_folder)

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
       
      })

   # Refresh plot (action button's action)
   observeEvent(input$replotting, {
           output$plot <- renderPlot({
           file = input$File1
           real_data <- do_plotting(file$datapath, input, exclusion=input$sick)
           real_data$plot
           })
   })

   observeEvent(input$plottingvalidation, {
         output$plotvalidation <- renderPlot({
            ref <- read.csv2(input$rerself$name, na.strings = c("-","NA"), header=FALSE, sep=";")
            calr <- read.csv2(input$rercalr$name, na.strings = c("-","NA"), header=FALSE, sep=";")
            print(ref)
            df_comparison <- data.frame(ref=as.numeric(gsub(",", ".", gsub("\\.", "", ref$V11))), calr=as.numeric(gsub(",", ".", gsub("\\.", "", calr$V11))))
            print(head(df_comparison))
            print(df_comparison)
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
   # Show plot (action button's action)
   observeEvent(input$plotting, {
      output$plot <- renderPlot({
         if (is.null(input$File1)) {
            print("No cohort data given!");
            output$message <- renderText("Not any cohort data given")
         } else {
           file = input$File1
           print(file$datapath)
           real_data <- do_plotting(file$datapath, input, input$sick)
           
           print(real_data$status)
           if (! (is.null(real_data$status)) && (real_data$status == FALSE)) {
               output$message <- renderText("Input data incompatible, make sure you either supply only TSE or Sable system files not a combination of both file types.")
           } else {
               output$message <- renderText("Success")
           }

           if ((! is.null(real_data$animals)) && is.null(input$sick)) {
              output$sick = renderUI(
              multiInput(inputId="sick", label="Remove outliers (sick animals, etc.) ", selected="", choices=unique(real_data$animals)))
           }
            
           real_data$plot
        }
      })
    })

   # Reset session
   observeEvent(input$reset, {
      session$reload()
   })
}
