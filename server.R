# Load libraries, data ------------------------------------------------
library(ggplot2)
library(viridis)
library(dplyr)
library(lubridate)
require(tidyverse)

do_plotting <- function(file, input) {

cbPalette <- viridis(3, option = "cividis", begin = 0.1, end = 0.8, alpha = 1)
cbPalette2 <- cbPalette[c(1,3)]

#theme_pubr_update <- theme_pubr(base_size = 8.5) +
#  theme(legend.key.size = unit(0.3, "cm")) +
#  theme(strip.background = element_blank()) +
#  theme(strip.text = element_text(hjust = 0)) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#theme_set(theme_pubr_update)

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
  filter(Datetime2 >= '2020-05-08 12:02:00' & # you need to adjust this to your means!
         Datetime2 <=  '2020-05-11 11:32:00') %>% # you need to adjust this to your means!
  mutate(MeasPoint = row_number())

C1 <- C1[!is.na(C1$MeasPoint),]

C1$HP <- C1$`VO2(3)_[ml/h]` * (6 * C1$RER_NA + 15.3) * 0.278
C1$HP2 <- (4.44 + 1.43 * C1$RER_NA) * C1$`VO2(3)_[ml/h]`
p <- ggplot(data = C1, aes_string(x = input$variable1, y = input$variable2)) +
  geom_point() +
  stat_smooth(method = "lm") # adds regression line
  # stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) # adds correlation coefficient
p
#HPcomp <- lm(HP ~ HP2, data = C1)
#HPcomp
}


# Create server -------------------------------------------------------
server <- function(input, output, session) {

   observeEvent(input$plotting, {
      output$plot <- renderPlot({
         if (is.null(input$File)) {
            print("No data given!");
         } else {
           file = input$File
           real_data <- do_plotting(file$name, input)
           real_data
           #C1 <- read.csv2(file$name, header = F, skip = 10, na.strings = c("-","NA"))
           #p <- ggplot(data=C1, aes_string(x=input$variable2, y=input$variable2)) + 
           #geom_boxplot() 
           #p
        }
      })
    })
}

