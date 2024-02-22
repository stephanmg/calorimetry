library("readxl")
library("dplyr")
library("tidyr")
library("lubridate")

helper_fn <- function(time) {
   td <- seconds_to_period(time)
   sprintf('%02d:%02d', minute(td), second(td))
}

import_cosmed  <- function(file, file_out) {
   df <- read_excel(file)
   #print(length(excel_sheets(file)))
   
   # TODO: get duration and interval from excel sheet
   duration <- 30 * 6 # 30 minutes and 6 mesasureents at 10 s intervals)
   interval <- 10
   data <- df[seq(3, nrow(df)),seq(10, ncol(df))]
   units = df[1, seq(10, ncol(df))]

   # TODO: Get ID and BMI from Excel sheet
   id <- 1
   bmi <- 25
   date <- colnames(df[1,5])
   date <- gsub('\\.', '/', date)

   data <- df %>% select(c("t", "VO2", "VCO2", "RQ"))
   data$t <- c("s", "NA", seq(from = interval, to = duration * interval, by = interval))
   data <- data[-c(1, 2), ] # get rid of empty lines in excel and units
   data$Date <- rep(date, nrow(data))
   data$Animal <- rep(id, nrow(data))
   header <- data.frame(matrix(ncol = length(colnames(data)), nrow = 0))
   data <- data %>% mutate(VO2 = gsub('\\.', ',', VO2))
   data <- data %>% mutate(VCO2 = gsub('\\.', ',', VCO2))
   data <- data %>% mutate(RQ = gsub('\\.', ',', RQ))
   data <- data %>% rename("Animal No." = "Animal")
   data <- data %>% rename("VO2(3)" = "VO2")
   data <- data %>% rename("VCO2(3)" = "VCO2")
   data <- data %>% rename("RER" = "RQ")
   data <- data %>% rename("Time" = "t")

   data <- data %>% mutate(Time = helper_fn(Time))
   colnames(header) <- colnames(data)

   fileinfo <- c(file, rep("", 6))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 5))
   boxInfo <- c("Box", "Animal No.", "Weight [g]", rep("", 4))
   header[nrow(header) + 1, ] <- fileinfo
   header[nrow(header) + 1, ] <- extendedinfo
   header[nrow(header) + 1, ] <- boxInfo

   # add animals
   header[nrow(header) + 1, ] <- c(id, id, bmi, rep("", 3))

   units <- c("", "[ml/h]", "[ml/h]", rep("", 4))
   header[nrow(header) + 1 ,] <- rep("", 7)
   header[nrow(header) + 1, ] <- colnames(header)
   header[nrow(header) + 1, ] <- units

   full_data <- rbind(header, data)
   write.table(full_data, file_out, col.names = FALSE, row.names = FALSE, sep = ";", quote = FALSE)
}
