library(dplyr)
library(lubridate)

check_for_cosmed_QNRG <- function(file) {
   print("here")
   print(file)
   return(grepl("QNRG", file, fixed = TRUE))
}

file_path <- "/home/stephan/Downloads/Daten Alex/ID1_CE_QNRG_pre_2.csv"
file_out <- "test.csv"

import_cosmed_QNRG <- function(file_path, file_out, intervention, treatment, id) {
   reformat_time_for_cosmed <- function(time) {
      td <- seconds_to_period(time)
      sprintf("%02d:%02d", minute(td), second(td))
   }


   #parts <- strsplit(tools::file_path_sans_ext(basename(file_path)), "_")[[1]]
   #id = gsub("\\D+", "", parts[[1]])

   all_data <- read.csv2(file_path, stringsAsFactors = FALSE)

   # Define the columns you want to keep
   cols_to_keep <- c("Time", "VP", "VO2", "VCO2", "EE",
                     "FeO2", "FeCO2", "FiO2", "FiCO2", "Battery")

   # Subset the dataframe to keep only the relevant columns
   df <- all_data[ , cols_to_keep]
   df$Date <- "01.01.1970"
   df$`Animal No.` <- id
   df$Treatment <- treatment
   df$Intervention <- intervention
   df <- df[-c(1,3), ]
   print(df$Time)

   df <- df %>% rename("VO2(3)" = "VO2")
   df <- df %>% rename("VCO2(3)" = "VCO2")

   cols_to_keep <- c("Time", "VP", "VO2(3)", "VCO2(3)", "EE",
                     "FeO2", "FeCO2", "FiO2", "FiCO2", "Battery")



   colnames(df) <- rep("", ncol(df))
   print(df)

   #df$ID <- id

   print(colnames(df))
   print(df$Time)

   header <- data.frame(matrix(ncol = length(cols_to_keep)+4, nrow = 0)) # +1 for date, and +1 for animal ID +1 for Treatment +1 for intervention

   print(length(cols_to_keep))

   fileinfo <- c(file_path, rep("", 13))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 12))
   boxInfo <- c("Box", "Animal No.", "Weight [g]", "Treatment", "Intervention", rep("", 9))
   print("fileinfo:")
   print(fileinfo)
   print("extnededinfo:")
   print(extendedinfo)
   print("boxinfo:")
   print(boxInfo)

   print("Here?")

   header[nrow(header) + 1, ] <- fileinfo

   print("a")
   header[nrow(header) + 1, ] <- extendedinfo
   print("b")
   header[nrow(header) + 1, ] <- boxInfo
   print("c")
   header[nrow(header) + 1, ] <- c(id, id, 0, treatment, intervention, rep("", 9))
   print("foo:")

   units <- c("", "[ml/h]", "[ml/h]", "[ml/h]", "[kcal/day]", "[%]", "[%]", "[%]", "[%]", "[%]", rep("", 5))

   header[nrow(header) + 1, ] <- rep("", 14)

   header[nrow(header) + 1, ] <- c(cols_to_keep, "Date", "Animal No.", "Treatment", "Intervention")
   header[nrow(header) + 1, ] <- units

   print("Header")
   print(header)

   print("cols")
   colnames(header) <- c(cols_to_keep, "Date", "Animal No.", "Treatment", "Intervention")
   print("foobar")
   colnames(df) <- c(cols_to_keep, "Date", "Animal No.", "Treatment", "Intervention")
   print("barbar")

   print("rbind")
   full_data <- rbind(header, df)

   print(full_data)
   write.table(full_data, file_out, col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
}


import_cosmed_QNRG(file_path, file_out, 1, 2, 3)
