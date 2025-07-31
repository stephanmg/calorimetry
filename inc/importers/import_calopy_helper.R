library(dplyr)
#library(lubridate)

   reformat_time_for_cosmed <- function(time) {
      td <- seconds_to_period(time)
      sprintf("%02d:%02d", minute(td), second(td))
   }

#sample_id,Animal_No,body_weight,body_weight_change_percent,body_weight_change,genotype,diet,date_time,XT+YT,XT,VO2(3),VCO2(3),Temp,RER,H(3),Flow,Feed1,Drink


file_path <- "/home/stephan/Downloads/revision shiny draft/example_calopy.csv"
file_out <- "test.csv"

import_calopy <- function(file_path, file_out) {

   all_data <- read.csv(file_path, stringsAsFactors = FALSE)
   print(colnames(all_data))


   cols_to_keep <- c("sample_id", "Animal_No", "body_weight", "diet", "date_time", "VO2.3.", "VCO2.3.", "genotype", "RER")


   # Subset the dataframe to keep only the relevant columns
   df <- all_data[ , cols_to_keep]
   #df <- df %>% dplyr::filter(vo2 >= 0)
   #df <- df %>% dplyr::filter(vco2 >= 0)
   #df <- df %>% dplyr::filter(rer >= 0)

   print(colnames(df))
   unique_vals <- df %>% dplyr::distinct(sample_id, diet, genotype, body_weight, Animal_No)
   print(unique_vals)


   df <- df %>% rename("VCO2(3)" = "VCO2.3.")
   df <- df %>% rename("VO2(3)" = "VO2.3.")
   df <- df %>% rename("Box" = "sample_id")
   df <- df %>% rename("Animal No." = "Animal_No")


   df$date_time <- as.character(df$date_time)  # ensure it's character if it's not
   datetime <- as.POSIXct(df$date_time, format = "%Y-%m-%d %H:%M:%S")
   df$Date <- format(datetime, "%d.%m.%Y")
   df$Time <- format(datetime, "%H:%M")

   df <- df %>% select(-date_time)

   cols_to_keep <- c("VO2(3)", "VCO2(3)", "Animal No.", "Box", "RER", "Time", "Date")
   df <- df[ , cols_to_keep]

   #df$`VO2(3)` = as.numeric(df$`VO2(3)`)
   #df$`VCO2(3)` = as.numeric(df$`VCO2(3)`)

   #print(df$`VO2(3)`)
   #return(1);

   
   cols = c("VO2(3)", "VCO2(3)", "RER")
   df[cols] <- lapply(df[cols], function(x) gsub("\\.", ",", as.character(x)))


   colnames(df) <- rep("", ncol(df))

   header <- data.frame(matrix(ncol = length(cols_to_keep), nrow = 0)) # for animal ID and box 9 columns


   print(length(cols_to_keep))

   fileinfo <- c(file_path, rep("", 6))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 5))
   boxInfo <- c("Box", "Animal No.", "Diet", "Genotype", "Weight [g]", rep("", 2)) # TSE does require that we have at least Diet and Genotype!!! If this is missing, we can't correctly combine the metadata currently

   header[nrow(header) + 1, ] <- fileinfo

   header[nrow(header) + 1, ] <- extendedinfo
   header[nrow(header) + 1, ] <- boxInfo

   
   apply(unique_vals, 1, function(row) {
  sid <- row["Animal_No"]
  mass <- as.numeric(row["body_weight"])
  cage <- row["sample_id"]
  diet <- row["diet"]
  genotype <- row["genotype"]
   header[nrow(header) + 1, ] <<- c(cage, sid, diet, genotype, mass, rep("", 2))
})



   units <- c("[ml/h]", "[ml/h]", "", "", "", "", "")

   header[nrow(header) + 1, ] <- rep("", 7)



   header[nrow(header) + 1, ] <- c(cols_to_keep)
   header[nrow(header) + 1, ] <- units


   print("Header")
   print(header)


   print("cols::")
   colnames(header) <- c(cols_to_keep)
   print("foobar")
   print(length(colnames(df)))
   colnames(df) <- c(cols_to_keep)
   print("barbar")


   print("rbind")
   full_data <- rbind(header, df)

   print(full_data)
   write.table(full_data, file_out, col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
}
