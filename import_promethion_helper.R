library("readxl")
library("dplyr")
library("tidyr")
library("lubridate")

import_promethion <- function(file, file_out) {

   df <- read_excel(file)

   data <- df %>% select(c("Animal", "VO2_M", "VCO2_M", "RER_M", "DateTime"))
   weights <- df %>% select(c("BodyMass_Mnz", "Animal"))
   animals_with_weights <- na.omit(weights %>% group_by(Animal) %>% slice(c(1)))

   data <- data.frame(lapply(data, function(x) { gsub('\\.', ',', x) }))
   data <- data %>% separate(DateTime, c("Date", "Time"), " ")
   data["Box"] <- data["Animal"]

   header = data.frame(matrix(ncol=length(colnames(data)), nrow=0))

   data <- data %>% rename("Animal No."="Animal")
   data <- data %>% rename("VO2(3)"= "VO2_M")
   data <- data %>% rename("VCO2(3)"= "VCO2_M")
   data <- data %>% rename("RER"="RER_M")

   colnames(header) <- colnames(data)

   fileinfo <- c(file, rep("", 6))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 5))
   boxInfo <- c("Box", "Animal No.", "Weight [g]", rep("", 4))
   header[nrow(header) + 1,] <- fileinfo
      header[nrow(header) + 1,] <- extendedinfo
   header[nrow(header) + 1,] <- boxInfo

   data$Date <- sub("^(....)/(..)/(..)", "\\3.\\2.\\1", data$Date)
   data$Time <- sub("(..):(..):(..)", "\\1:\\2", data$Time)

   for (i in 1:nrow(animals_with_weights)) {
      header[nrow(header) + 1,] <- c(animals_with_weights[i,]['Animal'], animals_with_weights[i,]['Animal'], animals_with_weights[i,]['BodyMass_Mnz'], rep("", 2))
   }

   units <- c("", "[ml/h]", "[ml/h]", rep("", 4))
   header[nrow(header) + 1,] <- rep("",7)
   header[nrow(header) + 1,] <- colnames(header)
   header[nrow(header) + 1,] <- units

   full_data <- rbind(header, data)
   write.table(full_data, file_out, col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
}
