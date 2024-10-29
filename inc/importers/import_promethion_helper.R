library("readxl")
library("dplyr")
library("tidyr")
library("lubridate")

################################################################################
#' import_promethion
#' 
#' This function imports Sable Systems Prometion indirect calorimetry data sets
#' @param file input file
#' @param file_out output file
#' @examples 
#' import_promethion(input_file, output_file)
#' @export
################################################################################
import_promethion <- function(file, file_out) {
   # when adding more metadata or columns need to be increased by 1
   NUM_METADATA <- 3
   NUM_TOTAL_COLUMNS_EXPECTED <- 8

   df <- read_excel(file)
   # TODO: v0.5.0 - support not only row-wise format but column-wise format, i.e. without Animal column, but VO2_M_1, VO2_M2, ...
   data <- df %>% select(c("Animal", "VO2_M", "VCO2_M", "RER_M", "EnviroTemp_M", "DateTime"))
   weights <- df %>% select(c("BodyMass_Mnz", "Animal"))
   animals_with_weights <- na.omit(weights %>% group_by(Animal) %>% slice(c(1)))

   data <- data.frame(lapply(data, function(x) {
       gsub("\\.", ",", x)
   }))
   data <- data %>% separate(DateTime, c("Date", "Time"), " ")
   data["Box"] <- data["Animal"]


   header <- data.frame(matrix(ncol = length(colnames(data)), nrow = 0))

   data <- data %>% rename("Animal No." = "Animal")
   data <- data %>% rename("VO2(3)" = "VO2_M")
   data <- data %>% rename("VCO2(3)" = "VCO2_M")
   data <- data %>% rename("RER" = "RER_M")
   data <- data %>% rename("Temp" = "EnviroTemp_M")

   colnames(header) <- colnames(data)

   # convert the promethion format to the TSE format for now
   fileinfo <- c(file, rep("", NUM_TOTAL_COLUMNS_EXPECTED - 1))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", NUM_TOTAL_COLUMNS_EXPECTED - 2))
   boxInfo <- c("Box", "Animal No.", "Weight [g]", rep("", NUM_TOTAL_COLUMNS_EXPECTED - 3))
   header[nrow(header) + 1, ] <- fileinfo
   header[nrow(header) + 1, ] <- extendedinfo
   header[nrow(header) + 1, ] <- boxInfo

   data$Date <- sub("^(....)/(..)/(..)", "\\3.\\2.\\1", data$Date)
   data$Time <- sub("(..):(..):(..)", "\\1:\\2", data$Time)

   for (i in seq_len(nrow(animals_with_weights))) {
      header[nrow(header) + 1, ] <- c(animals_with_weights[i, ]["Animal"], animals_with_weights[i, ]["Animal"], animals_with_weights[i, ]["BodyMass_Mnz"], rep("", NUM_METADATA))
   }

   # units must follow the column order in the compiled data datafame
   units <- c("", "[ml/h]", "[ml/h]", "", "C", rep("", NUM_METADATA))
   header[nrow(header) + 1, ] <- rep("", NUM_TOTAL_COLUMNS_EXPECTED)
   header[nrow(header) + 1, ] <- colnames(header)
   header[nrow(header) + 1, ] <- units

   full_data <- rbind(header, data)
   write.table(full_data, file_out, col.names = FALSE, row.names = FALSE, sep = ";", quote = FALSE)
}
