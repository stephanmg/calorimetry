library(dplyr)
#library(lubridate)

check_for_cosmed_QNRG <- function(file) {
   print("here")
   print(file)
   return(grepl("QNRG", file, fixed = TRUE))
}

################################################################################
#' import_cosmed_QNRG
#' 
#' This function imports a COSMED QNRG indirect calorimetry data file
#' @param file_path input file
#' @param file_out output file
#' @params intervention intervention
#' @params treatment treatment
#' @params training training
#' @params id subject id
#' @params normalize_to_body_weight should respiratory gas exchange be scaled by body weight of subject
#' @examples 
#' import_cosmed_QNRG(input_file, output_file, intervention, treatment, training, id, FALSE)
#' @export
################################################################################
import_cosmed_QNRG <- function(file_path, file_out, intervention, treatment, training, id, normalize_to_body_weight) {
   reformat_time_for_cosmed <- function(time) {
      td <- seconds_to_period(time)
      sprintf("%02d:%02d", minute(td), second(td))
   }

   all_data <- read.csv2(file_path, stringsAsFactors = FALSE, sep=",")
   row_idx <- which(all_data[[1]] %in% c("Gewicht (kg)", "Weight (kg)"))

   # Find the column index in that row
   # This assumes the headings are in first row; `df[row_idx, ]` is character.
   col_idx <- which(!is.na(all_data[row_idx, ]))

   target_col <- col_idx[1] + 1

   # Extract the value (assuming you're only interested in the first matching row/col)
   value_found <- all_data[row_idx[1], target_col]

   # Define the columns you want to keep
   cols_to_keep <- c("Time", "VP", "VO2", "VCO2", "EE",
                     "FeO2", "FeCO2", "FiO2", "FiCO2", "Battery")

   # Subset the dataframe to keep only the relevant columns
   df <- all_data[ , cols_to_keep]
   df$Date <- "01.01.1970" # dummy date as none is provided
   df$`Animal No.` <- id
   df$Treatment <- as.factor(treatment)
   df$Intervention <- as.factor(intervention)
   df$Training <- as.factor(training)
   df <- df[-c(1,3), ]

   df <- df %>% rename("VO2(3)" = "VO2")
   df <- df %>% rename("VCO2(3)" = "VCO2")

   if (normalize_to_body_weight) {
      df$`VO2(3)` <- as.numeric(df$`VO2(3)`)
      df$`VO2(3)` <- df$`VO2(3)` / as.numeric(value_found)
      df$`VO2(3)` <- tidyr::replace_na(df$`VO2(3)`, 0)
      df$`VO2(3)` <- format(df$`VO2(3)`, decimal.mark=",", nsmall=6)
      df$`VO2(3)` <- as.character(df$`VO2(3)`)

      df$`VCO2(3)` <- as.numeric(df$`VCO2(3)`)
      df$`VCO2(3)` <- df$`VCO2(3)` / as.numeric(value_found)
      df$`VCO2(3)` <- tidyr::replace_na(df$`VCO2(3)`, 0)
      df$`VCO2(3)` <- format(df$`VCO2(3)`, decimal.mark=",", nsmall=6)
      df$`VCO2(3)` <- as.character(df$`VCO2(3)`)
   }

   cols_to_keep <- c("Time", "VP", "VO2(3)", "VCO2(3)", "EE",
                     "FeO2", "FeCO2", "FiO2", "FiCO2", "Battery")

   colnames(df) <- rep("", ncol(df))

   header <- data.frame(matrix(ncol = length(cols_to_keep)+5, nrow = 0)) # +1 for date, and +1 for animal ID +1 for Treatment +1 for intervention +1 for training

   fileinfo <- c(file_path, rep("", 14))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 13))
   boxInfo <- c("Box", "Animal No.", "Weight [g]", "Treatment", "Intervention", "Training", "Genotype", rep("", 8))

   header[nrow(header) + 1, ] <- fileinfo
   header[nrow(header) + 1, ] <- extendedinfo
   header[nrow(header) + 1, ] <- boxInfo
   header[nrow(header) + 1, ] <- c(id, id, as.character(as.numeric(value_found)*1000), treatment, intervention, training, "WT", rep("", 8))

   units <- c("", "[ml/h]", "[ml/h]", "[ml/h]", "[kcal/day]", "[%]", "[%]", "[%]", "[%]", "[%]", rep("", 5))

   header[nrow(header) + 1, ] <- rep("", 15)
   header[nrow(header) + 1, ] <- c(cols_to_keep, "Date", "Animal No.", "Treatment", "Intervention", "Training")
   header[nrow(header) + 1, ] <- units

   colnames(header) <- c(cols_to_keep, "Date", "Animal No.", "Treatment", "Intervention", "Training")
   colnames(df) <- c(cols_to_keep, "Date", "Animal No.", "Treatment", "Intervention", "Training")
   full_data <- rbind(header, df)
   write.table(full_data, file_out, col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
}