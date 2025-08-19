library(dplyr)

################################################################################
#' import_calR
#' 
#' This function imports a minimal CalR file
#' @param file input file
#' @param file_out output file
#' @examples 
#' import_calR(input_file, output_file)
#' @export
################################################################################
import_calR <- function(file_path, file_out) {
   all_data <- read.csv(file_path, stringsAsFactors = FALSE)

   cols_to_keep <- c("subject.id", "subject.mass", "cage", "Date.Time", "vo2", "vco2", "ee", "rer")

   # Subset the dataframe to keep only the relevant columns
   df <- all_data[ , cols_to_keep]

   # subject id, mass and cage/box 
   unique_vals <- df %>% dplyr::distinct(subject.id, subject.mass, cage)
   
   # reformat data frame columns to match what Shiny-Calorie expects
   df <- df %>% rename("VO2(3)" = "vo2")
   df <- df %>% rename("VCO2(3)" = "vco2")
   df <- df %>% rename("EE" = "ee")
   df <- df %>% rename("Box" = "cage")
   df <- df %>% rename("Animal No." = "subject.id")
   df <- df %>% rename("RER" = "rer")
   df$Date.Time <- as.character(df$Date.Time) 
   datetime <- as.POSIXct(df$Date.Time, format = "%Y-%m-%d %H:%M:%S")
   df$Date <- format(datetime, "%d.%m.%Y")
   df$Time <- format(datetime, "%H:%M")
   df <- df %>% select(-Date.Time)
   cols_to_keep <- c("VO2(3)", "VCO2(3)", "Animal No.", "Box", "EE", "Time", "Date")
   df <- df[ , cols_to_keep]
   cols = c("VO2(3)", "VCO2(3)", "EE")
   df[cols] <- lapply(df[cols], function(x) gsub("\\.", ",", as.character(x)))
   colnames(df) <- rep("", ncol(df))

   # header
   header <- data.frame(matrix(ncol = length(cols_to_keep), nrow = 0)) # for animal ID and box 9 columns
   fileinfo <- c(file_path, rep("", 6))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 5))
   boxInfo <- c("Box", "Animal No.", "Diet", "Genotype", "Weight [g]", rep("", 2)) # TSE does require that we have at least Diet and Genotype!!! If this is missing, we can't correctly combine the metadata currently
   header[nrow(header) + 1, ] <- fileinfo
   header[nrow(header) + 1, ] <- extendedinfo
   header[nrow(header) + 1, ] <- boxInfo

   # populate header with subject id, mass and cage/box
   apply(unique_vals, 1, function(row) {
      sid <- row["subject.id"]
      mass <- as.numeric(row["subject.mass"])
      cage <- as.numeric(row["cage"])
      header[nrow(header) + 1, ] <<- c(cage, sid, "NA", "NA", mass, rep("", 2))
   })

   # set units
   units <- c("[ml/h]", "[ml/h]", "", "", "[kcal/day]", "", "")
   header[nrow(header) + 1, ] <- rep("", 7)
   header[nrow(header) + 1, ] <- c(cols_to_keep)
   header[nrow(header) + 1, ] <- units

   # assign correct column names
   colnames(header) <- c(cols_to_keep)
   colnames(df) <- c(cols_to_keep)

   # combine data with metadata and write table
   full_data <- rbind(header, df)
   write.table(full_data, file_out, col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
}

