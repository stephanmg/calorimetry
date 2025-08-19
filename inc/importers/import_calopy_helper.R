library(dplyr)

################################################################################
#' import_calopy
#' 
#' This function imports a non-standardized CaloPy indirect calorimetry data file
#' @param file input file
#' @param file_out output file
#' @examples 
#' import_calopy(input_file, output_file)
#' @export
################################################################################
import_calopy <- function(file_path, file_out) {
   # read all data
   all_data <- read.csv(file_path, stringsAsFactors = FALSE)
   cols_to_keep <- c("sample_id", "Animal_No", "body_weight", "diet", "date_time", "VO2.3.", "VCO2.3.", "genotype", "RER")

   # Subset the dataframe to keep only the relevant columns
   df <- all_data[ , cols_to_keep]
   # get unique sample id (box/cage), diet, genotype body weight and Animal_No from non-standardized metadata header
   unique_vals <- df %>% dplyr::distinct(sample_id, diet, genotype, body_weight, Animal_No)

   # compile data
   df <- df %>% rename("VCO2(3)" = "VCO2.3.")
   df <- df %>% rename("VO2(3)" = "VO2.3.")
   df <- df %>% rename("Box" = "sample_id")
   df <- df %>% rename("Animal No." = "Animal_No")

   df$date_time <- as.character(df$date_time) 
   datetime <- as.POSIXct(df$date_time, format = "%Y-%m-%d %H:%M:%S")
   df$Date <- format(datetime, "%d.%m.%Y")
   df$Time <- format(datetime, "%H:%M")
   df <- df %>% select(-date_time)

   cols_to_keep <- c("VO2(3)", "VCO2(3)", "Animal No.", "Box", "RER", "Time", "Date")
   df <- df[ , cols_to_keep]
   cols = c("VO2(3)", "VCO2(3)", "RER")
   df[cols] <- lapply(df[cols], function(x) gsub("\\.", ",", as.character(x)))
   colnames(df) <- rep("", ncol(df))

   # preprare header
   header <- data.frame(matrix(ncol = length(cols_to_keep), nrow = 0)) # for animal ID and box 9 columns
   fileinfo <- c(file_path, rep("", 6))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 5))
   boxInfo <- c("Box", "Animal No.", "Diet", "Genotype", "Weight [g]", rep("", 2)) # TSE does require that we have at least Diet and Genotype!!! If this is missing, we can't correctly combine the metadata currently
   header[nrow(header) + 1, ] <- fileinfo
   header[nrow(header) + 1, ] <- extendedinfo
   header[nrow(header) + 1, ] <- boxInfo

   # populate metadata in header
   apply(unique_vals, 1, function(row) {
      sid <- row["Animal_No"]
      mass <- as.numeric(row["body_weight"])
      cage <- row["sample_id"]
      diet <- row["diet"]
      genotype <- row["genotype"]
      header[nrow(header) + 1, ] <<- c(cage, sid, diet, genotype, mass, rep("", 2))
   })

   # set units
   units <- c("[ml/h]", "[ml/h]", "", "", "", "", "")
   header[nrow(header) + 1, ] <- rep("", 7)
   header[nrow(header) + 1, ] <- c(cols_to_keep)
   header[nrow(header) + 1, ] <- units

   # prepare column names of data and metadata
   colnames(header) <- c(cols_to_keep)
   colnames(df) <- c(cols_to_keep)

   # combine data and metadata, and write data file
   full_data <- rbind(header, df)
   write.table(full_data, file_out, col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
}
