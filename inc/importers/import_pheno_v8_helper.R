library(stringr)
library(dplyr)

################################################################################
#' import_pheno_v8
#' 
#' This function imports TSE Systems PhenoMaster v8 indirect calorimetry data sets
#' @param file input file
#' @param file_out output file
#' @examples 
#' import_pheno_v8(input_file, output_file)
################################################################################
import_pheno_v8 <- function(file, file_out) {
   # TODO: PhenoMaster v8 typically uses ";" as separator, but sometimes also "," 
   # We need to accomodate for both the occasionally occuring "," separator too
   toskip <- 0
   con <- file(file, "r")
   filetype <- ""
   while (TRUE) {
      line <- readLines(con, n = 1)
      if (toskip == 1) {
         filetype <- str_extract(line, "TSE.*\\)")
      }
      toskip <- toskip + 1

      if (str_detect(line, "^;+$")) {
         break
      }
   }
   close(con)

   df <- read.csv2(file, skip = toskip)
   
   additional_fields = c()
   if ("XT.YT" %in% colnames(df)) {
      additional_fields <- append(additional_fields, "XT.YT")
   }

   if ("TempL" %in% colnames(df)) {
      additional_fields <- append(additional_fields, "TempL")
   }

   if ("TempC" %in% colnames(df)) {
      additional_fields <- append(additional_fields, "TempC")
   }

   print(colnames(df))

   df_selected <- df %>% select(c("Animal.No.", "VO2.3.", "VCO2.3.", "RER", "Time", "Date", "LightC", "Box", "O2", "CO2", "Weight", additional_fields))
   print("df selected before")
   print(df_selected)
   # PhenoMaster v8 has the following time format HH:MM:SS
   df_selected$Time <- sub("(..):(..):(..)", "\\1:\\2", df_selected$Time)
   units <- df_selected[1,]
   units[is.na(units)] <- ''
   df_selected <- df_selected[-1, ]
   df_selected <- df_selected[!grepl("-", `$`(df_selected, "VO2.3.")), ]
   df_selected <- df_selected[!grepl("-", `$`(df_selected, "VCO2.3.")), ]
   df_selected <- df_selected[!grepl("-", `$`(df_selected, "O2")), ]
   df_selected <- df_selected[!grepl("-", `$`(df_selected, "CO2")), ]
   df_selected <- df_selected[!grepl("-", `$`(df_selected, "Weight")), ]

   print("selected head:")
   print(head(df_selected))
   for (additional_field in additional_fields) {
      print("selected head loop:")
      print(head(df_selected))
      df_selected <<- df_selected[!grepl("-", `$`(df_selected, additional_field)), ]
   }

   print("selected:")
   print(df_selected)

   # 9
   header <- data.frame(matrix(ncol = length(colnames(df_selected)), nrow = 0))
   colnames(df_selected) <- c("Animal No.", "VO2(3)", "VCO2(3)", "RER", "Time", "Date", "LightC", "Box", "O2", "CO2", "WeightBody", additional_fields)
   colnames(header) <- colnames(df_selected)
   header[nrow(header) + 1, ] <- c(file, rep("", 10 + length(additional_fields)))
   header[nrow(header) + 1, ] <- c("", filetype, rep("", 9 + length(additional_fields)))

   metadata <- read.csv2(file, skip = 2, nrows = toskip - 4)
   cc <- colnames(metadata)
   cc <- cc[!grepl("^X", cc)]
   header[nrow(header) + 1, ] <- c(cc, rep("", 5 + length(additional_fields)))
   metadata_selected <- metadata %>% select(cc)

   for (i in 1:nrow(metadata_selected)) {
      header[nrow(header) + 1, ] <- c(metadata_selected[i, ], rep("", 1))
   }
   header[nrow(header) + 1, ] <- rep("", 11 + length(additional_fields))
   header[nrow(header) + 1, ] <- colnames(header)
   header[nrow(header) + 1, ] <- units

   full_data <- rbind(header, df_selected)
   write.table(full_data, file_out, col.names = FALSE, row.names = FALSE, sep = ";", quote = FALSE)
}
