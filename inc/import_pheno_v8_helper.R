library(stringr)
library(dplyr)

import_pheno_v8 <- function(file, file_out) {
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
   df_selected <- df %>% select(c("Animal.No.", "VO2.3.", "VCO2.3.", "RER", "Time", "Date", "LightC", "Box"))
   df_selected$Time <- sub("(..):(..):(..)", "\\1:\\2", df_selected$Time)
   units <- df_selected[1,]
   units[is.na(units)] <- ''
   df_selected <- df_selected[-1, ]
   df_selected <- df_selected[!grepl("-", `$`(df_selected, "VO2.3.")), ]
   df_selected <- df_selected[!grepl("-", `$`(df_selected, "VCO2.3.")), ]

   # 8
   header <- data.frame(matrix(ncol = length(colnames(df_selected)), nrow = 0))
   colnames(df_selected) <- c("Animal No.", "VO2(3)", "VCO2(3)", "RER", "Time", "Date", "LightC", "Box")
   colnames(header) <- colnames(df_selected)
   header[nrow(header) + 1, ] <- c(file, rep("", 7))
   header[nrow(header) + 1, ] <- c("", filetype, rep("", 6))

   metadata <- read.csv2(file, skip = 2, nrows = toskip - 4)
   cc <- colnames(metadata)
   cc <- cc[!grepl("^X", cc)]
   header[nrow(header) + 1, ] <- c(cc, rep("", 2))
   metadata_selected <- metadata %>% select(cc)
   for (i in 1:nrow(metadata_selected)) {
      header[nrow(header) + 1, ] <- c(metadata_selected[i, ], rep("", 1))
   }
   header[nrow(header) + 1, ] <- rep("", 8)
   header[nrow(header) + 1, ] <- colnames(header)
   header[nrow(header) + 1, ] <- units

   full_data <- rbind(header, df_selected)
   write.table(full_data, file_out, col.names = FALSE, row.names = FALSE, sep = ";", quote = FALSE)
}