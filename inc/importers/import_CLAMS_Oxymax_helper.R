# oxymax_read_clean.R
library(dplyr)

################################################################################
#' import_CLAMS_Oxymax
#' 
#' This function imports the Columbus Instruments CLAMS/Oxymax data file format
#' @param file_path input file
#' @param file_out output file
#' @examples 
#' import_CLAMS_Oxymax(file_path, file_out)
#' @export
################################################################################
import_CLAMS_Oxymax  <- function(file_path, file_out) {
   # Read whole file
   lines <- readLines(file_path)

   # Extract numeric values using regex
   subject_id_line   <- grep("^Subject ID,", lines, value = TRUE)
   subject_mass_line <- grep("^Subject Mass", lines, value = TRUE)
   subject_cage_line <- grep("^Group/Cage", lines, value = TRUE)

   subject_id   <- as.integer(sub("Subject ID,([0-9]+).*", "\\1", subject_id_line))
   subject_mass <- as.numeric(sub("Subject Mass.*?,([0-9.]+).*", "\\1", subject_mass_line))
   subject_cage <- as.numeric(sub("Group/Cage.*?,([0-9.]+).*", "\\1", subject_cage_line))

   data_start <- grep("^:DATA", lines)
   if (length(data_start) == 0L) stop("Error: ':DATA' section not found")

   header_idx <- data_start + 2
   data_lines <- lines[header_idx:length(lines)]

   df_all <- read.csv(text = data_lines, header = TRUE, stringsAsFactors = FALSE)

   df <- df_all[, c("DATE.TIME", "VO2", "VCO2", "HEAT")]
   colnames(df)[1] <- "DATETIME"
   df <- df[-1, ] 

   df$DATETIME <- as.POSIXct(df$DATETIME, format = "%m/%d/%y %H:%M", tz = Sys.timezone())

   df$Date <- as.Date(df$DATETIME, format="%Y/%m/%d")
   df$Time <- format(df$DATETIME, "%H:%M:%S")
   df$Box <- subject_cage

   df <- df[, !names(df) %in% c("DATETIME")]
   df <- df %>% rename("VO2(3)" = "VO2")
   df <- df %>% rename("VCO2(3)" = "VCO2")
   df <- df %>% rename("EE" = "HEAT")

   df$`Animal. No.` <- as.character(subject_id)
   df$EE <- gsub("\\.", ",", as.character(df$EE))
   cols_to_keep <- c("VO2(3)", "VCO2(3)", "EE")
   colnames(df) <- rep("", ncol(df))

   header <- data.frame(matrix(ncol = length(cols_to_keep)+4, nrow = 0)) # +1 for date, and +1 for time + 1 for animal ID

   fileinfo <- c(file_path, rep("", 6))
   extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 5)) 
   boxInfo <- c("Box", "Animal No.", "Weight [g]", "Diet", "Genotyp", rep("", 2)) # NOTE: is is crucially that we always have at least these informations, otherwise the TSE format is not well defined in SHiny-Calorie
   header[nrow(header) + 1, ] <- fileinfo
   header[nrow(header) + 1, ] <- extendedinfo
   header[nrow(header) + 1, ] <- boxInfo
   header[nrow(header) + 1, ] <- c(subject_id, subject_cage, subject_mass, "CD", "WT", "", "")

   units <- c("", "", "", "", "[ml/h]", "[ml/h]", "[kcal/day]")

   header[nrow(header) + 1, ] <- rep("", 7)

   header[nrow(header) + 1, ] <- c("Animal No.", "Box", "Date", "Time", cols_to_keep)
   header[nrow(header) + 1, ] <- units

   colnames(header) <- c("Animal No.", "Box", "Date", "Time", cols_to_keep)
   colnames(df) <- c("Animal No.", "Box", "Date", "Time", cols_to_keep)

   # Rerdering of columns
   df <- df[, c(4, 5, 7, 6, 1, 2, 3)]
   colnames(df) <- c("Date", "Time", "Box", "Animal No.", cols_to_keep)

   df$`Animal No.` <- as.character(df$`Animal No.`)
   df$`Box` <- as.character(df$`Box`)
   df$Date <- as.Date(df$Date)
   full_data <- bind_rows(mutate(header, Date=as.character(Date)), mutate(df, Date=as.character(Date)))

   full_data$Date <- sub("^([0-9]{4})-([0-9]{2})-([0-9]{2})$",
               "\\3.\\2.\\1",
               full_data$Date)

   full_data <- na.omit(full_data)
   header_rows <- full_data[1:8, ]  # header contains nans, but ends after line 7
   data_rows   <- full_data[-(1:8), ] # header contains nans, but ends after line 7

   last_col <- data_rows[[ncol(data_rows)]]
   is_numeric <- !is.na(suppressWarnings(as.numeric(as.character(last_col))))

   data_clean <- data_rows[is_numeric, ]
   full_data <- rbind(header_rows, data_clean)

   write.table(full_data, file_out, col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
}

