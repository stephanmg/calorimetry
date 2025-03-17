library(dplyr)
library(tidyr)

################################################################################
#' check_for_calobox
#' 
#' Calobox file identifies themselves by having DD-MM-YYYY in the first line
#' Caloboy files have always 25 columns separated by a comma (",")
#' 
################################################################################
check_for_calobox <- function(filename) {
	header =  readLines(filename, n=2)
	pattern <- "^\\d{2}-\\d{2}-\\d{4}:\\d*"
	first_line_matches_date <- grepl(pattern, header[1])
   number_of_columns_matches <- as.integer(str_count(header[2], ","))
   return(first_line_matches_date & (number_of_columns_matches == 24))
}

get_animal_id_for_calobox <- function(filename) {
	header =  readLines(filename, n=2)
   return(strsplit(header, ":")[[1]][2])
}


################################################################################
#' import_calobox
#' 
#' This function imports a single calobox file
#' 
################################################################################
import_calobox <- function(filename, file_out) {
   # Skip header (containing recording date only as a single line)
   df <-  read.csv(filename, skip=1)

   
   df <- df %>% mutate(VO2.mL.min. = VO2.mL.min. * 60) 
   df <- df %>% mutate(VCO2.prod..mL.min. = CO2.prod..mL.min. * 60)
   df <- df %>% mutate(vH2O..mL.min. = vH2O..mL.min. * 60)
   df <- df %>% mutate(HP.mW. = HP.mW. * 0.0036)

   df$group_index <- cumsum(c(TRUE, diff(df$Function != "Zero") > 0))
   last_x_indices <- df %>%
      mutate(Index = row_number()) %>%
      filter(Function == "Zero") %>%
      group_by(group_index) %>%
      summarize(LastX = max(Index)) %>%
      pull(LastX)

   indices_to_remove <- NULL
   difference_df = df %>%  filter(Function == "Measure") %>% slice_head(n=2)
   difference_df <- df %>% mutate(Time. = as.POSIXct(Time., format="%H:%M:%S", tz = "UTC"))
   difference <- as.numeric(difftime(difference_df$Time.[2], difference_df$Time.[1], units = "secs"))

   if (difference == 4) {
      indices_to_remove <- sort(c(last_x_indices + 1, last_x_indices + 2, last_x_indices + 3, last_x_indices + 4, last_x_indices + 5, last_x_indices + 6, last_x_indices + 7))
   } else if (difference == 10) {
      indices_to_remove <- sort(c(last_x_indices+1, last_x_indices + 2, last_x_indices + 3))
   } else if (difference == 15) {
      indices_to_remove <- sort(c(last_x_indices+1, last_x_indices + 2))
   } else if (difference == 30) {
      indices_to_remove <- sort(c(last_x_indices+1))
   } else if (difference >= 60) {
      # No Measurements after Zero Adjustments should be removed
   }

   indices_to_remove <- indices_to_remove[indices_to_remove <= nrow(df)]

   df <- df %>% filter(!(row_number() %in% indices_to_remove))

   # measurements
   df <- df %>% filter(Function == "Measure")
   df <- df %>% select(c(Date.Time, Time., X.RER., HP.mW., VO2.mL.min., VCO2.prod..mL.min., vH2O..mL.min., AirPressure..kPa., EE.cal.min.))
   # This should be in principle not necessary anymore, see timeScale feature in backend
   df <- df %>% mutate(group=rep(1:(nrow(df) %/% 2 + (nrow(df) %% 2 > 0)), each = 2, length.out = nrow(df))) %>%
                group_by(group) %>%
                summarize(
                Date.Time = first(Date.Time),
                Time. = first(Time.),
               X.RER. = mean(X.RER.),
                HP.mW. =  mean(HP.mW.),
                VO2.mL.min. = mean(VO2.mL.min.),
                VCO2.prod..mL.min. = mean(VCO2.prod..mL.min.),
               AirPressure..kPa. = mean(AirPressure..kPa.),
                EE.cal.min. = mean(EE.cal.min.),
                vH2O..mL.min. = mean(vH2O..mL.min.)) %>% select(-group)
 


   df <- df %>% mutate(across(where(is.numeric), ~ gsub("\\.", ",", as.character(.)))) # replace "." with "," for TSE 6.3 format


   animal_id <- get_animal_id_for_calobox(filename)
   print("calobox animal id:")
   print(animal_id)
   df$AnimalID <- animal_id
   df$Box <- animal_id
   df$HP2 <- df$HP.mW.

   df <- df %>% select(-Time.)
   df <- df %>% separate(Date.Time, into=c("Date", "Time"), sep = " ") %>% mutate(Time = format(strptime(Time, format="%H:%M:%S"), "%H:%M"))
   df <- df %>% rename(RER=X.RER., `Animal No.`=AnimalID, HP=HP.mW.)
   df <- df %>% rename(`VO2(3)`=VO2.mL.min., `VCO2(3)`=VCO2.prod..mL.min.)
   df <- df %>% rename(`VH2O(3)`=vH2O..mL.min.)
   df <- df %>% rename(`EE`=EE.cal.min.)
   df <- df %>% rename(`AirPressure`=AirPressure..kPa.)

   # units and count number of fields 
   units <- c("", "", "", "[kJ/h]", "[ml/h]", "[ml/h]", "[ml/h]", "[kPa]", "[kJ/h]", "", "", "[ml/h]")
   fields = length(units)

   df_row_one <- df[1,, drop=FALSE]
   df_row_one <- rbind(units, df_row_one)
   df <- rbind(df_row_one, df[-1,, drop=FALSE])

   # TSE 6.3 metadata header
   df_meta <- data.frame()
   description <- c(filename, "CaloBox", rep("", fields - 2))
   df_meta <- rbind(df_meta, description)
   file_format <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", fields - 2))
   df_meta <- rbind(df_meta, file_format)
   sample_section <- c("Box", "Animal No.", "Weight [g]", rep("", fields - 3))
   df_meta <- rbind(df_meta, sample_section)
   sample_entry <- c(animal_id, animal_id, "NA", rep("", fields - 3))
   df_meta <- rbind(df_meta, sample_entry)
   separator <- rep("", fields)
   df_meta <- rbind(df_meta, separator)

   output1 <- capture.output(write.table(df_meta, sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE))
   output2 <- capture.output(write.table(df, sep=";", row.names=FALSE, quote=FALSE))

   combined_output <- c(output1, output2)
   write.table(combined_output, file=file_out, sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE)
   write.table(combined_output, file="test_file.csv", sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE)
}
