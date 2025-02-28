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
	pattern <- "^\\d{2}-\\d{2}-\\d{4}:"
	first_line_matches_date <- grepl(pattern, header[1])
   number_of_columns_matches <- as.integer(str_count(header[2], ","))
   return(first_line_matches_date & (number_of_columns_matches == 24))
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
   df <- df %>% mutate(CO2.prod..mL.min. = CO2.prod..mL.min. * 60)
   df <- df %>% mutate(vH2O..mL.min. = vH2O..mL.min. * 60)
   df <- df %>% mutate(HP.mW. = HP.mW. * 0.0036)


   # measurements
   df <- df %>% filter(Function == "Measure")
   df <- df %>% select(c(Date.Time, Time., X.RER., HP.mW., VO2.mL.min., CO2.prod..mL.min., vH2O..mL.min., AirPressure..kPa., EE.cal.min.))
   # TODO: CaloBox measures below 1 minute, which we do not support currently, thus we average two values each
   df <- df %>% mutate(group=rep(1:(nrow(df) %/% 2 + (nrow(df) %% 2 > 0)), each = 2, length.out = nrow(df))) %>%
                group_by(group) %>%
                summarize(
                Date.Time = first(Date.Time),
                Time. = first(Time.),
                X.RER. = mean(X.RER.),
                HP.mW. =  mean(HP.mW.),
                VO2.mL.min. = mean(VO2.mL.min.),
                CO2.prod..mL.min. = mean(CO2.prod..mL.min.),
                AirPressure..kPa. = mean(AirPressure..kPa.),
                EE.cal.min. = mean(EE.cal.min.),
                vH2O..mL.min. = mean(vH2O..mL.min.)) %>% select(-group)


   df <- df %>% mutate(across(where(is.numeric), ~ gsub("\\.", ",", as.character(.)))) # replace "." with "," for TSE 6.3 format


   print(head(df))

   df$AnimalID <- 1
   df$Box <- 1
   df$HP2 <- df$HP.mW.

   df <- df %>% select(-Time.)
   df <- df %>% separate(Date.Time, into=c("Date", "Time"), sep = " ") %>% mutate(Time = format(strptime(Time, format="%H:%M:%S"), "%H:%M"))
   df <- df %>% rename(RER=X.RER., `Animal No.`=AnimalID, HP=HP.mW.)
   df <- df %>% rename(`VO2(3)`=VO2.mL.min., `VCO2(3)`=CO2.prod..mL.min.)
   df <- df %>% rename(`VH2O(3)`=vH2O..mL.min.)
   df <- df %>% rename(`EE`=EE.cal.min.)
   df <- df %>% rename(`AirPressure`=AirPressure..kPa.)

   print(head(df))

   # units and count number of fields 
   units <- c("", "", "", "[kJ/h]", "[ml/h]", "[ml/h]", "[kPa]", "[kcal/min]", "[ml/h]", "", "", "[kJ/h]")
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
   sample_entry <- c(1, 1, "0", rep("", fields - 3))
   df_meta <- rbind(df_meta, sample_entry)
   separator <- rep("", fields)
   df_meta <- rbind(df_meta, separator)

   output1 <- capture.output(write.table(df_meta, sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE))
   output2 <- capture.output(write.table(df, sep=";", row.names=FALSE, quote=FALSE))

   combined_output <- c(output1, output2)
   write.table(combined_output, file=file_out, sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE)
}
