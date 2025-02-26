library(dplyr)
library(tidyr)

check_for_calobox <- function(filename) {
	header =  readLines(filename, n=1)
	pattern <- "^\\d{2}-\\d{2}-\\d{4}:"
	return(grepl(pattern, header))
}

import_calobox <- function(filename, file_out) {
   # Skip header (containing recording date only as a single line)
   df <-  read.csv(filename, skip=1)
   
   df <- df %>% mutate(VO2.mL.min. = VO2.mL.min. * 60) # mL / min to mL / h
   df <- df %>% mutate(HP.mW. = HP.mW. * 0.001 * 3600 * 0.000239006) # mW to kJ/h
   df <- df %>% mutate(across(where(is.numeric), ~ gsub("\\.", ",", as.character(.)))) # replace "." with "," for TSE 6.3 format

   # measurements
   df <- df %>% filter(Function == "Measure")
   df <- df %>% select(c(Date.Time, Time., X.RER., HP.mW., VO2.mL.min.))
   df$AnimalID <- filename
   df$Box <- filename

   df <- df %>% select(-Time.)
   df <- df %>% separate(Date.Time, into=c("Date", "Time"), sep = " ") %>% mutate(Time = format(strptime(Time, format="%H:%M:%S"), "%H:%M"))
   df <- df %>% rename(RER=X.RER., `Animal No.`=AnimalID, HP=HP.mW.)
   df <- df %>% rename(`VO2(3)`=VO2.mL.min.)
   # VCO2 not measured, but required for CALOR currently
   df$`VCO2(3)` <- 1

   # units and count number of fields 
   units <- c("", "", "", "[kJ/h]", "[ml/h]", "", "", "[ml/h]")
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
   sample_entry <- c(filename, filename, "0", rep("", fields - 3))
   df_meta <- rbind(df_meta, sample_entry)
   separator <- rep("", fields)
   df_meta <- rbind(df_meta, separator)

   output1 <- capture.output(write.table(df_meta, sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE))
   output2 <- capture.output(write.table(df, sep=";", row.names=FALSE, quote=FALSE))

   combined_output <- c(output1, output2)
   write.table(combined_output, file=file_out, sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE)
}
