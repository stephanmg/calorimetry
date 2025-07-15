library(dplyr)
library(lubridate)

file_path <- "/home/stephan/Downloads/Daten Alex/ID1_CE_QNRG_pre_2.csv"

reformat_time_for_cosmed <- function(time) {
   td <- seconds_to_period(time)
   sprintf("%02d:%02d", minute(td), second(td))
}


parts <- strsplit(tools::file_path_sans_ext(basename(file_path)), "_")[[1]]
id = gsub("\\D+", "", parts[[1]])

all_data <- read.csv2(file_path, stringsAsFactors = FALSE)

# Define the columns you want to keep
cols_to_keep <- c("Time", "VP", "VO2", "VCO2", "EE",
                  "FeO2", "FeCO2", "FiO2", "FiCO2", "Battery")

# Subset the dataframe to keep only the relevant columns
df <- all_data[ , cols_to_keep]
df$Date <- "01.01.1970"
df$`Animal No.` <- id
df <- df[-c(1,3), ]
print(df$Time)

df <- df %>% rename("VO2(3)" = "VO2")
df <- df %>% rename("VCO2(3)" = "VCO2")

cols_to_keep <- c("Time", "VP", "VO2(3)", "VCO2(3)", "EE",
                  "FeO2", "FeCO2", "FiO2", "FiCO2", "Battery")



colnames(df) <- rep("", ncol(df))
print(df)

#df$ID <- id

print(colnames(df))
print(df$Time)

header <- data.frame(matrix(ncol = length(cols_to_keep)+2, nrow = 0)) # +1 for date, and +1 for animal ID

print(length(cols_to_keep))

fileinfo <- c(file_path, rep("", 12))
extendedinfo <- c("", "TSE Labmaster V6.3.3 (2017-3514)", rep("", 10))
boxInfo <- c("Box", "Animal No.", "Weight [g]", rep("", 10))
print("fileinfo:")
print(fileinfo)
print("extnededinfo:")
print(extendedinfo)
print("boxinfo:")
print(boxInfo)

print("Here?")

header[nrow(header) + 1, ] <- fileinfo
header[nrow(header) + 1, ] <- extendedinfo
header[nrow(header) + 1, ] <- boxInfo
header[nrow(header) + 1, ] <- c(id, id, 0, rep("", 9))
print("foo:")

units <- c("", "[ml/h]", "[ml/h]", "[ml/h]", "[kcal/day]", "[%]", "[%]", "[%]", "[%]", "[%]", rep("", 3))

header[nrow(header) + 1, ] <- rep("", 13)

header[nrow(header) + 1, ] <- c(cols_to_keep, "Date", "Animal No.")
header[nrow(header) + 1, ] <- units

print("Header")
print(header)

colnames(header) <- c(cols_to_keep, "Date", "Animal No.")
colnames(df) <- c(cols_to_keep, "Date", "Animal No.")

full_data <- rbind(header, df)

print(full_data)
write.table(full_data, "test2.csv", col.names=FALSE, row.names=FALSE, sep=";", quote=FALSE)
