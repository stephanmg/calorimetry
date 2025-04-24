# Load required libraries
library(readr)
library(dplyr)
library(stringr)

#############################################################################
#' import_pheno_v5
#' 
#' This function adds PhenoMaster loader v5 for column-based datasets
#' 
#' @param file input file
#' @param file output file
#' @export
#############################################################################
import_pheno_v5 <- function(file, file_out) {
  # Define header
  header <- "TSE study converted by Shiny-Calorie v0.4.0;;;;;;;;;;;;;;;;\nTSE PhenoMaster V5.3.3 (2015-4474);;;;;;;;;;;;;;;"

  # Find number of rows to skip
  file_path <- file$datapath
  lines <- readLines(file_path)
  skiprows <- which(lines == "")[1] - 1

  # Read metadata
  df_meta <- read_delim(file_path, delim = ";", skip = 2, col_names = FALSE, n_max = skiprows - 2)

  # Process rows and normalize column names
  lines_to_write <- lapply(seq_len(nrow(df_meta)), function(index) {
    row <- unlist(df_meta[index, ])
    if (length(row) != 16) {
      row <- c(row, rep("", 16 - length(row)))
    }
    row <- str_replace_all(row, c("genotype" = "Genotype", "diet" = "Diet", "weight_week0[g]" = "Weight [g]"))
    return(row)
  })

  # Convert metadata to mappings
  box <- df_meta[[1]][-1]
  animals <- df_meta[[2]][-1]
  mapping <- setNames(box, animals)
  reverse_mapping <- setNames(animals, box)

  # Read the main data after skipping metadata rows
  df <- read_delim(file_path, delim = ";", col_names = FALSE, skip = skiprows)

  # Identify date indices
  date_col <- 1  # Adjusting to R's 1-based indexing
  date_indices <- which(!is.na(df[[date_col]]))
  date_indices <- date_indices[-1]  # Exclude the first index

  # Group rows by date indices
  rows_grouped <- lapply(seq_along(date_indices), function(i) {
    start <- date_indices[i]
    end <- ifelse(i < length(date_indices), date_indices[i + 1] - 1, nrow(df))
    df[start:end, ]
  })

  # Read additional lines for metadata extraction
  line2 <- read_delim(file_path, delim = ";", col_names = FALSE, skip = skiprows, n_max = 1)
  line3 <- read_delim(file_path, delim = ";", col_names = FALSE, skip = skiprows + 1, n_max = 1)

  # Extract non-empty columns and box indices
  non_empty <- which(!is.na(line2))
  box_indices <- non_empty[grep("^Box-", line2[non_empty])]

  # Extract names and units
  names_units <- c("", as.character(df[3, 2:(box_indices[2] - 1)]), "", "")

  # Calculate offset between box indices
  offset <- box_indices[2] - box_indices[1]

  # Initialize lists for measurements and metadata
  meas <- list()
  metadata <- list()
  boxes <- list()

  # Group rows by day and extract relevant data
  for (day in seq_along(rows_grouped)) {
    row_group <- rows_grouped[[day]]
  }

  # Extract rows for day and box-specific data 
  for (day in seq_along(rows_grouped)) {
    row_group <- rows_grouped[[day]]
    
    for (box_index in box_indices) {
      # Example processing of the row group and offset logic (to match Python behavior)
      box_data <- row_group[, (box_index):(box_index + offset)]
      meas[[day]] <- rbind(if (exists("meas[[day]]")) meas[[day]] else NULL, box_data)
      
      # Example: append metadata and box-specific data
      metadata[[day]] <- c(metadata[[day]], box_data[[1]])
      boxes[[day]] <- c(boxes[[day]], box_index)
    }
  }

  # Combine measurement, metadata, and other derived information
  combined_data <- list(
    measurements = meas,
    metadata = metadata,
    boxes = boxes
  )

  # Perform additional data restructuring and cleanup as needed
  processed_data <- do.call(rbind, lapply(combined_data$measurements, as.data.frame))
  write.table(processed_data, file_out, col.names = FALSE, row.names = FALSE, sep = ";", quote = FALSE)
}
