library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggExtra)
library(plotly)
library(shinyalert)

################################################################################
#' get_study_description_from_metadata
#' 
#' This function extracts study description from metadata sheet
#' 
#' Note: If more metadata is relevant, these have to be extracted here.
#' We can in theory pull all information from a fully filled metadata sheet,
#' or from the expected/specified fields from the metadata converter which
#' writes an abridged, thus not full, metadata sheet for usage in the app
#' @param file input file
#' @examples 
#' get_study_description_from_metadata(input_file)
#' @export
#' 
################################################################################
get_study_description_from_metadata <- function(file) {
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))
   title <- df %>% filter(if_any(everything(), ~str_detect(., "Title"))) %>% slice(1)
   comment <- df %>% filter(if_any(everything(), ~str_detect(., "Date"))) %>% slice(1)
   strain <- df %>% filter(if_any(everything(), ~str_detect(., "mouse_strain"))) %>% slice(1)
   system <- df %>% filter(if_any(everything(), ~str_detect(., "Group"))) %>% slice(1)
   date <- df %>% filter(if_any(everything(), ~str_detect(., "Date"))) %>% slice(1)
   name <- df %>% filter(if_any(everything(), ~str_detect(., "Name"))) %>% slice(1)
   number_of_samples <- length(df %>% filter(if_any(everything(), ~str_detect(., "Name")))) - 1 # first column is identifier name
   number_of_sexes <- length(levels(as.factor(as.character(unlist(df %>% filter(if_any(everything(), ~str_detect(., "sex")))))))) - 1
   number_of_diets <- length(levels(as.factor(as.character(unlist(df %>% filter(if_any(everything(), ~str_detect(., "diet_group")))))))) - 1
   number_of_genotypes <- length(levels(as.factor(as.character(unlist(df %>% filter(if_any(everything(), ~str_detect(., "genotype_group")))))))) - 1
   return(list(
      study_name = title$`2`,
      comment = comment$`2`,
      mouse_strain = ifelse(is.null(strain$`2`), "Not specified", strain$`2`),
      lab = system$`2`,
      date = date$`2`,
      name = name$`2`,
      number_of_samples = number_of_samples,
      number_of_genotypes = number_of_genotypes,
      number_of_diets = number_of_diets,
      number_of_sexes = number_of_sexes
   ))
}

################################################################################
#' get_covariates_and_units
#' 
#' This function gets covariates with units from metadata sheet
#' @param file input_file
#' @examples 
#' get_covariates_and_units(input_file)
#' @export
################################################################################
get_covariates_and_units <- function(file) {
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))

   # covariates
   covariate_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "covariates"))) %>% pull(ind)
   if (length(covariate_index) > 1) { covariate_index = covariate_index[2]}
   covariates <- df %>% slice(covariate_index)
   covariates$`1` <- NULL
   covariates <- covariates[!is.na(covariates)]
   covariates_values <- df %>% slice(covariate_index + 1)
   covariates_values$`1` <- NULL
   covariates_values <- covariates_values[!is.na(covariates_values)]

   # units
   unit_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "unit"))) %>% pull(ind)
   if (length(unit_index) > 1) { unit_index = unit_index[2] }
   units <- df %>% slice(unit_index)
   units$`1` <- NULL
   units <- units[!is.na(units)]
   units_values <- df %>% slice(unit_index)
   units_values$`1` <- NULL
   units_values <- units_values[!is.na(units_values)]

   # check for empty metadata
   df_meta <- data.frame()
   if ( ! ( (length(covariates) == 0) || (length(units_values) == 0))) {
      df_meta <- data.frame(covariates, units_values)            
   }
   return(df_meta)
}

################################################################################
#' get_constants
#' 
#' This function gets constant and value pairs from metadata sheet
#' @param file input file
#' @examples 
#' get_constants(input_file)
#' @export
################################################################################
get_constants <- function(file) {
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))
   constant_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "constants"))) %>% pull(ind)
   if (length(constant_index) > 1) { constant_index = constant_index[2]}
   constants <- df %>% slice(constant_index)
   constants$`1` <- NULL
   constants <- constants[!is.na(constants)]
   constants_values <- df %>% slice(constant_index + 1)
   constants_values$`1` <- NULL
   constants_values <- constants_values[!is.na(constants_values)]
   df_meta <- data.frame()

   # miraculously this does not fail locally as it should, server R version seems 
   # to be more strict, will throw zero length vector error with undefined type set
   if (! ((length(constants) == 0) || (length(constants_values) == 0))) {
     df_meta <- data.frame(constant = constants, value = constants_values)
   }
   return(df_meta)
}

################################################################################
#' get_true_metadata
#'  
#' This function loads the true metadata from the Metadata Sheet (.xlsx)
#' @param file input file
#' @param load_example_data TRUE if example data should be loaded (default: FALSE)
#' @examples 
#' get_true_metadata(input_file, FALSE)
#' @export
################################################################################
get_true_metadata <- function(file, load_example_data) {
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))

   from_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "Sample-Section"))) %>% pull(ind)
   to_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "Sub-Sample Section"))) %>% pull(ind)

   df <- df %>% slice(from_index:to_index[1])

   # indices of metadata from sheet
   # TODO: Adapt to get all sample-sections between Sample-Section .. Sub-Sample Section do not manually specify these.
   # Manually get only the absolute minimum required, Diet, Genotype, and also maybe these we do not want to get.
   lean_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "lm_start"))) %>% pull(ind)
   lean_index_end <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "lm_end"))) %>% pull(ind)
   fat_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "fm_start"))) %>% pull(ind)
   fat_index_end <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "fm_end"))) %>% pull(ind)
   id_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "personal_ID"))) %>% pull(ind)
   diet_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "diet_group"))) %>% pull(ind)
   genotype_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "genotype_group"))) %>% pull(ind)
   body_weight_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "bw_start"))) %>% pull(ind)
   body_weight_index_end <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "bw_end"))) %>% pull(ind)
   sexes_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "sex"))) %>% pull(ind)
   age_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "age"))) %>% pull(ind)
   training_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "training"))) %>% pull(ind)
   treatment_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "treatment_group"))) %>% pull(ind)
   intervention_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "intervention"))) %>% pull(ind)

   # number of samples
   ids <- df %>% slice(id_index)
   ids$`1` <- NULL
   samples <- ids[!is.na(ids)]

   print("samples:")
   print(samples)

   # fat mass (start)
   fats <- df %>% slice(fat_index)
   fats$`1` <- NULL
   fats <- fats[!is.na(fats)]

   print("fats:")
   print(fats)
   # fat mass (end)
   fats_end <- df %>% slice(fat_index_end)
   fats_end$`1` <- NULL
   fats_end <- fats_end[!is.na(fats_end)]

   # lean mass (start)
   leans <- df %>% slice(lean_index)
   leans$`1` <- NULL
   leans <- leans[!is.na(leans)]

   print("leans:")
   print(leans)

   # lean mass (end)
   leans_end <- df %>% slice(lean_index_end)
   leans_end$`1` <- NULL
   leans_end <- leans_end[!is.na(leans_end)]

   print("leans_end:")
   print(leans_end)

   # body weights (start)
   body_weights <- df %>% slice(body_weight_index)
   body_weights$`1` <- NULL
   body_weights <- body_weights[!is.na(body_weights)]

   print("body_weihts:")
   print(body_weights)

   # body weights (end)
   body_weights_end <- df %>% slice(body_weight_index_end)
   body_weights_end$`1` <- NULL
   body_weights_end <- body_weights_end[!is.na(body_weights_end)]

   print("body_weights end")
   print(body_weights_end)

   # genotypes
   genotypes <- df %>% slice(genotype_index)
   genotypes$`1` <- NULL
   genotypes <- genotypes[!is.na(genotypes)]

   print("gfenotpyes:")
   print(genotypes)

   # diets
   diets <- df %>% slice(diet_index)
   diets$`1` <- NULL
   diets <- diets[!is.na(diets)]

   print("diets:")
   print(diets)

   # sexes
   sexes <- df %>% slice(sexes_index)
   sexes$`1` <- NULL
   sexes <- sexes[!is.na(sexes)]

   print("sexes:")
   print(sexes)

   # ages
   ages <- df %>% slice(age_index)
   ages$`1` <- NULL
   ages <- ages[!is.na(ages)]

   print("ages:")
   print(ages)

   # training
   training <- df %>% slice(training_index)
   training$`1` <- NULL
   training <- training[!is.na(training)]

   # treatment
   treatment <- df %>% slice(treatment_index)
   treatment$`1` <- NULL
   treatment <- treatment[!is.na(treatment)]

   # intervention
   intervention <- df %>% slice(intervention_index)
   intervention$`1` <- NULL
   intervention <- intervention[!is.na(intervention)]

   print("training:")
   print(training)

   # compile metadata and we require that all fields above are contained within the metadata sheet
   df_meta <- try({
      data.frame(Intervention=intervention, Treatment=treatment, Training = training, lm_start = leans, lm_end = leans_end, fm_start = fats, fm_end = fats_end, Animals = as.factor(samples), Diet = as.factor(diets), Genotype = as.factor(genotypes), bw_start = body_weights, bw_end = body_weights_end, Sex = as.factor(sexes), Age = as.numeric(ages))
   }, silent = TRUE)

      print("metadata we read:")
      print(df_meta)
   # check if metadata has been formatted properly 
   if (inherits(df_meta, "try-error")) {
      shinyalert("Warning", "Metadata sheet is lacking informations. Fallback to data file metadata headers. Required columns: Genotype, Sex, Age, Diet, lm_start, lm_end, fm_start, fm_end, bw_start and bw_end", showCancelButton = TRUE)
      return(NULL)
   }

   # check for all NA columns, which is incorrect metadata and can lead to downstream processing errors
   all_na_columns <- sapply(df_meta, function(col) all(is.na(col)))
   if (any(all_na_columns)) {
      shinyalert("Warning", paste0(paste("The following columns are all NA: ", names(all_na_columns[all_na_columns]), collapse= ","), showCancelButton = TRUE, " Fallback to TSE metadata header, since Metadata sheet wrongly formatted. Required information missing: Genotype, Sex, Age, Diet, lean_mass, fat_mass and body_weight are required."))
      return(NULL)
   }

   # return the compiled metadata
   return(df_meta)
}
