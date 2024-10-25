library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggExtra)
library(plotly)
library(shinyalert)

################################################################################
# get study description
################################################################################
get_study_description_from_metadata <- function(file) {
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))
   title <- df %>% filter(if_any(everything(), ~str_detect(., "Title"))) %>% slice(1)
   comment <- df %>% filter(if_any(everything(), ~str_detect(., "comment"))) %>% slice(1)
   strain <- df %>% filter(if_any(everything(), ~str_detect(., "name of mouse strain"))) %>% slice(1)
   system <- df %>% filter(if_any(everything(), ~str_detect(., "Experimental System"))) %>% slice(2)
   return(paste0(title$`2`, " (", comment$`2`, ") with ", strain$`2`, " (", system$`2`, ")"))
}

################################################################################
# get covariates and units
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

   # miraculously this does not fail locally as it should, server R version seems 
   # to be more strict, will throw zero length vector error with undefined type set
   df_meta <- data.frame()
   if ( ! ( (length(covariates) == 0) || (length(units_values) == 0))) {
      df_meta <- data.frame(covariates, units_values)            
   }
   print(df_meta)
   return(df_meta)
}

################################################################################
# get constant and value pairs from metadata sheet
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
   print(df_meta)
   return(df_meta)
}

################################################################################
# get full metadata from metadata sheet
################################################################################
get_true_metadata <- function(file) {
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))

   from_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "Sample-Section"))) %>% pull(ind)
   to_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "Sub-Sample Section"))) %>% pull(ind)

   df <- df %>% slice(from_index:to_index[1])

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

   # number of samples
   ids <- df %>% slice(id_index)
   ids$`1` <- NULL
   samples <- ids[!is.na(ids)]

   # fat mass (start)
   fats <- df %>% slice(fat_index)
   fats$`1` <- NULL
   fats <- fats[!is.na(fats)]

   # fat mass (end)
   fats_end <- df %>% slice(fat_index_end)
   fats_end$`1` <- NULL
   fats_end <- fats_end[!is.na(fats_end)]

   # lean mass (start)
   leans <- df %>% slice(lean_index)
   leans$`1` <- NULL
   leans <- leans[!is.na(leans)]

   # lean mass (end)
   leans_end <- df %>% slice(lean_index_end)
   leans_end$`1` <- NULL
   leans_end <- leans_end[!is.na(leans_end)]

   # body weights (start)
   body_weights <- df %>% slice(body_weight_index)
   body_weights$`1` <- NULL
   body_weights <- body_weights[!is.na(body_weights)]

   # body weights (end)
   body_weights_end <- df %>% slice(body_weight_index_end)
   body_weights_end$`1` <- NULL
   body_weights_end <- body_weights_end[!is.na(body_weights_end)]

   # genotypes
   genotypes <- df %>% slice(genotype_index)
   genotypes$`1` <- NULL
   genotypes <- genotypes[!is.na(genotypes)]

   # diets
   diets <- df %>% slice(diet_index)
   diets$`1` <- NULL
   diets <- diets[!is.na(diets)]

   # sexes
   sexes <- df %>% slice(sexes_index)
   sexes$`1` <- NULL
   sexes <- sexes[!is.na(sexes)]

   # ages
   ages <- df %>% slice(age_index)
   ages$`1` <- NULL
   ages <- ages[!is.na(ages)]

   # compile metadata and we require that all fields above are contained within the metadata sheet
   df_meta <- try({
      data.frame(lm_start = leans, lm_end = leans_end, fm_start = fats, fm_end = fats_end, Animals = as.factor(samples), Diet = as.factor(diets), Genotype = as.factor(genotypes), bw_start = body_weights, bw_end = body_weights_end, Sex = as.factor(sexes), Age = as.numeric(ages))
   }, silent = TRUE)

   # check if metadata has been formatted properly 
   if (inherits(df_meta, "try-error")) {
      shinyalert("Warning", "Metadata sheet wrongly formatted. Please check within Excel for correctness. Fallback to TSE metadata header. Required information missing: Genotype, Sex, Age, Diet, lean_mass, fat_mass and body_weight are required.", showCancelButton = TRUE)
      return(NULL)
   }

   # check for all NA columns, which is incorrect metadata and can lead to downstream processing errors
   all_na_columns <- sapply(df_meta, function(col) all(is.na(col)))
   if (any(all_na_columns)) {
      shinyalert("Warning", paste0(paste("The following columns are all NA: ", names(all_na_columns[all_na_columns]), collapse= ","), showCancelButton = TRUE, " Fallback to TSE metadata header, since Metadata sheet wrongly formatted. Required information missing: Genotype, Sex, Age, Diet, lean_mass, fat_mass and body_weight are required."))
      return(NULL)
   }

   # return the compiled metadata
   print(df_meta)
   return(df_meta)

}
