library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggExtra)
library(plotly)

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
# get constant and value pairs from metadata sheet
################################################################################
get_constants <- function(file) {
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))
   constant_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "constants"))) %>% pull(ind)
   constants <- df %>% slice(constant_index[2])
   constants$`1` <- NULL
   constants <- constants[!is.na(constants)]
   constants_values <- df %>% slice(constant_index[2] + 1)
   constants_values$`1` <- NULL
   constants_values <- constants_values[!is.na(constants_values)]
   df_meta <- data.frame(constant = constants, value = constants_values)
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

   lean_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "lean_mass"))) %>% pull(ind)
   fat_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "fat_mass"))) %>% pull(ind)
   id_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "personal_ID"))) %>% pull(ind)
   diet_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "diet_group"))) %>% pull(ind)
   genotype_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "genotype_group"))) %>% pull(ind)
   body_weight_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "body weight"))) %>% pull(ind)
   sexes_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "sex"))) %>% pull(ind)
   age_index <- df %>% mutate(ind = row_number()) %>% filter(if_any(everything(), ~str_detect(., "age"))) %>% pull(ind)

   # number of samples
   ids <- df %>% slice(id_index)
   ids$`1` <- NULL
   samples <- ids[!is.na(ids)]

   # fat mass
   fats <- df %>% slice(fat_index)
   fats$`1` <- NULL
   fats <- fats[!is.na(fats)]

   # lean mass
   leans <- df %>% slice(lean_index)
   leans$`1` <- NULL
   leans <- leans[!is.na(leans)]

   # body weights
   body_weights <- df %>% slice(body_weight_index)
   body_weights$`1` <- NULL
   body_weights <- body_weights[!is.na(body_weights)]

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

   # return compiled metadata
   df_meta <- data.frame(lean_mass = leans, fat_mass = fats, Animals = as.factor(samples), Diet = as.factor(diets), Genotype = as.factor(genotypes), body_weight = body_weights, Sex = as.factor(sexes), Age = as.numeric(ages))
   print("df_meta:")
   print(df_meta)
   return(df_meta)
}
