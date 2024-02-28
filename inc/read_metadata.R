library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggExtra)
library(plotly)


get_true_metadata <- function(file) {
   ## read metadata
   #file <- '/home/stephan/Downloads/MetaDataSheet_UCP1 KO_cb8eb8c0-aa63-4a25-a495-6eaa8dc4a243 (2).xlsx'
   df <- read_excel(file)
   colnames(df) <- seq(1, length(colnames(df)))

   from_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'Sample-Section'))) %>% pull(ind)
   to_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'Sub-Sample Section'))) %>% pull(ind)

   df <- df %>% slice(from_index:to_index[1])

   lean_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'lean_mass'))) %>% pull(ind)
   fat_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'fat_mass'))) %>% pull(ind)
   id_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'personal_ID'))) %>% pull(ind)
   diet_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'diet_group'))) %>% pull(ind)
   genotype_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'genotype_group'))) %>% pull(ind)
   body_weight_index <- df %>% mutate(ind=row_number()) %>% filter(if_any(everything(), ~str_detect(., 'body weight'))) %>% pull(ind)


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

   df_meta <- data.frame(lean_mass=leans, fat_mass=fats, Animals=as.factor(samples), Diet=diets, Genotype=genotypes, body_weight=body_weights)
   write.csv(df_meta, "testen_meta.csv")

      # TODO: rename animal id no to id or vice versa before join
   ## combine with actual data

#   df_data <- read.csv2('finalC1.csv')
#   print(head(df_data))

#   all_data <- df_data %>% full_join(y=df_meta, by=c("id"))
#   print(head(all_data))
#   write.csv(all_data, "all_data_merged_with_metadata.csv")

   return(df_meta)
}
