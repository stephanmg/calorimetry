source("inc/constants.R")
source("inc/metadata/read_metadata.R")

################################################################################
# coarsening data sets
################################################################################
coarsen_data_sets <- function(df, coarsening_factor) {
   # assumes that the data frame is sorted ascending in time, which should be the case
   df_filtered <- df %>% group_by(`Animal No._NA`) %>% mutate(row_num = row_number()) %>% filter(row_num == 1 | row_num %% coarsening_factor == 0) %>% select(-row_num)
   df_filtered <- df_filtered %>% mutate(diff.sec = diff.sec * coarsening_factor)
   return(df_filtered)
}

################################################################################
# remove zero values
################################################################################
remove_zero_values <- function(df, eps) {
   target_columns <- c("O2_[%]", "CO2_[%]", "VCO2_[ml/h]", "VO2_[ml/h]")
   available_columns = intersect(target_columns, colnames(df))
   df_filtered <- df
   if (length(available_columns) > 0) {
      df_filtered <- df %>% filter(if_any(all_of(available_columns), ~abs(.) < eps))
   }
   return(df_filtered)
}

################################################################################
# remove z score outliers
################################################################################
remove_z_score_outliers <- function(df, sd=1) {
   target_columns <- c("O2_[%]", "CO2_[%]", "VCO2_[ml/h]", "VO2_[ml/h]")
   available_columns = intersect(target_columns, colnames(df))
   df_filtered <- df
   if (length(available_columns) > 0) {
      z_scores <- df %>% 
      select(all_of(available_columns)) %>%
      mutate(across(everything(), ~ scale(.) %>% as.vector()))
      df_with_z <- df %>% bind_cols(z_scores %>% setNames(paste0("z_", available_columns)))
      df_filtered <- df_with_z %>% filter(if_any(starts_with("z_"), ~abs(.) < sd))
      df_filtered <- df_filtered %>% select(-starts_with("z_"))
   } 
   return(df_filtered)
}

################################################################################
# enrich with metadata
################################################################################
enrich_with_metadata <- function(finalC1, C1meta, havemetadata, metadatafile) {
   df <- finalC1
   if (havemetadata) {
      metadata <- get_true_metadata(metadatafile$datapath)
      print(metadata)
      # fall back to TSE metadata
      if (is.null(metadata)) {
         return(enrich_with_metadata(finalC1, C1meta, FALSE, metadatafile))
      }
      # instead na.omit() we need to use select to remove columns which are all NA before joining
      print("before metadata join")
      print(metadata)
      # need to remove all nan columns because individual cohorts might not have the same columns
      df <- finalC1 %>% select(where(~ !all(is.na(.)))) %>% full_join(y = metadata, by = c("Animals")) 
   } else {
      empty_row_index <-which(apply(C1meta[,-1], 1, function(row) all(row == "")))
      rows_to_remove <- unique(c(empty_row_index, empty_row_index+1))
      C1meta <- C1meta[-rows_to_remove[rows_to_remove <= nrow(C1meta)], ]
      df_filtered <- C1meta[, colSums(is.na(C1meta)) == 0]
      df_filtered <- df_filtered[, !grepl("Text", names(df_filtered))]
      df_filtered <- df_filtered[, !grepl("^X", names(df_filtered))]
      colnames(df_filtered)[colnames(df_filtered) == "Box"] <- "Box_NA"
      if ("Animals" %in% colnames(finalC1)) {
         colnames(df_filtered)[colnames(df_filtered) == "Animal.No."] <- "Animals"
      } else {
         colnames(df_filtered)[colnames(df_filtered) == "Animal.No."] <- "Animal No._NA"
      }
      df <- NULL
      if ("Animals" %in% colnames(finalC1)) {
         df <- merge(finalC1, df_filtered, by = "Animals")
      } else {
         df <- merge(finalC1, df_filtered, by = "Animal No._NA")
      }
      colnames(df_filtered)[colnames(df_filtered) == "Animal No._NA"] <- "Animals"
      df_filtered$Animals <- as.factor(df_filtered$Animals)
      for (col in colnames(df_filtered)) {
         if (col %in% c("Sex", "Diet", "Genotype", "Box")) { # factor columns from TSE standard header
            df_filtered[[col]] = as.factor(df_filtered[[col]])
         } # remaning columns are assumed to be numerical and used as covariates
      }
      metadata <- df_filtered 
      # TODO: figure out prime reason why sometimes doubly-joined, potential reason: happens when switching between pnales occassionally?
      # data needs to be filtered because sometimes we double merge the finalC1, creating duplicates
      df <- df %>% select(-ends_with(".y")) %>% rename_with(~ sub("\\.x$", "", .), ends_with(".x"))
   }
   return(list("data"=df, "metadata"=metadata))
}



################################################################################
# convert df to zeitgeber zeit
################################################################################
zeitgeber_zeit <- function(df, light_on) {
   write.csv2(df, "directly_before_offsets.csv")
   offsets <- df %>% group_by(`Animal No._NA`) %>% filter(running_total.sec == 0) %>% select(Datetime, `Animal No._NA`) %>% as.data.frame()
   print("even earlier")
   print(offsets)
   offsets <- offsets %>% mutate(offset = format(as.POSIXct(Datetime, format="%d/%m/%Y %H:%M"), "%H")) %>% select(offset, `Animal No._NA`)
   print("earlier offsets:")
   print(offsets)
   offsets$`offset`  <- as.numeric(offsets$`offset`)
   offsets$offset <- offsets$offset - min(offsets$offset)
   print("offsets:")
   print(offsets)
   offsets$offset <- offsets$offset + (light_on - min(offsets$offset)) - light_on
   offsets <- offsets %>% unique()
   df_joined <- df %>% left_join(offsets, by = "Animal No._NA")
   df_joined <- df_joined %>% mutate(running_total.hrs = running_total.hrs + offset)
   df_joined <- df_joined %>% mutate(running_total.hrs.halfhour = running_total.hrs.halfhour + offset)
   return(df_joined)
}

################################################################################
# pretty print variable 
################################################################################
pretty_print_variable <- function(variable, metadata) {
   pretty_variable <- gsub("O2", "O<sub>2</sub>", variable)
   pretty_variable <- gsub("CO2", "CO<sub>2</sub>", pretty_variable)
   pretty_variable <- gsub("\\(3\\)", "", pretty_variable)
   return(pretty_variable)
}

################################################################################
# pretty print label
################################################################################
pretty_print_label <- function(label, metadata, ee_unit) {
   # remove underscores 
   pretty_label <- gsub("_", " ", label)
   # these units are fixed by convenience of plotting typically
   pretty_label <- gsub("TEE", paste0("TEE [kJ/day]"), pretty_label)
   pretty_label <- gsub("RMR", paste0("RMR [kJ/day]"), pretty_label)
   pretty_label <- gsub("GoxLox", paste0("GoxLox [ml/h]"), pretty_label)
   pretty_label <- gsub("HP", paste0("Energy expenditure [kJ/day]"), pretty_label)
   # get relevant data from metadata
   if (!is.null(metadata)) {
      metadata <- get_covariates_and_units(metadata)
      if (nrow(metadata) > 0) {
         pretty_label <- gsub("body weight", paste0("body weight [", metadata %>% filter(covariates == "body weight") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("lean mass", paste0("lean mass [", metadata %>% filter(covariates == "lean mass") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("fat mass", paste0("fat mass [", metadata %>% filter(covariates == "fat mass") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("Age", paste0("Age [", metadata %>% filter(covariates == "age") %>% pull("units_values"), "]"), pretty_label)
      } else {
         # TSE header assumes body weight always in grams [g]
         pretty_label <- gsub("body weight", paste0("body weight [", "g", "]"), pretty_label)
         pretty_label <- gsub("lean mass", paste0("lean mass [", "g", "]"), pretty_label)
         pretty_label <- gsub("fat mass", paste0("fat mass [", "g", "]"), pretty_label)
      }
   } else {
      # TSE header assumes body weight always in grams [g]
      pretty_label <- gsub("body weight", paste0("body weight [", "g", "]"), pretty_label)
      pretty_label <- gsub("lean mass", paste0("lean mass [", "g", "]"), pretty_label)
      pretty_label <- gsub("fat mass", paste0("fat mass [", "g", "]"), pretty_label)
   }
   # from TSE header there might be Weight available, rename to have a consistent name
   pretty_label <- gsub("Weight..g", "Weight [g]", pretty_label)
   return(pretty_label)
}

################################################################################
# create annotations for Days on x-axis when using zeitgeber zeit
################################################################################
annotate_zeitgeber_zeit <- function(df, light_on, input_var, with_facets=FALSE) {
   df_annotated <- df %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
   day_counts <- df_annotated %>% select(`Animal No._NA`, DayCount) %>% unique() %>% na.omit()

   print(day_counts)
   # we set for animals no ID since we are not interested for now only in the total days of recordings and want to select consecutive 3 days for instance
   annotations <- NULL
   # specify if we wish to use facets or not during annotation
   if (with_facets) {
      animals <- unique(day_counts$`Animal No._NA`)
      for (i in 1:length(animals)) {
         animal = animals[i]
         anno_for_animal = data.frame(Animals = rep(animal, length(sort(unique(day_counts$DayCount)))), x = seq(12+light_on, length(unique(day_counts$DayCount))*24, by=24), y=min(df[[input_var]], na.rm = TRUE), label = sapply(sort(unique(day_counts$DayCount)), function(x) paste0("Day #", x)))
         annotations <- rbind(annotations, anno_for_animal)
      }
   } else {
      annotations = data.frame(Animals = rep(NA, length(sort(unique(day_counts$DayCount)))), x = seq(12+light_on,length(unique(day_counts$DayCount))*24, by=24), y=min(df[[input_var]], na.rm = TRUE), label = sapply(sort(unique(day_counts$DayCount)), function(x) paste0("Day #", x)))
   }
   return(list(df_annotated=df_annotated, annotations=annotations))
}

################################################################################
# get number of days and sample ids to select from in EnergyExpenditure (Data curation)
################################################################################
get_days_and_animals_for_select <- function(df) {
   df_annotated <- df %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
   day_counts <- df_annotated %>% pull(DayCount) %>% unique() %>% sort()
   animal_counts <- df_annotated %>% pull(`Animal No._NA`) %>% unique() %>% sort()
   return(list(days=day_counts, animals=animal_counts))
}

################################################################################
# get factor columns
################################################################################
get_factor_columns <- function(df) {
	return(names(df)[sapply(df, is.factor)])
}

################################################################################
# get non-factor columns
################################################################################
get_non_factor_columns <- function(df) {
	return(names(df)[sapply(df, Negate(is.factor))])
}

################################################################################
# generate new download buttons
################################################################################
get_new_download_buttons <- function() {
   new_buttons <- list(
      list(
      name = "Download plot as a SVG",
      icon = list(path = svg_path_svg, width=30, height=30),
      click = htmlwidgets::JS("
         function(gd) {
            Plotly.downloadImage(gd, {format: 'svg', filename: 'plot'});
         }
      ")
      ),
      list(
      name = "Download plot as a PDF",
      icon = list(path = svg_path_pdf, width=30, height=30, transform="matrix(0.59119871,0,0,0.48085484,21.328186,-59.600539)"),
      click = htmlwidgets::JS("
         function(gd) {
            var plotDiv = document.getElementById('plot');
            var width = plotDiv.offsetWidth;
            var height = plotDiv.offsetHeight;
            Plotly.toImage(gd, {format: 'png', width: width, height: height, scale: 4, compression: 'NONE'}).then(function(dataUrl) {
               const { jsPDF } = window.jspdf;
               const doc = new jsPDF('landscape');
               const ARimage = width/height;
               /// internal pagesize should be always around 297 (mm) since landscape a4 format
               var widthPDF = doc.internal.pageSize.getWidth(); // or: 280 and 
               var widthPDFoffset = 17; // to avoid image boundaries out of PDF size
               doc.addImage(dataUrl, 'eps', 10, 10, widthPDF-widthPDFoffset, Math.floor(widthPDF/ARimage)); // or: remove -17 and use 280 above
               doc.save('plot.pdf');
            })
         }
      ")
      )
   )
   return(new_buttons)
}
