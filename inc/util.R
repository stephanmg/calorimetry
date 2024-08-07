source("inc/constants.R")

################################################################################
# convert df to zeitgeber zeit
################################################################################
zeitgeber_zeit <- function(df, light_on) {
   offsets <- df %>% group_by(`Animal No._NA`) %>% filter(running_total.hrs == 0) %>% pull(Datetime, `Animal No._NA`) %>% as.data.frame()
   colnames(offsets) <- "offset"
   offsets <- offsets %>% mutate_all(~format(as.POSIXct(., format="%d/%m/%Y %H:%M"), "%H"))
   offsets$ID <- rownames(offsets)
   offsets <- offsets %>% select(ID, everything()) %>% rename(`Animal No._NA`=ID)
   offsets$`Animal No._NA`  <- as.numeric(offsets$`Animal No._NA`)
   offsets$`offset`  <- as.numeric(offsets$`offset`)
   rownames(offsets) <- NULL
   offsets$offset <- offsets$offset - min(offsets$offset)
   offsets$offset <- offsets$offset + (light_on - min(offsets$offset)) - light_on
   df_joined <- df %>% left_join(offsets, by = "Animal No._NA")
   df_joined <- df_joined %>% mutate(running_total.hrs = running_total.hrs + offset)
   df_joined <- df_joined %>% mutate(running_total.hrs.halfhour = running_total.hrs.halfhour + offset)
   return(df_joined)
}

################################################################################
# pretty print variable 
################################################################################
pretty_print_variable <- function(variable) {
   pretty_variable <- gsub("O2", "O<sub>2</sub>", variable)
   pretty_variable <- gsub("CO2", "CO<sub>2</sub>", pretty_variable)
   pretty_variable <- gsub("\\(3\\)", "", pretty_variable)
   return(pretty_variable)
}

################################################################################
# pretty print label
################################################################################
pretty_print_label <- function(label) {
   # Weights are always in [g], TODO: get units from metadata sheet
   pretty_label <- gsub("_", " ", label)
   pretty_label <- gsub("body weight", "body weight [g]", pretty_label)
   pretty_label <- gsub("lean mass", "lean mass [g]", pretty_label)
   pretty_label <- gsub("fat mass", "fat mass [g]", pretty_label)
   pretty_label <- gsub("TEE", paste0("TEE [kJ/day]"), pretty_label)
   pretty_label <- gsub("RMR", paste0("RMR [kJ/day]"), pretty_label)
   pretty_label <- gsub("GoxLox", paste0("GoxLox [ml/h]"), pretty_label)
   pretty_label <- gsub("HP", paste0("Energy expenditure [kJ/day]"), pretty_label)
}

################################################################################
# create annotations for Days on x-axis when using zeitgeber zeit
################################################################################
annotate_zeitgeber_zeit <- function(df, light_on, input_var, with_facets=FALSE) {
   df_annotated <- df %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
   day_counts <- df_annotated %>% select(`Animal No._NA`, DayCount) %>% unique() #%>% sort()
   print(day_counts)
   # we set for animals no ID since we are not interested for now only in the total days of recordings and want to select consecutive 3 days for instance
   annotations <- NULL
   # TODO: We do not need this if/else statement, will work also without with_facets, refactor to remove the superfluous parameter
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
      #list(
      #name = "Download plot as a PDF",
      #icon = list(path = svg_path_pdf, width=30, height=30, transform="matrix(0.59119871,0,0,0.48085484,21.328186,-59.600539)"),
      #click = htmlwidgets::JS("
      #   function(gd) {
      #      Plotly.downloadImage(gd, {format: 'pdf', filename: 'plot'});
      #  }
      #")
      #),
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
