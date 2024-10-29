source("inc/constants.R")
source("inc/metadata/read_metadata.R")
library(glue)

################################################################################
# style plot
################################################################################
style_plot <- function(p, input) {
      # TODO: it is easier to configure the plot a-priori, i.e. before we convert to a plotly object with ggplotly, see how to do do this, convert later?
      # if plot has been rendered we display stylize_plot checkbox in UI, then we can trigger stylizing of the plot
      # we need to use the ggplot2 object to make all modificatiosn required, the below options might not alway work too
      if (input$stylize_plot) {
         p <- p %>% layout(xaxis = list(title=input$stylize_plot_axes_x_axis_label, tickfont = list(size=input$stylize_plot_axes_x_axis_tickfont_size, color=input$stylize_plot_axes_x_axis_color), font = list(size=input$stylize_plot_axes_x_axis_font_size, color=input$stylize_plot_axes_x_ticks_color)))
         p <- p %>% layout(yaxis = list(title=input$stylize_plot_axes_y_axis_label, tickfont = list(size=input$stylize_plot_axes_y_axis_tickfont_size, color=input$stylize_plot_axes_y_axis_color), font = list(size=input$stylize_plot_axes_y_axis_font_size, color=input$stylize_plot_axes_y_ticks_color)))
         p <- p %>% layout(title = list(text=input$stylize_plot_theme_and_title_title_label, font = list(input$stylize_plot_theme_and_title_title_font_size, color=input$stylize_plot_theme_and_title_title_color)))
         p <- p %>% layout(font = list(family=input$stylize_plot_general_font_family, size=input$stylize_plot_theme_and_title_font_size))
         p <- p %>% layout(width = input$stylize_plot_general_width, height=input$stylize_plot_general_height)
      }
      return(p)
}

################################################################################
# indicate if plot has been rendered or not
################################################################################
indicate_plot_rendered <- function(p, output) {
      output$plotRendered <- reactive({
         !is.null(p)
      })
      outputOptions(output, "plotRendered", suspendWhenHidden = FALSE)
}

################################################################################
# generate statistical table in case we have multiple comparisons
################################################################################
generate_statistical_table <- function(results) {
   if (length(results$statistics$p) == 1) { # only one comparison, e.g. WT vs KO, i.e. only one row in table
      return(tags$tr(
      tags$td(process_return_value_for_statistic(results$statistics$p, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$p.adj, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$p.adj.signif, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$df, FALSE), style="width: 100px"),
      tags$td(process_return_value_for_statistic(results$statistics$statistic, FALSE), style="width: 100px")
      ))
   } else { # for multiple testing with ANCOVA, we need all (n over p) groups to be summarized in a table with multiple rows
      num_rows <- length(results$statistics$p)
      rows_p_value <- lapply(seq_along(results$statistics$p), function(i) {
         tags$tr(
         tags$td(process_return_value_for_statistic(results$statistics$p[i], FALSE)),
         tags$td(process_return_value_for_statistic(results$statistics$p.adj[i], FALSE)),
         tags$td(process_return_value_for_statistic(results$statistics$p.adj.signif[i], FALSE)),
         tags$td(process_return_value_for_statistic(results$statistics$df[i], FALSE)),
         tags$td(process_return_value_for_statistic(results$statistics$statistic[i], FALSE))
         )
      })
      return(rows_p_value)
   }
}

################################################################################
# process return value and creates UI if requested
################################################################################
process_return_value_for_statistic <- function(value, as_ui=TRUE) {
   if (is.numeric(value) && length(value) == 1) {
      return(round(value, digits=6))
   } else {
      # create UI if requested
      if (is.numeric(value)) {
         if (as_ui) {
               rows <- lapply(seq_along(value), function(i) {
               tags$tr(
                  tags$td(value[i]))
            })
            return(rows) 
         } else {
           return(paste(round(value, digits=6), collapse=","))
         }
      } else {
         if (as_ui) {
            rows <- lapply(seq_along(value), function(i) {
               tags$tr(
                  tags$td(value[i]))
            })
            return(rows)
         } else {
            return(paste(value, collapse=","))
         }
      }
   }
}

################################################################################
# coarsening data sets
#' coarsen_data_sets
#' 
#' This function coarsens a data frame according to the numerical coarsening factor
#' 
#' @param df data frame
#' @param coarsening_factor coarsening factor
#' @examples 
#' coarsen_data_sets(values, 2)
#' @export 
################################################################################
coarsen_data_sets <- function(df, coarsening_factor) {
   # assumes that the data frame is sorted ascending in time, which should be the case
   df_filtered <- df %>% group_by(`Animal No._NA`) %>% mutate(row_num = row_number()) %>% filter(row_num == 1 | row_num %% coarsening_factor == 0) %>% select(-row_num)
   df_filtered <- df_filtered %>% mutate(diff.sec = diff.sec * coarsening_factor)
   return(df_filtered)
}

################################################################################
# remove zero values
#' remove_zero_values
#'
#' This function removes zero measurement values, based on epsilon threshold
#'
#' @param df data frame
#' @param eps epsilon threshold
#' @examples
#' remove_zero_values(values, 0.0001)
#' @export
################################################################################
remove_zero_values <- function(df, eps) {
   target_columns <- c("O2_[%]", "CO2_[%]", "VCO2_[ml/h]", "VO2_[ml/h]")
   available_columns = intersect(target_columns, colnames(df))
   df_filtered <- df
   if (length(available_columns) > 0) {
      df_filtered <- df %>% filter(if_any(all_of(available_columns), ~abs(.) > eps))
   }
   return(df_filtered)
}

################################################################################
# remove z score outliers
################################################################################
#' remove_z_score_outliers
#'
#' This function removes outliers based on multiples of the standard deviation
#' 
#' @param df data frame
#' @param sd multiple of the standard deviation (default=1)
#' @examples
#' remove_z_score_outliers(values, sd=2)
#' @export
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
#' enrich_with_metadata
#' 
#' Enrich data frame with metadata
#' 
#' @param finalC1 data frame
#' @param C1meta metadata from data files in finalC1
#' @param havemetadatafile boolean to indicate if we have structured metadata sheet
#' @param metadatafile structured metadata sheet
#' @examples 
#' enrich_with_metadata(values, metadata_basic, TRUE, "metadata.xlsx")
#' @export
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

 # add some calculated metadata 
   delta_pairs <- list(
      c("bw_start", "bw_end", "delta_bw"),
      c("fm_start", "fm_end", "delta_fm"),
      c("lm_start", "lm_end", "delta_lm")
   )
   

    add_delta_columns <- function(df, column_pairs) {
    for (pair in column_pairs) {
      start_col <- pair[1]
      end_col <- pair[2]
      delta_col <- pair[3]
      if (all(c(start_col, end_col) %in% names(df)) && !(delta_col %in% names(df))) {
         df <- df %>%
         mutate(!!delta_col := as.numeric(!!sym(end_col)) - as.numeric(!!sym(start_col)))
      }
   }
   return(df)
   }

   metadata <- add_delta_columns(metadata, delta_pairs)

   print("columns:")
   print(names(metadata))


      # instead na.omit() we need to use select to remove columns which are all NA before joining, 
      # this might be because we have columns, e.g. dob which are not present at all in TSE files and also not in the metadata sheet as well
      # or also if not all cohort  files are used with the animal ids recorded in the metadata sheet, or: if
      # the metadata sheet contains nan values... for i.e. lm_start etc.
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
      colnames(df_filtered)[colnames(df_filtered) == "Animal"] <- "Animals"
      df_filtered$Animals <- as.factor(df_filtered$Animals)
      for (col in colnames(df_filtered)) {
         if (col %in% c("Sex", "Diet", "Genotype", "Box", "Box_NA", "Dob")) { # factor columns from TSE standard header
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
# detect day night alternative, as the other method seems not to be correct
################################################################################
detect_day_night <- function(df, offset) {
   df_day_night <- df
   day_label = "Day"
   night_label = "Night"
    if (offset > 0) {
      day_label = "Night"
      night_label = "Day"
   }

   df_day_night$NightDay <- ifelse(df_day_night$running_total.hrs < offset, day_label, NA)

   day_label = "Day"
   night_label = "Night"
   for (i in 1:nrow(df_day_night)) {
         if (is.na(df_day_night$NightDay[i]) && df_day_night$running_total.hrs[i] > offset) {
            interval_index <- floor((df_day_night$running_total.hrs[i] - offset) / 12)
            df_day_night$NightDay[i] <- ifelse(interval_index %% 2 == 0, day_label, night_label)
         }
   }
   return(df_day_night)
}

################################################################################
# get global offset for day/night: when (hour) does the very first experiment start
################################################################################
get_global_offset_for_day_night <- function(df) {
   write.csv2(df, "before_getting_global_offset.csv")
   # Note: if this method or the method below is applied again to a filtered or
   # day/night selected data frame, we might not have any row with running_total.sec == 0.
   # thus, care if required when applying this method, make sure to shift the data frame
   # to 0 before, otherwise we will never find a row with running_total.sec == 0.
   offsets <- df %>% group_by(`Animal No._NA`) %>% filter(running_total.sec == 0) %>% select(Datetime, `Animal No._NA`) %>% as.data.frame()
   offsets <- offsets %>% mutate(offset = format(as.POSIXct(Datetime, format="%d/%m/%Y %H:%M"), "%H")) %>% select(offset, `Animal No._NA`)
   return(min(offsets$offset))
}

################################################################################
# convert df to zeitgeber zeit
#' zeitgeber_zeit
#' 
#' Converts the time to zeitgeber zeit
#' @param df data frame
#' @param light_on indicates when day starts (light on typically)
#' @examples 
#' zeitgeber_zeit(values, 8)
#' @export
################################################################################
zeitgeber_zeit <- function(df, light_on) {
   # TODO: this needs to be revised, if one want to select indvidual calendrical days, because running_total.sec == 0 will not be found
   # TODO: For RMR  this is grouped, need to see why
   write.csv2(apply(df, 2, as.character), "directly_before_offsets.csv")
   offsets <- df %>% group_by(`Animal No._NA`) %>% filter(running_total.sec == 0) %>% select(Datetime, `Animal No._NA`) %>% as.data.frame()
   offsets <- offsets %>% mutate(offset = format(as.POSIXct(Datetime, format="%d/%m/%Y %H:%M"), "%H")) %>% select(offset, `Animal No._NA`)
   offsets$`offset`  <- as.numeric(offsets$`offset`)
   offsets$offset2 <- offsets$offset - (offsets$offset - light_on)
   offsets$offset3 <- light_on - offsets$offset 

   #offsets$offset <- offsets$offset + (light_on - min(offsets$offset)) - light_on
   offsets <- offsets %>% unique()
   df_joined <- df %>% left_join(offsets, by = "Animal No._NA")
   df_joined <- df_joined %>% mutate(running_total.hrs = running_total.hrs + offset3)
   df_joined <- df_joined %>% mutate(running_total.hrs.halfhour = running_total.hrs.halfhour + offset3)
   return(df_joined)
}

################################################################################
# pretty print variable 
################################################################################
pretty_print_equation <- function(equation) {
   pretty_equation <- gsub("Heldmaier1", "Heldmaier #1", equation)
   pretty_equation <- gsub("Heldmaier2", "Heldmaier #2", pretty_equation)
   return(pretty_equation)
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
         pretty_label <- gsub("body_weight", paste0("body weight [", metadata %>% filter(covariates == "body weight") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("lean_mass", paste0("lean mass [", metadata %>% filter(covariates == "lean_mass") %>% pull("units_values"), "]"), pretty_label)
         pretty_label <- gsub("fat_mass", paste0("fat mass [", metadata %>% filter(covariates == "fat_mass") %>% pull("units_values"), "]"), pretty_label)
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

   print("minimum?")
   print(df[[input_var]])
   print(input_var)
   print(min(df[[input_var]]))

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
# get number of days and sample ids to select from alternative
################################################################################
get_days_and_animals_for_select_alternative <- function(df) {
   day_counts <- df %>% pull(DayCount) %>% unique() %>% sort()
   animal_counts <- df %>% pull(`Animal No._NA`) %>% unique() %>% sort()
   return(list(days=day_counts, animals=animal_counts))
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
   # TODO: should non-factor columns be numeric? in ANOVA/ANCOVA covariates should be numeric
   return(names(df)[sapply(df, Negate(is.factor))])
	#non_factor_columns <- sapply(df, Negate(is.factor))
   #return(names(df)[non_factor_columns & sapply(df, is.numeric)])
}

################################################################################
# generate new download buttons
################################################################################
get_new_download_buttons <- function(plot_div='') {
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
      click = htmlwidgets::JS(glue("
         function(gd) {{
            var plotDiv;
            if ('' === '{plot_div}') {{
               // Basic plot should always be visible, that is the first plot for each panel, if not specified a desired
               // plot, we will as a default pick the basic plot with plotlyOutput identifier 'plot'
               plotDiv = document.getElementById('plot');
            }} else {{
               plotDiv = document.getElementById('{plot_div}');
            }}
            
            /// Debug, remove in production, alert if plot is not visible with JS
            if (! (plotDiv.offsetWidth > 0 && plotDiv.offsetHeight > 0)) {{
               alert('The plot {plot_div} is not visible!')
            }}

            var width = plotDiv.offsetWidth;
            var height = plotDiv.offsetHeight;
            Plotly.toImage(gd, {{format: 'png', width: width, height: height, scale: 4, compression: 'NONE'}}).then(function(dataUrl) {{
               const {{ jsPDF }} = window.jspdf;
               const doc = new jsPDF('landscape');
               const ARimage = width/height;
               /// internal pagesize should be always around 297 (mm) since landscape a4 format
               var widthPDF = doc.internal.pageSize.getWidth(); // or: 280 and 
               var widthPDFoffset = 17; // to avoid image boundaries out of PDF size
               doc.addImage(dataUrl, 'eps', 10, 10, widthPDF-widthPDFoffset, Math.floor(widthPDF/ARimage)); // or: remove -17 and use 280 above
               doc.save('plot.pdf');
            }})
         }}
      "))
      )
   )
   return(new_buttons)
}
