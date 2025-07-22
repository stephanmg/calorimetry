source("inc/session_management.R")

################################################################################
#' prepare_data_frame_for_export
#' 
#' This function prepares the data frame with calculated quantities for export
#' @param df_to_plot data frame
#' @param global_data hash table containing variables during session
#' @param session shiny session
#' @export
################################################################################
prepare_data_frame_for_export <- function(df_to_plot, global_data, session) {
   interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
   df_to_plot$Cohort <- sapply(df_to_plot$Animals, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)
   rename_list <- c("HP"="Heat Production", "HP2"="Heat Production Alternative", "Animal No._NA"="Animal", "Box_NA"="Box", "Datetime Alternative"="Datetime")
   df_to_plot <- df_to_plot %>% rename_with(~rename_list[.x], .cols = intersect(names(rename_list), colnames(df_to_plot)))
   df_to_plot <- df_to_plot %>% select(-all_of(c("Datetime4", "Animals", "offset", "Animal.No.")))
   return(df_to_plot)
}


################################################################################
#' do_export_all_data
#' 
#' This function exports all data calculated during the app usage as a .zip file
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param file_output output file path chosen by user
#' @param do_plotting plotting routines
#' @param global_data hash table containing variables during session
#' @export
################################################################################
do_export_all_data <- function(input, output, session, file_output, do_plotting, global_data) {
   # Individual files for zip archive
   df_df_input <- file.path(tempdir(), "df_raw.csv")
   df_df_output <- file.path(tempdir(), "df_energy_expenditure.csv")
   df_tee_and_rmr <- file.path(tempdir(), "df_tee_and_rmr.csv")
   df_day_night <- file.path(tempdir(), "df_day_night.csv")
   df_gox_lox <- file.path(tempdir(), "df_gox_lox.csv")

   # Additional data from other panels
   day_night <- getSession(session$token, global_data)[["df_day_night"]]
   TEE_and_RMR <- getSession(session$token, global_data)[["TEE_and_RMR"]]
   goxlox <- getSession(session$token, global_data)[["df_gox_lox"]]
   df <- getSession(session$token, global_data)[["finalC1"]]

   # to store in zip archive
   filenames <- c()

   # Raw input data
   if (!is.null(df_df_input)) { 
      write.csv2(df, file = df_df_input)
      filenames <- c(filenames, df_df_input)
   }

   # Day and Night Activity
   if (!is.null(day_night)) { 
      write.csv2(day_night, file = df_day_night)
      filenames <- c(filenames, df_day_night)
   }

   # TEE and RMR 
   if (!is.null(TEE_and_RMR)) { 
      write.csv2(TEE_and_RMR, file = df_tee_and_rmr)
      filenames <- c(filenames, df_tee_and_rmr)
   }

   # Fuel Oxidation
   if (!is.null(goxlox)) {
      write.csv2(goxlox, file = df_gox_lox)
      filenames <- c(filenames, df_gox_lox)
   }

   # Energy expenditure for two formulas
   if (!is.null(df)) { 
      df_calc <- df %>% select(c(HP, HP2)) %>% rename("Energy Expenditure #1"=HP, "Energy Expenditure #2"=HP2)
      df_calc <- df_calc %>% rename(Animals=`Animal No._NA`)
      write.csv2(df_calc, file = df_df_output) 
      filenames <- c(filenames, df_df_output)
   }

   #############################################################################
   # Main plots
   #############################################################################
   raw_plots <- getSession(session$token, global_data)[["plot_for_raw"]]
   for (key in names(raw_plots)) {
      plot_for_raw_input <- file.path(tempdir(), paste0("raw_plot_", key, ".html"))
      htmlwidgets::saveWidget(raw_plots[[key]], plot_for_raw_input, selfcontained=TRUE)
      filenames <- c(filenames, plot_for_raw_input)
   }

   plot_for_tee_input <- file.path(tempdir(), "tee_plot.html")
   plot_for_tee <- getSession(session$token, global_data)[["plot_for_tee"]]
   if (!is.null(plot_for_tee)) {
      htmlwidgets::saveWidget(plot_for_tee, plot_for_tee_input, selfcontained=TRUE)
      filenames <- c(filenames, plot_for_tee_input)
   }

   plot_for_ee_input <- file.path(tempdir(), "ee_plot.html")
   plot_for_ee <- getSession(session$token, global_data)[["plot_for_ee"]]
   if (!is.null(plot_for_ee)) {
      htmlwidgets::saveWidget(plot_for_ee, plot_for_ee_input, selfcontained=TRUE)
      filenames <- c(filenames, plot_for_ee_input)
   }

   goxlox_plots <- getSession(session$token, global_data)[["plot_for_goxlox"]]
   for (key in names(goxlox_plots)) {
      plot_for_goxlox_input <- file.path(tempdir(), paste0("goxlox_plot_", gsub(" ", "_", key), ".html"))
      htmlwidgets::saveWidget(goxlox_plots[[key]], plot_for_goxlox_input, selfcontained=TRUE)
      filenames <- c(filenames, plot_for_goxlox_input)
   }

   plot_for_metadata_input <- file.path(tempdir(), "metadata_plot.html")
   plot_for_metadata <- getSession(session$token, global_data)[["plot_for_metadata"]]
   if (!is.null(plot_for_metadata)) {
      htmlwidgets::saveWidget(plot_for_metadata, plot_for_metadata_input, selfcontained=TRUE)
      filenames <- c(filenames, plot_for_metadata_input)
   }

   plot_for_RMR_input <- file.path(tempdir(), "RMR_plot.html")
   plot_for_RMR <- getSession(session$token, global_data)[["plot_for_RMR"]]
   if (!is.null(plot_for_RMR)) {
      htmlwidgets::saveWidget(plot_for_RMR, plot_for_RMR_input, selfcontained=TRUE)
      filenames <- c(filenames, plot_for_RMR_input)
   }

   #############################################################################
   # Windowed time-trace plots
   #############################################################################
   time_trace_plots <- c()
   is_Raw_window_calculated <- getSession(session$token, global_data)[["is_Raw_window_calculated"]]
   if (!is.null(is_Raw_window_calculated)) {
      if (is_Raw_window_calculated) {
         plot_for_raw_window_input <- file.path(tempdir(), "raw_plot_window.html")
         plot_for_raw_window <- getSession(session$token, global_data)[["plot_for_raw_window"]]
         if (!is.null(plot_for_raw_window)) {
         htmlwidgets::saveWidget(plot_for_raw_window, plot_for_raw_window_input, selfcontained=TRUE)
         }
         time_trace_plot <- c(time_trace_plots, plot_for_raw_window_input)
         filenames <- c(filenames, plot_for_raw_window_input)
      }
   }

   is_TEE_window_calculated <- getSession(session$token, global_data)[["is_TEE_window_calculated"]]
   if (!is.null(is_TEE_window_calculated)) {
      if (is_TEE_window_calculated) {
         plot_for_tee_window_input <- file.path(tempdir(), "tee_plot_window.html")
         plot_for_tee_window <- getSession(session$token, global_data)[["plot_for_tee_window"]]
         if (!is.null(plot_for_tee_window)) {
            htmlwidgets::saveWidget(plot_for_tee_window, plot_for_tee_window_input, selfcontained=TRUE)
         }
         time_trace_plot <- c(time_trace_plots, plot_for_tee_window_input)
         filenames <- c(filenames, plot_for_tee_window_input)
      }
   }

   is_EE_window_calculated <- getSession(session$token, global_data)[["is_EE_window_calculated"]]
   if (!is.null(is_EE_window_calculated)) {
      if (is_EE_window_calculated) {
         plot_for_ee_window_input <- file.path(tempdir(), "ee_plot_window.html")
         plot_for_ee_window <- getSession(session$token, global_data)[["plot_for_ee_window"]]
         if (!is.null(plot_for_ee_window)) {
            htmlwidgets::saveWidget(plot_for_ee_window, plot_for_ee_window_input, selfcontained=TRUE)
         }
         time_trace_plot <- c(time_trace_plots, plot_for_ee_window_input)
         filenames <- c(filenames, plot_for_ee_window_input)
      }
   }

   is_GoxLox_window_calculated <- getSession(session$token, global_data)[["is_GoxLox_window_calculated"]]
   if (!is.null(is_GoxLox_window_calculated)) {
      if (is_GoxLox_window_calculated) {
         plot_for_ee_window_input <- file.path(tempdir(), "goxlox_plot_window.html")
         plot_for_ee_window <- getSession(session$token, global_data)[["plot_for_goxlox_window"]]
         if (!is.null(plot_for_ee_window)) {
            htmlwidgets::saveWidget(plot_for_ee_window, plot_for_ee_window_input, selfcontained=TRUE)
         }
         time_trace_plot <- c(time_trace_plots, plot_for_ee_window_input)
         filenames <- c(filenames, plot_for_ee_window_input)
      }
   }

   is_RMR_window_calculated <- getSession(session$token, global_data)[["is_RMR_window_calculated"]]
   if (!is.null(is_RMR_window_calculated)) {
      if (is_RMR_window_calculated) {
         plot_for_rmr_window_input <- file.path(tempdir(), "rmr_plot_window.html")
         plot_for_rmr_window <- getSession(session$token, global_data)[["plot_for_RMR_window"]]
         if (!is.null(plot_for_rmr_window)) {
            htmlwidgets::saveWidget(plot_for_rmr_window, plot_for_rmr_window_input, selfcontained=TRUE)
         }
         time_trace_plot <- c(time_trace_plots, plot_for_rmr_window_input)
         filenames <- c(filenames, plot_for_rmr_window_input)
      }
   }

   #filenames <- c()
   statistics_tables <- getSession(session$token, global_data)[["statistics_table"]]
   print("before exporting:")
   print(statistics_tables)
   print("num tables:")
   print(length(statistics_tables))
   for (i in seq_along(statistics_tables)) {
      print("table i:")
      print(statistics_tables[[i]])
      used_variable <- unique(statistics_tables[[i]]$variable)
      used_quantity <- unique(statistics_tables[[i]]$quantity)
      if (used_quantity == "Raw") {
        if (!is.null(used_variable)) {
           statistics_table_output <- file.path(tempdir(), sprintf("statistics_table_%d_%s_%s.csv", i, unique(statistics_tables[[i]]$quantity), used_variable))
         } 
      } else {
        statistics_table_output <- file.path(tempdir(), sprintf("statistics_table_%d_%s.csv", i, unique(statistics_tables[[i]]$quantity)))
      }
      print("filename:")
      print(statistics_table_output)
      write.csv(statistics_tables[[i]], file = statistics_table_output)
      filenames <- c(filenames, statistics_table_output)
   }
   print("after for loop")


   # Create zip file of all files
   zip_file <- file.path(tempdir(), "all_data.zip")
   files = filenames
   zip(zipfile=zip_file, files=files)
   return(zip_file)

}

################################################################################
#' do_export_plotting_data
#' 
#' This function export plotting data frame as csv
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param do_plotting indicate to plot or not
#' @param global_data global data
#' @export
################################################################################
do_export_plotting_data <- function(input, output, session, file_output, do_plotting, global_data) {
   file <- input$File1
   if (is.null(input$File1)) {
      output$message <- renderText("Not any cohort data given by the user.")
   } else {
      file <- input$File1
      real_data <- do_plotting(file$datapath, input, input$sick, output, session)
      df_to_plot <- getSession(session$token, global_data)[["reactive_data"]]()
      df_to_plot <- prepare_data_frame_for_export(df_to_plot, global_data, session)
      write.csv2(df_to_plot, file = file_output)
   }
}

################################################################################
#' do_export_alternative
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param file_output file output path
#' @param do_plotting indicate to plot or not
#' @export
#' This function exports to CalR compatible file format and Excel (alternative method)
################################################################################
do_export_alternative <- function(format, input, output, session, file_output, do_plotting, global_data) {
      file <- input$File1
      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file <- input$File1
         real_data <- do_plotting(file$datapath, input, input$sick, output, session)
         h <- hash()
         # Specific mapping of column names from TSE to CalR to produce
         # a compatible .csv file to be re-used in downstream analysis
         h[["Datetime"]] <- "Date_Time"
         h[["VCO2(3)_[ml/h]"]] <- "RT_VCO2_3"
         h[["VO2(3)_[ml/h]"]] <- "RT_VO2_3"
         # caloric equivalent by 1st formula (HeatProduction formula 1)
         h[["HP"]] <- "RT_Kcal_hr_1"
         # caloric equivalent by 2nd formula (HeatProduction formula 2)
         h[["HP2"]] <- "RT_Kcal_hr_2"
         h[["Animal No._NA"]] <- "Animal"
         # animal
         # rename data
         for (v in ls(h)) {
            names(real_data$data)[names(real_data$data) == v] <- h[[v]]
         }
         if (format == "CalR") {
            write.csv(real_data$data[values(h)], file = file_output, row.names = FALSE)
            writeLines(gsub(pattern = '"', replace = "", x = readLines(file_output)), con = file_output)
         }

         if (format == "Excel") {
            tmp <- cbind(real_data$data[values(h)], real_data$animals)
            df <- getSession(session$token, global_data)[["finalC1"]]
            write_xlsx(df, path = file_output)
         }
      }
   }

################################################################################
#' do_export
#' 
#' This function exports to CalR compatible file format and Excel
#' @param format chosen format
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param do_plotting indicate to plot or not
#' @export
################################################################################
do_export <- function(format, input, output, session, do_plotting) {
   if (format == "CalR") {
      file <- input$File1
      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file <- input$File1
         real_data <- do_plotting(file$datapath, input, input$sick, output, session)
         h <- hash()

         # Specific mapping of column names from TSE to CalR to produce
         # a compatible .csv file to be re-used in downstream analysis
         h[["Datetime"]] <- "Date_Time"
         h[["VCO2(3)_[ml/h]"]] <- "RT_VCO2_3"
         h[["VO2(3)_[ml/h]"]] <- "RT_VO2_3"
         # caloric equivalent by 1st formula (HeatProduction formula 1)
         h[["HP"]] <- "RT_Kcal_hr_1"
         # caloric equivalent by 2nd formula (HeatProduction formula 2)
         h[["HP2"]] <- "RT_Kcal_hr_2"
         # rename data
         for (v in ls(h)) {
            names(real_data$data)[names(real_data$data) == v] <- h[[v]]
         }
         fname <- paste(paste(path_home(), input$export_folder$path[[2]],
            sep <- "/"), input$export_file_name, sep <- "/")
         write.csv(real_data$data[values(h)], file = fname, row.names = FALSE)
         writeLines(gsub(pattern = '"', replace = "", x = readLines(fname)),
          con = fname)
      }
   }
}
