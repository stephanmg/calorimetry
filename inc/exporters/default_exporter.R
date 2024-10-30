source("inc/session_management.R")

################################################################################
#' prepare_data_frame_for_export
#' 
#' This function prepares the data frame with calculated quantities for export
#' @param df_to_plot data frame
#' @param global_data hash table containing variables during session
#' @param session shiny session
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
#' This function exports all data calculated during the app usage as an archive
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param file_output output file path chosen by user
#' @param do_plotting plotting routines
#' @param global_data hash table containing variables during session
#' @export
################################################################################
do_export_all_data <- function(input, output, session, file_output, do_plotting, global_data) {
   plot_csv <- file.path(tempdir(), "df_plot.csv")
   df_csv <- file.path(tempdir(), "df_input_data.csv")
   df_tee_and_rmr <- file.path(tempdir(), "df_tee_and_rmr.csv")
   df_day_night <- file.path(tempdir(), "df_day_night.csv")
   df_gox_lox <- file.path(tempdir(), "df_gox_lox.csv")
   df_raw <- file.path(tempdir(), "df_raw.csv")
   file <- input$File1
   if (is.null(input$File1)) {
      output$message <- renderText("Not any cohort data given by the user.")
   } else {
      file <- input$File1
      real_data <- do_plotting(file$datapath, input, input$sick, output, session)

      # compiled input data cohorts (should always be available after loading of data sets)
      write.csv2(read.csv2("finalC1.csv"), file = df_csv)

      # Data from main plotting panel not always available
      df_to_plot <- getSession(session$token, global_data)[["reactive_data"]]
      if (!is.null(df_to_plot)) {
         write.csv2(prepare_data_frame_for_export(df_to_plot(), global_data, session), file = plot_csv)
      }

      # Additional data from other panels
      day_night <- getSession(session$token, global_data)[["df_day_night"]]
	   TEE_and_RMR <- getSession(session$token, global_data)[["TEE_and_RMR"]]
	   goxlox <- getSession(session$token, global_data)[["df_gox_lox"]]
	   raw <- getSession(session$token, global_data)[["df_raw"]]

      # Day and Night Activity
      if (!is.null(day_night)) { write.csv2(day_night, file = df_day_night) }
      # TEE and RMR 
      if (!is.null(TEE_and_RMR)) { write.csv2(TEE_and_RMR, file = df_tee_and_rmr) }
      # GoxLox 
      if (!is.null(goxlox)) { write.csv2(goxlox, file = df_gox_lox) }
      # Raw
      if (!is.null(raw)) { write.csv2(raw, file = df_raw) }

      # Create zip file of all files
      zip_file <- file.path(tempdir(), "all_data.zip")
      zip(zipfile=zip_file, files = c(plot_csv, df_csv, df_gox_lox, df_tee_and_rmr, df_day_night, df_raw))
      return(zip_file)
   }

}
################################################################################
# Export plotting data frame as csv
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
# Export to CalR compatible file format and Excel (alternative method)
################################################################################
do_export_alternative <- function(format, input, output, session, file_output, do_plotting) {
      file <- input$File1
      if (is.null(input$File1)) {
         output$message <- renderText("Not any cohort data given by the user.")
      } else {
         file <- input$File1
         real_data <- do_plotting(file$datapath, input, input$sick, output, session)
         h <- hash()
         # Specific mapping of column names from TSE to CalR to produce
         # a compatible .csv file
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
            df <- read.csv2("finalC1.csv")
            write_xlsx(df, path = file_output)
         }
      }
   }

################################################################################
# Export to CalR compatible file format and Excel
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
         # a compatible .csv file
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
