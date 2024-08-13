source("inc/session_management.R")

################################################################################
# Export all data as zip archive
################################################################################
do_export_all_data <- function(input, output, session, file_output, do_plotting, global_data) {
   plot_csv <- file.path(tempdir(), "df_plot.csv")
   df_csv <- file.path(tempdir(), "df_input_data.csv")
   file <- input$File1
   if (is.null(input$File1)) {
      output$message <- renderText("Not any cohort data given by the user.")
   } else {
      file <- input$File1
      real_data <- do_plotting(file$datapath, input, input$sick, output, session)
      df_to_plot <- getSession(session$token, global_data)[["reactive_data"]]()
      write.csv2(df_to_plot, file = plot_csv)
      write.csv2(read.csv2("finalC1.csv"), file = df_csv)
      zip_file <- file.path(tempdir(), "all_data.zip")
      zip(zipfile=zip_file, files = c(plot_csv, df_csv))
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
