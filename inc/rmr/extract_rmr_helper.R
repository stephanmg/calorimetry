library(dplyr)
source("inc/rmr/extract_rmr.R")

################################################################################
#' pretty_print_interval_length_list
#' 
#' This function pretty prints the interval length list with cohort information
#' @param interval_length_list_per_cohort_and_animals
################################################################################
pretty_print_interval_length_list <- function(interval_length_list_per_cohort_and_animals) {
   for (name in names(interval_length_list_per_cohort_and_animals)) {
      cat("\nList name: ", name, "\n")
      cat("Values: ", paste(interval_length_list_per_cohort_and_animals[[name]]$values, collapse=","), "\n")
      cat("Interval length [minutes]", interval_length_list_per_cohort_and_animals[[name]]$interval_length, "\n")
   }
}

################################################################################
#' lookup_interval_length
#' 
#' This function is used to lookup the interval length based on a cohort
#' @param interval_length_list_per_cohort_and_animals
#' @param value interval length (default = 5 minutes)
################################################################################
lookup_interval_length <- function(interval_length_list_per_cohort_and_animals, value) {
   for (item in interval_length_list_per_cohort_and_animals) {
      if (value %in% item$values) {
         return(item$interval_length)
      }
   }
   return(5)
}

################################################################################
#' lookup_cohort_belonging
#' 
#' This function looks up the cohort a given animal id belongs to
#' @param interval_length_list_per_cohort_and_animals
#' @param id animal ID 
#' Return NA in case animal ID does not belong to any cohort
#' ################################################################################
lookup_cohort_belonging <- function(interval_length_list_per_cohort_and_animals, id) {
   for (name in names(interval_length_list_per_cohort_and_animals)) {
      if (id %in% interval_length_list_per_cohort_and_animals[[name]]$values) {
         return(name)
      }
   }
   return(NA)
}


################################################################################
#' extract_rmr_helper
#' 
#' This function is a helper method/wrapper to calculate the resting metabolic rate
#' @param interval_length length of interval
#' @param percentage_best fraction of best matches
#' @param M subintervals
################################################################################
extract_rmr_helper <- function(interval_length = 15, percentage_best = 1, M = 1) {
   # TODO: Refactor this
   df <- read.csv2("df_for_cov_analysis.csv")
   res <- df %>%
      group_by(Animal) %>%
      group_map(~ extract_rmr(.x, M, percentage_best, interval_length))
   animal_names <- as.data.frame(df %>% group_by(Animal) %>% select(Animal) %>% distinct(Animal))

   total_data <- data.frame()
   for (animal in seq_along(res)) {
      data <- res[[animal]]$df_plot_total
      tmp <- cbind(c(data$HP), c(data$Time), c(data$Component), c(rep(animal_names$Animal[animal], length(data$Component)))) # nolint
      total_data <- rbind(total_data, tmp)
   }

   colnames(total_data) <- c("HP", "Time", "Component", "Animal")
   total_data$Animal <- as.factor(total_data$Animal)
   total_data$Component <- as.factor(total_data$Component)
   return(total_data)
}
