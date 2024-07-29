library(dplyr)
source("inc/rmr/extract_rmr.R")

################################################################################
# pretty_print_interval_length_list
################################################################################
pretty_print_interval_length_list <- function(interval_length_list_per_cohort_and_animals) {
   for (name in names(interval_length_list_per_cohort_and_animals)) {
      cat("\nList name: ", name, "\n")
      cat("Values: ", paste(interval_length_list_per_cohort_and_animals[[name]]$values, collapse=","), "\n")
      cat("Interval length [minutes]", interval_length_list_per_cohort_and_animals[[name]]$interval_length, "\n")
   }
}


################################################################################
# lookup_interval_length
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
# lookup_cohort_belonging
################################################################################
lookup_cohort_belonging <- function(interval_length_list_per_cohort_and_animals, id) {
   for (name in names(interval_length_list_per_cohort_and_animals)) {
      if (id %in% interval_length_list_per_cohort_and_animals[[name]]$values) {
         return(name)
      }
   }
   return(NA)
}


################################################################################
# extract_rmr_helper
################################################################################
extract_rmr_helper <- function(interval_length = 15, percentage_best = 1, M = 1) {
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
