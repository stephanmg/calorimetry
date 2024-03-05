library(dplyr)
source("extract_rmr.R")
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
