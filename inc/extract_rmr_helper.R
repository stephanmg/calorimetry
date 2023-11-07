################################################################################
# extract_rmr_helper
################################################################################
extract_rmr_helper <- function() {
   df <- read.csv2("df_for_cov_analysis.csv")
   library(dplyr)
   source("extract_rmr.R")
   res <- df %>%
      group_by(Animal) %>%
      group_map(~ extract_rmr(.x, 1, 1))
   animal_names <- as.data.frame(df %>% group_by(Animal) %>% select(Animal) %>% distinct(Animal))

   total_data <- data.frame()
   for (animal in 1:length(res)) {
      data <- res[[animal]]$df_plot_total
      tmp <- cbind(c(data$HP), c(data$Time), c(data$Component), c(rep(animal_names$Animal[animal], length(data$Component)))) # nolint
      total_data <- rbind(total_data, tmp)
   }

   colnames(total_data) <- c("HP", "Time", "Component", "Animal")
   total_data$Animal <- as.factor(total_data$Animal)
   total_data$Component <- as.factor(total_data$Component)
   #print(total_data)
   return(total_data)
   # TODO: double check, total data contain Heat production, not O2 and CO2?
}
