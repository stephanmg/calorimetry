library(testthat)
source("../../inc/metadata/read_metadata.R")

################################################################################
### metadata tests
################################################################################
test_that("get_true_metadata", {
   df <- get_true_metadata("../data/metadata.xslx")
   expect_equal(ncol(df), 6)
   expect_equal(nrow(df), 16)
   expect_equal(colnames(df), c("lean_mass", "fat_mass", "Animals", "Diet", "Genotype", "body_weight"))
})

test_that("get_constants", {
   df <- get_constants("../data/metadata.xlsx")
   expect_equal(colnames(df), c("constant", "value"))
   expect_equal(ncol(df), 2)
   expect_equal(nrow(df), 3)
})


################################################################################
### RMR tests
################################################################################
test_that("get_rmr", {
   df <- read.csv2("../data/input_rmr_for_testing.csv")
   res <- df %>%
      group_by(Animal) %>%
      group_map(~ extract_rmr(.x, 5, 5, 15))
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

   df_ground_truth <- read.csv2("../output_rmr_for_testing.csv")
   expect_equal(df_ground_truth, total_data)
})
