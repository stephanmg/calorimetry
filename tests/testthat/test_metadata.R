library(testthat)

source("../../inc/metadata/read_metadata.R")

setup({
   df_meta_test_1 <<- get_true_metadata("../../example_data/UCP1KO/example_metadata_1.xlsx")
   df_constants_test_1  <<- get_constants("../../example_data/UCP1KO/example_metadata_1.xlsx")
   df_meta_test_2 <<- get_true_metadata("../../example_data/DAKO/example_metadata_2.xlsx")
   df_constants_test_2  <<- get_constants("../../example_data/DAKO/example_metadata_2.xlsx")
})

################################################################################
### metadata tests (example data set 1)
################################################################################
test_that("get_true_metadata", {
   expect_equal(ncol(df_meta_test_1), 11)
   expect_equal(nrow(df_meta_test_1), 16)
   expect_equal(colnames(df_meta_test_1), c("lm_start","lm_end","fm_start","fm_end","Animals","Diet", "Genotype","bw_start","bw_end","Sex","Age"))
})

test_that("get_constants", {
   expect_equal(ncol(df_constants_test_1), 2)
   expect_equal(nrow(df_constants_test_1), 2)
   expect_equal(colnames(df_constants_test_1), c("constant", "value"))
})

test_that("get_light_cycle", {
   expect_equal(df_constants_test_1[["constant"]], c("light_on", "light_off"))
   expect_equal(as.numeric(df_constants_test_1[["value"]]), c(6, 18))
})

################################################################################
### metadata tests (example data set 2)
################################################################################
test_that("get_true_metadata", {
   expect_equal(ncol(df_meta_test_2), 11)
   expect_equal(nrow(df_meta_test_2), 16)
   expect_equal(colnames(df_meta_test_2), c("lm_start","lm_end","fm_start","fm_end","Animals","Diet", "Genotype","bw_start","bw_end","Sex","Age"))
})

test_that("get_constants", {
   expect_equal(ncol(df_constants_test_2), 2)
   expect_equal(nrow(df_constants_test_2), 2)
   expect_equal(colnames(df_constants_test_2), c("constant", "value"))
})

test_that("get_light_cycle", {
   expect_equal(df_constants_test_2[["constant"]], c("light_on", "light_off"))
   expect_equal(as.numeric(df_constants_test_2[["value"]]), c(6, 18))
})

teardown({
   rm(df_meta_test_1)
   rm(df_constants_test_1)
   rm(df_meta_test_2)
   rm(df_constants_test_2)
})
