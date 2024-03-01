library(testthat)
source("../../inc/read_metadata.R")

################################################################################
### data tests
################################################################################
test_that("get_true_metadata", {
   df <- get_true_metadata("../../MetaDataSheet_UCP1\ KO_cb8eb8c0-aa63-4a25-a495-6eaa8dc4a243 (2).xlsx")
   expect_equal(ncol(df), 6)
   expect_equal(nrow(df), 16)
   expect_equal(colnames(df), c("lean_mass", "fat_mass", "Animals", "Diet", "Genotype", "body_weight"))
}
)

test_that("get_constants", {
   df <- get_constants("../../MetaDataSheet_UCP1\ KO_cb8eb8c0-aa63-4a25-a495-6eaa8dc4a243 (2).xlsx")
   expect_equal(colnames(df), c("constant", "value"))
   expect_equal(ncol(df), 2)
   expect_equal(nrow(df), 3)
})
