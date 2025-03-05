library(shinytest2)
library(jsonlite)
# TODO: tests need to be refactored for new plot_type names

#################################################################################
#### Raw
#################################################################################
#test_that("Raw", {
#  app <- AppDriver$new(name = "BATEnergy", height = 895, width = 1619)
#  app$click("example_data_single")
#  app$click("showTabPC")
#  app$set_inputs(plot_type = "Raw")
#  app$click("plotting")
#  expected_values <- fromJSON(app$expect_values())
#  expect_equal(expected_values$input$plot_type, "Raw")
#})
#
#################################################################################
#### EnergyExpenditure
#################################################################################
#test_that("EnergyExpenditure", {
#  app <- AppDriver$new(name = "BATEnergy", height = 895, width = 1619)
#  app$click("example_data_single")
#  app$click("showTabPC")
#  app$set_inputs(plot_type = "EnergyExpenditure")
#  app$click("plotting")
#  expected_values <- fromJSON(app$expect_values())
#  expect_equal(expected_values$input$plot_type, "EnergyExpenditure")
#})
#
#################################################################################
#### GoxLox
#################################################################################
#test_that("GoxLox", {
#  app <- AppDriver$new(name = "BATEnergy", height = 895, width = 1619)
#  app$click("example_data_single")
#  app$click("showTabPC")
#  app$set_inputs(plot_type = "GoxLox")
#  app$click("plotting")
#  expected_values <- fromJSON(app$expect_values())
#  expect_equal(expected_values$input$plot_type, "GoxLox")
#})
#
#################################################################################
#### DayNightActivity
#################################################################################
#test_that("DayNightActivity", {
#  app <- AppDriver$new(name = "BATEnergy", height = 895, width = 1619)
#  app$click("example_data_single")
#  app$click("showTabPC")
#  app$set_inputs(plot_type = "DayNightActivity")
#  app$click("plotting")
#  expected_values <- fromJSON(app$expect_values())
#  expect_equal(expected_values$input$plot_type, "DayNightActivity")
#})
#
#
#################################################################################
#### TotalEnergyExpenditure
#################################################################################
#test_that("TotalEnergyExpenditure", {
#  app <- AppDriver$new(name = "BATEnergy", height = 895, width = 1619)
#  app$click("example_data_single")
#  app$click("showTabPC")
#  app$set_inputs(plot_type = "TotalEnergyExpenditure")
#  app$click("plotting")
#  expected_values <- fromJSON(app$expect_values())
#  expect_equal(expected_values$input$plot_type, "TotalEnergyExpenditure")
#})
#
#
#################################################################################
#### RestingMetabolicRate
#################################################################################
#test_that("RestingMetabolicRate", {
#  app <- AppDriver$new(name = "BATEnergy", height = 895, width = 1619)
#  app$click("example_data_single")
#  app$click("showTabPC")
#  app$set_inputs(plot_type = "RestingMetabolicRate")
#  app$click("plotting")
#  expected_values <- fromJSON(app$expect_values())
#  expect_equal(expected_values$input$plot_type, "RestingMetabolicRate")
#})
