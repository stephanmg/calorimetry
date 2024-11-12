library(shinytest2)
app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$uploadFile(File1 = "20200618_SD_Ucpdd_K4 (1).csv")
app$setInputs(showTabPC = "click")
app$setInputs(plot_type = "TotalEnergyExpenditure")
app$setInputs(plotting = "click")
app$snapshot()
