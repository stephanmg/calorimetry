# Load data and libraries -------------------------------------------
library("shiny")
library("shinyWidgets")
library("shinyFiles")
source("ui.R")
source("server.R")

# Create shiny application ------------------------------------------
shinyApp(ui = ui, server = server)
