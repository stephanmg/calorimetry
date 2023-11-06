library("shiny")
library("shinyWidgets")
library("shinyFiles")

################################################################################
# Include UI and server/backend components
################################################################################
source("ui.R")
source("server.R")

################################################################################
# Create Shiny application
################################################################################
shinyApp(ui = ui, server = server)
