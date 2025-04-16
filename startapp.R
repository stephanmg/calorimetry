library(shiny)
library(shinyFiles)

################################################################################
# Command line arguments
################################################################################
args <- commandArgs(trailingOnly = TRUE)

port <- 1338
host <- "0.0.0.0"

if (length(args) == 2) {
   port <- as.numeric(args[1])
   host <- as.character(args[2])
}
   

################################################################################
# Configuration options
################################################################################
maxRequestSize <- 100 * 1024^2 # 100 MiB
printSessionInfo <- TRUE

################################################################################
# Set Shiny options
################################################################################
options(shiny.host = host)
options(shiny.port = port)
options(shiny.maxRequestSize = maxRequestSize)
#options(shiny.trace = TRUE)
#options(shiny.fullstacktrace = TRUE)

################################################################################
# Debug session
################################################################################
if (printSessionInfo) {
   print(sessionInfo())
}

################################################################################
# Start application
################################################################################
runApp(".")
