library(shiny)
library(shinyFiles)

# configuration
port <- 1338
host <- "0.0.0.0"
maxRequestSize <- 30 * 1024^2 # 30 MiB
printSessionInfo <- TRUE

# set Shiny options
options(shiny.host = host)
options(shiny.port = port)
options(shiny.maxRequestSize = maxRequestSize)

# debug
if (printSessinInfo) {
   print(sessionInfo())
}

# start app
runApp(".")
