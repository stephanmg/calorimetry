# Load libraries, data ------------------------------------------------
library(ggplot2)

characters <- read.csv("data/characters.csv")

# Create server -------------------------------------------------------
server <- function(input, output) {
  
  output$plot <- renderPlot({

   if (is.null(input$File)) {
   } else {
   file = input$File
   C1 <- read.csv2(file$name, header = F, skip = 10, na.strings = c("-","NA"))
   
   p <- ggplot(data=C1, aes(x=V4, y=V16)) + 
   geom_boxplot() 

   p
   }
   
  })
}

