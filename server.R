# Load libraries, data ------------------------------------------------
library(ggplot2)

# Create server -------------------------------------------------------
server <- function(input, output, session) {
   observeEvent(
      input$plotting,
      {
       output$plot <- renderPlot({

   if (is.null(input$File)) {
   } else {
   file = input$File
   C1 <- read.csv2(file$name, header = F, skip = 10, na.strings = c("-","NA"))
  
   p <- ggplot(data=C1, aes_string(x="V4", y=input$feature)) + 
   geom_boxplot() 

   p
   }
   
  })

      }
   )

}

