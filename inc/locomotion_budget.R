library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggExtra)
library(stringr)

plot_locomotion_budget <- function(file) {
   sheet <- "Behavior_List"
   df <- read_excel(file, sheet = sheet)
   df <- na.omit(df)
   animals <- unique(df$Animal)

   # all animals
   df <- df %>% group_by(Animal, Activity) %>% summarize(across(Durat_Sec, sum)) %>% group_by(Animal) %>% mutate(Durat_Sec = Durat_Sec / sum(Durat_Sec))
   p <- ggplot(data = df, aes(x = Animal, y = Durat_Sec, fill = Activity))
   p <- p + geom_bar(stat = "identity")
   p <- p + scale_x_continuous(breaks = 0:(length(animals)), labels = seq(0, length(animals)))
   p <- p + ylab("Duration [%]") + xlab("Animal No.")
   p <- p + ggtitle("Total locomotion budget stratified across animals and activity")
   p
}
