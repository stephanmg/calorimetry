library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggExtra)

################################################################################
#' plot_locomotion
#' 
#' This function plots the locomotion / probability density map of a standard 
#' Sable Systems Promethion file
#' @param file input file
#' @param x_min_food min x coordinate of food hamper
#' @param x_max_food max x coordinate of food hamper
#' @param y_min_food min y coordinate of food hamper
#' @param y_max_food max y coordinate of food hamper
#' @param x_min_scale min x scale factor
#' @param x_max_scale max x scale factor
#' @param y_min_scale min y scale factor
#' @param y_max_scale max y scale factor
#' @param x_min_bottle min x coordinate water bottle
#' @param x_max_bottle max x coordinate water bottle
#' @param y_min_bottle min y coordinate water bottle
#' @param y_max_bottle max y coordinate water bottle
#' 
################################################################################
plot_locomotion <- function(file, x_min_food = 2, x_max_food = 9.5, y_min_food = 4, y_max_food = 16, x_min_scale = 20.5, x_max_scale = 29, y_min_scale = 4.2, y_max_scale = 9.5, x_min_bottle = 20.5, x_max_bottle = 28, y_min_bottle = 12, y_max_bottle = 15.5) {
   # Sable stores data in Behaviour_List sheet in the Excel workbook
   sheet <- "Behavior_List"

   # Read df and unique animal IDs
   df <- read_excel(file, sheet = sheet)
   df <- na.omit(df)
   animals <- unique(df$Animal)

   # Fixed cage specifications for current Sable Systems
   rects <-  data.frame(xmin = c(2, 20.5, 20.5), xmax = c(9.5, 29, 28), ymin = c(4, 4.2, 12), ymax = c(16, 9.5, 15.5), r = c("Running Wheel", "Food Hopper", "Water Bottle"))

   # Preprare data for plotting
   df <- df %>% group_by(Animal) %>% mutate(Durat_Sec_Norm = Durat_Sec / max(Durat_Sec))
   rows <- floor(sqrt(length(unique(df$Animal))))
   cols <- rows
   df <- df[rep(seq_len(nrow(df)), df$Durat_Sec), ]

   # Create plot of probability density for animal IDs
   p <- ggplot(df, aes(Y_cm, X_cm)) + stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) + scale_fill_viridis_c()
   p <- p + facet_wrap(~ Animal, ncol = cols)
   p <- p + xlab("Position Y [cm]")
   p <- p + ylab("Position X [cm]")
   p <- p + ggtitle(paste("Position (Y, X) probability heat map for ", length(unique(df$Animal)), " animals", sep = ""))
   p <- p + geom_rect(data = rects, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "#FAF9F6", fill = NA)
   p <- p + geom_text(data = rects, inherit.aes = FALSE, aes(x = xmin + (xmax - xmin) / 2, y = ymin + (ymax - ymin) / 2, label = r), color = "#FAF9F6", size = 4)
   p <- p + scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) + scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))
   p <- p + guides(fill = guide_legend(title = "Probability"))
   p
}
