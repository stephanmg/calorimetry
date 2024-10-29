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
#' @param x_min_food
#' @param x_max_food
#' @param y_min_food
#' @param y_max_food
#' @param x_min_scale
#' @param x_max_scale
#' @param y_min_scale
#' @param y_max_scale
#' @param x_min_bottle
#' @param x_max_bottle
#' @param y_min_bottle
#' @param y_max_bottle
#' 
################################################################################
plot_locomotion <- function(file, x_min_food = 2, x_max_food = 9.5, y_min_food = 4, y_max_food = 16, x_min_scale = 20.5, x_max_scale = 29, y_min_scale = 4.2, y_max_scale = 9.5, x_min_bottle = 20.5, x_max_bottle = 28, y_min_bottle = 12, y_max_bottle = 15.5) {
   sheet <- "Behavior_List"

   df <- read_excel(file, sheet = sheet)
   df <- na.omit(df)
   animals <- unique(df$Animal)
   rects <-  data.frame(xmin = c(2, 20.5, 20.5), xmax = c(9.5, 29, 28), ymin = c(4, 4.2, 12), ymax = c(16, 9.5, 15.5), r = c("Running Wheel", "Food Hopper", "Water Bottle"))
   df <- df %>% group_by(Animal) %>% mutate(Durat_Sec_Norm = Durat_Sec / max(Durat_Sec))
   rows <- floor(sqrt(length(unique(df$Animal))))
   cols <- rows

   df <- df[rep(seq_len(nrow(df)), df$Durat_Sec), ]
   p2 <- ggplot(df, aes(Y_cm, X_cm)) + stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) + scale_fill_viridis_c()

   p2 <- p2 + facet_wrap(~ Animal, ncol = cols)
   p2 <- p2 + xlab("Position Y [cm]")
   p2 <- p2 + ylab("Position X [cm]")
   p2 <- p2 + ggtitle(paste("Position (Y, X) probability heat map for ", length(unique(df$Animal)), " animals", sep = ""))
   p2 <- p2 + geom_rect(data = rects, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "#FAF9F6", fill = NA)
   p2 <- p2 + geom_text(data = rects, inherit.aes = FALSE, aes(x = xmin + (xmax - xmin) / 2, y = ymin + (ymax - ymin) / 2, label = r), color = "#FAF9F6", size = 4)
   p2 <- p2 + scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) + scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))
   p2 <- p2 + guides(fill = guide_legend(title = "Probability"))
   p2
}
