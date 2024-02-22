library(ggplot2)

draw_day_night_rectangles <- function(df, p, light_start = 7, light_end = 19, light_offset = 0, day_color = "yellow", night_color = "grey") {
   intervals <- seq(min(df$x), max(df$x), 12)
   light_on <- TRUE
   color <- night_color
   lapply(intervals, function(item) { p <<- p + annotate("rect", xmin = item - 12 + light_offset, xmax = item + light_offset, ymin = min(df$y), ymax = max(df$y), fill = color, alpha = 0.1); if (light_on) { color <<- day_color; light_on <<- FALSE }  else {  color <<- night_color; light_on <<- TRUE}; })
   p <- p + annotate("rect", xmin = tail(intervals, n = 1), xmax = min(tail(intervals, n = 1) + 12, max(df$x)), ymin = min(df$y), ymax = max(df$y), fill = color, alpha = 0.1)
   p <- p + ylim(min(df$y), max(df$y))
   p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(df$x), max(df$x)))
   p <- p + scale_y_continuous(expand = c(0, 0), limits = c(min(df$y), max(df$y)))
   p
}