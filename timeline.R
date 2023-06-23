library(ggplot2)

draw_day_night_rectangles <- function(df, p, light_start=7, light_end=19, light_offset=0) {
   intervals = seq(min(df$x), max(df$x), 12)
   light_on = TRUE
   color="grey"
   lapply(intervals, function(item) { p <<- p + geom_rect(aes(xmin=item-12+light_offset, xmax=item+light_offset, ymin=min(df$y), ymax=max(df$y)), fill=color, alpha=0.1); if (light_on) { color <<- "yellow"; light_on <<- FALSE }  else {  color <<- "grey"; light_on <<- TRUE}; })
   p <- p + geom_rect(aes(xmin=tail(intervals, n=1), xmax=min(tail(intervals, n=1) + 12, max(df$x)), ymin=min(df$y), ymax=max(df$y)), fill=color, alpha=0.1)
   p <- p + ylim(min(df$y), max(df$y))
   p <- p + scale_x_continuous(expand=c(0,0), limits=c(min(df$x), max(df$x)))
   p <- p + scale_y_continuous(expand=c(0,0), limits=c(min(df$y), max(df$y)))
   p

}

# example usage
# existing plot
#df <- data.frame(x=c(0,5,10,20,40,70,80,100), y=c(1,3,1,3,1,3,1,3))
#p = ggplot(data=df, aes(x=x, y=y)) + geom_line()

# add day night rectangles
#p <- draw_day_night_rectangles(df, p, 7, 19, 0)
#ggsave(p, filename="blubb.png")
