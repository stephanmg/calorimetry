library(ggplot2)
add_simple_rmr <- function(df, measurement) {
	window_size <- 3600

	dt = mean(diff(df$running_total.sec))
	print("dt:")
	print(dt)

	df$rolling_mean <- rollapply(df[[measurement]], width=length(which(df$running_total.sec - df$running_total.sec[1] <= window_size)), FUN=function(x) mean(sort(x)[1:5]), fill = NA, partial=TRUE)
	return(df)
}