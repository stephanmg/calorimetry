library(ggplot2)

################################################################################
#' do_add_wavelet_analysis_alt
#' 
#' This function performs wavelet analysis of a time signal in hours
#' @param df
do_add_wavelet_analysis_alt <- function(df, measurement) {
	# Get Time consistenly for wavelet function
	df$Time <- df$running_total.hrs.halfhour

	# Perform wavelet analysis
	dt_value = mean(diff(df$Time))

	# regular time for the signal (should already be the case, but better to be sure)
	time_regular <- seq(min(df$Time), max(df$Time), length.out = nrow(df))
	df <- data.frame(
		Time = time_regular,
		Measurement = approx(x = df$Time, y = df[[measurement]], xout = time_regular)$y
	)

	# now, we also interpolate the measurement onto a regular time grid, 0.5 h interval...
	time_new <- seq(min(df$Time), max(df$Time), by = 0.5) # since we use the time-scale running_total.hrs.halfhour
	# otherwise we should interpolate on the regular time grid of diff(df$Time) in seconds (CaloBox timescale)
	df <- data.frame(
		Time = time_new,
		Measurement = approx(x=df$Time, y= df$Measurement, xout=time_new)$y
	)

	# biwavelet package needs data frame as matrix
	df = as.matrix(df)
	wavelet_result <- biwavelet::wt(df)

	wavelet_df <- data.frame(
		Time = rep(wavelet_result$t, each = length(wavelet_result$period)),
		Period = rep(wavelet_result$period, times = length(wavelet_result$t)),
		Power = as.vector(wavelet_result$power)
	)

	sig_matrix <- wavelet_result$signif

	sig_threshold <- 0.005
	sig_rows <- apply(sig_matrix, 1, function(row) any(row < sig_threshold, na.rm=TRUE))

	wavelet_power_spectrum <- ggplot(wavelet_df, aes(x = Time, y = Period, fill = Power)) +
	geom_tile() +
	scale_fill_viridis_c(option = "plasma") +  
	labs(title = "Wavelet Power Spectrum", x = "Time [h]", y = "Period (Scale)", fill = "Power") +
	theme_minimal()

	global_spectrum_df <- data.frame(
		Period = wavelet_result$period,
		Power = rowMeans(wavelet_result$power, na.rm = TRUE)
	)

	sig_global <- global_spectrum_df[sig_rows, ]

	wavelet_global_spectrum <- ggplot(global_spectrum_df, aes(x=Period, y=Power)) +
	geom_line(color = "blue", size = 1) +
    geom_point(data = sig_global, aes(x = Period, y = Power), color = "red", size = 2) +
	labs(title = "Global Wavelet Spectrum", x = "Period (Scale)", y = "Power") +
	theme_minimal()

	return(list("wavelet_power_spectrum"=wavelet_global_spectrum, "wavelet_plot"=wavelet_power_spectrum))
	
}