################################################################################
#' instantaneous_measurement
#' 
#' This function calculates teh instanteneous metanbolic rate according to the
#' paper on the z-transformation by Bartholomew and Vleck 1981
#' @param measurement measurement
#' @param a correction factor accounting for delay in gas exchange measurements
#' @export
################################################################################
instantaneous_measurement <- function(measurement, a = 0.9) {
	meas_inst <- numeric(length(measurement))
	meas_inst[1] <- measurement[1]
	for (i in 2:length(measurement)) {
		meas_inst[i] <- (measurement[i] - a * measurement[i-1]) / (1-a)
	}
	return(meas_inst)
}