#this script draws a simple spline through an eeg data timeline
library(eegkit)

plot_spline <- function(time_slice, nknots=NULL) {
  #specify the time_slice and number of knots to use in this experiment
  # nknots = length(time_slice)/10
  
  if(is.null(nknots))
    {spln <- eegsmooth(neural_data[time_slice, 2], time=neural_data[time_slice,1])
    } else
    #get the spline
    {spln <- eegsmooth(neural_data[time_slice, 2], time=neural_data[time_slice,1], nknots=nknots)}
  
  #either plot the smooth spline
#   plot(neural_data[time_slice,1], neural_data[time_slice,2], type="l", col="red")
#   lines(spln$x, spln$fitted.values, type="l", lwd=2)
  
  #or plot just the knot points
  skip_factor = 10
  knot_indecies <- seq(first_value, last_value, by=as.integer(knots_range/10))
  knots_x <- spln$x[seq(1, length(spln$x), by = length(spln$x)/length(spln$fitted.values))]
  knots_y <- spln$fitted.values
  plot(neural_data[time_slice,1], neural_data[time_slice,2], type="l", col="red", ylab="Voltage", xlab="Time")
  lines(knots_x, knots_y, type="l", lwd=3, col="black")
  lines(neural_data[time_slice,1], spln$fitted.values, type="l", lwd=3, col="purple")
  legend(x = "topleft", legend = c("Raw signal from channel 2", "Knots of spline fitted through raw signal", "Full spline"), col=c("red","black", "purple"), lwd = c(1,2), inset = 0.02, cex = 0.8)
}


plot_spline(time_slice = 447000:557000)

