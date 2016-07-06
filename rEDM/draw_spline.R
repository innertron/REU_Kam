#this script draws a simple spline through an eeg data timeline
library(eegkit)

#specify the time_slice and number of knots to use in this experiment
time_slice = 1:1000
nknots = length(time_slice)/10

#get the spline
spln <- eegsmooth(neural_data[time_slice, 2], time=neural_data[time_slice,1], nknots = nknots)

#either plot the smooth spline
plot(neural_data[time_slice,1], neural_data[time_slice,2], type="l", col="red")
lines(spln$x, spln$fitted.values, type="l", lwd=2)

#or plot just the knot points
knot_indecies <- spln$x %in% spln$myknots
knots_x <- spln$x[knot_indecies]
knots_y <- spln$fitted.values[knot_indecies]
plot(neural_data[time_slice,1], neural_data[time_slice,2], type="l", col="red", ylab="Voltage", xlab="Time")
lines(knots_x, knots_y, type="l", lwd=2)