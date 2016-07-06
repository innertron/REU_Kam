#this script converts the neural data collected 
library(eegkit)

time_slice = 1:1000
nknots = as.integer(length(time_slice)/10)

splined_data <- lapply(2:32, function(i){
  spln <- eegsmooth(neural_data[time_slice,i], time=neural_data[time_slice,1], nknots=nknots)
  indecies <- spln$x %in% spln$myknots
  x <- spln$x[indecies]
  y <- spln$fitted.values[indecies]
  data.frame(y)
})

#do one more time to calculate the time indecies
spln <- eegsmooth(neural_data[time_slice,2], time=neural_data[time_slice,1], nknots=nknots)
indecies <- spln$x %in% spln$myknots
x <- spln$x[indecies]

do.call(data.frame, splined_data)
splined_data <- as.data.frame(splined_data)
colnames(splined_data) <- lapply(2:32, function(i) paste("voltage.",i,sep=""))
splined_data["Time"] <- x

splined_data <- splined_data[,c(32, 1:31)]
