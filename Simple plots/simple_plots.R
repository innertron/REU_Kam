
#Plot the first second of channel 1 and 2
Ch1<-subset(neural_data, select = c(Time, X1.Chan..1), Time > 0 & Time < 1, drop=T)
Ch2<-subset(neural_data, select = c(Time, X2.Chan..2), Time > 0 & Time < 1, drop=T)

plot(Ch1, ylab="Voltage", col="green", type="l", main="Simple plot of the first and second channel \n during first second")
lines(Ch2, type="l", col="red")
legend(0.025, 0.17, c("Channel 1", "Channel 2"), col=c("green", "red"), pch="ll")


#plot all the data from channel 1 and 2
Ch1<-subset(neural_data, select = c(Time, X1.Chan..1), drop=T)
Ch2<-subset(neural_data, select = c(Time, X2.Chan..2), drop=T)

plot(Ch1, ylab="Voltage", col="green", type="l", main="Simple plot of the first and second channel \n during the entire experiment")
lines(Ch2, type="l", col="red")
legend(4, 4, c("Channel 1", "Channel 2"), col=c("green", "red"), pch="ll")
