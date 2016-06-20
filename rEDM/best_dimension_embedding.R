#Import the rEDM library
library(rEDM)

#you need to import the neural_data.txt file by hand.



len <- 1000 #Length of each segment
loop_seq <- seq(1, length(neural_data$X1.Chan..1)-len, by=len) #sequence of segment separations
output <- replicate(length(loop_seq), NA) #an empty list to store the output

#for each segment
for (i in loop_seq)
{
  #get the segment of channel 1
  Ch1 <- neural_data$X1.Chan..1[i:(i+len)]
  
  lib <- c(1, length(Ch1))
  pred <- c(1, length(Ch1))
  
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:32)
  #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
  #     ylab = "Forecast Skill (rho)")
  
  bestE <- which.max(simplex_output$rho)
  output[[(i/len)+1]] <- bestE
  print(paste("finished", i))
}

plot(output, main="The best embedding dimension from 1 to 30", xlab="Second", ylab="Best embedding dimension")