#Import the rEDM library
library(rEDM)

#you need to import the neural_data.txt file by hand.


len <- 1000 #the length of each segment
#loop_seq <- seq(1, length(neural_data$X1.Chan..1)-len, by=len) #segments that separate the data
loop_seq <- seq(1, length(neural_data$Time)/10, by=len) #segments that separate the data
Ch1_Ch2_rho <- replicate(length(loop_seq), NA) #empty list to contain the causality measure
Ch2_Ch1_rho <- replicate(length(loop_seq), NA) #empty list to contain the caucality measure
Ch1_Ch2_rho.sd <- replicate(length(loop_seq), NA) #empty list to contain the standard deviation of the causality measure
Ch2_Ch1_rho.sd <- replicate(length(loop_seq), NA) #empty list to contain the standard deviation of the causality measure

#get the first .2 seconds of the data
for (i in loop_seq)
{
  
  #get the segment of the data from channel 1 and 2
  Ch1 <- neural_data$X3.Chan..3[i:(i+len)]
  Ch2 <- neural_data$X6.Chan..6[i:(i+len)]
  
  #get the segment of the data from challen 1 and 2. used for different purpose
  nd <- neural_data[i:(i+len),]
  lib <- c(1, length(nd))
  pred <- c(1, length(nd))
  
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:7)
#   plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
#       ylab = "Forecast Skill (rho)")
  bestE <- which.max(simplex_output$rho)
  print(paste("best E", bestE))
  
  #get the convergent cross map calculations
  Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = 1, first_column_time = TRUE,
                      target_column = 2, lib_sizes = len)
  
  
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch2, lib, pred, E=1:7)
#   plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
#        ylab = "Forecast Skill (rho)")
  bestE <- which.max(simplex_output$rho)
  print(paste("best E", bestE))
  
  #get the ccm models 
  Ch2_xmap_Ch1 <- ccm(nd, E = bestE, lib_column = 2, first_column_time = TRUE,
                      target_column = 1, lib_sizes = len)
  
  
  #take the means of the ccm's and get the standard deviation
  ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
                    tapply(rho, lib_size, sd)))
  ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
                    tapply(rho, lib_size, sd)))
  
  
  #record the ccm measure and the standard deviation in the output lists
  Ch1_Ch2_rho.sd[[(i/len)+1]] <- ch2_map_1_mean$sd.rho
  Ch1_Ch2_rho[[(i/len)+1]] <- ch2_map_1_mean$rho
  Ch2_Ch1_rho.sd[[(i/len)+1]] <- ch1_map_2_mean$sd.rho
  Ch2_Ch1_rho[[(i/len)+1]] <- ch1_map_2_mean$rho
  
  #print progress
  print(paste("finished", i))
}