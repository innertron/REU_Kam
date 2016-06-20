#Import the rEDM library
library(rEDM)

#you need to import the neural_data.txt file by hand.


len <- 1000
loop_seq <- seq(1, length(neural_data$X1.Chan..1)-len, by=len)
Ch1_Ch2_rho <- replicate(length(loop_seq), NA)
Ch2_Ch1_rho <- replicate(length(loop_seq), NA)
Ch1_Ch2_rho.sd <- replicate(length(loop_seq), NA)
Ch2_Ch1_rho.sd <- replicate(length(loop_seq), NA)

#get the first .2 seconds of the data
for (i in loop_seq)
{
  
  Ch1 <- neural_data$X1.Chan..1[i:(i+len)]
  Ch2 <- neural_data$X2.Chan..2[i:(i+len)]
  
  nd <- neural_data[i:(i+len),]
  lib <- c(1, length(nd))
  pred <- c(1, length(nd))
  
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:32)
  #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
  #     ylab = "Forecast Skill (rho)")
  bestE <- which.max(simplex_output$rho)
  
  
  #get the convergent cross map calculations
  Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = 1, first_column_time = TRUE,
                      target_column = 2, lib_sizes = len)
  
  
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch2, lib, pred, E=1:32)
  #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
  #     ylab = "Forecast Skill (rho)")
  bestE <- which.max(simplex_output$rho)
  
  
  #get the ccm models 
  Ch2_xmap_Ch1 <- ccm(nd, E = bestE, lib_column = 2, first_column_time = TRUE,
                      target_column = 1, lib_sizes = len)
  
  
  #take the means of the ccm's and get the standard deviation
  ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
                                                                      tapply(rho, lib_size, sd)))
  ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
                                                                      tapply(rho, lib_size, sd)))
  
  
  Ch1_Ch2_rho.sd[[(i/len)+1]] <- ch2_map_1_mean$sd.rho
  Ch1_Ch2_rho[[(i/len)+1]] <- ch2_map_1_mean$rho
  Ch2_Ch1_rho.sd[[(i/len)+1]] <- ch1_map_2_mean$sd.rho
  Ch2_Ch1_rho[[(i/len)+1]] <- ch1_map_2_mean$rho
  
  print(paste("finished", i))
  #specify the filename and plotting size
#   save_file <- "~/Desktop/SIP/Code/rEDM/plots/"
#   file_name <- paste(save_file,i,"-and-",j,"-maps.jpg", sep="")
#   png(file=file_name,width=600,height=525)
#   
#   
#   #plot the results
#   plot(ch1_map_2_mean$lib_size, pmax(0, ch1_map_2_mean$rho), type = "l", col = "red", 
#        xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim = c(0,1), lwd=3,
#        main=paste("Sugihara causality measure between channels ", i, "and", j))
#   lines(ch2_map_1_mean$lib_size, pmax(0, ch2_map_1_mean$rho), col = "blue")
#   legend(x = "topleft", legend = c(paste("Ch",i,"xmap Ch",j), paste("Ch",j," xmap Ch",i)), col = c("red", 
                   #                                                                                "blue"), lwd = 3, inset = 0.02, cex = 0.8)
  
  # Add CI's
#   lines(ch1_map_2_mean$lib_size, ch1_map_2_mean$rho + ch1_map_2_mean$sd.rho, col = "red", 
#         lty = 3, lwd = 1)
#   lines(ch1_map_2_mean$lib_size, ch1_map_2_mean$rho - ch1_map_2_mean$sd.rho, col = "red", 
#         lty = 3, lwd = 1)
#   lines(ch2_map_1_mean$lib_size, ch2_map_1_mean$rho + ch2_map_1_mean$sd.rho, col = "blue", 
#         lty = 3, lwd = 1)
#   lines(ch2_map_1_mean$lib_size, ch2_map_1_mean$rho - ch2_map_1_mean$sd.rho, col = "blue", 
#         lty = 3, lwd = 1)
#   
#   #turn the plotting device off to save the plot and report progress
#   dev.off ()
#   print(paste("plotted ",i, "and",j))
}