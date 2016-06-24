#Import the rEDM library
library(rEDM)

#you need to import the neural_data.txt file by hand.

#get the first .2 seconds of the data
nd <- neural_data[1:1000,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

for (i in 1:30)
{
  i2 = i+1
  for(j in i2:31)
  {
    Ch1 <- nd[,i]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch1, lib, pred, E=1:6)
    #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
    #     ylab = "Forecast Skill (rho)")
    bestE <- which.max(simplex_output$rho)
    
    
    #get the convergent cross map calculations
    Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = i, first_column_time = TRUE,
                        target_column = j, lib_sizes = seq(10, 80, by = 10), random_libs = FALSE)
    
    
    
    Ch2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch2, lib, pred, E=1:6)
    #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
    #     ylab = "Forecast Skill (rho)")
    bestE <- which.max(simplex_output$rho)
    
    
    #get the ccm models 
    Ch2_xmap_Ch1 <- ccm(nd, E = bestE, lib_column = j, first_column_time = TRUE,
                        target_column = i, lib_sizes = seq(10, 80, by = 10), random_libs=FALSE)
    
    #take the means of the ccm's and get the standard deviation
    ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
                                                                        tapply(rho, lib_size, sd)))
    ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
                                                                        tapply(rho, lib_size, sd)))
    
    
    #specify the filename and plotting size
#     save_file <- "~/Desktop/SIP/Code/rEDM/plots/"
#     file_name <- paste(save_file,i,"-and-",j,"-maps.jpg", sep="")
#     png(file=file_name,width=600,height=525)
    
    
    #plot the results
    plot(ch1_map_2_mean$lib_size, pmax(0, ch1_map_2_mean$rho), type = "l", col = "red", 
         xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim = c(0,1), lwd=3,
         main=paste("Sugihara causality measure between channels ", i, "and", j, "\n during first second"))
    lines(ch2_map_1_mean$lib_size, pmax(0, ch2_map_1_mean$rho), col = "blue")
    legend(x = "topleft", legend = c(paste("Ch",i,"xmap Ch",j), paste("Ch",j," xmap Ch",i)), col = c("red", 
                                                                                                     "blue"), lwd = 3, inset = 0.02, cex = 0.8)
    
    # Add CI's
    lines(ch1_map_2_mean$lib_size, ch1_map_2_mean$rho + ch1_map_2_mean$sd.rho, col = "red", 
          lty = 3, lwd = 1)
    lines(ch1_map_2_mean$lib_size, ch1_map_2_mean$rho - ch1_map_2_mean$sd.rho, col = "red", 
          lty = 3, lwd = 1)
    lines(ch2_map_1_mean$lib_size, ch2_map_1_mean$rho + ch2_map_1_mean$sd.rho, col = "blue", 
          lty = 3, lwd = 1)
    lines(ch2_map_1_mean$lib_size, ch2_map_1_mean$rho - ch2_map_1_mean$sd.rho, col = "blue", 
          lty = 3, lwd = 1)
    
    #turn the plotting device off to save the plot and report progress
    # dev.off ()
    print(paste("plotted ",i, "and",j))
  }
}
