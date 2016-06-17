
library(rEDM)

nd <- neural_data[1:200,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

for (i in 1:30)
{
  i2 = i+1
  for(j in i2:31)
  {
    Ch1 <- nd$X1.Chan..1
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch1, lib, pred, E=1:32)
    #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
    #     ylab = "Forecast Skill (rho)")
    bestE <- which.max(simplex_output$rho)
    
    
    #get the convergent cross map calculations
    Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = i, first_column_time = TRUE,
                        target_column = j, lib_sizes = seq(2, 80, by = 2))
    
    
    
    Ch2 <- nd$X2.Chan..2
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch2, lib, pred, E=1:32)
    par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
    #     ylab = "Forecast Skill (rho)")
    bestE <- which.max(simplex_output$rho)
    
    
    #get the ccm models 
    Ch2_xmap_Ch1 <- ccm(nd, E = bestE, lib_column = j, first_column_time = TRUE,
                        target_column = i, lib_sizes = seq(2, 80, by = 2))
    
    #take the means of the ccm's
    #simple version
    #ch1_map_2_mean <- ccm_means(Ch1_xmap_Ch2)
    #ch2_map_1_mean <- ccm_means(Ch2_xmap_Ch1)

    
    ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
        tapply(rho, lib_size, sd)))
    ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
        tapply(rho, lib_size, sd)))

    #plot the results
    par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    plot(ch1_map_2_mean$lib_size, pmax(0, ch1_map_2_mean$rho), type = "l", col = "red", 
         xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim = c(0,1), lwd=3)
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
    
    dev.copy(jpeg,filename=paste("~/Desktop/SIP/Midterm/rEDM/plots/",i,"-and-",j,"-maps.jpg"))
    dev.off ()
  }
}
