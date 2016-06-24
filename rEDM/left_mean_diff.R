left_mean_diff <- function(i, j, span=30, by=1)
{
  #Import the rEDM library
  library(rEDM)
  
  #you need to import the neural_data.txt file by hand.
  #get the first .2 seconds of the data
  nd <- neural_data[1:1000,]
  lib <- c(1, length(nd))
  pred <- c(1, length(nd))

  output_1_map_2 <- c()
  output_2_map_1 <- c()
  tp_span <- seq(-span, span, by=by)
  for (tp in tp_span)
  {
    Ch1 <- nd[,i]
    #run choose the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch1, lib, pred, E=1:6)
    bestE <- which.max(simplex_output$rho)
    
    
    #get the convergent cross map calculations
    Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = i, first_column_time = TRUE, tp=tp,
                        target_column = j, lib_sizes = 80)
    
    Ch2 <- nd[,j]
    #run and choose the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch2, lib, pred, E=1:6)
    bestE <- which.max(simplex_output$rho)
    
    #get the ccm models 
    Ch2_xmap_Ch1 <- ccm(nd, E = bestE, lib_column = j, first_column_time = TRUE, tp=tp,
                        target_column = i, lib_sizes = 80)
    
    #take the means of the ccm's and get the standard deviation
    ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
                                                                        tapply(rho, lib_size, sd)))
    ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
                                                                        tapply(rho, lib_size, sd)))
    
    output_1_map_2 <- c(output_1_map_2, ch1_map_2_mean$rho)
    output_2_map_1<- c(output_2_map_1, ch2_map_1_mean$rho)
  }
  
  
  save_file <- "~/Desktop/SIP/Code/rEDM/lag\ plots/"
  file_name <- paste(save_file,i,"-and-",j,"-laged.jpg", sep="")
  # png(file=file_name,width=600,height=525)
  
  plot(x=tp_span, y=output_1_map_2, col="red",
       main=paste("Causality measure over lagged predictions of channels \n",i, "and", j),
       ylim = c(0,1), ylab="cross map skill (rho)", xlab="cross map lag")
  legend(x = "topleft", legend = c(paste(i,"xmap",j), paste(j,"xmap",i)), col = c("red", 
         "blue"), lwd = 3)
  points(x=tp_span, y=output_2_map_1, col="blue")
  
  dev.off()
  i_map_j_left_mean <- mean(output_1_map_2[1:(length(output_1_map_2)/2)]) - mean(output_2_map_1[1:(length(output_2_map_1)/2)])
}