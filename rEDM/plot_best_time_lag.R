#This script calculates the best lag variable from a range of 0 to 100 ms 
#and plots the data accordingly

library(rEDM) #import the convergent cross mapping library
experiment_span = 1000:2000 #run the analysis only on the first second of the experiment
time_lag_span <- seq(0, 500, by=50) #since each index represents a milli second, this is 0:100 ms span
nd <- neural_data[experiment_span,] #select the experiment span

plot(1, type="n", xlim=c(0, 400), ylim=c(0, 1), ylab="rho", xlab="Lag applied")

lag_data <- c()

#for each channel pair, calculate best skill
for(i in 2:31)
{
  Ch1 <- nd[,i]
  #run the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:6)
  bestE_i <- which.max(simplex_output$rho)
  
  i2 = i+1
  for(j in i2:32)
  {
    lag_index <- c()
    from_i <- c()
    to_j <- c()
    rho_i_j <- c()
    from_j <- c()
    to_i <- c()
    rho_j_i <- c()
    for (lag in time_lag_span)
    {
      Ch2 <- nd[,j]
      #run the simplex algorithm to get the best embedding dimension
      simplex_output <- simplex(Ch2, lib, pred, E=1:6)
      bestE_j <- which.max(simplex_output$rho)
      
      #get the convergent cross map calculations
      Ch2_xmap_Ch1 <- ccm(nd, E = bestE_i, lib_column = j, first_column_time = FALSE, tau=lag,
        target_column = i, lib_sizes = 80, num_samples = 20)
      
      #take the means of the ccm's and get the standard deviation
      ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
        tapply(rho, lib_size, sd)))
      
      
      
      #get the convergent cross map calculations
      Ch1_xmap_Ch2 <- ccm(nd, E = bestE_j, lib_column = i, first_column_time = FALSE, tau=lag,
         target_column = j, lib_sizes = 80, num_samples = 20)
      
      #take the means of the ccm's and get the standard deviation
      ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
        tapply(rho, lib_size, sd)))
      
      
      #add the data to the lists
      from_i <- c(from_i, i)
      to_j <- c(to_j, j)
      lag_index <- c(lag_index, lag)
      rho_i_j <- c(rho_i_j, ch2_map_1_mean$rho)
      
      from_j <- c(from_j, j)
      to_i <- c(to_i, i)
      rho_j_i <- c(rho_j_i, ch1_map_2_mean$rho)
      # print(paste("done for lag", lag))
    }
    
    #add the data to the frame, 
    lag_data <- rbind(lag_data,
                      data.frame(from=from_i, to=to_j, lag=lag_index, rho =rho_i_j),
                      data.frame(from=from_j, to=to_i, lag=lag_index, rho =rho_j_i))
    
    #plot the data from the previous run
    lines(time_lag_span, rho_i_j)
    lines(time_lag_span, rho_j_i)
    print(paste("finished", i, "and", j))
  }
}

dput(lag_data,"lag_data.RData")
