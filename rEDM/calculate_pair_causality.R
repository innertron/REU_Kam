#This script creates a data frame that contains causality measures from each region
#to another measure based on a specific period and certain library length

#Import the rEDM and igraph libraries
library(rEDM)
library(igraph)

#you need to import the neural_data.txt file by hand.

time_span = 1:10000
#get the first second of the data
nd <- neural_data[time_span,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

from <- c()
to <- c()
left_mean <- c()
strength <- c()

for (i in 1:30)
{
  Ch1 <- nd[,i]
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:6)
  bestE_i <- which.max(simplex_output$rho)
  rhos <- c()
  
  i2 = i+1
  #for all nodes other than the current one, look for the edge that
  #most causes this one (i) and form an edge from it to i.
  for(j in i2:31)
  {
    
    #get the convergent cross map calculations
    Ch2_xmap_Ch1 <- ccm(nd, E = bestE_i, lib_column = j, first_column_time = TRUE,
                        target_column = i, lib_sizes = 80, num_samples = 20)
    
    #take the means of the ccm's and get the standard deviation
    ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
                       tapply(rho, lib_size, sd)))
    
    Ch2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch2, lib, pred, E=1:6)
    bestE_j <- which.max(simplex_output$rho)
    
    #get the convergent cross map calculations
    Ch1_xmap_Ch2 <- ccm(nd, E = bestE_j, lib_column = i, first_column_time = TRUE,
                        target_column = j, lib_sizes = 80, num_samples = 20)
    
    #take the means of the ccm's and get the standard deviation
    ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
                        tapply(rho, lib_size, sd)))
    
    
    #calculate the left mean difference for the causality
    left_mean_diff_i_j <- left_mean_diff(i, j)
    #record the rho measure for this connection
    from <- rbind(from, i)
    to <- rbind(to, j)
    strength <- rbind(strength, ch2_map_1_mean$rho)
    left_mean <- rbind(left_mean, left_mean_diff_i_j)
    print(paste("from", i, "to", j, ch2_map_1_mean$rho, "left_mean:", left_mean_diff_i_j))
    
    #and the connection the other way 
    from <- rbind(from, j)
    to <- rbind(to, i)
    strength <- rbind(strength, ch1_map_2_mean$rho)
    left_mean <- rbind(left_mean, -left_mean_diff_i_j)
    print(paste("from", j, "to", i, ch1_map_2_mean$rho, "left_mean:", -left_mean_diff_i_j))
  }
}


#create the network data frame for the data
netw <- data.frame(from=from, to=to, strength=strength, left_mean <- left_mean)