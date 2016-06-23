#Import the rEDM and igraph libraries
library(rEDM)
library(igraph)

#you need to import the neural_data.txt file by hand.

#get the first .2 seconds of the data
nd <- neural_data[1:1000,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))
lag_span <- 200

from <- c()
to <- c()
strength <- c()

lag_seq <- seq(-lag_span,lag_span, by=10)
for (i in 1:31)
{
  best_rhos <- c()
  best_lags <- c()
  #for all nodes other than the current one
  for(j in (1:31)[-i])
  {
    rhos_1 <- c()
    rhos_2 <- c()
    Ch1 <- nd[,i]
    
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch1, lib, pred, E=1:6)
    #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    bestE <- which.max(simplex_output$rho)
    
    Ch_2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch_2, lib, pred, E=1:6)
    #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    bestE_2 <- which.max(simplex_output$rho)
    
    for (tp in lag_seq)
    {
      #get the convergent cross map calculations
      Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = i, first_column_time = TRUE, tp=tp,
                          target_column = j, lib_sizes = 80)
      
      #take the means of the ccm's and get the standard deviation
      ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
                                                                          tapply(rho, lib_size, sd)))
      rhos_1 <- c(rhos_1, ch1_map_2_mean$rho)
      
      #get the convergent cross map calculations
      Ch2_xmap_Ch1 <- ccm(nd, E = bestE_2, lib_column = j, first_column_time = TRUE, tp=tp,
                          target_column = i, lib_sizes = 80)
      
      #take the means of the ccm's and get the standard deviation
      ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
                                                                          tapply(rho, lib_size, sd)))
      rhos_2 <- c(rhos_2, ch2_map_1_mean$rho)
    }
    
    #find directionality of the interaction. If the directionality is pointing in that direction
    #Ch1 -> Ch_2, then create an edge with weight = rho
    
    
#     best_rho <- max(rhos)
#     best_rho_ind <- which.max(rhos)
#     best_lag <- -lag_span + best_rho_ind -1
#     
#     best_lags <- c(best_lags, best_lag)
#     best_rhos <- c(best_rhos, best_rho)

    plot(x=lag_seq, y=rhos_1, col="red", main=paste(i, "and", j, "lagged "), ylim=c(0,1))
    points(x=lag_seq, y=rhos_2, col="blue")
    # print(paste(i,j,"best rho:",best_rho, "best lag:",best_lag))
  }
}
# from <- rbind(from, i)
# to <- rbind(to, j)
# strength <- rbind(strength, ch1_map_2_mean$rho)
# 
# from <- rbind(from, j)
# to <- rbind(to, i)
# strength <- rbind(strength, ch2_map_1_mean$rho)
# 
# 
# netw <- data.frame(from=from, to=to, strength=strength)
# 
# #get the network without the weights, only nodes and edges relations
# g <- graph.edgelist(as.matrix(netw[-3]))
# 
# #set the edges weights, multiply by five to make it visible
# E(g)$width <- netw$strength
# E(g)$curved <- 0.4
# plot(g)
# 
# #make an igraph object from the network
# gra <- graph_from_data_frame(netw)
# 
# for each n in nodes:
#   for each v in nodes connected to n:
#     l = lag of best ccm from v xmap n
#     
    


