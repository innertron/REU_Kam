#Import the rEDM and igraph libraries
library(rEDM)
library(igraph)

#you need to import the neural_data.txt file by hand.

#get the first .2 seconds of the data
nd <- neural_data[1:1000,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))
lag_span <- 100

from <- c()
to <- c()
strength <- c()

lag_seq <- seq(-lag_span,lag_span, by=10)
for (i in 1:31)
{
  
  Ch1 <- nd[,i]
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:6)
  #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  bestE_i <- which.max(simplex_output$rho)
  
  best_rhos <- c()
  left_lag_means <- c()
  #for all nodes other than the current one
  for(j in (1:31)[-i])
  {
    rhos_1 <- c()
    rhos_2 <- c()
    
    Ch_2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch_2, lib, pred, E=1:6)
    #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    bestE_j <- which.max(simplex_output$rho)
    
    for (tp in lag_seq)
    {
      #get the convergent cross map calculations
      Ch1_xmap_Ch2 <- ccm(nd, E = bestE_i, lib_column = i, first_column_time = TRUE, tp=tp,
                          target_column = j, lib_sizes = 80)
      
      #take the means of the ccm's and get the standard deviation
      ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
        tapply(rho, lib_size, sd)))
      rhos_1 <- c(rhos_1, ch1_map_2_mean$rho)
      
      #get the convergent cross map calculations
      Ch2_xmap_Ch1 <- ccm(nd, E = bestE_j, lib_column = j, first_column_time = TRUE, tp=tp,
                          target_column = i, lib_sizes = 80)
      
      #take the means of the ccm's and get the standard deviation
      ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
        tapply(rho, lib_size, sd)))
      rhos_2 <- c(rhos_2, ch2_map_1_mean$rho)
      
    }
    
    plot(x=lag_seq, y=rhos_1, col="red", main=paste(i, "and", j, "lagged prediction on the firs second"), ylim=c(0,1))
    points(x=lag_seq, y=rhos_2, col="blue")
    legend(x = "topleft", legend = c(paste(i,"xmap",j), paste(j,"xmap",i)), col = c("red", 
         "blue"), lwd = 3)
    #find directionality of the interaction. If the directionality is pointing in that direction
    #Ch1 -> Ch_2, then create an edge with weight = rho
    left_mean_diff <- mean(rhos_2[1:(length(rhos_2)/2)]) - mean(rhos_1[1:(length(rhos_1)/2)])
    # right_mean_diff <- mean(rhos_2[(length(rhos_2)/2+1):length(rhos_2)]) - mean(rhos_1[(length(rhoss_1)/2)+1:length(rhos_1)])
    
    best_rhos <- c(best_rhos, rhos_2[as.integer(length(rhos_2)/2)+1])
    left_lag_means <- c(left_lag_means, left_mean_diff)
    print(paste(i,j,"rho",rhos_2[as.integer(length(rhos_2)/2)+1],"left_mean_diff:",left_mean_diff))
  }
  
  best_rho <- max(best_rhos)
  best_node_connection <- which.max(best_rhos)
  best_rho_left_lag_mean <- left_lag_means[best_node_connection]
  print(paste("from",i,"to",best_node_connection,best_rho, "lag mean:",best_rho_left_lag_mean))
  if (best_rho_left_lag_mean > 0)
  {
    from <- rbind(from, i)
    to <- rbind(to, best_node_connection)
    strength <- rbind(strength, best_rho)
    print(paste("from",i,"to",best_node_connection,best_rho))
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
netw <- data.frame(from=from, to=to, strength=strength)

#get the network without the weights, only nodes and edges relations
g <- graph.edgelist(as.matrix(netw[-3]))

#set the edges weights, multiply by five to make it visible
E(g)$width <- netw$strength
E(g)$curved <- 0.4
plot(g)
# 
# #make an igraph object from the network
# gra <- graph_from_data_frame(netw)
# 
# for each n in nodes:
#   for each v in nodes connected to n:
#     l = lag of best ccm from v xmap n
#     



