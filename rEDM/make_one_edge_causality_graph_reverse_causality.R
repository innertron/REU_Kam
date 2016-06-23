#Import the rEDM and igraph libraries
library(rEDM)
library(igraph)

#you need to import the neural_data.txt file by hand.

#get the first second of the data
nd <- neural_data[1:1000,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

from <- c()
to <- c()
strength <- c()

for (i in 1:31)
{
  
  rhos <- c()
  #for all nodes other than the current one, look for the edge that
  #most causes this one (i) and form an edge from it to i.
  for(j in (1:31)[-i])
  {
    
    Ch_2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch_2, lib, pred, E=1:6)
    bestE_j <- which.max(simplex_output$rho)
    
    #get the convergent cross map calculations
    Ch1_xmap_Ch2 <- ccm(nd, E = bestE_j, lib_column = i, first_column_time = TRUE,
      target_column = j, lib_sizes = 80)
    
    #take the means of the ccm's and get the standard deviation
    ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
      tapply(rho, lib_size, sd)))
    
    #record the rho measure for this connection
    rhos[[j]] <- ch1_map_2_mean$rho
  }
  
  
  rhos[is.na(rhos)] <- 0 #replace NA with 0
  
  #take the heighest causality measure and form an edge from that
  best_rho <- max(rhos)
  best_node_connection <- which.max(rhos)
  from <- rbind(from, best_node_connection)
  to <- rbind(to, i)
  strength <- rbind(strength, best_rho)
  print(paste("from", best_node_connection, "to", i, best_rho))
  
}


#create the network data frame for the data
netw <- data.frame(from=from, to=to, strength=strength)
#create an igraph without the weights, only nodes and edges relations
g <- graph.edgelist(as.matrix(netw[-3]))


#set the edges weights, multiply by five to make it visible
E(g)$width <- netw$strength*2
E(g)$curved <- 0.4
l = layout.grid(g, width=8)
plot(g, edge.arrow.size=.3, layout=l, vertex.size = 7, vertex.label.cex=.7, main="A network of rat brain regions constructed from linking \n each node to the one it is caused by the most")
l = layout.fruchterman.reingold(g)
plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main="A network of rat brain regions constructed from linking \n each node to the one it is caused by the most")
