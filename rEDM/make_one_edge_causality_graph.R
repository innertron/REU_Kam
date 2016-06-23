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
  Ch1 <- nd[,i]
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:6)
  bestE_i <- which.max(simplex_output$rho)
  
  rhos <- c()
  #for all nodes other than the current one, look for the node
  #that this one (i) causes the most and form an edge to it
  for(j in (1:31)[-i])
  {
    
    Ch_2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch_2, lib, pred, E=1:6)
    bestE_j <- which.max(simplex_output$rho)
    
    #get the convergent cross map calculations
    Ch2_xmap_Ch1 <- ccm(nd, E = bestE_j, lib_column = j, first_column_time = TRUE,
                        target_column = i, lib_sizes = 80)
    
    #take the means of the ccm's and get the standard deviation
    ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
          tapply(rho, lib_size, sd)))
    
    #record the rho measure for this connection
    rhos[[j]] <- ch2_map_1_mean$rho
  }
  
  
  rhos[is.na(rhos)] <- 0 #replace NA with 0
  
  #take the heighest causality measure and form an edge from that
  best_rho <- max(rhos)
  best_node_connection <- which.max(rhos)
  from <- rbind(from, i)
  to <- rbind(to, best_node_connection)
  strength <- rbind(strength, best_rho)
  print(paste("from",i,"to",best_node_connection,best_rho))

}


#create the network data frame for the data
netw <- data.frame(from=from, to=to, strength=strength)
#create an igraph without the weights, only nodes and edges relations
g <- graph.edgelist(as.matrix(netw[-3]))


#set the edges weights, multiply by five to make it visible
E(g)$width <- netw$strength*2
E(g)$curved <- 0.4
l = layout.grid(g, width=8)
plot(g, edge.arrow.size=.3, layout=l, main="A network of rat brain regions constructed from linking \n each node to the one it causes the most")
l = layout.fruchterman.reingold(g)
plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main="A network of rat brain regions constructed from linking \n each node to the one it causes the most")