###This script creates a graph by createing an edge from each node to the node that 
#it is caused by the most. It does that while taking into account the lagging indicator
#in the case of observing unidirectional forcing between the two signals

#The script calculate_pair_causality.R needs to be run first to create the newt data frame

#Create empty lists to hold data
graph_from <- c()
graph_to <- c()
graph_strength <- c()

#for each region find the highest rho value with positive left_mean
for (i in 1:31)
{
  #get the information for that region
  edge_list <- subset(netw, to==i)
  #add a self-feeding loop to correct the one off error
  edge_list <- rbind(edge_list, c(i, i, -Inf, -Inf))
  
  #get the descending order of rho variables
  ord <- order(edge_list$strength, decreasing = TRUE)
  best_rho <- 0
  best_o <- NA
  
  #find the highest rho with positive left lag mean difference
  for (o in ord)
  {
    if (edge_list$strength[o] > best_rho & edge_list$left_mean[o] > 0)
    {
      best_rho <- edge_list$strength[o]
      best_o <- o
    }
  }
  
  #if a causal relationship was found, create an edge from it
  if (!is.na(best_o))
  {
    print(paste("max for ",i," is ", edge_list$from[best_o]))
    #since channel 1 is missing, offset all connections by 1
    graph_from <- c(graph_from, edge_list$from[best_o]+1)
    graph_to <- c(graph_to, i+1)
    graph_strength <- c(graph_strength, best_rho)
  }
}

#combine the lists into a data frame
filtered_netw <- data.frame(from=graph_from, to=graph_to, strength=graph_strength)

#create an igraph without the weights, only nodes and edges relations
g <- graph.edgelist(as.matrix(filtered_netw[-c(3,4)]))


#set the edges weights, multiply by five to make it visible
E(g)$width <- filtered_netw$strength*2
E(g)$curved <- 0.4
l = layout.grid(g, width=8)
plot(g, edge.arrow.size=.3, layout=l, vertex.size = 7, vertex.label.cex=.7, main="A network of rat brain regions constructed from linking \n each node to the one it is caused by the most.")
l = layout.fruchterman.reingold(g)
plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main="A network of rat brain regions constructed from linking \n each node to the one it is caused by the most.")
