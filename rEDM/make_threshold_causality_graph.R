#The script calculate_pair_causality.R needs to be run first to create the newt data frame
library(igraph)
#set the threshold
threshold <- 0.8
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
  
  for (j in 1:length(edge_list[,1]))
  {
    if (edge_list$strength[j] > threshold & edge_list$left_mean[j] > 0)
    {
      graph_from <- c(graph_from, edge_list$from[j]+1)
      graph_to <- c(graph_to, i+1)
      graph_strength <- c(graph_strength, edge_list$strength[j])
    }
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
plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=(paste("A network of rat brain regions constructed from linking \n each node to the one it is caused by above rho ", threshold)))
l = layout.fruchterman.reingold(g)
plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=(paste("A network of rat brain regions constructed from linking \n each node to the one it is caused by above rho ", threshold)))

