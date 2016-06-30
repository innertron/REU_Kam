#this script creates a graph whose nodes are brain regions and whose edges
#are causality relationships that are significant as defined by a random shuffle
#bootstrap permutation.

#import the igraph library
library(igraph)
#import the data if it is not already present
#dget("permutation_test_data")

#define the alpha value for significance
alpha_value <- 0.05

#create empty lists to add the node and edge relationsihps to 
to <- c()
from <- c()
strength <- c()

#for each pair
for(i in 1:31)
{  
  for(j in 1:31)
  {
    if(i != j)
    {
      #recover the observed and permutation values
      ch1_cause_2_observed <- subset(permutation_test_data, from==i & to==j & libs==80 &
                                       is.na(random_shuffle), drop=T, select = rho)
      ch1_cause_2_permutation <- subset(permutation_test_data, from==i & to==j & libs==80 &
                                          is.na(rho), drop=T, select = random_shuffle)
      
      #cauculate the P-value significance
      p_val = (sum(ch1_cause_2_observed < ch1_cause_2_permutation) + 1) / (length(ch1_cause_2_permutation) + 1)
      
      #if the causality is significant, create an edge that represnts it
      if (p_val < alpha_value)
      {
        from <- rbind(from, i)
        to <- rbind(to, j)
        strength <- rbind(strength, ch1_cause_2_observed)
        print(paste("from ",i,"to",j,"=",ch1_cause_2_observed, "pval =", p_val))
      }
    }
  }
}


#combine the lists into a data frame
filtered_netw <- data.frame(from=from, to=to, strength=strength)

#create an igraph without the weights, only nodes and edges relations
g <- graph.edgelist(as.matrix(filtered_netw[-c(3,4)]))


#set the edges weights, multiply by five to make it visible
E(g)$width <- filtered_netw$strength*2
E(g)$curved <- 0.4
l = layout.grid(g, width=8)
plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=("A network of rat brain regions constructed from linking \n all significant causality connections"))
l = layout.fruchterman.reingold(g)
plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=("A network of rat brain regions constructed from linking \n all significant causality connections"))


