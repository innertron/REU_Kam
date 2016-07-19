make_threshold_causality_graph <- function(netw, threshold=0.8, strt.tim=0, plt_main=paste("A network of rat brain regions constructed from linking \n each node to the one it is caused by above rho ", "\n at start time", strt.tim))
{
  #The script calculate_pair_causality.R needs to be run first to create the netw data frame
  # setwd("/Users/rorylewis/Documents/96_Kamal/REU_Kam")
  library(igraph)
  
  edge.list <- subset(as.data.frame(netw), rho >= threshold & start.time==strt.tim, drop=T)

  if (length(edge.list$to) > 0)
  {
    #create an igraph without the weights, only nodes and edges relations
    g <- graph.edgelist(as.matrix(edge.list[,1:2]))
    
    #set the edges weights, multiply by five to make it visible
    E(g)$width <- edge.list$rho*2
    E(g)$curved <- 0.4
    l = layout.grid(g, width=8)
    
    save_file <- "~/Desktop/SIP/Code/rEDM/region\ threshold\ graphs/"
    file_name <- paste(save_file,strt.tim,"-threshold-graph.pdf", sep="")
    pdf(file=file_name)
    plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=plt_main)
    l = layout.fruchterman.reingold(g)
    plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=plt_main)
    dev.off()
  }
}

for (i in start.times)
{
  make_threshold_causality_graph( threshold.data, strt.tim = i)  
}

  
  