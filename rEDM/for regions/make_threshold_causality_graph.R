make_causality_graphs <- function(netw, threshold=0, strt.tim=0, plt_main=paste("A network of rat brain regions constructed from linking \n each node to the one it is caused by above rho ", threshold, "\n at start time", strt.tim))
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
    l[1,] <- c(0.5, 0) #ss forelimb
    l[2,] <- c(2, 0) #ss disgran
    l[3,] <- c(3, 0) #parietal, postdorsal
    l[4,] <- c(7, 0.5) #1st visual
    l[5,] <- c(0.5, 1) #ss hindlimb
    l[6,] <- c(2, 1) #ss trunk
    l[7,] <- c(3, 1) #lateral parietal assoc
    l[8,] <- c(5.5, 1.5) # 2nd vis
    l[9,] <- c(1,2) #1st motor
    l[10,] <- c(3,2) #medial parietal assoc
    l[11,] <- c(0.5, 3) #2nd motor
    l[12,] <- c(4, 3) #retrosplenial disgran
    
    #make a color pallete
    rbPal <- colorRampPalette(c('grey', 'yellow', 'green', 'blue','red'))
    #assign colors to edges
    E(g)$color <- rbPal(100)[as.numeric(ceiling(edge.list$rho*100))]
    save_file <- "~/plots/CSD/threshold\ graphs/"
    file_name <- paste(save_file,strt.tim,"-threshold-graph.pdf", sep="")
    pdf(file=file_name)
    plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=plt_main, asp=0.5)
    legend("bottom", legend=seq(0,1,0.1), col=rbPal(11), pch=20, ncol = 11)
    l = layout.fruchterman.reingold(g)
    plot(g, edge.arrow.size=.3, vertex.size = 7, vertex.label.cex=.7, layout=l, main=plt_main)
    dev.off()
  }
}

time.window <- 2000
start.times <- seq(0, length(regional_EstCSD[,1]), by=time.window)
for (i in start.times)
{
  make_causality_graphs( pair.causality.CSD.data, threshold = 0, strt.tim = i)  
}

  
  