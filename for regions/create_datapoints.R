#This script creates data points of each time step. Each data point contains as features the
#strength of edges (causality measure) between each pair of regions.
library(parallel)
getDataPoint <- function(strt.tim)
{
  froms <- unique(pairwise.causality.data$from)
  tos <- unique(pairwise.causality.data$to)
  
  node.range <- range(froms)[1]:range(tos)[2]
  
  d <- c(strt.tim)
  for (i in node.range)
  {
    for (j in node.range[-i+1])
    {
      rho <- unlist(subset(pairwise.causality.data, select=rho, from==i & to==j & start.time==strt.tim,
        drop=T))
      d <- cbind(d, rho)
    }
  }
  print(paste("finished", strt.tim))
  d
}


causality.graph.data <- t(mcmapply(getDataPoint, unique(pairwise.causality.data$start.time)))


for (index in 1:length(causality.graph.data[,1]))
{
  tim <- causality.graph.data[index, 1]
  if (tim >=447000 & tim <= 457000 || tim >= 253900 & tim <= 259000)
  {
    causality.graph.data[index, 1] <- "seizure"
  }
  else if (tim >=250900 & tim <= 252400)
  {
    causality.graph.data[index, 1] <- "initiation"
  }
  else if (tim >=252500 & tim <= 253800)
  {
    causality.graph.data[index, 1] <- "interictal"
  }
  else
  {
    causality.graph.data[index, 1] <- "none"
  }
}

mydata <- data.frame(causality.graph.data)
