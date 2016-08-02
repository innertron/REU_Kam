#This script creates data points of each time step. Each data point contains as features the
#strength of edges (causality measure) between each pair of regions.
library(parallel)

getDataPoint <- function(data, strt.tim)
{
  froms <- unique(data$from)
  tos <- unique(data$to)
  
  node.range <- range(froms)[1]:range(tos)[2]
  
  d <- c(strt.tim)
  for (i in node.range)
  {
    for (j in node.range[-i+1])
    {
      rho <- unlist(subset(data, select=rho, from==i & to==j 
        & start.time==strt.tim & time.window==window & time.step==step, drop=T))
      d <- cbind(d, rho)
    }
  }
  print(paste("finished", strt.tim))
  d
}

data <- subset(pair.causality.CSD.data, time.window==200 & time.step==50)
causality.graph.data <- t(mcmapply(getDataPoint, data, unique(data$start.time)))


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
