
#reconfigures the dimensions of the CSD data to make it a regular data frame matrix
#expectes a N x 4 x 8 array, where N is the number of data points collected and 4 x 8 is the
#dimensions in regards to the spatial coordinates
#time points is a matrix of time point indecies that will make the first column of the returned data frame
#MUST RUN h5ReadIn.R FIRST
melt_CSD <- function(CSD, time_points)
{
  result <- time_points
  for (x in 1:8)
  {
    for (y in 1:4)
    {
      result <- cbind(result, CSD[,y,x])
    }
  }
  result
}

lump_channels_by_region <- function(CSD)
{
  #channels 1 and two
  ss.forelimb <- colMeans(rbind(CSD[, 2]))
  
  ss.trunk <- rowMeans(rbind(CSD[, c(11)]))
  
  prim.vis <- rowMeans(rbind(CSD[, c(5, 6, 7, 8, 14, 15, 16, 24)]))
  
  prim.mot <- rowMeans(rbind(CSD[, c(17, 18, 19)]))
  
  ss.hindlimb <- rowMeans(rbind(CSD[, c(9,10)]))
  
  second.vis <- rowMeans(rbind(CSD[, c(21, 22, 23, 13)]))
  
  second.mot <- rowMeans(rbind(CSD[, c(25,26)]))
  
  parietal.postdorsal <- colMeans(rbind(CSD[, 4]))
  
  medial.parietal.assoc <- colMeans(rbind(CSD[, 20]))
  
  lateral.parietal.assoc <- colMeans(rbind(CSD[, 12]))
  
  ss.disgranular <- colMeans(rbind(CSD[, c(3)]))
  
  retrosplenial.disgran <- rowMeans(rbind(CSD[, c(27, 28, 29, 30, 31)]))
  
  data.frame(Time = CSD[,1], ss.forelimb = ss.forelimb, ss.trunk = ss.trunk, prim.vis = prim.vis,
             prim.mot = prim.mot, ss.hindlimb = ss.hindlimb, second.vis = second.vis, second.mot = second.mot,
             parietal.postdorsal = parietal.postdorsal, medial.parietal.assoc = medial.parietal.assoc,
             lateral.parietal.assoc = lateral.parietal.assoc, ss.disgranular = ss.disgranular, retrosplenial.disgran = retrosplenial.disgran)
}


CSD <- melt_CSD(EstCSD, neural_data[,1])
regional_EstCSD <- lump_channels_by_region(CSD)
