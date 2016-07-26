lump_channels_by_region <- function()
{
  #channels 1 and two
  ss.forelimb <- colMeans(rbind(neural_data[, 2]))
  
  ss.trunk <- rowMeans(rbind(neural_data[, c(11)]))
  
  prim.vis <- rowMeans(rbind(neural_data[, c(5, 6, 7, 8, 14, 15, 16, 24)]))

  prim.mot <- rowMeans(rbind(neural_data[, c(17, 18, 19)]))
  
  ss.hindlimb <- rowMeans(rbind(neural_data[, c(9,10)]))
  
  second.vis <- rowMeans(rbind(neural_data[, c(21, 22, 23, 13)]))
  
  second.mot <- rowMeans(rbind(neural_data[, c(25,26)]))

  parietal.postdorsal <- colMeans(rbind(neural_data[, 4]))

  medial.parietal.assoc <- colMeans(rbind(neural_data[, 20]))

  lateral.parietal.assoc <- colMeans(rbind(neural_data[, 12]))
  
  ss.disgranular <- colMeans(rbind(neural_data[, c(3)]))

  retrosplenial.disgran <- rowMeans(rbind(neural_data[, c(27, 28, 29, 30, 31)]))
  
  data.frame(Time = neural_data[,1], ss.forelimb = ss.forelimb, ss.disgranular = ss.disgranular, parietal.postdorsal = parietal.postdorsal, prim.vis = prim.vis,  ss.hindlimb = ss.hindlimb, ss.trunk = ss.trunk, 
    lateral.parietal.assoc = lateral.parietal.assoc, second.vis = second.vis, prim.mot = prim.mot, 
    medial.parietal.assoc = medial.parietal.assoc, second.mot = second.mot,
   retrosplenial.disgran = retrosplenial.disgran)
}

regional_neural_data <- lump_channels_by_region()