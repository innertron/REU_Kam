lump_channels_by_region <- function()
{
  #channels 1 and two
  prim.soma.forlimb <- colMeans(rbind(neural_data[, 2]))
  
  prim.soma.trunk <- rowMeans(rbind(neural_data[, c(3,4,11,12,19,20,27,28,29)]))
  
  prim.vis <- rowMeans(rbind(neural_data[, c(5,6,7,8,14,15,16,24)]))
  
  prim.soma.hindlimb <- rowMeans(rbind(neural_data[, c(9,10)]))
  
  second.vis <- rowMeans(rbind(neural_data[, c(13,21,22,23)]))
  
  second.mot <- rowMeans(rbind(neural_data[, c(25,26)]))
  
  prim.soma.disgranular <- colMeans(rbind(neural_data[, c(30)]))
  
  data.frame(Time = neural_data[,1], prim.soma.forlimb = prim.soma.forlimb, prim.soma.trunk = prim.soma.trunk, prim.vis = prim.vis, prim.soma.hindlimb = prim.soma.hindlimb, second.vis = second.vis, second.mot = second.mot, prim.soma.disgranular = prim.soma.disgranular)
}

regional_neural_data <- lump_channels_by_region()