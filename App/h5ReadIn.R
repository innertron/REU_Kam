library(rhdf5)
# setwd("~/") must set the working directory to the current one
EstCSD<-h5read('kamal_data3.h5', '/est_csd')
EstPot<-h5read('kamal_data3.h5', '/est_pot')
SpaceX<-c(h5read('kamal_data3.h5', '/space_X'))
SpaceY<-c(h5read('kamal_data3.h5', '/space_Y'))
KCSDPar<-h5read('kamal_data3.h5', '/kcsd_result')
pots <- h5read('kamal_data3.h5', '/pots')
H5close()
Zlim<-range(EstCSD)

# for(i in 1:length(EstPot[,1,1])){
#   image(unique(SpaceX)*1000, unique(SpaceY)*1000, t(data.matrix(EstCSD[i,,])), col=rainbow(200),zlim=Zlim)
#   Sys.sleep(0.25)
# }


