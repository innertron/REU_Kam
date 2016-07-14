#this script produces surrogate data and calculates the ccm measure for
#each such surrogate data
#must run lump_channels_by_region.R first

#import libraries
library(eegkit)
library(rEDM)
library(snow)
library(parallel)

#for each brain region pair
combinations <- t(combn(2:7, 2))

time.window <- 2000
num.regions <- 6
num.windows <- as.integer(length(regional_neural_data[,1])/time.window)


#calculates and returns a data frame that contains to, from, rho, and random shuffle columns
calc_pair_causality <- function(i, j, start.time)
{
  end.time = start.time + time.window
  Ch2 <- regional_neural_data[start.time:end.time, j]
  #run simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch2, c(1, time.window), c(1, time.window), E=1:6)
  bestE_j <- which.max(simplex_output$rho)
  
  #get the ccm models 
  Ch2_xmap_Ch1 <- ccm(regional_neural_data[start.time:end.time,], E = bestE_j, lib_column = j, first_column_time = FALSE,
    target_column = i, lib_sizes = 80, random_libs=TRUE, num_samples=20)
  
  ch2_map_1_mean <- ccm_means(Ch2_xmap_Ch1)
  
  print(paste("finished", i, j))
  c(to=j, from=i, libs=ch2_map_1_mean$lib_size,random_shuffle=NA, rho=ch2_map_1_mean$rho,
    start.time=start.time, time.window=time.window)
}

#make function to calculate surrogate data
calculate_pair_surrogate_causality <- function(i, j, start.time, placeholder)
{
  end.time <- start.time+time.window
  sur.dat <- cbind(regional_neural_data[start.time:end.time,1],
    regional_neural_data[start.time:end.time,i],
    make_surrogate_data(regional_neural_data[start.time:end.time,j], num_surr = 1))

  Ch2 <- regional_neural_data[start.time:end.time, j]

  #run simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch2, c(1, time.window), c(1, time.window), E=1:6)
  bestE_j <- which.max(simplex_output$rho)
  Ch2_xmap_Ch1 <- ccm(sur.dat, E = bestE_j, lib_column = 3, first_column_time = FALSE,
    target_column = 2, lib_sizes = 80, random_libs=TRUE, num_samples=20)
  
  ch2_map_1_mean <- ccm_means(Ch2_xmap_Ch1)
  
  c(to=j, from=i, libs=ch2_map_1_mean$lib_size, random_shuffle=max(0, ch2_map_1_mean$rho), rho=NA,
    start.time=start.time, time.window=time.window)
}


cl<-makeCluster(detectCores(), type="SOCK")

clusterExport(cl, c("regional_neural_data", 'simplex', 'ccm', 'ccm_means', 'time.window', 'make_surrogate_data'))


start.times <- seq(0,length(regional_neural_data[,1]), by=time.window)

#calculate observed causality going one way
observed <- rbind(
  t(as.data.frame(clusterMap(cl, calc_pair_causality, combinations[,1], combinations[,2], start.times))),
  t(as.data.frame(clusterMap(cl, calc_pair_causality, combinations[,2], combinations[,1], start.times)))
)


tick = proc.time()
permutation_data <- t(as.data.frame(clusterMap(cl,
 calculate_pair_surrogate_causality,
  combinations[,1],
   combinations[,2],
    rep(start.times, 100*length(combinations[,2]))
    )))
tock = proc.time() - tick
print(tock)
stopCluster(cl)

permutation_test_data <- data.frame(rbind(permutation_data, observed))