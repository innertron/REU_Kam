#this script produces surrogate data and calculates the ccm measure for
#each such surrogate data
#must run lump_channels_by_region.R first

#import libraries
library(eegkit)
library(rEDM)
library(snow)
library(parallel)

threshold <- 0.8

#for each brain region pair
combinations <- t(combn(2:13, 2))
#double the combinations to account for both directions
combinations <- (cbind(c(combinations[,1], combinations[,2]), c(combinations[,2], combinations[,1])))

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
  
  print(paste("finished", i, j, "for start time", start.time))
  c(to=j, from=i, libs=ch2_map_1_mean$lib_size,random_shuffle=NA, rho=ch2_map_1_mean$rho,
    start.time=start.time, time.window=time.window)
}


cl<-makeCluster(detectCores(), type="SOCK", outfile="")

clusterExport(cl, c("regional_neural_data", 'simplex', 'ccm', 'ccm_means', 'time.window', 'make_surrogate_data'))


start.times <- seq(0, length(regional_neural_data[,1]), by=time.window)

tick = proc.time() #start timing
#calculate observed causality going one way
threshold.data <- t(as.data.frame(clusterMap(cl, calc_pair_causality, combinations[,2], combinations[,1], rep(start.times, each=length(combinations[,2])))))

threshold.data <- data.frame(threshold.data)
tock = proc.time() - tick #stop timing
print(tock)
stopCluster(cl)


