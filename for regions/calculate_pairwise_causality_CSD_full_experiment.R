#this script calculates pairwise causality between brain regions using the reconstructed CSD file then
#stores the data in a folder.
#MUST RUN lump_csd_by_region.R FIRST
calculate_pairwise_causality_CSD_full_experiment <- function(time.window=2000, time.step=2000)
{
  #import libraries
  library(rEDM)
  library(snow)
  library(parallel)
  
  #make a combination for each brain region pair
  combinations <- t(combn(1:12, 2))
  #double the combinations to account for both directions
  combinations <- (cbind(c(combinations[,1], combinations[,2]), c(combinations[,2], combinations[,1])))
  
  #calculate the number of regions and windows
  num.regions <- length(regional_EstCSD)-1
  num.windows <- as.integer(length(regional_EstCSD[,1])/time.window)
  
  
  #calculates causality measure between i and j from the start time.
  #and returns a data frame that contains to, from, rho
  calc_pair_causality <- function(i, j, start.time)
  {
    #calculate end time from global time.window variable
    end.time = start.time + time.window
    
    #run simplex algorithm to get the best embedding dimension for manifold reconstruction
    Ch2 <- regional_EstCSD[start.time:end.time, j]
    simplex_output <- simplex(Ch2, c(1, time.window), c(1, time.window), E=1:6)
    bestE_j <- which.max(simplex_output$rho)
    
    #get the ccm model and its results then calculate their mean
    Ch2_xmap_Ch1 <- ccm(regional_EstCSD[start.time:end.time,], E = bestE_j, lib_column = j, first_column_time = TRUE,
                        target_column = i, lib_sizes = 80, random_libs=TRUE, num_samples=20)
    
    ch2_map_1_mean <- ccm_means(Ch2_xmap_Ch1)
    
    #print status update
    print(paste("finished", i, j, "for start time", start.time))
    #return a data frame row
    c(to=j, from=i, rho=max(0,ch2_map_1_mean$rho),
      start.time=start.time, time.window=time.window)
  }
  
  
  #make a cluster to allow parallelization of the computation
  cl<-makeCluster(detectCores(), type="SOCK", outfile="")
  #export some values to each cluster instead of passing these values to the function
  clusterExport(cl, c("time.window"), envir= environment())
  clusterExport(cl, c("regional_EstCSD", 'simplex', 'ccm', 'ccm_means', 'make_surrogate_data'))
  
  #get start times with making sure to keep it in bound of the time step
  start.times <- seq(time.step, length(regional_EstCSD[,1]), by=time.step) - time.step
  #repeat the start times for each combination of 
  start.times.array <- rep(start.times, each=length(combinations[,2]))
  tick = proc.time() #start timing
  
  #calculate observed causality and put it in a dataframe
  pair.causality.data <- t(as.data.frame(clusterMap(cl, calc_pair_causality, combinations[,2], combinations[,1], start.times.array)))
  pair.causality.data <- data.frame(pair.causality.data)
  
  tock = proc.time() - tick #stop timing
  print(tock)
  
  #shut the cluster and return the calculated data frame
  stopCluster(cl)
  pair.causality.data
}

pairwise.causality.CSD.data <- calculate_pairwise_causality_CSD_full_experiment(time.window = 2000, time.step=250)

#store the data in a file
dput(pairwise.causality.CSD.data, "~/pairwise.causality.regional.CSD.data.RData")