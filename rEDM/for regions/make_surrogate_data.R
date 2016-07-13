#This script plots the causality measure for each pair of regions and saves
#them to a folder

#Import the rEDM library
library(rEDM)
library(scales)

#you need to import the neural_data.txt file by hand.
#neural_data <- dget(...) or press import above

#select the data and the time span of the experiment
time_span <- 1:2000
nd <- neural_data[time_span,]
#select the splined data
# nd <- splined_data
lib <- c(1, length(nd))
pred <- c(1, length(nd))

# shuffle_method = "random_shuffle"
shuffle_method = "ebisuzaki"
permutation_test_data <- data.frame(to=integer(), from=integer(), rho=double(), random_shuffle=double())

for (i in 2:6)
{
  
  Ch1 <- nd[,i]
  
  #make surrogate data for signficance testing
  surr_ch1 <- make_surrogate_data(Ch1, method=shuffle_method)
  
  #run the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:6)
  bestE_i <- which.max(simplex_output$rho)
  
  i2 = i+1
  for(j in i2:7)
  {
    #get the convergent cross map calculations
    Ch1_xmap_Ch2 <- ccm(nd, E = bestE_i, lib_column = i, first_column_time = FALSE,
      target_column = j, lib_sizes = 80, random_libs = TRUE, num_samples=20)
    
    
    Ch2 <- nd[,j]
    surr_ch2 <- make_surrogate_data(Ch2, method=shuffle_method)
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch2, lib, pred, E=1:6)
    bestE_j <- which.max(simplex_output$rho)
    
    
    #get the ccm models 
    Ch2_xmap_Ch1 <- ccm(nd, E = bestE_j, lib_column = j, first_column_time = FALSE,
      target_column = i, lib_sizes = 80, random_libs=TRUE, num_samples=20)
    
    #take the means of the ccm's and get the standard deviation
    ch1_map_2_mean <- ccm_means(Ch1_xmap_Ch2)
    ch2_map_1_mean <- ccm_means(Ch2_xmap_Ch1)
    
    #record ch1 xmap ch2
    permutation_test_data <- rbind(permutation_test_data, data.frame(to=i, from=j, libs=ch1_map_2_mean$lib_size, rho=ch1_map_2_mean$rho, random_shuffle=NA))
    
    
    #record ch2 xmap ch1
    permutation_test_data <- rbind(permutation_test_data, data.frame(to=j, from=i, libs=ch2_map_1_mean$lib_size, rho=ch2_map_1_mean$rho, random_shuffle=NA))
    
    
    permutation_test_data <- lapply(c())
    #do the same for the hundred surrogates
    for (sur_ind in 1:length(surr_ch1[1,]))
    {
      
      sur_dat <- cbind(Time=nd[,1], sur_1=Ch1, sur_2=surr_ch2[,sur_ind])
      #get the convergent cross map calculations
      Ch1_xmap_Ch2 <- ccm(sur_dat, E = bestE_i, lib_column = 1, first_column_time = TRUE,
        target_column = 2, lib_sizes = 80, random_libs=TRUE, num_samples=20)
      
      
      sur_dat <- cbind(Time=nd[,1], sur_1=surr_ch1[sur_ind], sur_2=Ch2)
      #get the ccm models 
      Ch2_xmap_Ch1 <- ccm(sur_dat, E = bestE_j, lib_column = 2, first_column_time = TRUE,
        target_column = 1, lib_sizes = 80, random_libs=TRUE, num_samples=20)
      
      #take the means of the ccm's and get the standard deviation
      ch1_map_2_rho <- max(0, ccm_means(Ch1_xmap_Ch2)$rho)
      ch2_map_1_rho <- max(0, ccm_means(Ch2_xmap_Ch1)$rho)
      
      #record ch1 xmap ch2
      permutation_test_data <- rbind(permutation_test_data, cbind(to=i, from=j,
        libs=ch1_map_2_mean$lib_size, random_shuffle=ch1_map_2_rho, rho=NA))
      
      #record ch2 xmap ch1
      permutation_test_data <- rbind(permutation_test_data, cbind(to=j, from=i,
        libs=ch2_map_1_mean$lib_size, random_shuffle=ch2_map_1_rho, rho=NA))
    }
    
    print(tock)
    print(paste("finished ",i, "and",j))
  }
}

# dput(permutation_test_data, "permutation_test_data_spline_first_second.RData")