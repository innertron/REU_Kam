#import libraries
library(eegkit)
library(rEDM)

#for each brain region pair
combinations <- t(combn(2:7, 2))

#select what data to process
nd <- regional_neural_data[1:2000, ]
shuffle_method = "ebisuzaki"


#setting up what to use for prediction and what for library
lib <- c(1, length(nd))
pred <- c(1, length(nd))

#calculates and returns a data frame that contains to, from, rho, and random shuffle columns
calc_pair_causality <- function(i, j)
{
  Ch2 <- nd[,j]
  #run simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch2, lib, pred, E=1:6)
  bestE_j <- which.max(simplex_output$rho)
  
  #get the ccm models 
  Ch2_xmap_Ch1 <- ccm(nd, E = bestE_j, lib_column = j, first_column_time = FALSE,
    target_column = i, lib_sizes = 80, random_libs=TRUE, num_samples=20)
  
  ch2_map_1_mean <- ccm_means(Ch2_xmap_Ch1)
  
  print(paste("finishe", i, j))
  data.frame(to=j, from=i, libs=ch2_map_1_mean$lib_size,random_shuffle=NA, rho=ch2_map_1_mean$rho)
}

#calculate observed causality going one way
observed <- rbind(
  t(mapply(calc_pair_causality, combinations[,1], combinations[,2])),
  t(mapply(calc_pair_causality, combinations[,2], combinations[,1]))
)


#make surrogte data
surrogate.data <- data.frame(matrix(ncol=7, nrow=length(nd[,1])))
surrogate.data[1] <- nd[,1]
for(i in 2:7){
  surrogate.data[,i] <- make_surrogate_data(nd[,i], method=shuffle_method)
}

#make function to calculate surrogate data
calculate_pair_surrogate_causality <- function(i, j, surr.ind)
{
  sur.dat <- cbind(nd[,1], nd[,i], surrogate.data[,j][,surr.ind])
  Ch2 <- nd[,j]
  #run simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch2, lib, pred, E=1:6)
  bestE_j <- which.max(simplex_output$rho)
  Ch2_xmap_Ch1 <- ccm(sur.dat, E = bestE_j, lib_column = 3, first_column_time = FALSE,
    target_column = 2, lib_sizes = 80, random_libs=TRUE, num_samples=20)
  
  ch2_map_1_mean <- ccm_means(Ch2_xmap_Ch1)
  
  print(paste("finished with ", i, j, surr.ind, "rho:", ch2_map_1_mean$rho))
  data.frame(to=j, from=i, libs=ch2_map_1_mean$lib_size, random_shuffle=max(0, ch2_map_1_mean$rho), rho=NA)
}

#calculate ccm on surrogate data
permutation_data <- t(mapply(calculate_pair_surrogate_causality, combinations[,1], combinations[,2], rep(1:100, each=length(combinations[,1]))))


permutation_test_data <- data.frame(rbind(permutation_data, observed))
