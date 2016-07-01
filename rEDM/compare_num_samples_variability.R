#calculate the cross map skill for different sample sizes in order to
#find most efficient sample size to use that does not compromise accurazy

#Import the rEDM and igraph libraries
library(rEDM)

#you need to import the neural_data.txt file by hand.

time_span = 1:1000
#get the first second of the data
nd <- neural_data[time_span,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

sampleSizes <- seq(10, 100, by=10)
from <- c()
to <- c()
sample_size <- c()
strength <- c()

for (i in 1:30)
{
  Ch1 <- nd[,i]
  #run and plot the simplex algorithm to get the best embedding dimension
  simplex_output <- simplex(Ch1, lib, pred, E=1:6)
  bestE_i <- which.max(simplex_output$rho)
  rhos <- c()
  
  i2 = i+1
  #for all nodes other than the current one, look for the edge that
  #most causes this one (i) and form an edge from it to i.
  for(j in i2:31)
  {
    
    
    
    Ch2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch2, lib, pred, E=1:6)
    bestE_j <- which.max(simplex_output$rho)
    
   
    
    for (smp in sampleSizes)
    {
      #get the convergent cross map calculations
      Ch2_xmap_Ch1 <- ccm(nd, E = bestE_i, lib_column = j, first_column_time = TRUE,
      target_column = i, lib_sizes = 80, num_samples = smp)
      
      #take the means of the ccm's and get the standard deviation
      ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
        tapply(rho, lib_size, sd)))
      #get the convergent cross map calculations
      Ch1_xmap_Ch2 <- ccm(nd, E = bestE_j, lib_column = i, first_column_time = TRUE,
        target_column = j, lib_sizes = 80, num_samples = smp)
      
      #take the means of the ccm's and get the standard deviation
      ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
        tapply(rho, lib_size, sd)))
      
      from <- rbind(from, i+1)
      to <- rbind(to, j+1)
      strength <- rbind(strength, ch2_map_1_mean$rho)
      sample_size <- rbind(sample_size, smp)
      
      from <- rbind(from, j+1)
      to <- rbind(to, i+1)
      strength <- rbind(strength, ch1_map_2_mean$rho)
      sample_size <- rbind(sample_size, smp)
      print(paste("finished ",i,j,"with",smp))
      }
    
  }
}

samples_data <- data.frame(from=from, to=to, strength=strength, sample_size=sample_size)
#draw the histogram
histog <- data.frame(lib_size = integer(), rho_diff = double())

for (i in 1:31)
{
  for (j in 1:31)
  {
    if(i!=j)
    {
      best <- subset(samples_data, select=strength, from==i & to==j & sample_size==100)
      diffs <- subset(samples_data, select=c(strength,sample_size), from==i & to ==j & sample_size != 100)
     
      histog <- rbind(histog, data.frame(lib_size=diffs$sample_size, rho_diff=diffs$strength-best$strength)) 
    }
  }
}

plot(histog$lib_size, histog$rho_diff, ylab=expression(paste("Difference in ", rho, " from 100 samples")), xlab="Sample size", main="Rho strength is not dramatically affected by a \n small sample size")