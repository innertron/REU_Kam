#This script plots the causality measure for each pair of regions and saves
#them to a folder
#SCRIPT lump_csd_by_region.R MSUT BE RUN FIRST

#Import the rEDM library
library(rEDM)
# pair.causality.CSD.data <- dget("...")
#get the first second of the data
nd <- CSD[1:1000,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

i=1
j=2
Ch1 <- nd[,i]
#run  simplex algorithm to get the best embedding dimension
simplex_output <- simplex(Ch1, lib, pred, E=1:6)
bestE <- which.max(simplex_output$rho)


#get the convergent cross map calculations
Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = i, first_column_time = TRUE,
  target_column = j, lib_sizes = seq(10, 80, by = 10), random_libs = TRUE)



Ch2 <- nd[,j]
#run and plot the simplex algorithm to get the best embedding dimension
simplex_output <- simplex(Ch2, lib, pred, E=1:6)
bestE <- which.max(simplex_output$rho)


#get the ccm models 
Ch2_xmap_Ch1 <- ccm(nd, E = bestE, lib_column = j, first_column_time = TRUE,
  target_column = i, lib_sizes = seq(10, 80, by = 10), random_libs=TRUE)

#take the means of the ccm's and get the standard deviation
ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
  tapply(rho, lib_size, sd)))
ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
  tapply(rho, lib_size, sd)))


#specify the filename and plotting size
file_name <- paste(save_file,i,"-and-",j,"-maps.pdf", sep="")
if(save_plots)
{
  pdf(file=file_name,width=7.06,height=5.24)  
}

#plot the results
plot(ch1_map_2_mean$lib_size, pmax(0, ch1_map_2_mean$rho), type = "l", col = "red", 
  xlab = "Library Size", ylab = expression(paste("Cross Map Skill (",rho,")")), ylim = c(0,1), lwd=3,
  main=paste("Sugihara causality measure between region ", i, "and", j, "\n during first second"))
lines(ch2_map_1_mean$lib_size, pmax(0, ch2_map_1_mean$rho), col = "blue")
legend(x = "topleft", legend = c(paste("Region",i,"xmap region",j), paste("Region",j," xmap region",i)), col = c("red", "blue"), lwd = 3, inset = 0.02, cex = 0.8)

# Add CI's
lines(ch1_map_2_mean$lib_size, ch1_map_2_mean$rho + ch1_map_2_mean$sd.rho, col = "red", 
  lty = 3, lwd = 1)
lines(ch1_map_2_mean$lib_size, ch1_map_2_mean$rho - ch1_map_2_mean$sd.rho, col = "red", 
  lty = 3, lwd = 1)
lines(ch2_map_1_mean$lib_size, ch2_map_1_mean$rho + ch2_map_1_mean$sd.rho, col = "blue", 
  lty = 3, lwd = 1)
lines(ch2_map_1_mean$lib_size, ch2_map_1_mean$rho - ch2_map_1_mean$sd.rho, col = "blue", 
  lty = 3, lwd = 1)



#PLOT HISTOGRAM CAUSALITY STRENGTH ACROSS EXPERIMENT
combinations <- t(combn(1:12, 2))
combinations <- (cbind(c(combinations[,1], combinations[,2]), c(combinations[,2], combinations[,1])))
rhos <- rbind(unlist(mapply(function(i,j) { 
  time.step = 50
  time.window = 200
  i.to.j <- unlist(subset(pair.causality.CSD.data, time.step == time.step &
      time.window == time.window & from==i & to==j, drop=T, select=rho))
  # print(paste("finished for start time ", start.time))
  i.to.j
}, combinations[,1], combinations[,2])))
hist(rhos, main="", xlab=expression(paste("Causality strength ", rho)))



#PLOT CAUSALITY BETWEEN PAIRS DIFFERENCE HISTOGRAM
combinations <- t(combn(1:12, 2))
start.times.array <- rep(unique(pair.causality.CSD.data$start.time), each=length(combinations[,1]))
pair.diff.all <- c(unlist(mapply(function(i,j) { 
  step = 50
  window = 200
  i.to.j <- unlist(subset(pair.causality.CSD.data, time.step == step &
      time.window == window & from==i & to==j, drop=T, select=rho))
  j.to.i <- unlist(subset(pair.causality.CSD.data, time.step == step &
      time.window == window & from==j & to==i, drop=T, select=rho))
  abs(i.to.j - j.to.i)  
  }, combinations[,1], combinations[,2])))
hist(pair.diff, main = "", xlab=expression(paste("Absolute difference in causation measure ", rho)))




#PLOT CAUSALITY BETWEEN PAIRS DIFFERENCE HISTOGRAM WITHOUT THOSE BELOW 0.2
combinations <- t(combn(1:12, 2))
start.times.array <- rep(unique(pair.causality.CSD.data$start.time), each=length(combinations[,1]))
pair.diff.some <- c(unlist(mapply(function(i,j) { 
  step = 50
  window = 200
  i.to.j <- unlist(subset(pair.causality.CSD.data, time.step == step &
      time.window == window & from==i & to==j, drop=T, select=rho))
  j.to.i <- unlist(subset(pair.causality.CSD.data, time.step == step &
      time.window == window & from==j & to==i, drop=T, select=rho))
  
  nas <- i.to.j < 0.2 | j.to.i < 0.2
  
  if(is.na(sum(nas))) abs(i.to.j - j.to.i) else abs(i.to.j[!nas] - j.to.i[!nas])
  
}, combinations[,1], combinations[,2])))
options(scipen=10)
hist(pair.diff.some, main = "", xlab=expression(paste("Absolute difference in causation measure ", rho)))
#PLOT 
