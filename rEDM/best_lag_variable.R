#Import the rEDM library
library(rEDM)

#you need to import the neural_data.txt file by hand.

#get the first .2 seconds of the data
nd <- neural_data[1:1000,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

# for (i in 1:30)
# {
#   i2 = i+1
#   for(j in i2:31)
#   {
i=10
j=16
span = 30
output_1_map_2 <- replicate(10, NA)
output_2_map_1 <- replicate(10, NA)

for (tp in -span:span)
{
    Ch1 <- nd[,i]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch1, lib, pred, E=1:6)
    #par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
    #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
    #     ylab = "Forecast Skill (rho)")
    bestE <- which.max(simplex_output$rho)
    
    
    #get the convergent cross map calculations
    Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = i, first_column_time = TRUE, tp=tp,
                        target_column = j, lib_sizes = 80)
    
    
    
    Ch2 <- nd[,j]
    #run and plot the simplex algorithm to get the best embedding dimension
    simplex_output <- simplex(Ch2, lib, pred, E=1:6)
    #plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
    #     ylab = "Forecast Skill (rho)")
    bestE <- which.max(simplex_output$rho)
    
    
    #get the ccm models 
    Ch2_xmap_Ch1 <- ccm(nd, E = bestE, lib_column = j, first_column_time = TRUE, tp=tp,
                        target_column = i, lib_sizes = 80)
    
    #take the means of the ccm's and get the standard deviation
    ch1_map_2_mean <- data.frame(ccm_means(Ch1_xmap_Ch2), sd.rho = with(Ch1_xmap_Ch2,
                                                                        tapply(rho, lib_size, sd)))
    ch2_map_1_mean <- data.frame(ccm_means(Ch2_xmap_Ch1), sd.rho = with(Ch2_xmap_Ch1,
                                                                        tapply(rho, lib_size, sd)))
    
    output_1_map_2[[tp+span+1]] <- ch1_map_2_mean$rho
    output_2_map_1[[tp+span+1]] <- ch2_map_1_mean$rho
}
plot(x=-span:span, y=output_1_map_2, col="red", main=paste(i, "and", j, "lagged "), ylim = c(0,1))
legend(x = "topleft", legend = c(paste(i,"xmap",j), paste(j,"xmap",i)), col = c("red", 
    "blue"), lwd = 3)
points(x=-span:span, y=output_2_map_1, col="blue", main=paste(i,"causes",j))
