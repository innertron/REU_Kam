#this script creates plots and saves them into the specified directory


#import the data if it is not already present
#dget("permutation_regional_data.RData")


#for each pair
for(i in 1:12)
{  
  for(j in 1:12)
  {
    if(i != j)
    {
      #recover the observed and permutation values
      ch1_cause_2_observed <- subset(permutation_regional_data, from==i & to==j  &
          is.na(random_shuffle), drop=T, select = c(rho,libs))
      ch1_cause_2_permutation <- subset(permutation_regional_data, from==i & to==j & 
          is.na(rho), drop=T, select = c(random_shuffle,libs))
      
      
      #cauculate the P-value significance
      p_val = (sum(ch1_cause_2_observed$rho < ch1_cause_2_permutation$random_shuffle) + 1) / (length(ch1_cause_2_permutation$random_shuffle) + 1)
      
      save_file <- "~/Desktop/SIP/Code/rEDM/significant causality plots/"
      file_name <- paste(save_file,j,"-map-",i,"-maps.jpg", sep="")
      # pdf(file=file_name)
      
      plot(ch1_cause_2_observed$libs, ch1_cause_2_observed$rho, col="red", type="o", ylim=c(0,1),
        ylab=expression(paste("cross map skill (",rho,")")), xlab="Library size",
        main=paste("Significance of cross map convergence skill from \n channel ", j, "to", i,
          "(p-val = ",signif(p_val,digits=4),")"), lwd="2")
      points(ch1_cause_2_permutation$libs, ch1_cause_2_permutation$random_shuffle, col="grey")
      legend(x = "topleft", legend = c("Observed causality", "Causality from Ebisuzaki shuffling"),
        col = c("red", "grey"), lwd = 3, inset = 0.02, cex = 0.8)
      # dev.off()
    }
  }
}