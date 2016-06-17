plotCCM <- function(x, y, da)
{
library(multispatialCCM)
#Simulate data to use for multispatial CCM test
#See function for details - A is causally forced by B,
#but the reverse is not true.
#ccm_data_out<-make_ccm_data()

Ch1<-subset(da, select =x, Time > 0 & Time < .1, drop=T)
Ch2<-subset(da, select =y, Time > 0 & Time < .1, drop=T)


#Calculate optimal E
maxE<-5 #Maximum E to test
#Matrix for storing output
Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A", "B")


#Loop over potential E values and calculate predictive ability
#of each process for its own dynamics
for(E in 2:maxE) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat[E-1,"A"]<-SSR_pred_boot(A=Ch1, E=E, predstep=1, tau=1)$rho
  Emat[E-1,"B"]<-SSR_pred_boot(A=Ch2, E=E, predstep=1, tau=1)$rho
}


#Look at plots to find E for each process at which
#predictive ability rho is maximized
#matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2, xlab="E", ylab="rho", lwd=2)
#legend("bottomleft", c("Ch1", "Ch2"), lty=1:2, col=1:2, lwd=2, bty="n")


#Results will vary depending on simulation.
#Using the seed we provide,
#maximum E for A should be 2, and maximum E for B should be 3.
#For the analyses in the paper, we use E=2 for all simulations.
E_Ch1<- which.max(Emat[,1]) + 1
E_Ch2<- which.max(Emat[,2]) + 1


#Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with
#increasing time distance
#See manuscript and R code for details
signal_Ch1_out<-SSR_check_signal(A=Ch1, E=E_Ch1, tau=1, predsteplist=1:10)
signal_Ch2_out<-SSR_check_signal(A=Ch2, E=E_Ch2, tau=1, predsteplist=1:10)


#Run the CCM test
#E_A and E_B are the embedding dimensions for A and B.
#tau is the length of time steps used (default is 1)
#iterations is the number of bootsrap iterations (default 100)
# Does A "cause" B?
#Note - increase iterations to 100 for consistant results
CCM_boot_Ch1<-CCM_boot(Ch1, Ch2, E_Ch1, tau=1, iterations=10)
# Does B "cause" A?
CCM_boot_Ch2<-CCM_boot(Ch2, Ch1, E_Ch2, tau=1, iterations=10)


#Test for significant causal signal
#See R function for details
(CCM_significance_test<-ccmtest(CCM_boot_Ch1,CCM_boot_Ch2))
#Plot results
plotxlimits<-range(c(CCM_boot_Ch1$Lobs, CCM_boot_Ch2$Lobs))
#Plot "A causes B"
plot(CCM_boot_Ch1$Lobs, CCM_boot_Ch1$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho", main=paste("Testing Sugihara causality between ",x,"\n and", y))
#Add +/- 1 standard error
matlines(CCM_boot_Ch1$Lobs,
         cbind(CCM_boot_Ch1$rho-CCM_boot_Ch1$sdevrho,
               CCM_boot_Ch1$rho+CCM_boot_Ch1$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_Ch2$Lobs, CCM_boot_Ch2$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_Ch2$Lobs,
         cbind(CCM_boot_Ch2$rho-CCM_boot_Ch2$sdevrho,
               CCM_boot_Ch2$rho+CCM_boot_Ch2$sdevrho),
         lty=3, col=2)
legend("topleft",
       c(paste(x, "causes", y), paste(y, "causes", x)),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n")
dev.copy(jpeg,filename=paste("~/Desktop/SIP/Midterm/Sugihara\ plots/",x,y,".jpg"))
dev.off ()

}

for (Ch1 in 2:(length(vals)-1))
{
  Ch1pl = Ch1+1
  for (Ch2 in Ch1pl:length(vals))
  {
    plotCCM(vals[Ch1],vals[Ch2], neural_data)
  }
}