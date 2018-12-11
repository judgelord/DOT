# Replicates with R 3.3.1.

rm(list=ls())


setwd("C:/Users/Shawna/Desktop/PA replic")


load("Simulations.RData")

############################
##### Plot the Results #####
############################

#Generate Figure 8

plot(final.logdp.1$t, final.logdp.1$pstate1, type="l", lty=1, lwd=2 ,xlim=c(0,40), ylim=c(0,1), xlab="Years", ylab="Probability of Democracy", main="Less Developed States")
lines(final.logdp.1$t, final.logdp.2$pstate1, lty=2, lwd=2)
lines(final.logdp.1$t, final.logdp.3$pstate1, lty=3, lwd=2)
legend(15, .3, c("Current Stage is Democracy", "Current Stage is Coup", "Current Stage is Self-Coup"), lty = c(1,2,3), lwd=c(2,2,2), bty="n")


plot(final.higdp.1$t, final.higdp.1$pstate1, type="l", lty=1, lwd=2 ,xlim=c(0,40), ylim=c(0,1), xlab="Years", ylab="Probability of Democracy", main="More Developed States")
lines(final.higdp.1$t, final.higdp.2$pstate1, lty=2, lwd=2)
lines(final.higdp.1$t, final.higdp.3$pstate1, lty=3, lwd=2)
legend(15, .3, c("Current Stage is Democracy", "Current Stage is Coup", "Current Stage is Self-Coup"), lty = c(1,2,3), lwd=c(2,2,2), bty="n")

