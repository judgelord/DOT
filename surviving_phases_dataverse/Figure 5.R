# Replicates with R 3.3.1.


rm(list=ls())

setwd("C:/Users/Shawna/Desktop/PA replic")


load("Simulations.RData")
                      
############################
##### Plot the Results #####
############################


plot(final.logdp.1$t, final.logdp.1$pstate1, type="l" , lty=2, lwd=2, xlim=c(0,40), ylim=c(0,1), xlab="Years", ylab="Probability of Democracy", main="")
lines(final.logdp.1$t, final.logdp.1$pstate1_l, lty=2)
lines(final.logdp.1$t, final.logdp.1$pstate1_u, lty=2)
lines(final.logdp.1$t, final.higdp.1$pstate1, lty=1, lwd=2)
lines(final.logdp.1$t, final.higdp.1$pstate1_l, lty=1)
lines(final.logdp.1$t, final.higdp.1$pstate1_u, lty=1)
legend(0, .3, c("Less Developed", "More Developed"), col="black", lty=c(2,1), lwd=c(2,2), bty="n")


plot(final.logdp.1$t, final.logdp.1$pstate2, type="l", lty=2, lwd=2 ,xlim=c(0,40), ylim=c(0,.3), xlab="Years", ylab="Probability of Coup", main="")
lines(final.logdp.1$t, final.logdp.1$pstate2_l, lty=2)
lines(final.logdp.1$t, final.logdp.1$pstate2_u, lty=2)
lines(final.logdp.1$t, final.higdp.1$pstate2, lty=1, lwd=2)
lines(final.logdp.1$t, final.higdp.1$pstate2_l, lty=1)
lines(final.logdp.1$t, final.higdp.1$pstate2_u, lty=1)
legend(0, .3, c("Less Developed", "More Developed"), col="black", lty=c(2,1), lwd=c(2,2), bty="n")


plot(final.logdp.1$t, final.logdp.1$pstate3, type="l", lty=2, lwd=2 , xlim=c(0,40), ylim=c(0,.3), xlab="Years", ylab="Probability of Self-Coup", main="")
lines(final.logdp.1$t, final.logdp.1$pstate3_l, lty=2)
lines(final.logdp.1$t, final.logdp.1$pstate3_u, lty=2)
lines(final.logdp.1$t, final.higdp.1$pstate3, lty=1, lwd=2)
lines(final.logdp.1$t, final.higdp.1$pstate3_l, lty=1)
lines(final.logdp.1$t, final.higdp.1$pstate3_u, lty=1)
legend(0, .3, c("Less Developed", "More Developed"), col="black", lty=c(2,1), lwd=c(2,2), bty="n")
