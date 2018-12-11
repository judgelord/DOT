# Replicates with R 3.3.1.

rm(list=ls())

setwd("C:/Users/Shawna/Desktop/PA replic")

load("MonteCarlo_Simulations.RData")

#p-value for B(1->3)
pnorm(mean(betas.comb.1[,5])/mean(std.comb.1[,5]))

#PH test for Stage 1 collapsed exiting transitions
mean(ph.1)

#p-value for B(2->3)
pnorm(mean(betas.comb.2[,5])/mean(std.comb.2[,5]))

#PH test for Stage 3 collapsed entering transitions
mean(ph.2)