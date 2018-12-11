# Replicates with R 3.3.1.

rm(list=ls())

setwd("C:/Users/Shawna/Desktop/PA replic")

load("MonteCarlo_Simulations.RData")

###Evaluate Results for Model with Combined Transitions 2&4 - Collapsed Covariate Effect For Transitions 2 & 4###
colMeans(betas.comb.3)
  # last coefficient is for the dummy indicator for transition 2->3.