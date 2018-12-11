# Replicates with R 3.3.1.

rm(list=ls())

setwd("C:/Users/Shawna/Desktop/PA replic")

load("MonteCarlo_Simulations.RData")

###Evaluate Results for Model with Combined Transitions 1&2 - Collapsed Covariate Effect For Transitions 1 & 2###
colMeans(betas.comb.1a)