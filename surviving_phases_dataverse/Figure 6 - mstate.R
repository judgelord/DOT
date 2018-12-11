# Replicates with R 3.3.1.


rm(list=ls())
library(foreign)    #v0.8-66


setwd("D:/Users/Shawna/Desktop/PA replic")

load("Simulations.RData")


write.dta(final.higdp.1, "MS_higdp1.dta")