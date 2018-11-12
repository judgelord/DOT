# Replicates with R 3.3.1.


##############################################
###### Code for Baseline Hazard PH Plots #####
##############################################
rm(list=ls())
setwd("C:/Users/Shawna/Desktop/PA replic")


#Scenario A - Proportional, Not Significantly Different

t <- seq(0,19)
a1 <- c(rep(.1, 3), rep(.15,1), rep(.27,4), rep(.35, 3), rep(.42,2), rep(.5, 5), rep(.6,2)) 
a2 <- a1+.13

plot(t, a1, type="n", xlab="Time", ylab="Hazard", main="", ylim=c(0,1))
lines(t,a1, type="s", lty=2)
lines(t,a2, type="s")


#Scenario B - Proportional, Significantly Different
t <- seq(0,19)
b1 <- c(rep(.1, 3), rep(.15,1), rep(.27,4), rep(.35, 3), rep(.42,2), rep(.5, 5), rep(.6,2)) 
b2 <- a1+.42

plot(t, b1, type="n", xlab="Time", ylab="Hazard", main="", ylim=c(0,1))
lines(t,b1, type="s", lty=2)
lines(t,b2, type="s")

#Scenario C - Not Proportional, Not Significantly Different

t <- seq(0,19)
c1 <- 1 - c(rep(.03, 2), rep(.19,2), rep(.3,3), rep(.55, 5), rep(.31,1), rep(.25, 3), rep(.18,1), rep(.06,3)) 
c2 <- c(rep(.03, 2), rep(.19,2), rep(.3,3), rep(.55, 5), rep(.31,1), rep(.25, 3), rep(.18,1), rep(.06,3)) 

plot(t, c1, type="n", xlab="Time", ylab="Hazard", main="", ylim=c(0,1))
lines(t,c1, type="s", lty=2)
lines(t,c2, type="s")

#Scenario D - Not Proportional, Significantly Different

t <- seq(0,19)
d1 <- c(rep(.1, 3), rep(.15,1), rep(.27,4), rep(.35, 3), rep(.42,2), rep(.5, 5), rep(.6,2)) 
d2 <- c(rep(.03, 2), rep(.19,2), rep(.3,3), rep(.55, 5), rep(.31,1), rep(.25, 3), rep(.18,1), rep(.06,3)) 

plot(t, d1, type="n", xlab="Time", ylab="Hazard", main="", ylim=c(0,1))
lines(t,d1, type="s", lty=2)
lines(t,d2, type="s")


