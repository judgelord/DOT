# Replicates with R 3.3.1, given the following package versions.

#############################################
######### Use Non-Panel, long data ##########
#############################################
rm(list=ls())
library(foreign)    #v0.8-66
library(survival)   #v.2.39.5 (!! Important.)
library(mstate)     #v0.2.9


setwd("C:/Users/Shawna/Desktop/PA replic")
source("alt_mssample.R")

dat.gap <- read.dta("3 - svolik 2015 - continuous, no TVC [gap time].dta")

dat.gap$from <- dat.gap$stage
dat.gap$to <- dat.gap$nextStage
dat.gap$start <- dat.gap$t0
dat.gap$stop <- dat.gap$t


#####################################
##### Prepare Transition Matrix #####
#####################################

#Subset Dataframe to eliminate all other transitions other than 1 and 2 (for CR)
dat.gap <- subset(dat.gap, dat.gap$trans == 1 | dat.gap$trans ==2)


#Use Matric Omitting Other Category
tmat <- transMat(list(c(2, 3), #transitions from democracy
                      c(), 		 		  #transitions from exogenous
                      c()), 		 		  #transitions from endogenous
                 names = c("democracy", "exogenous", "endogenous"))

##### Convert Data Type
attr(dat.gap, "trans") <- tmat
class(dat.gap) <- c("msdata", "data.frame")

#Check number of transitions is correct
events(dat.gap)


#############################################
###### Estimate Semi-Parametric Models ######
#############################################

#Estimate Final Model - Unique Covars for All

mod.1.gap <- coxph(Surv(start, stop, status) ~  
                       lgdp_1_std1 + growth_1_std1 + exec_pres1 + prevd_mil1  +
                       lgdp_1_std2 + growth_1_std2 + exec_pres2 + prevd_mil2  +
                        strata(trans), data = dat.gap, method = "breslow")
summary(mod.1.gap)

#################################################
##### Plot Results of SemiParametric Models #####
#################################################

#####Set Economic Development to High (75th %)#####
lgdp_1_std1 <- as.vector(c(quantile(dat.gap$lgdp_1_std1, .75, na.rm=TRUE),0))
lgdp_1_std2 <- as.vector(c(0,quantile(dat.gap$lgdp_1_std2, .75, na.rm=TRUE)))

growth_1_std1 <- as.vector(c(quantile(dat.gap$growth_1_std1, .5, na.rm=TRUE), 0))
growth_1_std2 <- as.vector(c(0,quantile(dat.gap$growth_1_std2, .5, na.rm=TRUE)))

exec_pres1 <- as.vector(c(quantile(dat.gap$exec_pres1, .5, na.rm=TRUE), 0))
exec_pres2 <- as.vector(c(0,quantile(dat.gap$exec_pres2, .5, na.rm=TRUE)))

prevd_mil1 <- as.vector(c(quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE), 0))
prevd_mil2 <- as.vector(c(0,quantile(dat.gap$prevd_mil2, .5, na.rm=TRUE)))

sit.higdp <- data.frame(cbind(
    lgdp_1_std1, lgdp_1_std2, growth_1_std1, growth_1_std2, prevd_mil1, prevd_mil2
))

sit.higdp$trans <- 1:2
attr(sit.higdp, "trans") <- tmat
class(sit.higdp) <- c("msdata", "data.frame")
sit.higdp$strata<-sit.higdp$trans

msf.higdp <- msfit(mod.1.gap, sit.higdp, trans=tmat, variance=TRUE)

###################################################
#####     Generate Trans Probs with CIs      #####
###################################################

#Simulations for High GDP In Stage 1 at Time 0
tv <- seq(1,40)

# trying a loop
sims <- 1000
nIndvs <- length(unique(dat.gap$ccode))


####Simulations for Hi GDP In Stage 1 at Time 0
set.seed(280116)
    # first loop
    s <- 1
    temp <- mssamplea(Haz = msf.higdp$Haz, trans = tmat, tvec=tv, history=list(state=1, time=0), M=nIndvs, clock="reset", output="state")
    transPrs <- cbind(s, temp)
    
    for(s in 2:sims) {
        print(s)
        temp <-mssamplea(Haz = msf.higdp$Haz, trans = tmat, tvec=tv, history=list(state=1, time=0), M=nIndvs, clock="reset", output="state")
        temp <- cbind(s, temp) 
        transPrs <- rbind(transPrs, temp)
    }
    names(transPrs)[1] <- "simNo"
    
# WRITE FINAL PROCESSING HERE
final <- matrix(0, length(tv), dim(tmat)[1]*3 + 1)

for(t in 1:40)     {
    final[t,1] <- t 
    
    # stage 1
    final[t,2] <- mean(transPrs$pstate1[transPrs$time==t])
    final[t,3] <- quantile(transPrs$pstate1[transPrs$time==t], .025)
    final[t,4] <- quantile(transPrs$pstate1[transPrs$time==t], .975)
    
    # stage 2
    final[t,5] <- mean(transPrs$pstate2[transPrs$time==t])
    final[t,6] <- quantile(transPrs$pstate2[transPrs$time==t], .025)
    final[t,7] <- quantile(transPrs$pstate2[transPrs$time==t], .975)
    
    # stage 3
    final[t,8] <- mean(transPrs$pstate3[transPrs$time==t])
    final[t,9] <- quantile(transPrs$pstate3[transPrs$time==t], .025)
    final[t,10] <- quantile(transPrs$pstate3[transPrs$time==t], .975)
    
}
final.higdp.1 <- data.frame(final)
names(final.higdp.1) <- c("t",  "pstate1", "pstate1_l", "pstate1_u", "pstate2", "pstate2_l", "pstate2_u", 
                      "pstate3", "pstate3_l", "pstate3_u")
                 
   
library(foreign)                
write.dta(final.higdp.1, "CR_higdp1.dta")
