# Replicates with R 3.3.1, given the following package versions.

# DEVIN'S COMMENTS IN ALL CAPS =

#############################################
######### Use Non-Panel, long data ##########
#############################################
rm(list=ls())
library(foreign)    #v0.8-66
library(survival)   #v.2.39.5 (!! Important.)
library(mstate)     #v0.2.9

# EDITED FOR GITHUB FOLDER
source(here("surviving_phases_dataverse/alt_mssample_ms.R"))
dat.gap <- read.dta(here("surviving_phases_dataverse/3 - svolik 2015 - continuous, no TVC [gap time].dta"))

# THESE ARE THE SAME VAR NAMES IN THE FAKE HOR DATA, I.E. THESE ARE MINIMAL 
dat.gap$from <- dat.gap$stage
dat.gap$to <- dat.gap$nextStage
dat.gap$start <- dat.gap$t0
dat.gap$stop <- dat.gap$t


#####################################
##### Prepare Transition Matrix #####
#####################################

#Subset Dataframe to elimintate other category 3 and 6
dat.gap <- subset(dat.gap, dat.gap$trans != 3 & dat.gap$trans !=6)

# WHAT IS THIS? BUMPING DOWN TRANSITION INDICIES BECAUSE 3 WAS ELIMINATED
dat.gap$trans[dat.gap$trans==4] <- 3
dat.gap$trans[dat.gap$trans==5] <- 4

#Use Matrix, Omitting "Other" Category 4,5
tmat <- transMat(list(c(2, 3), #transitions from democracy
			c(1), 		 		  #transitions from exogenous
			c(1)), 		 		  #transitions from endogenous
			names = c("democracy", "exogenous", "endogenous"))

##### Convert Data Type
attr(dat.gap, "trans") <- tmat
class(dat.gap) <- c("msdata", "data.frame")



#############################################
###### Estimate Semi-Parametric Models ###### THIS MODEL IS JUST COXPH(SURV())
#############################################

#Estimate Final Model As Identified by Wald & PH Tests in Stata

# MAKING COBINATION VARS

# THIS ONE IS A SUM OF 5 OTHERS 
dat.gap$growth_1_std <- dat.gap$growth_1_std1 + dat.gap$growth_1_std2 + dat.gap$growth_1_std4 + dat.gap$growth_1_std5

# THIS ONE IS A SUM OF 2 OTHERS 
dat.gap$lgdp_1_stdOut <- dat.gap$lgdp_1_std1 + dat.gap$lgdp_1_std2
dat.gap$lgdp_1_stdIn <- dat.gap$lgdp_1_std4 + dat.gap$lgdp_1_std5

# I.E. IS IT EVER EXEC PRES (1-5 ARE BINARY)
dat.gap$exec_pres <- dat.gap$exec_pres1 + dat.gap$exec_pres2 + dat.gap$exec_pres4 + dat.gap$exec_pres5

dat.gap$prevd_milIn <- dat.gap$prevd_mil4 + dat.gap$prevd_mil5


# MODEL 1 
mod.1.gap <- coxph(
  Surv(start, stop, status) ~
    # ALL COVARIATES? IS THERE STAGE INFO IN HERE?
    lgdp_1_stdOut + 
    growth_1_std + 
    exec_pres + 
    prevd_mil1  +
    prevd_mil2  +
    lgdp_1_stdIn + 
    prevd_milIn +
    strata(trans),
  data = dat.gap,
  method = "breslow" #  BEST METHOD FOR TIES ? 
)

summary(mod.1.gap)


#################################################
##### Plot Results of SemiParametric Models #####
#################################################

#####Set Economic Development to Low (25th %)#####
lgdp_1_stdOut <- as.vector(c(quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .25, na.rm=TRUE),quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .25, na.rm=TRUE),0,0))

lgdp_1_stdIn <- as.vector(c(0,0,quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .25, na.rm=TRUE),quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .25, na.rm=TRUE)))

growth_1_std <- as.vector(c(rep(quantile(dat.gap$growth_1_std, .5, na.rm=TRUE), 4)))

exec_pres <- as.vector(c(rep(quantile(dat.gap$exec_pres, .5, na.rm=TRUE), 4)))

prevd_mil1 <- as.vector(c(quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE),0,0,0))
prevd_mil2 <- as.vector(c(0,quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE),0,0))

prevd_milIn <- as.vector(c(0,0,quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE),quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE)))


sit.logdp <- data.frame(cbind(
	lgdp_1_stdOut, growth_1_std, exec_pres, prevd_mil1, prevd_mil2, lgdp_1_stdIn, prevd_milIn
	))

sit.logdp$trans <- 1:4
attr(sit.logdp, "trans") <- tmat
class(sit.logdp) <- c("msdata", "data.frame")
sit.logdp$strata<-sit.logdp$trans

msf.logdp <- msfit(mod.1.gap, sit.logdp, trans=tmat, variance=TRUE)


#####Set Economic Development to High (75th %)#####
lgdp_1_stdOut <- as.vector(c(quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .75, na.rm=TRUE),quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .75, na.rm=TRUE),0,0))

lgdp_1_stdIn <- as.vector(c(0,0,quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .75, na.rm=TRUE),quantile(dat.gap$lgdp_1_stdOut[which(dat.gap$from==1)], .75, na.rm=TRUE)))

growth_1_std <- as.vector(c(rep(quantile(dat.gap$growth_1_std, .5, na.rm=TRUE), 4)))

exec_pres <- as.vector(c(rep(quantile(dat.gap$exec_pres, .5, na.rm=TRUE), 4)))

prevd_mil1 <- as.vector(c(quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE),0,0,0))
prevd_mil2 <- as.vector(c(0,quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE),0,0))

prevd_milIn <- as.vector(c(0,0,quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE),quantile(dat.gap$prevd_mil1, .5, na.rm=TRUE)))


sit.higdp <- data.frame(cbind(
	lgdp_1_stdOut, growth_1_std, exec_pres, prevd_mil1, prevd_mil2, lgdp_1_stdIn, prevd_milIn
	))

sit.higdp$trans <- 1:4
attr(sit.higdp, "trans") <- tmat
class(sit.higdp) <- c("msdata", "data.frame")
sit.higdp$strata<-sit.higdp$trans

msf.higdp <- msfit(mod.1.gap, sit.higdp, trans=tmat, variance=TRUE)



###################################################
##### Generate the Same Results Employing CIs #####
###################################################

#Simulations for Lo GDP In Stage 1 at Time 0
tv <- seq(1,40)

# trying a loop
sims <- 1000
nIndvs <- length(unique(dat.gap$ccode))


####Simulations for Lo GDP In Stage 1 at Time 0
set.seed(280116)
    # first loop
    s <- 1
    temp <- mssamplea(Haz = msf.logdp$Haz, trans = tmat, tvec=tv, history=list(state=1, time=0), M=nIndvs, clock="reset", output="state")
    transPrs <- cbind(s, temp)
    
    for(s in 2:sims) {
        print(s)
        temp <-mssamplea(Haz = msf.logdp$Haz, trans = tmat, tvec=tv, history=list(state=1, time=0), M=nIndvs, clock="reset", output="state")
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
final.logdp.1 <- data.frame(final)
names(final.logdp.1) <- c("t",  "pstate1", "pstate1_l", "pstate1_u", "pstate2", "pstate2_l", "pstate2_u", 
                      "pstate3", "pstate3_l", "pstate3_u")
                      
                      
####Simulations for Lo GDP In Stage 2 at Time 0
set.seed(280116)
    # first loop
    s <- 1
    temp <- mssamplea(Haz = msf.logdp$Haz, trans = tmat, tvec=tv, history=list(state=2, time=0), M=nIndvs, clock="reset", output="state")
    transPrs <- cbind(s, temp)
    
    for(s in 2:sims) {
        print(s)
        temp <-mssamplea(Haz = msf.logdp$Haz, trans = tmat, tvec=tv, history=list(state=2, time=0), M=nIndvs, clock="reset", output="state")
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
final.logdp.2 <- data.frame(final)
names(final.logdp.2) <- c("t",  "pstate1", "pstate1_l", "pstate1_u", "pstate2", "pstate2_l", "pstate2_u", 
                      "pstate3", "pstate3_l", "pstate3_u")                      
  
####Simulations for Lo GDP In Stage 3 at Time 0
set.seed(280116)
    # first loop
    s <- 1
    temp <- mssamplea(Haz = msf.logdp$Haz, trans = tmat, tvec=tv, history=list(state=3, time=0), M=nIndvs, clock="reset", output="state")
    transPrs <- cbind(s, temp)
    
    for(s in 2:sims) {
        print(s)
        temp <-mssamplea(Haz = msf.logdp$Haz, trans = tmat, tvec=tv, history=list(state=3, time=0), M=nIndvs, clock="reset", output="state")
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
final.logdp.3 <- data.frame(final)
names(final.logdp.3) <- c("t",  "pstate1", "pstate1_l", "pstate1_u", "pstate2", "pstate2_l", "pstate2_u", 
                      "pstate3", "pstate3_l", "pstate3_u")

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

####Simulations for Hi GDP In Stage 2 at Time 0
set.seed(280116)
    # first loop
    s <- 1
    temp <- mssamplea(Haz = msf.higdp$Haz, trans = tmat, tvec=tv, history=list(state=2, time=0), M=nIndvs, clock="reset", output="state")
    transPrs <- cbind(s, temp)
    
    for(s in 2:sims) {
        print(s)
        temp <-mssamplea(Haz = msf.higdp$Haz, trans = tmat, tvec=tv, history=list(state=2, time=0), M=nIndvs, clock="reset", output="state")
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
final.higdp.2 <- data.frame(final)
names(final.higdp.2) <- c("t",  "pstate1", "pstate1_l", "pstate1_u", "pstate2", "pstate2_l", "pstate2_u", 
                      "pstate3", "pstate3_l", "pstate3_u")


####Simulations for Hi GDP In Stage 3 at Time 0
set.seed(280116)
    # first loop
    s <- 1
    temp <- mssamplea(Haz = msf.higdp$Haz, trans = tmat, tvec=tv, history=list(state=3, time=0), M=nIndvs, clock="reset", output="state")
    transPrs <- cbind(s, temp)
    
    for(s in 2:sims) {
        print(s)
        temp <-mssamplea(Haz = msf.higdp$Haz, trans = tmat, tvec=tv, history=list(state=3, time=0), M=nIndvs, clock="reset", output="state")
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
final.higdp.3 <- data.frame(final)
names(final.higdp.3) <- c("t",  "pstate1", "pstate1_l", "pstate1_u", "pstate2", "pstate2_l", "pstate2_u", 
                      "pstate3", "pstate3_l", "pstate3_u")                     
                      
save.image("Simulations.Rdata")                     
