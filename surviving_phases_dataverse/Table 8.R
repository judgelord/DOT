# Replicates with R 3.3.1, given the following package versions.

#############################################
######### Use Non-Panel, long data ##########
#############################################
rm(list=ls())
library(foreign)    #v0.8-66
library(survival)   #v.2.39.5 (!! Important.)
library(mstate)     #v0.2.9


setwd("C:/Users/Shawna/Desktop/PA replic")
source("alt_mssample_ms.R")


dat.gap <- read.dta("3 - svolik 2015 - continuous, no TVC [gap time].dta")

dat.gap$from <- dat.gap$stage
dat.gap$to <- dat.gap$nextStage
dat.gap$start <- dat.gap$t0
dat.gap$stop <- dat.gap$t


#####################################
##### Prepare Transition Matrix #####
#####################################

#Subset Dataframe to elimintate other category
dat.gap <- subset(dat.gap, dat.gap$trans != 3 & dat.gap$trans !=6)

dat.gap$trans[dat.gap$trans==4] <- 3
dat.gap$trans[dat.gap$trans==5] <- 4

#Use Matric Omitting Other Category
tmat <- transMat(list(c(2, 3), #transitions from democracy
			c(1), 		 		  #transitions from exogenous
			c(1)), 		 		  #transitions from endogenous
			names = c("democracy", "exogenous", "endogenous"))

##### Convert Data Type
attr(dat.gap, "trans") <- tmat
class(dat.gap) <- c("msdata", "data.frame")



#############################################
###### Estimate Semi-Parametric Models ######
#############################################

#Estimate Final Model As Identified by Wald & PH Tests in Stata

dat.gap$growth_1_std <- dat.gap$growth_1_std1 + dat.gap$growth_1_std2 + dat.gap$growth_1_std4 + dat.gap$growth_1_std5

dat.gap$lgdp_1_stdOut <- dat.gap$lgdp_1_std1 + dat.gap$lgdp_1_std2
dat.gap$lgdp_1_stdIn <- dat.gap$lgdp_1_std4 + dat.gap$lgdp_1_std5

dat.gap$exec_pres <- dat.gap$exec_pres1 + dat.gap$exec_pres2 + dat.gap$exec_pres4 + dat.gap$exec_pres5

dat.gap$prevd_milIn <- dat.gap$prevd_mil4 + dat.gap$prevd_mil5



mod.1.gap <- coxph(Surv(start, stop, status) ~  
	lgdp_1_stdOut + growth_1_std + exec_pres + prevd_mil1  +
                          	                 prevd_mil2  +
  lgdp_1_stdIn                             + prevd_milIn +
strata(trans), data = dat.gap, method = "breslow")

summary(mod.1.gap)


#################################################
##### Plot Results of SemiParametric Models #####
#################################################

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

tv <- seq(1,40)
nIndvs <- length(unique(dat.gap$ccode))

  
####Simulations for Hi GDP In Stage 3 at Time 0
set.seed(280116)
    # first loop
    s <- 1
    temp <- mssamplea(Haz = msf.higdp$Haz, trans = tmat, tvec=tv, history=list(state=3, time=0), M=nIndvs, clock="reset", output="state")
    transPrs <- cbind(s, temp)
    
    names(transPrs)[1] <- "simNo"

transPrs    
