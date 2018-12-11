# Replicates with R 3.3.1, given the following package versions.

#############################################
######### Use Non-Panel, long data ##########
#############################################
rm(list=ls())
library(foreign)    #v0.8-66
library(survival)   #v.2.39.5 (!! Important.)
library(mstate)     #v0.2.9

setwd("C:/Users/Shawna/Desktop/PA replic")

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



## // TABLE 5 
#Check number of transitions is correct
events(dat.gap)                                   


#############################################
###### Estimate Semi-Parametric Models ######
#############################################

## TABLE 2: Section A
mod.1.gapA <- coxph(Surv(start, stop, status) ~  
	lgdp_1_std1 + growth_1_std1 + exec_pres1 + prevd_mil1  +
	lgdp_1_std2 + growth_1_std2 + exec_pres2 + prevd_mil2  +
	lgdp_1_std4 + growth_1_std4 + exec_pres4 + prevd_mil4  +
	lgdp_1_std5 + growth_1_std5 + exec_pres5 + prevd_mil5  +

strata(trans), data = dat.gap, method = "breslow")

summary(mod.1.gapA)


## TABLE 2: Section B
#Estimate Final Model As Identified by Wald & PH Tests in Stata

dat.gap$growth_1_std <- dat.gap$growth_1_std1 + dat.gap$growth_1_std2 + dat.gap$growth_1_std4 + dat.gap$growth_1_std5
    # ^ constrains growth_1_std1, growth_1_std2, growth_1_std4, growth_1_std5 to be equal

dat.gap$lgdp_1_stdOut <- dat.gap$lgdp_1_std1 + dat.gap$lgdp_1_std2
    # ^ constrains lgdp_1_std1 & lgdp_1_std2 to be equal
dat.gap$lgdp_1_stdIn <- dat.gap$lgdp_1_std4 + dat.gap$lgdp_1_std5
    # ^ constrains lgdp_1_std4 & lgdp_1_std5 to be equal

dat.gap$exec_pres <- dat.gap$exec_pres1 + dat.gap$exec_pres2 + dat.gap$exec_pres4 + dat.gap$exec_pres5
    # ^ constrains exec_pres1, exec_pres2, exec_pres4, exec_pres5 to be equal

dat.gap$prevd_milIn <- dat.gap$prevd_mil4 + dat.gap$prevd_mil5
    # ^ constrains prevd_mil4 & prevd_mil5 to be equal


mod.1.gapB <- coxph(Surv(start, stop, status) ~  
	lgdp_1_stdOut + growth_1_std + exec_pres + prevd_mil1  +
                          	                 prevd_mil2  +
  lgdp_1_stdIn                             + prevd_milIn +
strata(trans), data = dat.gap, method = "breslow")

summary(mod.1.gapB)
