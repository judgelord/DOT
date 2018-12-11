# Replicates with R 3.3.1, given the following package versions.

rm(list=ls())
library(msm)        #v1.6.1
library(survival)   #v.2.39.5 (!! Important.)
library(mstate)     #v0.2.9

setwd("C:/Users/Shawna/Desktop/PA replic")

set.seed(031415)

### Simulate 250 individuals with common observation times
sim.df <- data.frame(subject = rep(1:250, rep(101,250)), time = rep(seq(0, 100, 1), 250), x=rep(NA,250))

for(i in 1:nrow(sim.df)){
	tmp <- unique(sim.df$subject)[i]
	sim.df$x[sim.df$subject==tmp] <- rnorm(1)
}

#Set Baseline Hazards for Each Transition in the Model 
qmatrix <- rbind(c(0,   0.15,  0.05 ),
                 c(0.02,   0,  0.05 ),
                 c(0,0,0))

#Set Number of Simulations                                
loops <- 1000

#Define storage objects to store simulation results 
betas.sep <- matrix(NA, nrow=loops, ncol=4)
std.sep <- matrix(NA, nrow= loops, ncol=4)

betas.comb.1 <- matrix(NA, nrow=loops, ncol=5)
std.comb.1 <- matrix(NA, nrow= loops, ncol=5)
ph.1 <- matrix(NA, nrow= loops, ncol=1)

betas.comb.1a <- matrix(NA, nrow=loops, ncol=3)
std.comb.1a <- matrix(NA, nrow= loops, ncol=3)

betas.comb.2 <- matrix(NA, nrow=loops, ncol=5)
std.comb.2 <- matrix(NA, nrow= loops, ncol=5)
ph.2 <- matrix(NA, nrow= loops, ncol=1)

betas.comb.3 <- matrix(NA, nrow=loops, ncol=4)
std.comb.3 <- matrix(NA, nrow= loops, ncol=4)
ph.3 <- matrix(NA, nrow= loops, ncol=1)


for(a in 1:loops){

#Simulate data, and set True coefficients                                  
dat <- simmulti.msm(sim.df, qmatrix, death = 3, covariates = list(x = c(.5, 3, 1, -1)))

#Create empty data frame to store simulated data in multi-state format
dat.ms <- data.frame(id = numeric(0), entry = numeric(0), exit = numeric(0), from = numeric(0), to = numeric(0), status = numeric(0), x = numeric(0)) 

#Reformat simulated data into multi-state format
for (i in 1:nrow(dat)){
	tmp <- unique(dat$subject)[i]
	dat.tmp <- subset(dat, dat$subject == tmp)
	dat.tmp$stop <- dat.tmp$time+1	
	startTemp <- dat.tmp$time[1]

	for(k in 1:nrow(dat.tmp)){
		if (k > 1){
			if (dat.tmp$state[k] == dat.tmp$state[k-1]){
	 		}else{
				endStop <- dat.tmp$time[k]
  				tempRow <- data.frame(id = dat.tmp$subject[k], entry = startTemp, exit = endStop, from = dat.tmp$state[k-1], to = dat.tmp$state[k], status = 1, x = dat.tmp$x[k]) 
  
  				startTemp <- dat.tmp$time[k]
  				dat.ms <- rbind(dat.ms, tempRow)
 			}
 		}
 	}
}

#Rename multi-state formatted dataset		
dat <- dat.ms

dat <- dat[order(dat$id, dat$entry),]
tid <- unique(dat$id)
	
dat.msf <- data.frame(id = numeric(0), entry = numeric(0), exit = numeric(0), from = numeric(0), to = numeric(0), status = numeric(0), x1 = numeric(0), x2 = numeric(0)) 

for (j in 1:length(tid)){
	tmp <- dat[dat$id == tid[j],]

	for (k in 1:nrow(tmp)){
		tmp.ms <- tmp[k,]
		tmp.ms$status <- 0
		if(tmp[k,]$from==1 & tmp[k,]$to==2){tmp.ms$to <- 3}
		if(tmp[k,]$from==1 & tmp[k,]$to==3){tmp.ms$to <- 2}			
		if(tmp[k,]$from==2 & tmp[k,]$to==1){tmp.ms$to <- 3}
		if(tmp[k,]$from==2 & tmp[k,]$to==3){tmp.ms$to <- 1}

		dat.msf <- rbind(dat.msf, tmp[k,], tmp.ms)
	}			
}

########Transitions Seperated###########
dat.msf.sep <- dat.msf

#Define transitions
dat.msf.sep$trans <- 1
	dat.msf.sep$trans[dat.msf.sep$from==1 & dat.msf.sep$to==3] <- 2
	dat.msf.sep$trans[dat.msf.sep$from==2 & dat.msf.sep$to==1] <- 3
	dat.msf.sep$trans[dat.msf.sep$from==2 & dat.msf.sep$to==3] <- 4

#Define transitions-specific covariates		
	dat.msf.sep$x.1 <- dat.msf.sep$x
	dat.msf.sep$x.1[dat.msf.sep$trans != 1] <- 0
	dat.msf.sep$x.2 <- dat.msf.sep$x
	dat.msf.sep$x.2[dat.msf.sep$trans != 2] <- 0
	dat.msf.sep$x.3 <- dat.msf.sep$x
	dat.msf.sep$x.3[dat.msf.sep$trans != 3] <- 0
	dat.msf.sep$x.4 <- dat.msf.sep$x
	dat.msf.sep$x.4[dat.msf.sep$trans != 4] <- 0
		
	
#Estimate model and store results	
	mod.sep <- coxph(Surv(entry, exit, status) ~ x.1 + x.2 + x.3 + x.4 +  strata(trans), data = dat.msf.sep, method = "breslow")
	summary(mod.sep)
	
	betas.sep[a,] <- mod.sep$coef
	std.sep[a,] <- sqrt(diag(mod.sep$var))	

########Transitions 2 & 4 Collapsed, Unique Coefficients for X2 and X4###########
dat.msf.comb.2 <- dat.msf

#Define transitions
dat.msf.comb.2$trans <- 1
	dat.msf.comb.2$trans[dat.msf.comb.2$from==1 & dat.msf.comb.2$to==3] <- 2
	dat.msf.comb.2$trans[dat.msf.comb.2$from==2 & dat.msf.comb.2$to==1] <- 3
	dat.msf.comb.2$trans[dat.msf.comb.2$from==2 & dat.msf.comb.2$to==3] <- 4

#Define transition-specific covariates		
	dat.msf.comb.2$x.1 <- dat.msf.comb.2$x
	dat.msf.comb.2$x.1[dat.msf.comb.2$trans != 1] <- 0
	dat.msf.comb.2$x.2 <- dat.msf.comb.2$x
	dat.msf.comb.2$x.2[dat.msf.comb.2$trans != 2] <- 0
	dat.msf.comb.2$x.3 <- dat.msf.comb.2$x
	dat.msf.comb.2$x.3[dat.msf.comb.2$trans != 3] <- 0
	dat.msf.comb.2$x.4 <- dat.msf.comb.2$x
	dat.msf.comb.2$x.4[dat.msf.comb.2$trans != 4] <- 0

#Collapse transitions 2 and 4	
	dat.msf.comb.2$trans[dat.msf.comb.2$trans==4] <- 2
	
#Add Time Varying Illness Indicator
	dat.msf.comb.2$ill <- 0
	dat.msf.comb.2$ill[dat.msf.comb.2$from == 2 & dat.msf.comb.2$to == 3] <- 1

#Estimate model and store results							
	mod.comb.2 <- coxph(Surv(entry, exit, status) ~ x.1 + x.2 + x.3 + x.4  + ill +  strata(trans), data = dat.msf.comb.2, method = "breslow")
	summary(mod.comb.2)
	
	betas.comb.2[a,] <- mod.comb.2$coef
	std.comb.2[a,] <- sqrt(diag(mod.comb.2$var))
	
	sto <- cox.zph(mod.comb.2, transform="km", global=TRUE)	
	ph.2[a,] <- sto$table[5,3]
	
########Transitions 2 & 4 Collapsed, Collapsed Coefficient for X2 and X4###########
dat.msf.comb.3 <- dat.msf

#Define transitions
dat.msf.comb.3$trans <- 1
	dat.msf.comb.3$trans[dat.msf.comb.3$from==1 & dat.msf.comb.3$to==3] <- 2
	dat.msf.comb.3$trans[dat.msf.comb.3$from==2 & dat.msf.comb.3$to==1] <- 3
	dat.msf.comb.3$trans[dat.msf.comb.3$from==2 & dat.msf.comb.3$to==3] <- 4

#Define transition-specific covariates		
	dat.msf.comb.3$x.1 <- dat.msf.comb.3$x
	dat.msf.comb.3$x.1[dat.msf.comb.3$trans != 1] <- 0
	dat.msf.comb.3$x.2 <- dat.msf.comb.3$x
	dat.msf.comb.3$x.2[dat.msf.comb.3$trans != 2 & dat.msf.comb.3$trans !=4] <- 0
	dat.msf.comb.3$x.3 <- dat.msf.comb.3$x
	dat.msf.comb.3$x.3[dat.msf.comb.3$trans != 3] <- 0

#Collapse transitions 2 and 4	
	dat.msf.comb.3$trans[dat.msf.comb.3$trans==4] <- 2
	
#Add Time Varying Illness Indicator
	dat.msf.comb.3$ill <- 0
	dat.msf.comb.3$ill[dat.msf.comb.3$from == 2 & dat.msf.comb.3$to == 3] <- 1

#Estimate model and store results										
	mod.comb.3 <- coxph(Surv(entry, exit, status) ~ x.1 + x.2 + x.3 + ill +  strata(trans), data = dat.msf.comb.3, method = "breslow")
	summary(mod.comb.3)
	
	betas.comb.3[a,] <- mod.comb.3$coef
	std.comb.3[a,] <- sqrt(diag(mod.comb.3$var))
	
	sto <- cox.zph(mod.comb.3, transform="km", global=TRUE)	
	ph.3[a,] <- sto$table[4,3]

########Transitions 1 & 2 Collapsed - Unique Coefficients for X1 and X2###########
dat.msf.comb.1 <- dat.msf

#Define transitions
dat.msf.comb.1$trans <- 1
	dat.msf.comb.1$trans[dat.msf.comb.1$from==1 & dat.msf.comb.1$to==3] <- 2
	dat.msf.comb.1$trans[dat.msf.comb.1$from==2 & dat.msf.comb.1$to==1] <- 3
	dat.msf.comb.1$trans[dat.msf.comb.1$from==2 & dat.msf.comb.1$to==3] <- 4

#Define transition-specific covariates			
	dat.msf.comb.1$x.1 <- dat.msf.comb.1$x
	dat.msf.comb.1$x.1[dat.msf.comb.1$trans != 1] <- 0
	dat.msf.comb.1$x.2 <- dat.msf.comb.1$x
	dat.msf.comb.1$x.2[dat.msf.comb.1$trans != 2] <- 0
	dat.msf.comb.1$x.3 <- dat.msf.comb.1$x
	dat.msf.comb.1$x.3[dat.msf.comb.1$trans != 3] <- 0
	dat.msf.comb.1$x.4 <- dat.msf.comb.1$x
	dat.msf.comb.1$x.4[dat.msf.comb.1$trans != 4] <- 0

#Collapse transitions 1 and 2	
	dat.msf.comb.1$trans[dat.msf.comb.1$trans==2] <- 1
	
	#Add Transition Indicator
	dat.msf.comb.1$ill <- 0
	dat.msf.comb.1$ill[dat.msf.comb.1$from == 1 & dat.msf.comb.1$to == 3] <- 1
	
#Estimate model and store results													
	mod.comb.1 <- coxph(Surv(entry, exit, status) ~ x.1 + x.2 + x.3 + x.4  + ill +  strata(trans), data = dat.msf.comb.1, method = "breslow")
	summary(mod.comb.1)
	
	betas.comb.1[a,] <- mod.comb.1$coef
	std.comb.1[a,] <- sqrt(diag(mod.comb.1$var))
	
	sto <- cox.zph(mod.comb.1, transform="km", global=TRUE)	
	ph.1[a,] <- sto$table[5,3]

########Transitions 1 & 2 - Collapsed Coefficients for X1 and X2###########
dat.msf.comb.1a <- dat.msf

#Define transitions
dat.msf.comb.1a$trans <- 1
	dat.msf.comb.1a$trans[dat.msf.comb.1a$from==1 & dat.msf.comb.1a$to==3] <- 2
	dat.msf.comb.1a$trans[dat.msf.comb.1a$from==2 & dat.msf.comb.1a$to==1] <- 3
	dat.msf.comb.1a$trans[dat.msf.comb.1a$from==2 & dat.msf.comb.1a$to==3] <- 4

#Define transition-specific covariates				
	dat.msf.comb.1a$x.1 <- dat.msf.comb.1a$x
	dat.msf.comb.1a$x.1[dat.msf.comb.1a$trans != 1 & dat.msf.comb.1a$trans != 2] <- 0
	dat.msf.comb.1a$x.3 <- dat.msf.comb.1a$x
	dat.msf.comb.1a$x.3[dat.msf.comb.1a$trans != 3] <- 0
	dat.msf.comb.1a$x.4 <- dat.msf.comb.1a$x
	dat.msf.comb.1a$x.4[dat.msf.comb.1a$trans != 4] <- 0
		
#Estimate model and store results														
	mod.comb.1a <- coxph(Surv(entry, exit, status) ~ x.1 + x.3 + x.4  +  strata(trans), data = dat.msf.comb.1a, method = "breslow")
	summary(mod.comb.1a)
	
	betas.comb.1a[a,] <- mod.comb.1a$coef
	std.comb.1a[a,] <- sqrt(diag(mod.comb.1a$var))

	print(a)

}	

save.image("MonteCarlo_Simulations.RData")
