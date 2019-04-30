### -------------------------------- ###
### corn_simulation.R                ###
### Peter Norwood                    ###
### ST 790 (004) - Stat Methods 2    ###
### Dr. Hughes-Oliver                ###
###                                  ###
### Purpose: generate simualted      ###
### datasets based on the sleep      ###
### study and corn study presented   ###
### in class.  Then use the data to  ###
### look at test performance for     ###
### asympotic tests for variance     ###
### components                       ###
### -------------------------------- ###

## loading libraries
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)

## setting the working directory
setwd("~/Classes/ST790 - Spring 2019")

## importing the gen_sleep function
source("C:\\Users\\peter\\OneDrive\\Documents\\Classes\\ST790 - Spring 2019\\gen_data.R")


## different number of replications for simulation
rs <- c(1,2,4,
        8,16,32,
        64,128,256)

## different standard deviations for the random effect
sds <- c(0,.5,1,1.5,2,2.5)

## number of simulations
nsim <- 100

lrt_list <- list()
df <- matrix(nrow=nsim,ncol=length(rs))
col <- c()
start <- Sys.time()
## looping through the different levels of signal
for (i in 1:length(sds)) {
  ## looping through the different sample sizes
  for (j in 1:length(rs)) {
    ## looping through the the number of simulations
    for (k in 1:nsim) {
      ## generate the dataset with the ith signal and jth number of subjects
      dat <- gen_corn(reps=rs[j], sigma1=5, sigma2=sds[i])
      ## fitting the data
      fit <- lmerTest::lmer(yield~pesticide*method + (1|field:pesticide), data=dat)
      ## pulling out the likelihood ratio stat from the random anova and storing it
      ranov <- ranova(fit)
      lrt <- ranov$LRT[2]
      col[k] <- lrt  
    }
    df[,j] <- col
  }
  lrt_list[[i]] <- df
}
end <- Sys.time()

save(lrt_list,file="Apr18_CornSim.RData")