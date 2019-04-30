### -------------------------------- ###
### sleep_simulation.R               ###
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
library(lme4)
library(lmerTest)

## setting the working directory
setwd("~/Classes/ST790 - Spring 2019")

## importing the gen_sleep function
source("C:\\Users\\peter\\OneDrive\\Documents\\Classes\\ST790 - Spring 2019\\gen_data.R")


## different number of subjects for simulation
subs <- c(2,5,10,
          20,50,100,
          200,500,1000)

## different standard deviations for the random effect
sds <- c(0,5,10,15,20,25)

## number of simulations
nsim <- 100

lrt_list <- list()
df <- matrix(nrow=nsim,ncol=length(subs))
col <- c()
start <- Sys.time()
## looping through the different levels of signal
for (i in 1:length(sds)) {
  ## looping through the different sample sizes
  for (j in 1:length(subs)) {
    ## looping through the the number of simulations
    for (k in 1:nsim) {
      ## generate the dataset with the ith signal and jth number of subjects
      dat <- gen_sleep(subjects=subs[j], sigmaDAY=sds[i], sigmaINT=sds[i], sigma=56)
      ## fitting the data
      fit <- lmerTest::lmer(reaction ~ day + (day|subject),dat)
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

save(lrt_list,file="Apr17_SleepSim.RData")

