### -------------------------------- ###
### gen_data.R                       ###
### Peter Norwood                    ###
### ST 790 (004) - Stat Methods 2    ###
### Dr. Hughes-Oliver                ###
###                                  ###
### Purpose: generate simualted      ###
### datasets based on the sleep      ###
### study and corn study presented   ###
### in class                         ###
### -------------------------------- ###

library(readr)

## vector of days 
days <- seq(0,9)

## gen_sleep ##
## Author: Peter Norwood 
## Purpose: Generates a simulated dataset based on the sleepstudy dataset

## Inputs:
## subjects  : the number of subjects in the study
## sigma1    : the error variance
## sigma2    : the random effect from days nested in subject (days|subject)


## Output:
## sim_sleep : a data frame with simulated data

gen_sleep <- function(subjects,sigmaINT=0,sigmaDAY=0,sigma=56) {
  
  subject <- c(rep(1:subjects,each=10))
  day <- c(rep(0:9,subjects))
  
  sim_sleep <- data.frame(subject,day)
  sim_sleep$subject <- as.factor(subject)
  
  rand_int <- rnorm(subjects,mean=0,sd=sigmaINT)
  
  
  sim_sleep$reaction <- 10*sim_sleep$day + ## fixed effect of day
                        rep(rand_int,each=10) + ## random intercept
                        rnorm(subjects*day,mean=0,sd=sigmaDAY)*sim_sleep$day + ## random effect of day|subject
                        rnorm(length(sim_sleep$day),mean=300,sd=sigma) ## error
  
  return(sim_sleep)
  
}

##########################################################################################
##########################################################################################


## reading in the corn dataset
corn <- read_csv(file="C:\\Users\\peter\\OneDrive\\Documents\\Classes\\ST790 - Spring 2019\\corn.txt",
                 col_names=TRUE)

## switching to factors, not integers
corn$pesticide <- factor(corn$pesticide, levels=c("3","1","2"))
corn$field <- factor(corn$field, levels=c("2","1"))
corn$method <- factor(corn$method, levels=c("4","1","2","3"))

## gen_corn ##
## Author: Peter Norwood 
## Purpose: Generates a simulated dataset based on the corn dataset

## Inputs:
## reps      : the number of replications from certain pesticide*method*field
## sigma1    : the error variance
## sigma2    : the random effect from field nested within pesticide


## Output:
## sim_sleep : a data frame with simulated data

gen_corn <- function(reps,sigma1=5,sigma2=0) {
  
  
  sim_corn <- corn %>% slice(rep(row_number(), reps))
  sim_corn$yield <- NA
  
  ## design
  #X <- model.matrix(~(sim_corn$pesticide)*(sim_corn$method))
  #Z <- model.matrix(~(sim_corn$field)*(sim_corn$pesticide))
  
  ## matrix formulation (may be used later)
  #design <- model.matrix(~(sim_corn$pesticide)*(sim_corn$method)+(sim_corn$field)*(sim_corn$pesticide))
  
  #beta <- c(63.6, ## intercept
          #  -8.2, 2.05, ## pesticides
          #  6.95, -4.2, -3.8, ## methods
          #  1.5, 2.95, 1.25, 4.85, 2.10, 0.85) ## pesticide*corn
  
  #gamma <- rnorm(ncol(Z), mean=0,sd=sigma2) ## random effect for corn
  
  sim_corn$yield <- 63.6 - 8.2*I(corn$pesticide==1) + 2.050*I(corn$pesticide==2) - 
                    6.95*I(corn$method==1) - 4.2*I(corn$method==2) - 3.8*I(corn$method==3) +
                    1.5*I(corn$pesticide==1 & corn$method==1) -
                    2.95*I(corn$pesticide==2 & corn$method==1) +
                    1.25*I(corn$pesticide==1 & corn$method==2) -
                    4.85*I(corn$pesticide==2 & corn$method==2) +
                    2.10*I(corn$pesticide==1 & corn$method==3) -
                    0.85*I(corn$pesticide==2 & corn$method==3) +
                    rnorm(1,mean=0,sd=sigma2)*I(corn$field==1 & corn$pesticide==1) +
                    rnorm(1,mean=0,sd=sigma2)*I(corn$field==1 & corn$pesticide==2) +
                    rnorm(1,mean=0,sd=sigma2)*I(corn$field==1 & corn$pesticide==3) +
                    rnorm(1,mean=0,sd=sigma2)*I(corn$field==2 & corn$pesticide==1) +
                    rnorm(1,mean=0,sd=sigma2)*I(corn$field==2 & corn$pesticide==2) +
                    rnorm(1,mean=0,sd=sigma2)*I(corn$field==2 & corn$pesticide==3) +
                    rnorm(length(sim_corn$pesticide),mean=0,sd=sigma1)
                    
  return(sim_corn)
  
}








