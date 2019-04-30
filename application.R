### -------------------------------- ###
### sleep_application.R              ###
### Peter Norwood                    ###
### ST 790 (004) - Stat Methods 2    ###
### Dr. Hughes-Oliver                ###
###                                  ###
### Purpose: use the findings from   ###
### the report to analyze the true   ###
### sleep and corn datasets          ###
### -------------------------------- ###

## loading libraries
#library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(scales)

## setting the working directory
setwd("~/Classes/ST790 - Spring 2019")

## loading in the datasets
data(sleepstudy)
corn <- read_csv(file="C:\\Users\\peter\\OneDrive\\Documents\\Classes\\ST790 - Spring 2019\\corn.txt",
                 col_names=TRUE)


### fitting the sleep model
fit_sleep <- lmerTest::lmer(Reaction ~ Days + (Days|Subject),sleepstudy)
## pulling out the likelihood ratio stat from the random anova and storing it
ranov_sleep <- ranova(fit_sleep)
lrt_sleep <- ranov_sleep$LRT[2]

## chi-square p-value
sleep_chisq_pval <- 1-pchisq(lrt_sleep,df=2) ## 0

p_sleep <- .2025
custom_mix_sleep <- p_sleep*rchisq(100000,df=0) + (1-p_sleep)*rchisq(100000,df=2)
## custom mixture p-value
sleep_custom_pval <- mean(custom_mix_sleep > lrt_sleep) ## 0 

mix_sleep <- .5*rchisq(100000,df=0) + .5*rchisq(100000,df=2)
## 50/50 mixture p-value
sleep_5050_pval <- mean(mix_sleep > lrt_sleep) ## 0 


### fitting the corn model

corn$pesticide=factor(corn$pesticide,levels=c("3","1","2"))
corn$field=factor(corn$field,levels=c("2","1"))
corn$method=factor(corn$method,levels=c("4","1","2","3"))


fit_corn <- lmerTest::lmer(yield ~ pesticide*method + (1|field:pesticide), data=corn)
ranov_corn <- ranova(fit_corn)
lrt_corn <- ranov_corn$LRT[2]

## chi-square p-value
corn_chisq_pval <- 1-pchisq(lrt_corn,df=1) ## 0.006908451

p_corn <- .642
custom_mix_corn <- p_corn*rchisq(100000,df=0) + (1-p_corn)*rchisq(100000,df=1)
## custom mixture p-value
corn_custom_pval <- mean(custom_mix_corn > lrt_corn) ## 1e-05

mix_corn <- .5*rchisq(100000,df=0) + .5*rchisq(100000,df=1)
## 50/50 mixture p-value
sleep_5050_pval <- mean(mix_corn > lrt_corn) ## 0.00014
