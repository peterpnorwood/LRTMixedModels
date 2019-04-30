### -------------------------------- ###
### sleep_results.R                  ###
### Peter Norwood                    ###
### ST 790 (004) - Stat Methods 2    ###
### Dr. Hughes-Oliver                ###
###                                  ###
### Purpose: summarize results       ###
### from the sleep simulation        ###
### -------------------------------- ###

## loading libraries
#library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)

## setting the working directory
setwd("C:\\Users\\peter\\OneDrive\\Documents\\Classes\\ST790 - Spring 2019")

## loading the data
load("C:\\Users\\peter\\OneDrive\\Documents\\Classes\\ST790 - Spring 2019\\Apr20_SleepSim10k.RData")


col_names <- c("n2","n5","n10","n20","n50","n100","n200","n500","n1000")

## Making any variable less than zero just zero and reproducing the before results

## breaking up the lists

Z_lrt_list <- lrt_list

Z_lrt_list[[1]][Z_lrt_list[[1]] < 0] <- 0
Z_null_set <- Z_lrt_list[[1]] %>% as.data.frame()
colnames(Z_null_set) <- col_names

Z_lrt_list[[2]][Z_lrt_list[[2]] < 0] <- 0
Z_sd10_set <- Z_lrt_list[[2]] %>% as.data.frame()
colnames(sd10_set) <- col_names

Z_lrt_list[[3]][Z_lrt_list[[3]] < 0] <- 0
Z_sd20_set <- Z_lrt_list[[3]] %>% as.data.frame()
colnames(Z_sd20_set) <- col_names

Z_lrt_list[[4]][Z_lrt_list[[4]] < 0] <- 0
Z_sd30_set <- Z_lrt_list[[4]] %>% as.data.frame()
colnames(Z_sd30_set) <- col_names

Z_lrt_list[[5]][Z_lrt_list[[5]] < 0] <- 0
Z_sd40_set <- Z_lrt_list[[5]] %>% as.data.frame()
colnames(Z_sd40_set) <- col_names

Z_lrt_list[[6]][Z_lrt_list[[6]] < 0] <- 0
Z_sd50_set <- Z_lrt_list[[6]] %>% as.data.frame()
colnames(Z_sd50_set) <- col_names



##########################################################################################
##########################################################################################
##########################################################################################


## Using a chi-sq distribrution chi-sq(2)
chisq_vec <- data.frame(rchisq(10000,df=2))
colnames(chisq_vec) <- c("x")

Z_n2_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n1000,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=2") +
  theme_minimal() +
  NULL

Z_n5_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n5,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=5") +
  theme_minimal() +
  NULL

Z_n10_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n10,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=10") +
  theme_minimal() +
  NULL

Z_n20_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n20,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=20") +
  theme_minimal() +
  NULL

Z_n50_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n50,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=50") +
  theme_minimal() +
  NULL

Z_n100_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n100,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=100") +
  theme_minimal() +
  NULL

Z_n200_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n200,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=200") +
  theme_minimal() +
  NULL

Z_n500_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n500,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=500") +
  theme_minimal() +
  NULL

Z_n1000_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n1000,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=1000") +
  theme_minimal() +
  NULL

## grid to summarize all the results
Z_nullHist_grid <- grid.arrange(Z_n2_nullHist, Z_n5_nullHist, Z_n10_nullHist,
                              Z_n20_nullHist, Z_n50_nullHist, Z_n100_nullHist,
                              Z_n200_nullHist,Z_n500_nullHist,Z_n1000_nullHist,
                              nrow=3,ncol=3,top="Simulated Sleep LRTs, Chi-Square(2) Density in Carolina Blue")


##########################################################################################
##########################################################################################
##########################################################################################


## Using a mixture chi-sq distribrution 50% chi-sq(0) 50% chi-sq(2)

mix_vec <- data.frame(.5*rchisq(10000,0) + .5*rchisq(10000,df=2))
colnames(mix_vec) <- c("x")


M_n2_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n2,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=2") +
  theme_minimal() +
  NULL

M_n5_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n5,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=5") +
  theme_minimal() +
  NULL

M_n10_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n10,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=10") +
  theme_minimal() +
  NULL

M_n20_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n20,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=20") +
  theme_minimal() +
  NULL

M_n50_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n50,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=50") +
  theme_minimal() +
  NULL

M_n100_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n100,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=100") +
  theme_minimal() +
  NULL

M_n200_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n200,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=200") +
  theme_minimal() +
  NULL

M_n500_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n500,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=500") +
  theme_minimal() +
  NULL

M_n1000_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n1000,y=..scaled..),bins=10,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  #ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=1000") +
  theme_minimal() +
  NULL

## grid to summarize all the results
M_nullHist_grid <- grid.arrange(M_n2_nullHist, M_n5_nullHist, M_n10_nullHist,
                                M_n20_nullHist, M_n50_nullHist, M_n100_nullHist,
                                M_n200_nullHist,M_n500_nullHist,M_n1000_nullHist,
                                nrow=3,ncol=3, top="Simulated Sleep LRTs, 50/50  Chi-Sq Density in Red")


##########################################################################################
##########################################################################################
##########################################################################################


## Using a mixture chi-sq distribrution p% chi-sq(0) (1-p)% chi-sq(2)
## p is the percent of likelihood ratio statistics that equal 0

p <- mean(Z_null_set$n1000<.01)


mix_vecP <- data.frame(p*rchisq(10000,0) + (1-p)*rchisq(10000,df=2))
colnames(mix_vecP) <- c("x")


MP_n2_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n2,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=2") +
  theme_minimal() +
  NULL

MP_n5_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n5,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=5") +
  theme_minimal() +
  NULL

MP_n10_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n10,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=10") +
  theme_minimal() +
  NULL

MP_n20_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n20,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=20") +
  theme_minimal() +
  NULL

MP_n50_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n50,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=50") +
  theme_minimal() +
  NULL

MP_n100_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n100,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=100") +
  theme_minimal() +
  NULL

MP_n200_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n200,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=200") +
  theme_minimal() +
  NULL

MP_n500_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n500,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=500") +
  theme_minimal() +
  NULL

MP_n1000_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n1000,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=1000") +
  theme_minimal() +
  NULL

## grid to summarize all the results
MP_nullHist_grid <- grid.arrange(MP_n2_nullHist, MP_n5_nullHist, MP_n10_nullHist,
                                MP_n20_nullHist, MP_n50_nullHist, MP_n100_nullHist,
                                MP_n200_nullHist,MP_n500_nullHist,MP_n1000_nullHist,
                                nrow=3,ncol=3, top="Simulated Sleep LRTs, Custom Mixture Chi-Sq Density in Duke Blue")








