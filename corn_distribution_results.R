### -------------------------------- ###
### corn_results.R                   ###
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
setwd("//wolftech.ad.ncsu.edu/cos/stat/Redirect/pnorwoo/Documents/ST790 - Spring 2019")

## loading the data
load("//wolftech.ad.ncsu.edu/cos/stat/Redirect/pnorwoo/Documents/ST790 - Spring 2019/Apr17_CornSim.RData")

col_names <- c("n1","n2","n4","n8","n16","n32","n64","n128","n256")


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


## Using a chi-sq distribrution chi-sq(1)

chisq_vec <- data.frame(rchisq(1000,df=1))
colnames(chisq_vec) <- c("x")

Z_n1_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n1,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="red") +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=1") +
  theme_minimal() +
  NULL

Z_n2_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n2,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="red") +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=2") +
  theme_minimal() +
  NULL

Z_n4_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n4,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=4") +
  theme_minimal() +
  NULL

Z_n8_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n8,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=8") +
  theme_minimal() +
  NULL

Z_n16_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n16,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=16") +
  theme_minimal() +
  NULL

Z_n32_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n32,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=32") +
  theme_minimal() +
  NULL

Z_n64_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n64,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=64") +
  theme_minimal() +
  NULL

Z_n128_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n128,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=128") +
  theme_minimal() +
  NULL

Z_n256_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n256,y=..scaled..),bins=30,stat="density",color="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=chisq_vec, color="#4B9CD3",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=256") +
  theme_minimal() +
  NULL

## grid to summarize all the results
Z_nullHist_grid <- grid.arrange(Z_n1_nullHist, Z_n2_nullHist, Z_n4_nullHist,
                              Z_n8_nullHist, Z_n16_nullHist, Z_n32_nullHist,
                              Z_n64_nullHist,Z_n128_nullHist,Z_n256_nullHist,
                              nrow=3,ncol=3, top="Simulated Corn LRTs, Chi-Square(1) Density in Carolina Blue")

##########################################################################################
##########################################################################################
##########################################################################################


## Making any variable less than zero just zero and reproducing the before results

mix_vec <- data.frame(.5*rchisq(1000,0) + .5*rchisq(1000,df=1))
colnames(mix_vec) <- c("x")

M_n1_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n1,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=1") +
  theme_minimal() +
  NULL

M_n2_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n2,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=2") +
  theme_minimal() +
  NULL

M_n4_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n4,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=4") +
  theme_minimal() +
  NULL

M_n8_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n4,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=8") +
  theme_minimal() +
  NULL

M_n16_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n16,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=16") +
  theme_minimal() +
  NULL

M_n32_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n32,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=32") +
  theme_minimal() +
  NULL

M_n64_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n64,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=64") +
  theme_minimal() +
  NULL

M_n128_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n128,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  ylim(0,1) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=128") +
  theme_minimal() +
  NULL

M_n256_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n256,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vec, color="#CC0000",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=256") +
  theme_minimal() +
  NULL

## grid to summarize all the results
M_nullHist_grid <- grid.arrange(M_n1_nullHist, M_n2_nullHist, M_n4_nullHist,
                                M_n8_nullHist, M_n16_nullHist, M_n32_nullHist,
                                M_n64_nullHist,M_n128_nullHist,M_n256_nullHist,
                                nrow=3,ncol=3, top="Simulated Corn LRTs, 50/50 Mixture Chi-Square Density in Red")

##########################################################################################
##########################################################################################
##########################################################################################


## Making any variable less than zero just zero and reproducing the before results

p <- mean(Z_null_set$n256<.01)


mix_vecP <- data.frame(p*rchisq(10000,0) + (1-p)*rchisq(10000,df=2))
colnames(mix_vecP) <- c("x")

MP_n1_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n1,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=1") +
  theme_minimal() +
  NULL

MP_n2_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n2,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=2") +
  theme_minimal() +
  NULL

MP_n4_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n4,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=4") +
  theme_minimal() +
  NULL

MP_n8_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n8,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=8") +
  theme_minimal() +
  NULL

MP_n16_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n16,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Subjects=16") +
  theme_minimal() +
  NULL

MP_n32_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n32,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=32") +
  theme_minimal() +
  NULL

MP_n64_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n64,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=64") +
  theme_minimal() +
  NULL

MP_n128_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n128,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=128") +
  theme_minimal() +
  NULL

MP_n256_nullHist <- ggplot(data=Z_null_set) +
  geom_histogram(aes(x=n256,y=..scaled..),stat="density",color="#696969", fill="#696969") +
  geom_density(aes(x=x,y=..scaled..),data=mix_vecP, color="#001A57",size=1.25) +
  xlim(-.5,6) +
  labs(x="Likelihood Ratio Statistic",y="") +
  ggtitle("Replications=256") +
  theme_minimal() +
  NULL

## grid to summarize all the results
MP_nullHist_grid <- grid.arrange(MP_n1_nullHist, MP_n2_nullHist, MP_n4_nullHist,
                                MP_n8_nullHist, MP_n16_nullHist, MP_n32_nullHist,
                                MP_n64_nullHist,MP_n128_nullHist,MP_n256_nullHist,
                                nrow=3,ncol=3, top="Simulated Corn LRTs, Custom Mixture Chi-Square Density in Duke Blue")