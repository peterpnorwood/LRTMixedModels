### -------------------------------- ###
### corn_power_results.R             ###
### Peter Norwood                    ###
### ST 790 (004) - Stat Methods 2    ###
### Dr. Hughes-Oliver                ###
###                                  ###
### Purpose: plot power curves       ###
### from the corn simulation         ###
### -------------------------------- ###

## loading libraries
#library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

## setting the working directory
setwd("~/Classes/ST790 - Spring 2019")

## loading the data
load("~/Classes/ST790 - Spring 2019/Apr18_CornSim.RData")


col_names <- c("n1","n2","n4","n8","n16","n32","n64","n128","n256")

## Making any variable less than zero just zero and reproducing the before results

## breaking up the lists

Z_lrt_list <- lrt_list

Z_lrt_list[[1]][Z_lrt_list[[1]] < 0] <- 0
Z_null_set <- Z_lrt_list[[1]] %>% as.data.frame()
colnames(Z_null_set) <- col_names

Z_lrt_list[[2]][Z_lrt_list[[2]] < 0] <- 0
Z_sd5_set <- Z_lrt_list[[2]] %>% as.data.frame()
colnames(Z_sd5_set) <- col_names

Z_lrt_list[[3]][Z_lrt_list[[3]] < 0] <- 0
Z_sd10_set <- Z_lrt_list[[3]] %>% as.data.frame()
colnames(Z_sd10_set) <- col_names

Z_lrt_list[[4]][Z_lrt_list[[4]] < 0] <- 0
Z_sd15_set <- Z_lrt_list[[4]] %>% as.data.frame()
colnames(Z_sd15_set) <- col_names

Z_lrt_list[[5]][Z_lrt_list[[5]] < 0] <- 0
Z_sd20_set <- Z_lrt_list[[5]] %>% as.data.frame()
colnames(Z_sd20_set) <- col_names

Z_lrt_list[[6]][Z_lrt_list[[6]] < 0] <- 0
Z_sd25_set <- Z_lrt_list[[6]] %>% as.data.frame()
colnames(Z_sd25_set) <- col_names


### Creating the dataset with the respective sample sizes
col_namesN <- c("sd0", "sd5", "sd10", "sd15", "sd20", "sd25")

df2 <- cbind(Z_null_set$n2,
              Z_sd5_set$n2,
              Z_sd10_set$n2,
              Z_sd15_set$n2,
              Z_sd20_set$n2,
              Z_sd25_set$n2) %>% as.data.frame()

df8 <- cbind(Z_null_set$n8,
              Z_sd5_set$n8,
              Z_sd10_set$n8,
              Z_sd15_set$n8,
              Z_sd20_set$n8,
              Z_sd25_set$n8) %>% as.data.frame()

df32 <- cbind(Z_null_set$n32,
              Z_sd5_set$n32,
              Z_sd10_set$n32,
              Z_sd15_set$n32,
              Z_sd20_set$n32,
              Z_sd25_set$n32) %>% as.data.frame()

colnames(df2) <- col_namesN
colnames(df8) <- col_namesN
colnames(df32) <- col_namesN



chisqQ <- qchisq(df=1,.95)

mix_vec <- c(.5*rchisq(10000,0) + .5*rchisq(10000,df=1))
m50chisqQ <- quantile(mix_vec, .95)

p <- mean(Z_null_set$n256<.01)
mix_vecP <- c(p*rchisq(10000,0) + (1-p)*rchisq(10000,df=1))
mPchisqQ <- quantile(mix_vecP,.95)

### Calculating the Power at each signal level for the 10 replicates, 20 replicates, 50 replicates
df2_sum <- df2 %>%
  mutate(reject_chisqQ_sd0 = ifelse(sd0>chisqQ,1,0),
         reject_chisqQ_sd5=ifelse(sd5>chisqQ,1,0),
         reject_chisqQ_sd10=ifelse(sd10>chisqQ,1,0),
         reject_chisq0_sd15=ifelse(sd15>chisqQ,1,0),
         reject_chisq0_sd20=ifelse(sd20>chisqQ,1,0),
         reject_chisq0_sd25=ifelse(sd25>chisqQ,1,0),
         ## chisq 50/50
         reject_chisq50_sd0 = ifelse(sd0>m50chisqQ, 1, 0),
         reject_chisq50_sd5=ifelse(sd5>m50chisqQ,1,0),
         reject_chisq50_sd10=ifelse(sd10>m50chisqQ,1,0),
         reject_chisq50_sd15=ifelse(sd15>m50chisqQ,1,0),
         reject_chisq50_sd20=ifelse(sd20>m50chisqQ,1,0),
         reject_chisq50_sd25=ifelse(sd25>m50chisqQ,1,0),
         ## chisq custom
         reject_chisqP_sd0 = ifelse(sd0>mPchisqQ, 1, 0),
         reject_chisqP_sd5=ifelse(sd5>mPchisqQ,1,0),
         reject_chisqP_sd10=ifelse(sd10>mPchisqQ,1,0),
         reject_chisqP_sd15=ifelse(sd15>mPchisqQ,1,0),
         reject_chisqP_sd20=ifelse(sd20>mPchisqQ,1,0),
         reject_chisqP_sd25=ifelse(sd25>mPchisqQ,1,0)) %>%
  select(-c(sd0,sd5,sd10,sd15,sd20,sd25)) %>%
  summarise_all(mean) %>%
  gather()

df8_sum <- df8 %>%
  mutate(reject_chisqQ_sd0 = ifelse(sd0>chisqQ,1,0),
         reject_chisqQ_sd5=ifelse(sd5>chisqQ,1,0),
         reject_chisqQ_sd10=ifelse(sd10>chisqQ,1,0),
         reject_chisq0_sd15=ifelse(sd15>chisqQ,1,0),
         reject_chisq0_sd20=ifelse(sd20>chisqQ,1,0),
         reject_chisq0_sd25=ifelse(sd25>chisqQ,1,0),
         ## chisq 50/50
         reject_chisq50_sd0 = ifelse(sd0>m50chisqQ, 1, 0),
         reject_chisq50_sd5=ifelse(sd5>m50chisqQ,1,0),
         reject_chisq50_sd10=ifelse(sd10>m50chisqQ,1,0),
         reject_chisq50_sd15=ifelse(sd15>m50chisqQ,1,0),
         reject_chisq50_sd20=ifelse(sd20>m50chisqQ,1,0),
         reject_chisq50_sd25=ifelse(sd25>m50chisqQ,1,0),
         ## chisq custom
         reject_chisqP_sd0 = ifelse(sd0>mPchisqQ, 1, 0),
         reject_chisqP_sd5=ifelse(sd5>mPchisqQ,1,0),
         reject_chisqP_sd10=ifelse(sd10>mPchisqQ,1,0),
         reject_chisqP_sd15=ifelse(sd15>mPchisqQ,1,0),
         reject_chisqP_sd20=ifelse(sd20>mPchisqQ,1,0),
         reject_chisqP_sd25=ifelse(sd25>mPchisqQ,1,0)) %>%
  select(-c(sd0,sd5,sd10,sd15,sd20,sd25)) %>%
  summarise_all(mean) %>%
  gather()

df32_sum <- df32 %>%
  mutate(reject_chisqQ_sd0 = ifelse(sd0>chisqQ,1,0),
         reject_chisqQ_sd5=ifelse(sd5>chisqQ,1,0),
         reject_chisqQ_sd10=ifelse(sd10>chisqQ,1,0),
         reject_chisq0_sd15=ifelse(sd15>chisqQ,1,0),
         reject_chisq0_sd20=ifelse(sd20>chisqQ,1,0),
         reject_chisq0_sd25=ifelse(sd25>chisqQ,1,0),
         ## chisq 50/50
         reject_chisq50_sd0 = ifelse(sd0>m50chisqQ, 1, 0),
         reject_chisq50_sd5=ifelse(sd5>m50chisqQ,1,0),
         reject_chisq50_sd10=ifelse(sd10>m50chisqQ,1,0),
         reject_chisq50_sd15=ifelse(sd15>m50chisqQ,1,0),
         reject_chisq50_sd20=ifelse(sd20>m50chisqQ,1,0),
         reject_chisq50_sd25=ifelse(sd25>m50chisqQ,1,0),
         ## chisq custom
         reject_chisqP_sd0 = ifelse(sd0>mPchisqQ, 1, 0),
         reject_chisqP_sd5=ifelse(sd5>mPchisqQ,1,0),
         reject_chisqP_sd10=ifelse(sd10>mPchisqQ,1,0),
         reject_chisqP_sd15=ifelse(sd15>mPchisqQ,1,0),
         reject_chisqP_sd20=ifelse(sd20>mPchisqQ,1,0),
         reject_chisqP_sd25=ifelse(sd25>mPchisqQ,1,0)) %>%
  select(-c(sd0,sd5,sd10,sd15,sd20,sd25)) %>%
  summarise_all(mean) %>%
  gather()


## Renaming variables to reasonable things
test <- c(rep("Chi-Sq(2)",6),rep("50/50 Mix Chi-Sq",6), rep("Custom Mix Chi-Sq",6))
signal <- c(rep(c(0,5,10,15,20,25),3))
col_names <- c("key", "Power","Test","Signal")

df2_sum$test <-test
df2_sum$signal <- signal/10
colnames(df2_sum) <- col_names

df8_sum$test <-test
df8_sum$signal <- signal/10
colnames(df8_sum) <- col_names

df32_sum$test <-test
df32_sum$signal <- signal/10
colnames(df32_sum) <- col_names

pal <- c("#cc0000","#4B9CD3","#001A57")

## replications plot
  power2 <- ggplot(data=df2_sum,aes(x=Signal,y=Power,color=Test,group=Test)) +
    geom_point(size=1.25)  +
    geom_line(size=1.25) +
    ylim(0,1) +
    scale_color_manual(values=pal) +
    ggtitle("Replications=2") +
    theme_minimal()

## subjects 20
power8 <- ggplot(data=df8_sum,aes(x=Signal,y=Power,color=Test,group=Test)) +
  geom_point(size=1.25)  +
  geom_line(size=1.25) +
  ylim(0,1) +
  scale_color_manual(values=pal) +
  ggtitle("Replications=2") +
  theme_minimal()

## subjects 50
power32 <- ggplot(data=df32_sum,aes(x=Signal,y=Power,color=Test,group=Test)) +
  geom_point(size=1.25)  +
  geom_line(size=1.25) +
  ylim(0,1) +
  scale_color_manual(values=pal) +
  ggtitle("Subjects=32") +
  theme_minimal()


## grid to summarize all the results
sleep_powerGrid <- grid.arrange(power2,power8,power32,
                                nrow=3,ncol=1, top="Power Curves for Simulated Corn Data")



## work with Z null set
null_sum <- Z_null_set %>%
  mutate(reject_chisqQ_n1 = ifelse(n1>chisqQ,1,0),
         reject_chisqQ_n2=ifelse(n2>chisqQ,1,0),
         reject_chisqQ_n4=ifelse(n4>chisqQ,1,0),
         reject_chisq0_n8=ifelse(n8>chisqQ,1,0),
         reject_chisq0_n16=ifelse(n16>chisqQ,1,0),
         reject_chisq0_n32=ifelse(n32>chisqQ,1,0),
         reject_chisq0_n64=ifelse(n64>chisqQ,1,0),
         reject_chisq0_n128=ifelse(n128>chisqQ,1,0),
         reject_chisq0_n256=ifelse(n256>chisqQ,1,0),
         ## chisq 50/50
         reject_m50chisqQ_n1 = ifelse(n1>m50chisqQ,1,0),
         reject_m50chisqQ_n2=ifelse(n2>m50chisqQ,1,0),
         reject_m50chisqQ_n4=ifelse(n4>m50chisqQ,1,0),
         reject_m50chisq0_n8=ifelse(n8>m50chisqQ,1,0),
         reject_m50chisq0_n16=ifelse(n16>m50chisqQ,1,0),
         reject_m50chisq0_n32=ifelse(n64>m50chisqQ,1,0),
         reject_m50chisq0_n64=ifelse(n64>m50chisqQ,1,0),
         reject_m50chisq0_n128=ifelse(n128>m50chisqQ,1,0),
         reject_m50chisq0_n256=ifelse(n256>m50chisqQ,1,0),
         ## chisq custom
         reject_mPchisqQ_n1 = ifelse(n1>mPchisqQ,1,0),
         reject_mPchisqQ_n2=ifelse(n2>mPchisqQ,1,0),
         reject_mPchisqQ_n4=ifelse(n4>mPchisqQ,1,0),
         reject_mPchisq0_n8=ifelse(n8>mPchisqQ,1,0),
         reject_mPchisq0_n16=ifelse(n16>mPchisqQ,1,0),
         reject_mPchisq0_n32=ifelse(n32>mPchisqQ,1,0),
         reject_mPchisq0_n64=ifelse(n64>mPchisqQ,1,0),
         reject_mPchisq0_n128=ifelse(n128>mPchisqQ,1,0),
         reject_mPchisq0_n256=ifelse(n256>mPchisqQ,1,0)) %>%
  select(-c(n1,n2,n4,n8,n16,n32,n64,n128,n256)) %>%
  summarise_all(mean) %>%
  gather()

## Renaming variables to reasonable things
test_null <- c(rep("Chi-Sq(2)",9),rep("50/50 Mix Chi-Sq",9), rep("Custom Mix Chi-Sq",9))
n_null <- c(rep(c(1,2,4,8,16,32,64,128,256),3))
col_names_null <- c("key", "Size","Test","Subjects")

null_sum$Test <- test_null
null_sum$Subjects <- n_null
colnames(null_sum) <- col_names_null


size_plot <- ggplot(data=null_sum,aes(x=Subjects,y=Size,color=Test,group=Test)) +
  geom_point(size=3)  +
  geom_line(size=1.25,alpha = .1) +
  ylim(0,.15) +
  scale_color_manual(values=pal) +
  geom_hline(yintercept=.05, linetype="dashed", color = "black") +
  ggtitle("Size of Test") +
  labs(x="Replicates (Log Scale)") +
  scale_x_continuous(breaks=c(1,2,4,8,16,32,64,128,256),trans = log2_trans()) +
  scale_y_continuous(breaks=c(seq(0,.15,.025))) +
  theme_minimal()