### -------------------------------- ###
### sleep_power_results.R            ###
### Peter Norwood                    ###
### ST 790 (004) - Stat Methods 2    ###
### Dr. Hughes-Oliver                ###
###                                  ###
### Purpose: plot power curves       ###
### from the sleep simulation        ###
### -------------------------------- ###

## loading libraries
#library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(scales)

## setting the working directory
setwd("~/Classes/ST790 - Spring 2019")

## loading the data
load("~/Classes/ST790 - Spring 2019/Apr17_SleepSim.RData")


col_names <- c("n2","n5","n10","n20","n50","n100","n200","n500","n1000")

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

df10 <- cbind(Z_null_set$n10,
              Z_sd5_set$n10,
              Z_sd10_set$n10,
              Z_sd15_set$n10,
              Z_sd20_set$n10,
              Z_sd25_set$n10) %>% as.data.frame()

df20 <- cbind(Z_null_set$n20,
              Z_sd5_set$n20,
              Z_sd10_set$n20,
              Z_sd15_set$n20,
              Z_sd20_set$n20,
              Z_sd25_set$n20) %>% as.data.frame()

df50 <- cbind(Z_null_set$n50,
               Z_sd5_set$n50,
               Z_sd10_set$n50,
               Z_sd15_set$n50,
               Z_sd20_set$n50,
               Z_sd25_set$n50) %>% as.data.frame()


colnames(df10) <- col_namesN
colnames(df20) <- col_namesN
colnames(df50) <- col_namesN



chisqQ <- qchisq(df=2,.95)

mix_vec <- c(.5*rchisq(10000,0) + .5*rchisq(10000,df=2))
m50chisqQ <- quantile(mix_vec, .95)

p <- mean(Z_null_set$n1000<.01)
mix_vecP <- c(p*rchisq(10000,0) + (1-p)*rchisq(10000,df=2))
mPchisqQ <- quantile(mix_vecP,.95)

### Calculating the Power at each signal level for the 10 replicates, 20 replicates, 50 replicates
df10_sum <- df10 %>%
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

df20_sum <- df20 %>%
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

df50_sum <- df50 %>%
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

df10_sum$test <-test
df10_sum$signal1 <- signal
colnames(df10_sum) <- col_names

df20_sum$test <-test
df20_sum$signal <- signal
colnames(df20_sum) <- col_names

df50_sum$test <-test
df50_sum$ssignal <- signal
colnames(df50_sum) <- col_names

pal <- c("#cc0000","#4B9CD3","#001A57")

## subjects 10 plot
power10 <- ggplot(data=df10_sum,aes(x=Signal,y=Power,color=Test,group=Test)) +
          geom_point(size=1.25)  +
          geom_line(size=1.25) +
          ylim(0,1) +
          scale_color_manual(values=pal) +
          ggtitle("Subjects=10") +
          theme_minimal()

## subjects 20
power20 <- ggplot(data=df20_sum,aes(x=Signal,y=Power,color=Test,group=Test)) +
  geom_point(size=1.25)  +
  geom_line(size=1.25) +
  ylim(0,1) +
  scale_color_manual(values=pal) +
  ggtitle("Subjects=20") +
  theme_minimal()

## subjects 50
power50 <- ggplot(data=df50_sum,aes(x=Signal,y=Power,color=Test,group=Test)) +
  geom_point(size=1.25)  +
  geom_line(size=1.25) +
  ylim(0,1) +
  scale_color_manual(values=pal) +
  ggtitle("Subjects=50") +
  theme_minimal()



## grid to summarize all the results
sleep_powerGrid <- grid.arrange(power10,power20,power50,
                                 nrow=3,ncol=1, top="Power Curves for Simulated Sleep Data")


## create datasets with the power for just the null cases
null10 <- df10_sum %>% filter(Signal==0)
null20 <- df20_sum %>% filter(Signal==0)
null50 <- df50_sum %>% filter(Signal==0)



size10 <- ggplot(data=null10, aes(x=Test,y=Power,color=Test,fill=Test)) +
          geom_point(size=10) +
          ylab("Size") +
          ylim(0,.1) +
          geom_hline(yintercept=.05, linetype="dashed", color = "black") +
          scale_color_manual(values=pal) +
          ggtitle("Subjects=10") +
  theme_light()

size20 <- ggplot(data=null20, aes(x=Test,y=Power,color=Test,fill=Test)) +
  geom_point(size=10) +
  ylab("Size") +
  ylim(0,.1) +
  geom_hline(yintercept=.05, linetype="dashed", color = "black") +
  scale_color_manual(values=pal) +
  ggtitle("Subjects=20") +
  theme_light()
  
size50 <- ggplot(data=null50, aes(x=Test,y=Power,color=Test,fill=Test)) +
  geom_point(size=10) +
  ylab("Size") +
  ylim(0,.1) +
  geom_hline(yintercept=.05, linetype="dashed", color = "black") +
  scale_color_manual(values=pal) +
  ggtitle("Subjects=50") +
  theme_light()

sleep_sizeGrid <- grid.arrange(size10,size20,size50,
                               nrow=1,ncol=3, top="Size of Different Tests for Simulated Sleep Data")




## work with Z null set
null_sum <- Z_null_set %>%
            mutate(reject_chisqQ_n2 = ifelse(n2>chisqQ,1,0),
                   reject_chisqQ_n5=ifelse(n5>chisqQ,1,0),
                   reject_chisqQ_n10=ifelse(n10>chisqQ,1,0),
                   reject_chisq0_n20=ifelse(n20>chisqQ,1,0),
                   reject_chisq0_n50=ifelse(n50>chisqQ,1,0),
                   reject_chisq0_n100=ifelse(n100>chisqQ,1,0),
                   reject_chisq0_n200=ifelse(n200>chisqQ,1,0),
                   reject_chisq0_n500=ifelse(n500>chisqQ,1,0),
                   reject_chisq0_n1000=ifelse(n1000>chisqQ,1,0),
                   ## chisq 50/50
                   reject_m50chisqQ_n2 = ifelse(n2>m50chisqQ,1,0),
                   reject_m50chisqQ_n5=ifelse(n5>m50chisqQ,1,0),
                   reject_m50chisqQ_n10=ifelse(n10>m50chisqQ,1,0),
                   reject_m50chisq0_n20=ifelse(n20>m50chisqQ,1,0),
                   reject_m50chisq0_n50=ifelse(n50>m50chisqQ,1,0),
                   reject_m50chisq0_n100=ifelse(n100>m50chisqQ,1,0),
                   reject_m50chisq0_n200=ifelse(n200>m50chisqQ,1,0),
                   reject_m50chisq0_n500=ifelse(n500>m50chisqQ,1,0),
                   reject_m50chisq0_n1000=ifelse(n1000>m50chisqQ,1,0),
                   ## chisq custom
                   reject_mPchisqQ_n2 = ifelse(n2>mPchisqQ,1,0),
                   reject_mPchisqQ_n5=ifelse(n5>mPchisqQ,1,0),
                   reject_mPchisqQ_n10=ifelse(n10>mPchisqQ,1,0),
                   reject_mPchisq0_n20=ifelse(n20>mPchisqQ,1,0),
                   reject_mPchisq0_n50=ifelse(n50>mPchisqQ,1,0),
                   reject_mPchisq0_n100=ifelse(n100>mPchisqQ,1,0),
                   reject_mPchisq0_n200=ifelse(n200>mPchisqQ,1,0),
                   reject_mPchisq0_n500=ifelse(n500>mPchisqQ,1,0),
                   reject_mPchisq0_n1000=ifelse(n1000>mPchisqQ,1,0)) %>%
  select(-c(n2,n5,n10,n20,n50,n100,n200,n500,n1000)) %>%
  summarise_all(mean) %>%
  gather()

## Renaming variables to reasonable things
test_null <- c(rep("Chi-Sq(2)",9),rep("50/50 Mix Chi-Sq",9), rep("Custom Mix Chi-Sq",9))
n_null <- c(rep(c(2,5,10,20,50,100,200,500,1000),3))
col_names_null <- c("key", "Size","Test","Subjects")

null_sum$Test <- test_null
null_sum$Subjects <- n_null
colnames(null_sum) <- col_names_null


size_plot <- ggplot(data=null_sum,aes(x=Subjects,y=Size,color=Test,group=Test)) +
  geom_point(size=1.25)  +
  geom_line(size=1.25) +
  ylim(0,.15) +
  scale_color_manual(values=pal) +
  geom_hline(yintercept=.05, linetype="dashed", color = "black") +
  ggtitle("Size of Test") +
  labs(x="Subjects (Log Scale)") +
  scale_x_continuous(breaks=c(2,5,10,20,50,100,200,500,1000),trans = log2_trans()) +
  scale_y_continuous(breaks=c(seq(0,.15,.025))) +
  theme_minimal()


