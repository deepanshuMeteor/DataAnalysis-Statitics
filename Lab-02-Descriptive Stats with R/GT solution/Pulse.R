#
# Prepare the environment
#
setwd("C:\\Users\\Admin\\Desktop\\R code")
getwd()
install.packages("tidyverse")
library(tidyverse)



# Q1
pulse <- read.csv("pulse.csv")



# Q2
dim(pulse)
str(pulse)
summary(pulse)
View(pulse)
colnames(pulse)



# Q3
unique(pulse$gender)
unique(pulse$smokes)
unique(pulse$ran)
unique(pulse$activity)

pulse$gender <- as.factor(pulse$gender)
pulse$smokes <- as.factor(pulse$smokes)
pulse$ran <- as.factor(pulse$ran)
pulse$activity <- as.factor(pulse$activity)
str(pulse)



# Q4a
mean(pulse$weight)
# OR
weight <- pulse$weight
weight %>% mean
#
# Q4b
mean(pulse$weight, trim=0.05)
median(pulse$weight)
# yes, the average value (mean) is robust -
# because median and trimmed average are very close to the mean
#
# Q4c
hist(pulse$weight)



# Q5
mean_pulse1   <- mean(pulse$pulse1)
median_pulse1 <- median(pulse$pulse1)
mean_pulse1
median_pulse1
# probably, more robust average is not required
# (because median is very close to the mean)
# however, if it did, we would do
mean_pulse1   <- mean(pulse$pulse1, trim=0.05)
mean_pulse1
hist(pulse$pulse1, breaks=5)



# Q6a
dim(pulse) # 91 records
summary(pulse$activity)
# can see activity with value of 0

# view that record(s)
pulse %>% filter(activity == 0)
pulse[pulse$activity == "0", ]

# and now remove it
pulse <- pulse %>% filter(activity != 0)
pulse <- pulse[pulse$activity != "0" , ]

dim(pulse) # now it's 90 records
summary(pulse$activity)
# activity 0 has now disappeared

# one more way to do it
pulse <- pulse[-c(35), ]

pulse <- droplevels(pulse)
summary(pulse$activity)

# Q6b
summary(pulse$activity)
prop.table(table(pulse$activity))
prop.table(table(pulse$activity)) * 100

# Answer: Activity 2 at 68%



# Q7
# check unique values
unique(pulse$activity)

# separate into 3 subsets
activity1 <- pulse %>% filter(activity == 1)
activity2 <- pulse %>% filter(activity == 2)
activity3 <- pulse %>% filter(activity == 3)
#
# OR
activity1 <- pulse[pulse$activity == 1,]
activity2 <- pulse[pulse$activity == 2,]
activity3 <- pulse[pulse$activity == 3,]

# check that these add up to 90 records
nrow(activity1) + nrow(activity2) + nrow(activity3) == nrow(pulse)

# find out means
mean(activity1$weight)
mean(activity2$weight)
mean(activity3$weight)

# build histograms
hist(activity1$weight) # activity 1 is attractive to people of any weight
hist(activity2$weight) # diagram is skewed to the left
hist(activity3$weight) # diagram is skewed to the right



# Q8
# check unique values
unique(pulse$smokes)
summary(pulse$smokes)

# split into subsets
smokes1 <- pulse %>% filter(smokes == 1)
smokes2 <- pulse %>% filter(smokes == 2)
# OR
#
smokes1 <- pulse[pulse$smokes == 1,]
smokes2 <- pulse[pulse$smokes == 2,]

# find out means
mean(smokes1$pulse2)
mean(smokes2$pulse2)

# we can assume that smokers are "1" and non-smokers are "2"

# build histograms
hist(smokes1$pulse2)
hist(smokes2$pulse2)

# the curve is also visually closer to the bell shape for non-smokers

