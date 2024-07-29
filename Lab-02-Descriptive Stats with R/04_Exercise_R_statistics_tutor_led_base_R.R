# Descriptive statistics with R

# set working directory
setwd("C:\\Users\\Admin\\Desktop\\R code")
# get working directory again - to confirm
getwd()

# read data
cdc <- read.csv("cdc.csv")

# data types
str(cdc)

# Which are the factors (categorical data) ?
cdc$gehhlth <- as.factor(cdc$genhlth)
cdc$exerany <- as.factor(cdc$exerany)
cdc$hlthplan <- as.factor(cdc$hlthplan)
cdc$smoke100 <- as.factor(cdc$smoke100)
str(cdc)

# all main descriptive statistics
# observe the different measures for
# numeric and categorical data
summary(cdc)

# split the data into 2 sets, for male and female
cdc_m <- cdc[cdc$gender == 'm',]
cdc_f <- cdc[cdc$gender == 'f',]

# separate some numeric columns
h_m <- cdc_m$height
h_f <- cdc_f$height
w_m <- cdc_m$weight
w_f <- cdc_f$weight

# measures of central tendency

# median
median_h_m <- median(h_m, na.rm = TRUE)

# mean
mean_h_m <- mean(h_m, na.rm = TRUE)

# trimmed mean
trim_mean_h_m <- mean(h_m, na.rm = TRUE, trim = 0.05)

# Try with the other vectors - h_f, w_m, w_f and compare
########################################################

# Let's create a vector with some missing values
v_na <- c(1:5,NA,7:15,NA)
# Apply the measures of central tendency 
# for na.rm = FALSE and na.rm = TRUE
########################################################

# Measures of spread (variability)

# range
range_h_m <- range(h_m, na.rm = TRUE)

# interquartile range (IQR)
IQR_h_m <- IQR(h_m, na.rm = TRUE)

# sample variance
var_h_m <- var(h_m, na.rm = TRUE)

# population variance
n <- length(h_m)
var_h_m_pop <- var(h_m) * (n-1)/n

# standard deviation
std_var_h_m <- sd(h_m, na.rm = TRUE)

# Try with the other vectors - h_f, w_m, w_f and compare
########################################################

# Bivariate

# covariance
# height and weight - males
cov_h_w_m <- cov(h_m, w_m)
# height and weight - females
cov_h_w_f <- cov(h_f, w_f)
# Can we compare the two ?
# Can we say that any of them is strong ?

# correlation
# height and weight - males
correl_h_w_m <- cor(h_m, w_m)
# height and weight - females
correl_h_w_f <- cor(h_f, w_f)
# Can we compare the two ?
# Can we say that any of them is strong ?

### CATEGORICAL DATA

# Contingency Tables

# 1, 2, 3 ... variables
health <- table(cdc$genhlth)
health

health_smoke <- table(cdc$genhlth, cdc$smoke100)
health_smoke

health_smoke_gender <- table(cdc$genhlth, cdc$smoke100, cdc$gender)
health_smoke_gender

