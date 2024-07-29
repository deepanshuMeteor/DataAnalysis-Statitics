# Task 1

# generate normally distributed random numbers
# different means same standard deviations
x <- rnorm(10, mean=100, sd=5) 
y <- rnorm(20, mean=105, sd=5) 

# Student's t-test
t.test(x, y, var.equal=TRUE) 

# Task 2

# generate normally distributed random numbers
# different means different standard deviations
x <- rnorm(10, mean=100, sd=5) 
y <- rnorm(20, mean=105, sd=10) 

# Welch's t-test
t.test(x, y, var.equal=FALSE) 

# Task 3

# set the work directory
setwd("c:\\EK\\work_data")

# read file cdc.csv
data <- read.csv("cdc.csv")

# separate the data for male heights and female heights
heights_m <- data[data$gender=="m","height"]
heights_f <- data[data$gender=="f","height"]

# equal variances ?
var_m <- var(heights_m, na.rm = T)
var_f <- var(heights_f, na.rm = T)
# The variances of heights_m and heights_f are different, 
# therefore use Welch test.
t.test(heights_m, heights_f, var.equal=FALSE) 

# T4

# ANOVA
offers <- sample(c("offer1", "offer2", "nopromo"), size=500, replace=T)

# Simulated 500 observations of purchase sizes on the 3 offer options
purchasesize <- ifelse(offers=="offer1", rnorm(500, mean=80, sd=30),
                       ifelse(offers=="offer2", rnorm(500, mean=85, sd=30),
                              rnorm(500, mean=40, sd=30)))

# create a data frame of offer option and purchase size
offertest <- data.frame(offer=as.factor(offers),
                        purchase_amt=purchasesize)

# display a summary of offertest where offer="offer1"
summary(offertest[offertest$offer=="offer1",])

# display a summary of offertest where offer="offer2"
summary(offertest[offertest$offer=="offer2",])

# display a summary of offertest where offer="nopromo"
summary(offertest[offertest$offer=="nopromo",])

# fit ANOVA model
model <- aov(purchase_amt ~ offers, data=offertest)
summary(model)

# Tukey's Honest Significant Difference (HSD) 
TukeyHSD(model)
