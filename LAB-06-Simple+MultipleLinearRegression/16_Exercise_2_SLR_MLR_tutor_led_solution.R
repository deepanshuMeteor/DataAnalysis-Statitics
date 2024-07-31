#
# Clocks: SLR
#
setwd("C:\\Users\\Admin\\Desktop\\R code")
install.packages("tidyverse")
library(tidyverse)

data<-read_csv("clocks.csv")
str(data)



# Q1
# first of all, find correlation coeffieients
cor(data$Price, data$Age)     # 0.73
cor(data$Price, data$Bidders) # 0.39
# one of them is strong, another one is weak - 
# but at least we've got two variables to play with



# Q2
plot(data$Age, data$Price,
     main="How price depends on age",
     xlab="Age",
     ylab="Price",
     col="blue")
model <- lm(Price ~ Age, data)
abline(model, col="red", lwd=3)
model
summary(model)
#
# Price = 10.48 * Age - 191.66
#
# Age variable is statistically significant (***)
# Explanatory power of the model is 53%
#
input  <- 120
output <- model$coefficients[2] * input + model$coefficients[1]
output
#
# the predicted price is 1066
#
# the automated way
Age <- 120
inp <- data.frame(Age)
out <- predict(model,inp)
out
#
# the predicted price is again 1066
#

abline(v=input, col="green")
abline(h=output, col="green")



# Q3
plot(data$Bidders, data$Price,
     main="How price depends on bidders",
     xlab="Bidders",
     ylab="Price",
     col="blue")
model <- lm(Price ~ Bidders, data)
abline(model, col="red", lwd=3)
model
summary(model)
#
# Price = 54.64 * Bidders + 806.40
#
# Bidders variable is kinda marginally significant (*)
# Explanatory power of the model is 16% - not very good at all
# Notice that data points on this plot are more spread out (comparing to the previous graph)
#
input  <- 10
output <- model$coefficients[2] * input + model$coefficients[1]
output
#
# the predicted price is 1353
#
# the automated way
Bidders <- 10
inp <- data.frame(Bidders)
out <- predict(model,inp)
out
# the predicted price is again 1353
#

abline(v=input, col="green")
abline(h=output, col="green")

# Q4
#
# Now, build MLR
# Will try to predict price based on Age AND Bidders
#
model <- lm(Price ~ Age + Bidders, data)
model
summary(model)
#
# p-values for the model are 1.67e-08 and ... (which is well below 0.05) - great!
# We can see that both variables are statistically significant (***)
#
# And explanatory power of the model is 89% - 
# which is much better than 53+16! (although can never take 53+16, it does not make any sense)
#
# Price = 12.74*Age + 85.82*Bidders - 1336.72
#
input_age <- 120
input_bid <- 10
output <- model$coefficients[2] * input_age +
        model$coefficients[3] * input_bid +
        model$coefficients[1]
output
#
# the predicted price is 1050
#
# the automated way
Age <- 120
Bidders <- 10
inp <- data.frame(Age,Bidders)
out <- predict(model,inp)
out
# the predicted price is again 1050
#