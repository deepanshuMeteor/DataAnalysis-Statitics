#
# Smoking and Cancer
#
setwd("C:\\Users\\Admin\\Desktop\\R Code")
install.packages("tidyverse")
library(tidyverse)



# Q1
data<-read_csv("smoking_cancer.csv")
View(data)

# Q2
# H0: There is NO correlation between smoking and cancer

# Q3
# H1: There IS    correlation between smoking and cancer

# Q4
data
dim(data)
colnames(data)
str(data)
summary(data)
View(data)

# Q5
plot(data$smoking, data$cancer,
     main="No smoking area",
     xlab="smoking",
     ylab="cancer",
     xlim=range(50:160),
     ylim=range(50:160),
     col="#3F8C6F",
     pch=16 #values between 0 and 25 (8,...)
)

# Q6
cor(data$smoking, data$cancer)

# Q7
model <- lm(cancer ~ smoking, data)
model

# Q8
abline(model, col="red", lwd=3)

# Q9
summary(model)

# p-value is 0.000184
# which means our confidence level is above 95%,
# and we have enough evidence to reject H0
# However, do we have enough evidence to accept H1?
# Probably not!
#
# And this is where you (as data scientist) make a choice, what to do next:
# come to a [possibly wrong] conclusion that smoking is not linked to cancer,
# or request more data to be analysed
#
# However, even with more data scientists will not be able to prove H1
# They will never be able to prove H1 (simply because statistics don't work that way)
