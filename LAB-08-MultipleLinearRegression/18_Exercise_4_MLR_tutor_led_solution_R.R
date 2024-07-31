#
# Income: MLR
#
setwd("C:\\Users\\Admin\\Desktop\\R code")

# read the data and study them
income_data <- read.csv("Income_data_categ.csv")
View(income_data)

str(income_data)
summary(income_data)

# Residence is categorical with multiple levels
income_data$Residence <- as.factor(income_data$Residence)
str(income_data)

# build the model starting with all possible inputs
results <- lm(Income~Age + Education + Smoker + Residence, income_data)
summary(results)

# How is the categorical variable Residence represented ?
# What goes on behind the scenes ?

# 3 out of the 4 levels (values) of Residence are randomly selected
# For each of them a dummy variable is generated
R_North <- rep(0,length(income_data$Residence))
R_North[income_data$Residence == 'North'] <- 1
R_South <- rep(0,length(income_data$Residence))
R_South[income_data$Residence == 'South'] <- 1
R_West <- rep(0,length(income_data$Residence))
R_West[income_data$Residence == 'West'] <- 1

# join the dummy variables to the rest of the data
income_data <- cbind(income_data, R_North, R_South, R_West)

# build the model starting with all possible inputs
# but using the dummy variables for Residence
results_d <- lm(Income~Age + Education + Smoker + R_North + R_South + R_West, income_data)
summary(results_d)
# and we got the same result!
# R deals automatically with the creation of the dummy variables

# Let's look at the first solution again
summary(results)
# the p-value for Smoker is 0.17 > 0.05
# it is not statistically significant and we should remove it and rebuild the model
results <- lm(Income~Age + Education + Residence, income_data)
summary(results)
 
# Calculate the predicted income for 4 people - they are all 30 years old
# and have studied 15 years. However, the first lives in North, the second 
# in South, the third in East and the fourth in West
Age <- rep(30,4)
Education <- rep(15,4)
Residence <- c('North', 'South', 'East', 'West')
new_data <- data.frame(Age, Education, Residence)
output <- predict(results, new_data)
output

# How does the predict() function calculate the results?
# Calculate manually the prediction for the person living in West
W_age <- 30
W_education <- 15
W_North <- 0
W_South <- 0
W_West <- 1
W_income <- results$coefficients[1] + results$coefficients[2]*W_age + 
  results$coefficients[3]*W_education + results$coefficients[4]*W_North + 
  results$coefficients[5]*W_South + results$coefficients[6]*W_West
W_income

# What about the prediction for the person living in East
E_age <- 30
E_education <- 15
E_North <- 0
E_South <- 0
E_West <- 0
E_income <- results$coefficients[1] + results$coefficients[2]*E_age + 
  results$coefficients[3]*E_education + results$coefficients[4]*E_North + 
  results$coefficients[5]*E_South + results$coefficients[6]*E_West
E_income

# What income would the model predict for a newborn ?
# Let's try for all types of residence
Age <- rep(0,4)
Education <- rep(0,4)
Residence <- c('North', 'South', 'East', 'West')
new_data <- data.frame(Age, Education, Residence)
output <- predict(results, new_data)
output
# Surely this can't be right ! Check manually for any residence, e.g. North
BB_age <- 0
BB_education <- 0
BB_North <- 1
BB_South <- 0
BB_West <- 0
BB_income <- results$coefficients[1] + results$coefficients[2]*BB_age + 
  results$coefficients[3]*BB_education + results$coefficients[4]*BB_North + 
  results$coefficients[5]*BB_South + results$coefficients[6]*BB_West
BB_income

# Let's look at our model again
summary(results)

# The intercept and all coefficients are positive.
# The model will always predict positive values.
# We can "force" the model to have intercept = 0, i.e. to pass
# through the origin of the coordinate system using
results_0 <- lm(Income ~ 0 + Age + Education + Residence, income_data)
summary(results_0)
# However, this is not the right thing to do.
# The data we have used to train the model is for people of ages between
min(income_data$Age) # 18
max(income_data$Age) # 70
# and education years between
min(income_data$Education) # 10
max(income_data$Education) # 20
# Age = 0 and Education = 0 are far away from the data we used to 
# train the model.
# A predictive model can be relied upon only when the input is 
# within the range of the training data. It has not "learnt" what 
# happens outside this range.
