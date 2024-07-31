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

# split the data into training and testing (hold out) set
set.seed(123)
rows_train <- sample(1:nrow(income_data), 0.8*nrow(income_data))
train = income_data[rows_train,]
test = income_data[-rows_train,]
dim(train)
dim(test)

# build the model starting with all possible inputs
# use the training set
results <- lm(Income~Age + Education + Smoker + Residence, train)
summary(results)

# the p-value for Smoker is 0.551 > 0.05
# it is not statistically significant and we should remove it and rebuild the model
results <- lm(Income~Age + Education + Residence, train)
summary(results)

# Calculate the predicted income for the test set
predict_test <- predict(results, test)

# How accurate is the prediction ?
# Compare with the actuals in the test set
# Mean Absolute Percentage Error (MAPE)
MAPE <- mean(abs(test$Income-predict_test)/abs(test$Income))

