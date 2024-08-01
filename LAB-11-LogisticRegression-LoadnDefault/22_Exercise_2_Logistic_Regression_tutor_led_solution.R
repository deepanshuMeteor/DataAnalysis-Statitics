# Logistic regression

# probability to default on a loan 
# depending on whether a student or not, bank balance and income

# set the work directory
setwd('C:\\EK\\work_data')

# get the data and view them
data_default <- ISLR::Default
summary(data_default)

# output 0-1 factor to be able to generate ROC curve later
DEF = rep(0,length(data_default$default))
DEF[data_default$default == 'Yes'] <- 1
data_default$default <- NULL
data_default <- cbind(DEF,data_default)
data_default$DEF <- as.factor(data_default$DEF)

#logistic regression model
model <- glm(DEF~student+balance+income, family=binomial(link="logit"), data=data_default)
summary(model)

#remove income - high p-value
model <- glm(DEF~student+balance, family=binomial(link="logit"), data=data_default)
summary(model)

# predict for new data
student <- c('Yes', 'No')
balance <- c(1500, 1500)
new_data <- data.frame(student, balance)
predict(model, new_data, type="response")

# probabilities for the data (in-sample)
probabilities <- predict(model,type='response')

# Classification with threshold 0.5
# in-sample predictions
classes <- ifelse( probabilities > 0.5, 1, 0 )
classes

# confusion matrix
table(classes,data_default$DEF)

accuracy <- mean(classes == data_default$DEF)
accuracy

# True Positives (TP)
TP <- sum(classes == data_default$DEF & classes > 0)

# True Negatives (TN)
TN <- sum(classes == data_default$DEF & classes <= 0)

# False Positives (FP)
FP <- sum(classes != data_default$DEF & classes <= 0)

# False Negatives (FN)
FN <- sum(classes != data_default$DEF & classes > 0)

# True Positive Rate (TPR) = recall
TPR <- TP / (TP + FN)

# False Positive Rate (FPR)
FPR <- FP / (FP + TN)

library(caret)
confusionMatrix(as.factor(classes),data_default$DEF)

# ROC curve
# TPR versus FPR for different thresholds
library(ROCR)
ROCRpred <- prediction(classes, data_default$DEF)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# The Area Under the Curve (AUC) is an aggregate measure of the 
# classifier performance across all possible thresholds. 

# calculate AUC
library(pROC)
auc_value <- auc(data_default$DEF, classes)
auc_value