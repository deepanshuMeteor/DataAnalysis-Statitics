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

# split the data into training and testing (hold out) set
set.seed(123)
rows_train <- sample(1:nrow(data_default), 0.8*nrow(data_default))
train = data_default[rows_train,]
test = data_default[-rows_train,]
dim(train)
dim(test)

#build the logistic regression model using the training set
model <- glm(DEF~student+balance+income, family=binomial(link="logit"), data=train)
summary(model)

#remove income - high p-value
model <- glm(DEF~student+balance, family=binomial(link="logit"), data=train)
summary(model)

# predict the probabilities for the testing (hold out) set
probabilities <- predict(model, test, type="response")

# Classification with threshold 0.5
# in-sample predictions
classes <- ifelse( probabilities > 0.5, 1, 0 )

# confusion matrix for the testing set
table(classes,test$DEF)

accuracy <- mean(classes == test$DEF)
accuracy

# True Positives (TP)
TP <- sum(classes == test$DEF & classes > 0)

# True Negatives (TN)
TN <- sum(classes == test$DEF & classes <= 0)

# False Positives (FP)
FP <- sum(classes != test$DEF & classes <= 0)

# False Negatives (FN)
FN <- sum(classes != test$DEF & classes > 0)

# True Positive Rate (TPR) = recall
TPR <- TP / (TP + FN)

# False Positive Rate (FPR)
FPR <- FP / (FP + TN)

library(caret)
confusionMatrix(as.factor(classes),test$DEF)

# ROC curve
# TPR versus FPR for different thresholds
library(ROCR)
ROCRpred <- prediction(classes, test$DEF)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# The Area Under the Curve (AUC) is an aggregate measure of the 
# classifier performance across all possible thresholds. 

# calculate AUC
library(pROC)
auc_value <- auc(test$DEF, classes)
auc_value
