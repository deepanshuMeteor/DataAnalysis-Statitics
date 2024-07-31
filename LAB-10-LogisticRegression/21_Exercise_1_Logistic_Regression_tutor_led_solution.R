# Logistic regression

# probability to pass or fail an exam depending on hours of study
# data source: Wikipedia
# https://en.wikipedia.org/wiki/Logistic_regression

# set the work directory
setwd('C:\\EK\\work_data')

# read the data and view the resulting data frame
data_exams <- read.csv("PassFail.csv")
View(data_exams)

# data types
str(data_exams)
data_exams$Pass <- as.factor(data_exams$Pass)
str(data_exams)

# build the logistic regression model and examine the results
exam_results <- glm(Pass~Hours, data=data_exams, family=binomial(link="logit"))
summary(exam_results)

# probabilities to pass the exam (in-sample)
probabilities <- predict(exam_results,type='response')
probabilities
plot(data_exams$Hours,probabilities,type='b')

# what is the probability of passing with 2 hours of study ?
Hours <- 2
prob <- predict(exam_results, data.frame(Hours), type='response')
prob
# 0.34 - not too good - must study some more

# One student studies 2.5 hours and another 3.5 hours
Hours <- c(2.5,3.5)
prob <- predict(exam_results, data.frame(Hours), type='response')
prob
# probability to pass is 0.52 for the first and 0.83 for the second

# What if we want to split the students into two groups: 
# "likely to pass" and "likely to fail"
# We need to decide what is high enough probability to pass,
# so any student with a probability to pass that is higher than that threshold
# will be assigned to the "likely to pass" group, otherwise - to the
# "likely to fail" group.

# Usually the threshold that is used is 0.5
# The in-sample predictions
classes <- ifelse( probabilities > 0.5, 1, 0 )
classes

# Out of sample predictions
Hours <- c(2.5,3.5)
prob <- predict(exam_results, data.frame(Hours), type='response')
prob
cls <- ifelse( prob > 0.5, 1, 0 )
cls
# Note that if the threshold is higher, e.g. 0.6, 
# the first student will move to the group of
# "likely to fail"

# How correct were we ?

# confusion matrix (contingency matrix)
table(classes,data_exams$Pass)

accuracy <- mean(classes == data_exams$Pass)

# True Positives (TP)
TP <- sum(classes == data_exams$Pass & classes > 0)

# True Negatives (TN)
TN <- sum(classes == data_exams$Pass & classes <= 0)

# False Positives (FP)
FP <- sum(classes != data_exams$Pass & classes <= 0)

# False Negatives (FN)
FN <- sum(classes != data_exams$Pass & classes > 0)

# True Positive Rate (TPR) = recall
TPR <- TP / (TP + FN)

# False Positive Rate (FPR)
FPR <- FP / (FP + TN)

library(caret)
confusionMatrix(as.factor(classes),data_exams$Pass)

# ROC curve
# TPR versus FPR for different thresholds

library(ROCR)
ROCRpred <- prediction(classes, data_exams$Pass)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# The Area Under the Curve (AUC) is an aggregate measure of the 
# classifier performance across all possible thresholds.

# calculate AUC
library(pROC)
auc_value <- auc(data_exams$Pass, classes)
auc_value
