# DECISION TREE
# golf play

library("rpart") # load libraries
library("rpart.plot")

# Read the data
golf_play <- read.csv("DTdata.csv")
golf_play
summary(golf_play)

# build the decision tree
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class",
             data=golf_play,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)

?rpart.plot
rpart.plot(fit, type=4, extra=1)
rpart.plot(fit, type=4, extra=1)
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE,
           varlen=0, faclen=0)

newdata <- data.frame(Outlook="rainy", Temperature="mild",
                      Humidity="high", Wind=FALSE)
predict(fit,newdata,type="prob")
predict(fit,newdata,type="class")
