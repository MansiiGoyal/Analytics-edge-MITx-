# Setting the working directory
setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/The Supreme Court")


# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)


# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)


# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)


# CART model making a classification tree
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)
# Minbucket parameter is used to specify splits

# The prp function abbreviates the values of the independent variables.
prp(StevensTree)
# A CART tree is a series of decision rules which can easily be explained


# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")

# Confusion matrix for accuracy calculation
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)


# To construct ROC curve
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)


# Install randomForest package
install.packages("randomForest")
library(randomForest)


# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )


# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)


# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )


# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)


# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 


# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )


# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)


# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

