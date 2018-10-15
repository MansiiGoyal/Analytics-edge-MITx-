# Setting the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/RPredicting earnings from census data")

 
# Reading the dataset 
 census<-read.csv(paste("census.csv"))
 str(census)

# Building logistic regression model to predict whether an individual's earnings are above $50,000
 model1=glm(over50k~., data=census, family="binomial")
 summary(model1) 

# To load the caTools library
 library(caTools)

# Splitting the data into test set and training set
 set.seed(2000)
 spl=sample.split(census$over50k, SplitRatio=0.6)
 train=subset(census, spl=TRUE)
 test=subset(census, spl=FALSE)

# Logistic regression on training set
 model2=glm(over50k~., data=train, family="binomial") 

# Predicting on test set using model2
 predict2=predict(model2, newdata=test, type="response")

# Confusion matrix
 table(test$over50k, predict2>0.1)
        

# Calculating accuracy
 (15557+7346)/nrow(test)

# Understanding the model better
 summary(model2) 

# Confusion matrix on a threshold of 0.5
 table(test$over50k, predict2>=0.5) 
        
# Calculating accuracy
 (22623+4644)/nrow(test)


        
# Logistic regression model
 censusglm = glm( over50k ~ . , family="binomial", data = train)
 
# Prediction over the above logistic regression model
 predictTest = predict(censusglm, newdata = test, type = "response")

# Confusion matrix on a threshold of 0.5 
 table(test$over50k, predictTest >= 0.5)

  
# Calculating baseline accuracy
 table(train$over50k)
 table(test$over50k)
 9713/(9713+3078) #accuracy

 
# To load the library ROCR
 library(ROCR)

# ROCR(Receiver Operator Characteristic Curve) for selecting the threshold value for prediction
 ROCRpred=prediction(predict2, test$over50k) 
 
# AUC calculation 
 auc=as.numeric(performance(ROCRpred,"auc")@y.values)
 auc #are under the ROC curve

# To load the library rpart and rpart.plot
 library(rpart)
 library(rpart.plot)

# Building a classification tree 
 model3=rpart(over50k~., data=train, method="class")
 
 
# Plotting the claasssification model
 prp(model3) 

# Prediction on test set using above model (classification)
 predict3=predict(model3, newdata=test, type="class")
 summary(model3)


# Confusion matrix  
 table(test$over50k, predict3)
       
# Accuracy calculation
 (9243+1596)/nrow(test)

# To load the library ROCR
 library(ROCR)

# Predictions on test set using above model (regression)
 predict3=predict(model3, newdata=test)
 ROCRpred3=prediction(predict3, test$over50k)
 predict3.1 = predict3[,2]
 ROCRpred3=prediction(predict3.1, test$over50k)

# Calculation of AUC value
 auc=as.numeric(performance(ROCRpred3,"auc")@y.values)
 auc

# Before building a random forest model, we'll down-sample our training set. While some modern personal computers can build a random forest model on the entire training set, others might run out of memory when trying to train the model since random forests is much more computationally intensive than CART or Logistic Regression. For this reason, before continuing we will define a new training set to be used when building our random forest model, that contains 2000 randomly selected obervations from the original training set.
 set.seed(1)
 trainSmall = train[sample(nrow(train), 2000), ]

 
# To build a random forest model to predict "over50k", using the dataset "trainSmall" as the data used to build the model
 library(randomForest)

# In case of error
 set.seed(1)
# Model
 model4=randomForest(over50k~., data=trainSmall)

# Prediction on test set
 predict4=predict(model4, newdata=test)
 table(test$over50k, predict4) # Confusion matrix

# Accuracy calculation
 (9586+1093)/nrow(test)

# A chart that for each variable measures the number of times that variable was selected for splitting
 vu = varUsed(model4, count=TRUE) #a certain variable is selected for a split
 vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
 dotchart(vusorted$x, names(model4$forest$xlevels[vusorted$ix]))

 
# A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. Therefore, one way to measure the importance of a variable is to average the reduction in impurity, taken over all the times that variable is selected for splitting in all of the trees in the forest. 
 varImpPlot(model4)
 
# cp parameter for our CART model using k-fold cross validation
 cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
# Pick up cp(complexity parameter-measures trade-off between model complexity and accuracy on the training set) values

 
# Installing the packages caret and e1071
install.packages("caret")
 install.packages("e1071")

 
# To load the library caret and e1071
 library(caret)
 library(e1071)

 
# To find number of folds for cross-validation
 numFolds=trainControl(method="cv", number=10)
 cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
 train(over50k~., data=train, method="rpart", trControl=numFolds, tuneGrid=cartGrid)
# The final value used for the model was cp = 0.002
 
# Building classification tree using the above cp value
 model5=rpart(over50k~., data=train, method="class", cp=0.002)

 
# Prediction on test set using above model
 predict5=predict(model5, newdata=test, type="class")


# Studying the predict5
 summary(predict5)

 
# Confusion matrix after using cross-validation in CART model
 table(test$over50k, predict5)
 
# Accuracy Calculation
 (9178+1838)/nrow(test) 

 
# Building the CART tree
 prp(model5)
# Number of splits in the new model so chosen
 
