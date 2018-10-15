# Setting the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/RLetter Recognition(incomp)")

 
# Reading the dataset
 letters<-read.csv(paste("letters_ABPR.csv"))
 str(letters)

 
# A new variable isB in the dataframe, which takes the value "TRUE" if the observation corresponds to the letter B, and "FALSE" if it does not 
 letters$isB = as.factor(letters$letter == "B") 

 
# Loadin the catools library 
 library(caTools)

 
# Splitting the data into training and test set  
 set.seed(1000)
 spl = sample.split(letters$isB, SplitRatio = 0.5)
 train = subset(letters, spl == TRUE)
 test = subset(letters, spl == FALSE)
 

# To compute the accuracy of the baseline method on the test set, we first need to see which outcome value is more frequent in the training set 
 table(train$isB)
 table(test$isB)
 1175/(1175+383) = 0.754172


# Load the library rpart 
 library("rpart")

 
# A classification tree to predict if B is present or not
 CARTb = rpart(isB ~ . - letter, data=train, method="class")
 predictions = predict(CARTb, newdata=test, type="class")

 
# Confusion matrix
 table(test$isB, predictions)

 
# Accuracy of the CART model  
 (1118+340)/nrow(test)

 
# Splitting the dataset on the basis of the variable letter 
letters$letter = as.factor( letters$letter )
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE) 
 

# To compute the accuracy of the baseline method on the test set
table(train2$letter)
table(test2$letter)
401/nrow(test)


# Setting the seed value
 set.seed(1000)

 
# Installing the packages
 install.packages("randomForest")


# To load the randomforest library
 library("randomForest")


# Random forest model to predict whetehr the letter is 'B' or not
 RFb = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=train)

 
# Making predictions on test set using the above classification tree
 predictions = predict(RFb, newdata=test)
 table(test$isB, predictions)

 
# Accuracy calculation 
 (1165+374)/nrow(test)


# A classification tree to predict the dependent variable letter
 modelCART=rpart(letter~xbox+ybox+width+height+width+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor, data=train2, method="class")
 predictCART=predict(modelCART, newdata=test2, type="class")


# Confusion matrix  
 table(test$letter, predictCART)


# Accuracy of the model
 (348+318+363+340)/nrow(test2) 

 
# Confusion matrix 
 table(test2$letter, predictCART)
 
   
# To build classification tree using all the variables in training set  
 modelrandomForest=randomForest(letter~xbox+ybox+width+height+width+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor, data=train2, method="class")
 predictrandomForest=predict(modelrandomForest, newdata=test2,type="class")

 
# Confusion matrix
 table(test2$letter, predictrandomForest)

 
# Accuracy of random forest model 
 (393+373+396+364)/nrow(test2)

  
# Creating classification tree enumeration all the independent variables
 RFletter = randomForest(letter ~ . - isB, data=train2)
 predictrandomForest=predict(RFletter, newdata=test2,type="class")
 
 
# Confusion matrix
 table(test2$letter, predictrandomForest)

 
# Building regression tree and respective confusion matrix 
 predictLetter = predict(RFletter, newdata=test2)
 table(test2$letter, predictLetter)

