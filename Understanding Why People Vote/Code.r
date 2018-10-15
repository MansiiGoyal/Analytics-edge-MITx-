# Setting the working directory
setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/RUnderstanding why people vote")


# Read in data
gerber = read.csv("gerber.csv")
str(gerber)
# Understanding the data better


# Proportion of people in this dataset voted in this election
table(gerber$voting)
108696/(108696+235388)


# The four "treatment groups" had the largest percentage of people who actually voted (voting = 1)
tapply(gerber$voting, gerber$civicduty, mean)

tapply(gerber$voting, gerber$hawthorne, mean)

tapply(gerber$voting, gerber$self, mean)

tapply(gerber$voting, gerber$neighbors, mean)


# Logistic regression model for voting using the four treatment group variables as the independent variables (civicduty, hawthorne, self, and neighbors) 
 model1=glm(voting~hawthorne+civicduty+neighbors+self, data=gerber,family="binomial")
 summary(model1) 
# Logistic regression model summary to know the significance of each variable

# Prediction and accuracy of logistic regression model
 predictLog = predict(model1, type="response")
 table(gerber$voting, predictLog > 0.3)
 (134513+51966)/(134513+100875+56730+51966)
 

# Confusion matrix
 table(gerber$voting, predictLog > 0.5) 
# Trying different thresholds to find more accurate model   
 (235388+0)/(235388+108696)
 
 
# To find auc value
 library(ROCR)
 ROCRpred = prediction(predictLog, gerber$voting)
 as.numeric(performance(ROCRpred, "auc")@y.values)

 
# Even though all of our variables are significant, our model does not improve over the baseline model of just predicting that someone will not vote, and the AUC is low. So while the treatment groups do make a difference, this is a weak predictive model.

 
# Building a CART model 
 library(rpart)
 library(rpart.plot)
 CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
 
 
# Plot the CART regression tree
 prp(CARTmodel)  
# No variables are used (the tree is only a root node) - none of the variables make a big enough effect to be split on. 


# To force the complete tree to be built   
 CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

 
# Plot the regression tree 
 prp(CARTmodel2) 
 
 
# A new tree that includes the "sex" variable 
 CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=gerber, cp=0.0) 
 
 
# Plot the regression tree 
 prp(CARTmodel3) 

 
# Making different trees to understand the data better 
 CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)  
 prp(CARTmodel4)
 
 
# Making different trees to understand the data better
 CARTmodel5 = rpart(voting ~ control+sex, data=gerber, cp=0.0) 
 prp(CARTmodel5, digits=6)
 
 
# Using logistic regression to interpret the coefficients of the variable sex 
 model2=glm(voting~control+sex, data=gerber,family="binomial")
 summary(model2)
# Coefficient of sex variable is negative, reflecting that women are less likely to vote
 

# The regression tree calculated the percentage voting exactly for every one of the four possibilities (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control). Logistic regression has attempted to do the same, although it wasn't able to do as well because it can't consider exactly the joint possibility of being a women and in the control group.  
# Dataframe containing all possible values of sex and control 
 Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))

 
# Prediction on new dataframe using model2  
predict(model2, newdata=Possibilities, type="response")
 

# Possibilities in the order they are stated above ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) )
 model3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")#intruducing a new variable combining "control" and "sex" variables
 summary(model3) # Understanding the model better
 predict(model3, newdata=Possibilities, type="response")

 
# Approximately zero difference between the CART prediction and logistic regression.
 
