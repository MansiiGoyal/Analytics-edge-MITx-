# Setting the working directory
setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge/Election Forecasting - Predicting the winner before any votes are cast")


# Read in data
polling = read.csv("PollingData.csv")
str(polling)
# There are 50 states and three election years, so we would expect 150 observations,we actually only have 145 observations in the data frame.


table(polling$Year)
# In 2004-2005 all the 50 states have been reported but in 2012 only 45 of the 50 states have been reported.


summary(polling)
# The pollsters were so sure about the five missing states that they didn't perform any polls in the months leading up to the 2012 election since these states are particularly easy to predict.
# A lot of missing values


# We cannot delete the missing values in this case as this would lead to deletion of more than 50% entries
# Install and load mice package
# "mice" package use an algorithm in a such a way that use information from other variables in the dataset to predict and impute the missing values
install.packages("mice")
library(mice)


# To get a better understanding of the data
md.pattern(polling)


# Visual representation of missing values
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(polling, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


# Boxplot of missing values two at a time
marginplot(polling[c(3,4)])
marginplot(polling[c(5,6)])


# List of available imputation methods which can be used
methods(mice)


# Creating dataset for Multiple imputation
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)


# Multiple imputation
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)


# Updating the missing values
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)


# Subset data into training set and test set
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)


# Simple baseline model would ging to predict more common outcome which is Republican willwin the elections with accuracy of 53% on training set which makes it a weak model as even for a very landslide Democratic state where Democrat was polling by 15% - 20% ahead of Republican
# Smart Baseline model with one of the polls taken
table(Train$Republican)


# Sign function gives -1 for negative +1 for positive and 0 for neither
# -1 means the Democrat won the election
table(sign(Train$Rasmussen))


# the Republican would win, and the Republican actually did win.Again, there were those two inconclusive observations.There were four times where the smart baseline model predicted that the Republican would win, but actually the Democrat won the state.
table(Train$Republican, sign(Train$Rasmussen))


# Multicollinearity
cor(Train)
str(Train)


# Correlation between Independent variables
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])


# Logistic Regression Model
mod1 = glm(Republican~PropR, data=Train, family="binomial")
summary(mod1)
# AIC measuring the strength of the model 19.8, seems a very reasonable model


# Training set predictions, type="response" to get probabilities as predictions
pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >= 0.5)
# 4 mistakes which is sames as smart baseine model


# Two-variable model
# Pair of variables that has a relatively lower correlation with each other, because they might kind of work together to improve the prediction overall of the Republican outcome.
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")


# Training dataset predictions
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >= 0.5)
# One-less mistake not too impressive


summary(mod2)
# AIC has a smaller value which suggests a stronger model


# Smart baseline accuracy
table(Test$Republican, sign(Test$Rasmussen))


# Test set predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5)


# Analyze mistake
subset(Test, TestPrediction >= 0.5 & Republican == 0)
# The Rasmussen poll gave the Republican a two percentage point lead, SurveyUSA called a tie,DiffCount said there were six more polls that predicted Republican than Democrat,and two thirds of the polls predicted the Republican was going to win. But actually in this case, the Republican didn't win. Barack won in the state of Florida over Mett Romney

