data(state)
 statedata = data.frame(state.x77)
# load the dataset
 str(statedata)

# Setting the working directory 
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/StateData")

# Reading the dataset
 statedata <-read.csv(paste("statedata.csv"))
 
# Scatterplot of all of the states' centers with latitude on the y axis (the "y" variable in our dataset) and longitude on the x axis (the "x" variable in our dataset)
 plot(statedata$x, statedata$y)

# Region of the US (West, North Central, South, or Northeast) which has the highest average high school graduation rate of all the states in the region
 tapply(statedata$HS.Grad, statedata$state.region, mean)

# Boxplot of murder rate by region
 boxplot(statedata$Murder ~ statedata$state.region)
 NortheastData = subset(statedata, state.region == "Northeast")
 
# linear regression model to predict life expectancy by state using the state statistics
 model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Area + Murder + HS.Grad + Frost, data=statedata) 
 summary(model1)

# Calculating the sum of squared errorr value 
 SSE1 = sum(model1$residuals^2)
 SSE1

# Scatterplot of Life expectancy versus Income
 plot(statedata$Income, statedata$Life.Exp)
 
# Refined linear regression model judged on the basis of significance
 model2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad , data = statedata) #linear regression model with significant independent variable
 summary(model2)
# When we remove insignificant variables, the "Multiple R-squared" will always be worse, but only slightly worse. This is due to the nature of a linear regression model. It is always possible for the regression model to make a coefficient zero, which would be the same as removing the variable from the model. The fact that the coefficient is not zero in the intial model means it must be helping the R-squared value, even if it is only a very small improvement. So when we force the variable to be removed, it will decrease the R-squared a little bit. However, this small decrease is worth it to have a simpler model. On the contrary, when we remove insignificant variables, the "Adjusted R-squred" will frequently be better. This value accounts for the complexity of the model, and thus tends to increase as insignificant variables are removed, and decrease as insignificant variables are added.
 
# Calculation the value of sum of squared errors
 SSE2 = sum((model2$residuals)^2)
 SSE2

# Predicted highest life expectancy
 sort(predict(model2))
 which.max(statedata$Life.Exp)
 
# Vector of residuals (the difference between the predicted and actual values)
 sort(abs(model$residuals))
# Or
 sort(abs(statedata$Life.Exp - predict(model)))
 