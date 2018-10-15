
# Setting the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Detecting flu epidemic via Search Engine Query Data")

# Reading the training dataset
 flutrain <- read.csv(paste("FluTrain.csv"))
 str(flutrain)
 summary(flutrain)
 
# Week corresponding to the highest percentage of ILI-related physician visits
 which.max(flutrain$ILI)
 flutrain$Week[303]

# Histogram of the dependent variable, ILI
 hist(flutrain$ILI)
 
# ScatterPlot of the natural logarithm of ILI versus Queries
 plot(flutrain$Queries, log(flutrain$ILI))
 
# When handling a skewed dependent variable, it is often useful to predict the logarithm of the dependent variable instead of the dependent variable itself -- this prevents the small number of unusually large or small observations from having an undue influence on the sum of squared errors of predictive models.

# Linear regression model with log(ILI) as dependent variable
 FluTrend1 = lm(log(ILI)~Queries, data=flutrain)
 summary(FluTrend1)

# Correlation between log(ILI) and Queries
 Correlation = cor(flutrain$Queries, log(flutrain$ILI))
 Correlation

# Reading the test dataset
 flutest <- read.csv(paste("FluTest.csv"))
 str(flutest)
 Predict1 = predict(FluTrend1, newdata = flutest)
 
# Obtaining predictions of ILI value
 Predict1 = predict(FluTrend1, newdata = flutest)
 Predict1 = exp(predict(FluTrend1, newdata = flutest))

# To determine which element in the test set is for March 11, 2012
 which(flutest$Week == "2012-03-11 - 2012-03-17")
 Predict1[11]

# To determine root mean squared error
 SSE = sum((Predict1-flutest$ILI)^2)
 RMSE = sqrt(SSE/nrow(flutest))
 RMSE

# Installing "zoo" package
# the "zoo" package, which provides a number of helpful methods for time series models
 install.packages("zoo")
 library(zoo)
 ILILag2 = lag(zoo(flutrain$ILI), -2, na.pad=TRUE)
 # The value of -2 passed to lag means to return 2 observations before the current one; a positive value would have returned future observations
 summary(ILILag2)

# Scatterplot of Log of ILILag2 versus Log of ILI
 plot(log(flutrain$ILI), log(ILILag2))

# Training the linear regression model using ILILag2 dataset
 FluTrend2 = lm(log(ILI)~Queries + log(ILILag2), data=flutrain)
 summary(FluTrend2)

# Adding ILILag2 dataframe to the flutest dataset for prediction
 ILILag2 = lag(zoo(flutest$ILI), -2, na.pad=TRUE)
 flutest$ILILag2 = coredata(ILILag2)
 summary(flutest$ILILag2)

# Filling the missing values of ILILag2 in flutest dataset using ILILag2 values in flutrain dataset
 flutest$ILILag2[1] = flutrain$ILI[416]
 flutest$ILILag2[2] = flutrain$ILI[417]

# Predicting the values of ILI using FluTrend2
 Predict2 = exp(predict(FluTrend2, newdata = flutest))
 summary(Predict2)
 
# Calculating the root mean squared value
 SSE = sum((Predict2-flutest$ILI)^2)
 RMSE = sqrt(SSE/nrow(flutest))
 RMSE