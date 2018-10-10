
# Setting the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Climate Change")

# Reading the csv file
 climate<-read.csv(paste("climate_change.csv"))

# Splitting the data on the basis of year variable
 train = subset(climate, Year <= 2006)
 test = subset(climate, Year > 2006)
 
# Creating the linear regression model
 climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
 summary(climatelm)
# Current scientific opinion is that nitrous oxide and CFC-11 are greenhouse gases: gases that are able to trap heat from the sun and contribute to the heating of the Earth. However, the regression coefficients of both the N2O and CFC-11 variables are negative, indicating that increasing atmospheric concentrations of either of these two compounds is associated with lower global temperatures.All of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set.
 
# Understanding the model
# To find correlations between variables
 cor(train)

# Simplifying the model
# Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, TSI, Aerosols and N2O as independent variables.
 LinReg = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
 summary(LinReg)

# In this particular problem many of the variables (CO2, CH4, N2O, CFC.11 and CFC.12) are highly correlated, since they are all driven by human industrial development.
# Using AIC ( Akaike information criterion )
 StepModel = step(climatelm)
 summary(climatelm)
# It is interesting to note that the step function does not address the collinearity of the variables, except that adding highly correlated variables will not improve the R2 significantly. The consequence of this is that the step function will not necessarily produce a very interpretable model - just a model that has balanced quality and simplicity for a particular weighting of quality and simplicity (AIC).
 
# To calculate the R-squared value
 tempPredict = predict(StepModel, newdata = test)
 SSE = sum((tempPredict-test$Temp)^2)
 SST = sum((mean(train$Temp)-test$Temp)^2)
 R2 = 1 - SSE/SST
 R2

