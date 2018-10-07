# Setting working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Playing Moneyball in the NBA")

# Reading the csv file
 NBA = read.csv(paste("NBA_train.csv"))
 
# Studying the dataset
 str(NBA)

# Relationship between Wins and Playoffs
 table(NBA$W, NBA$Playoffs)

# Difference between the points scored
 NBA$PTSdiff = NBA$PTS-NBA$oppPTS
 
# To study the relationship between the difference between the points scored and wins
 plot(NBA$W, NBA$PTSdiff)
 plot(NBA$PTSdiff, NBA$W)

# Linear Regression
 WinsReg = lm(W~PTSdiff, data = NBA)
 summary(WinsReg)

# Linear Regression with PTS as the dependent variable 
 PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
 summary(PointsReg)

# Computing the residuals
 PointsReg$residuals
 
# Computing of sum o squared errors

 SSE = sum(PointsReg$residual^2)

# Computing of root mean squared errors
 RMSE = sqrt(SSE/nrow(NBA))

# Average number of points in a season
 mean(NBA$PTS)

# Removing insignificant variables
 summary(PointsReg)
 
# Removing the variable turnover
 PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
 summary(PointsReg3)

# Removing the variable blocks
 PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
 summary(PointsReg4)

# Computing of sum of squared errors
 SSE_4 = sum(PointsReg4$residual^2)
 SSE_4
 
# Computing the root mean squared errors
 RMSE_4 = sqrt(SSE_4/nrow(NBA))
 RMSE_4

# Read in test set 
 NBA_test <- read.csv(paste("NBA_test.csv"))
 str(NBA_test)

# Make test set predictions
 Pointpredictions = predict(PointsReg4, newdata = NBA_test)

# Calculating the sum of squared errors
  SSE = sum((Pointpredictions - NBA_test$PTS)^2)
 
# Total sum of squares
 SST = (sum(mean(NBA$PTS) - NBA_test$PTS)^2)

# Calculating R-squared value
 R2 = 1 - SSE/SST
 R2

# Calculating root mean squared error value
 RMSE = sqrt(SSE/nrow(NBA_test))
 RMSE

