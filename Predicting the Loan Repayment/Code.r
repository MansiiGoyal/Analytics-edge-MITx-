# Setting the workig directory
setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/RPredicting the loan repayment")


# Reading the dataset
  loans<-read.csv(paste("loans.csv"))
  str(loans)
  summary(loans)

  
# Number of loans which were not paid  
 table(loans$not.fully.paid)

 
# Data frame with the observations missing at least one value 
 missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
 nrow(missing)
 table(missing$not.fully.paid)
 
 
# Load the library mice and VIM for imputing the data
 library(mice)
 md.pattern(loans)
 library(VIM)
 aggr_plot <- aggr(loans, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
 
 
# Set the seed of R's random number generator, which is useful for creating simulations or random objects that can be reproduced. seed - A number
  set.seed(144)

# Using imputation for missing values
 vars.for.imputation=setdiff(names(loans),"not.fully.paid") #
 imputed=complete(mice(loans[vars.for.imputation]))

# Reading the imputed dataset
 loans[vars.for.imputation]=imputed
 loansimputed<-read.csv(paste("loans_imputed.csv"))

# Setting the seed for splitting the data
 set.seed(144)

# Splitting the data into test and training set
 library(caTools)
 split=sample.split(loansimputed$not.fully.paid, SplitRatio=0.7)
 train=subset(loansimputed, split=TRUE)
 test=subset(loansimputed, split=FALSE)

# Logistic regression model enumerating all the independent variables
 model1=glm(not.fully.paid~.,data=train, family=binomial)
 summary(model1)

# To predict the dependent variable not.fully.paid using all the significant and independent variables
 predict1=predict(model1, type="response", newdata=test)
 
# Predicted probability
 predicted.risk=predict1
 
# Confusion matrix and accuracy 
 table(test$not.fully.paid,predicted.risk>0.5)

 
# Load the package ROCR to compute the test set AUC
 library(ROCR)
 ROCRpredTest=prediction(predicted.risk,test$not.fully.paid)
 auc=as.numeric(performance(ROCRpredTest,"auc")@y.values)
 auc
# Model has poor accuracy at the threshold 0.5
# Area under the curve is a curve generated using ROCR (Receiver operator characteristic curve) package which is used for the threshold in the logistic regression model

# Bivariate logistic regression model 
 model2=glm(not.fully.paid~int.rate, data=train) #LendingClub.com assigns the interest rate to a loan based on their estimate of that loan's risk.The variable int.rate is highly significant in the bivariate model, but it is not significant at the 0.05 level in the model trained with all the independent variables.
 summary(model2)
# Decreased significance between a bivariate and multivariate model is typically due to correlation. 
 
# Interest rate is moderately well correlated with a borrower's credit score 
 cor(train$int.rate, train$fico)

# Test set predictions on bivariate model
 predict2=predict(model2, type="response", newdata=test)
 summary(predict2)
# The maximum predicted probability of the loan not being paid back is 0.4266, which means no loans would be flagged at a logistic regression cutoff of 0.5 
 
# Test set AUC for the bivariate model   
 ROCRpredTest=prediction(predict2,test$not.fully.paid)
 auc=as.numeric(performance(ROCRpredTest,"auc")@y.values)
 auc

# Finding the profit in compounding interest through this formula c * exp(rt)
 10*exp(0.06*3)
 
# To create the profit variable, we first assign to the profit for a fully paid loan, exp(rt)-1, to every observation, and we then replace this value with -1 in the cases where the loan was not paid in full. All the loans in our dataset are 3-year loans, meaning t=3 in our calculations.
 test$profit = exp(test$int.rate*3) - 1
 test$profit[test$not.fully.paid == 1] = -1
 summary(test)
 
# Building a separate dataframe
 highInterest = subset(test, int.rate >= 0.15)
 summary(highInterest$profit)

# To obtain the breakdown of whether the loans were paid back in full
 table(highInterest$not.fully.paid)


# Prediction risks on test set using bivariate model
 test$predicted.risk=predict(model2, type="response", newdata=test)


# To determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks in increasing order and selecting the 100th element of this sorted list
 highInterest = subset(test, int.rate >= 0.15)
 cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

# Separate dataframe created using the cutoff
 selectedLoans = subset(highInterest, predicted.risk <= cutoff) #An Investment Strategy Based on Risk
 nrow(selectedLoans)
 sum(selectedLoans$profit)

# Breakdown whether each loan was fully paid or not
 table(selectedLoans$not.fully.paid)

