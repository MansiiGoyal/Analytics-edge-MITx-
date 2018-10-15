# Setting the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Predicting Parole Violators")

# Reading the datasets
 parole <- read.csv(paste("parole.csv"))

# Investigating the dataset
 str(parole)
 summary(parole)


 # Number of parolees in the dataset
 nrow(parole)
 
 
 # Number of parolees in the dataset which violated the terms
 table(parole$violator)


 # Variables that are unordered factors with at least 3 levels, so we need to convert them to factors for our prediction problem
 parole$state = as.factor(parole$state)
 parole$crime = as.factor(parole$crime)

 
 # Breakdown of the number of parolees with each level of the factor
 summary(parole$state)
 summary(parole$crime)

 
 # To ensure consistent training/testing set splits
 set.seed(144)
 
 # Loading the library caTools
 library(caTools)


 split = sample.split(parole$violator, SplitRatio = 0.7)
 train = subset(parole, split==TRUE)
 test = subset(parole, split==FALSE) 
 
 
 # Number of entries 
 nrow(test)
 nrow(train)


 # If you set a random seed, split, set the seed again to the same value, and then split again, you will get the same split. However, if you set the seed and then split twice, you will get different splits. If you set the seed to different values, you will get different splits.

 
 # You can also verify this by running the specified code in R. If you have training sets train1 and train2, the function sum(train1 != train2) will count the number of values in those two data frames that are different.

 
 # Building a logistic regression model on all independent variables
 model1 = glm(violator~., data = train, family = "binomial")
 summary(model1)


 # To make the predictions on the test set
 predictions = predict(model1, newdata=test, type="response")

 summary(model1)


 # To obtain the confusion matrix
 table(test$violator, as.numeric(predictions >= 0.5))

 
 # Accuracy
 174/(174+4)


 # Specificity
 (174+4)/(174+4+19+5)


 # Sensitivity
 4/(174+4+5+19) 


 # Specificity
 (174)/(174+19)


 table(test$violator)

 179/202

 # The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5
 # The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value

 
 # Calculation of AUC value
 library(ROCR)
 pred = prediction(predictions, test$violator) 
 as.numeric(performance(pred, "auc")@y.values)
 # AUC is the probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.

 
 # The dataset contains all individuals released from parole in 2004, either due to completing their parole term or violating the terms of their parole. However, it does not contain parolees who neither violated their parole nor completed their term in 2004, causing non-violators to be underrepresented. This is called "selection bias" or "selecting on the dependent variable," because only a subset of all relevant parolees were included in our analysis, based on our dependent variable in this analysis (parole violation).
 # As a result, a prospective dataset that tracks a cohort of parolees and observes the true outcome of each is more desirable. Unfortunately, such datasets are often more challenging to obtain (for instance, if a parolee had a 10-year term, it might require tracking that individual for 10 years before building the model). Such a prospective analysis would not be possible using the 2004 National Corrections Reporting Program dataset.
 
