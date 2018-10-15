# Setting the working directory
setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/RDetecting the Vandalism on Wikipedia")


# Reading the dataset
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)


# To convert Vandal to a factor
wiki$Vandal = as.factor(wiki$Vandal)

# To see how many cases of Vandalism are there
table(wiki$Vandal)

# Loading the libraries for text analytics
library(tm)
library(SnowballC)

# To create the corpus for the Added column
corpusAdded = VCorpus(VectorSource(wiki$Added))


# Preprocessing
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)


# To build document term matrix
dtmAdded = DocumentTermMatrix(corpusAdded)

# Sparse matrix of the sparse terms by keeping only terms that appear in 0.3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)

# To convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))


# To repeat the steps for the Removed column
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
corpusRemoved = VCorpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
length(wordsRemoved)
ncol(wordsRemoved) #To cout the number of columns


# Combine the two dataframes
 wikiWords = cbind(wordsAdded, wordsRemoved) 
 wikiWords$Vandal = wiki$Vandal
 
# Load caTools library
 library(caTools)

# Setting split for dividing the data
 set.seed(123)
 spl=sample.split(wikiWords$Vandal, SplitRatio=0.7)

# Dividing the data into test and training datasets
 wikitrain=subset(wikiWords, spl==TRUE)
 wikitest=subset(wikiWords, spl==FALSE)

# To compute baseline accuracy on test set 
 table(wikitest$Vandal)
 618/nrow(wikitest)

# Load the library rpart 
 library(rpart)

# Load the library rpart.plot
 library(rpart.plot)

# Building the classification tree over training set
 wikiCART=rpart(Vandal~., data=wikitrain, method="class")
 prp(wikiCART)

# Prediction over test set
testpredictCART=predict(wikiCART, newdata=wikitest, type="class")
 
# Confusion matrix
table(wikitest$Vandal, predictCART)
   

# True positive ratio   
 614/nrow(wikitest)


 wikiWords2 = wikiWords
 
 
# The grepl function returns TRUE if a string is found in another string
 wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0) 
# If "http" was in Added

  
# Splitting the data into test set and training set 
 wikitrain2=subset(wikiWords2, spl==TRUE)
 wikitest2=subset(wikiWords2, spl==FALSE)

 
# Building classification tree on new training set 
 wiki2CART=rpart(Vandal~.,data=wikitrain2, method="class")

 
# Prediction over new test set
 predict2CART=predict(wiki2CART, newdata=wikitest2, type="class")
 
# Confusion matrix 
 table(wikitest2$Vandal, predict2CART)
 prp(wiki2CART)   

   
# Accuracy calculation 
 605/nrow(wikitest2)

 
# The number of words added and removed is predictive, perhaps more so than the actual words themselves. We already have a word count available in the form of the document-term matrices (DTMs)
 wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
 wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
 
# Average number of words added
 mean(wikiWords2$NumWordsAdded)

 
# Splitting the data into training and test dataset 
 wikitrain3=subset(wikiWords2, spl==TRUE)
 wikitest3=subset(wikiWords2, spl==FALSE)

 
# Building classification tree
 wiki3CART=rpart(Vandal~.,data=wikitrain3, method="class")

 
# Predictions and confusion matrix
 predict3CART=predict(wiki3CART, newdata=wikitest3, type="class")
 table(wikitest3$Vandal, predict3CART)
 prp(wiki3CART)

 
# Accuracy calculation
 (514+248)/nrow(wikitest3)

 
# Using two pieces of "metadata" (data about data) that we haven't yet used
 wikiWords3 = wikiWords2
 wikiWords3$Minor = wiki$Minor
 wikiWords3$Loggedin = wiki$Loggedin

 
# Splitting the dataset into training and test dataset 
 wikitrain4=subset(wikiWords3, spl==TRUE)
 wikitest4=subset(wikiWords3, spl==FALSE)


# Another classification model  
 wiki4CART=rpart(Vandal~.,data=wikitrain4, method="class")

 predict4CART=predict(wiki4CART, newdata=wikitest4, type="class")

 table(wikitest4$Vandal, predict4CART)

 
# Plotting the tree
 prp(wiki4CART)
# By adding new independent variables, we were able to significantly improve our accuracy without making the model more complicated!
 
 
