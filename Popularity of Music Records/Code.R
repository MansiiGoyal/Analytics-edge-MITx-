# Setting the working directory
setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Popularity of Music Records")


# Reading the dataset
songs <- read.csv(paste("songs.csv"))


# Studying the data
str(songs)
summary(songs)


# Songs that the dataset include for which the artist name is "Michael Jackson"
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson)


# Songs by Michael Jackson made it to the Top 10
MichaelJackson[c("songtitle", "Top10")]


# Confidence interval for songs
table(songs$timesignature)


# Songs with the highest tempo
which.max(songs$Tempo)
songs$songtitle[6206]


# To predict whether or not a song will make it to the Top 10


# Split the data into the training set and the test set
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
str(SongsTrain)


# Building logistic regression enumeration all the independent variables
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)


# Building logistic regression model using only numerical attributes of songs
# Vector of variable names called nonvars - these are the variables that we won't use in our model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# Creating a logistic regression model without the independent variable "loudness"
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# Creating a logistic regression model withut the independent variable "energy"
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Validating the model
testPredict = predict(SongsLog3, newdata=SongsTest, type="response")

# Confusion matrix with a threshold of 0.45
table(SongsTest$Top10, testPredict >= 0.45)
(309+19)/(309+5+40+19)

# Baseline accuracy
table(SongsTest$Top10)
314/(314+59)

# Sensitivity of Model 3 on test set
19/(19+40)

#Specificity of Model 3 on test set
309/(309+5)

# Model 3 favors specificity over sensitivity. Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely. So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does predict to be Top 10 hits.
























