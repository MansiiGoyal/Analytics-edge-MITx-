
# Setting up the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Internet Privacy Policy")

# Reading the csv file
 poll<-read.csv(paste("AnonymityPoll.csv"))
 
# Understanding the structure of dataset
 str(poll)

# Number of people with smartphones
 table(poll$Smartphone)
 summary(poll$Smartphone)

# Census region of a particular state by looking at the region associated with all its interviewees
 table(poll$State, poll$Region)

 table(poll$Internet.Use, poll$Smartphone)
 
# Interviewees who reported Internet use or who reported smartphone use
 limited = subset(poll, Internet.Use == 1 | Smartphone == 1)

# Summarizing Opinions about Internet Privacy
# Number of missing values for each variable
 summary(limited)
 
# Average number of pieces of personal information on the Internet
 mean(limited$Info.On.Internet)
 table(limited$Info.On.Internet)

# Interviewees worry about their info
 table(limited$Worry.About.Info)
 
# Interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet
 table(limited$Anonymity.Possible)

# Interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet
 table(limited$Tried.Masking.Identity)

# Interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective
 table(limited$Privacy.Laws.Effective)

# Relating Demographics to Polling Results
# To investigate the relationship between the characteristics Age and Smartphone and outcome variables Info.On.Internet and Tried.Masking.Identity
# Histogram of the age of interviewees
 hist(limited$Age)

# Relationship between Age and Info.On.Internet variables
 plot(limited$Age, limited$Info.On.Internet)

# To compare the number of observations with different values of Age and Info.On.Internet
 table(limited$Age, limited$Info.On.Internet)
    
 plot(jitter(limited$Age), jitter(limited$Info.On.Internet)) #jitter function randomly adds or subtracts a small value from each number, and two runs will yield different results

# To obtain the summary of the Info.On.Internet value, broken down by whether an interviewee is a smartphone user
 tapply(limited$Info.On.Internet, limited$Smartphone, summary)

# To obtain the summary of the Tried.Masking.Identity variable for smartphone and non-smartphone users
 tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
 tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)

 
