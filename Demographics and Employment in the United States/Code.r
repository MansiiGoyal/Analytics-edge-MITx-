
# Setting working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Demographics and Employment in the United States")

# Reading the CPS dataset
 CPS <-read.csv(paste("CPSData.csv"))

# Understanding the dataset
 summary(CPS)

# Most common industry of employment
 table(CPS$Industry) 
 
# Number of Interviwees region-wise
 sort(table(CPS$Region))

# Evaluating Missing Values 
 table(CPS$Region, is.na(CPS$Married))

# All interviewees living in a non-metropolitan area
 table(CPS$State, is.na(CPS$MetroAreaCode))
                    
# Proportion of interviewees from each region not living in a metropolitan area
 sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

 MetroAreaMap<-read.csv(paste("MetroAreaCodes.csv"))
 CountryMap<-read.csv(paste("CountryCodes.csv"))
 
# Integrating Metropolitan Area Data
 CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
 summary(CPS)
 
# Metropolitan areas has the largest number of interviewees
 table(CPS$MetroArea)

# Interviewees from each metropolitan area who have not received a high school diploma 

 sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))
  
# Integrating Country of Birth Data
 CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
 summary(CPS)

