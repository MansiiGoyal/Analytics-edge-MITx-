# Setting the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/An analytical detective")

# Read the data
 mvt<-read.csv(paste("mvtWeek1.csv"))

# Understanding the data
 str(mvt)
 summary(mvt)
 
# Maximum value of variable ID
 max(mvt$ID)

# Learn more about LocationDescription Variable
 table(mvt$LocationDescription)

# Understanding dates in R
 mvt$Date[1]

# Convert characters into date object
 DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
 
 mvt$Month = months(DateConvert)
 
 mvt$Weekday = weekdays(DateConvert)
 
 mvt$Date = DateConvert
 
# Least motor vehicle thefts
 table(mvt$Month)

# Month with largest motor vehicle thefts and arrest made
 table(mvt$Arrest, mvt$Month)
  
# Visualizing crime trends
 hist(mvt$Date, breaks=100)
 
# Arrests changed over time with boxplot (In a boxplot, the bold horizontal line is the median value of the data, the box shows the range of values between the first quartile and third quartile, and the whiskers (the dotted lines extending outside the box) show the minimum and maximum values, excluding any outliers (which are plotted as circles). Outliers are defined by first computing the difference between the first and third quartile values, or the height of the box. This number is called the Inter-Quartile Range (IQR). Any point that is greater than the third quartile plus the IQR or less than the first quartile minus the IQR is considered an outlier.)
 boxplot(mvt$Date ~ mvt$Arrest)

# Top five locations where motor vehicle thefts occur
 sort(table(mvt$LocationDescription))

# Making subset out of top5 locations
 Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")

# Studying the structure of the subset dataset
 str(Top5)

# Popular locations
 Top5$LocationDescription = factor(Top5$LocationDescription)
 
