
# Setting the working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Understanding Food Nutritional Education with Data")

# Reading the dataset
 USDA<-read.csv(paste("USDA.csv"))

# Studying the structure of dataset
 str(USDA)
 summary(USDA)

# Maximum level of Sodium
 which.max(USDA$Sodium)

# To find the 265th food
 USDA$Description[265]

# To create subset of food with high sodium content
 HighSodium = subset(USDA, USDA$Sodium>10000)
 str(HighSodium)

# Name of foods with high sodium content
 HighSodium$Description
 
# To find the sodium content of a particular food item
 match("CAVIAR", USDA$Description)
 USDA$Sodium[4154]

# 1,500 milligrams of sodium in 100 grams of caviar
 
# To find the standard deviation of the sodium content
 sd(USDA$Sodium, na.rm = TRUE)

# Summing up sd and mean of sodium content gives 1400mg which is less than 15oomg i.e. for caviar, so caviar is pretty rich in sodium as compared to most of the food items in our dataset
 
# Scatterplot of protein and fat content
 plot(USDA$Protein, USDA$TotalFat)

# Foods that are higher in protein typically lower in fat
 plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")

# Histogram of VitaminC
 hist(USDA$VitaminC, xlab = "Vitamin C(mg)", main = "Histogram of Vitamin C levels")

# Most of our foods more than 6k of them to be precise, have less than 200 milligrams of vitamin C.

# Zoomed version
 hist(USDA$VitaminC, xlab = "Vitamin C(mg)", main = "Histogram of Vitamin C levels", xlim = c(0,100))

# Zoomed the area but didn't break huge cells 
 hist(USDA$VitaminC, xlab = "Vitamin C(mg)", main = "Histogram of Vitamin C levels", xlim = c(0,100), breaks = 100)
 hist(USDA$VitaminC, xlab = "Vitamin C(mg)", main = "Histogram of Vitamin C levels", xlim = c(0,100), breaks = 2000)
# More than 4000 food items have less than 1mg VitaminC levels
 
# Boxplot of sugar
 boxplot(USDA$Sugar, main = "Boxplot of Sugar levels", ylab = "Sugar(g)")
# Avergae is critically low almost 5gm and there exist foods with sugar level upto 100gm (like candies)
 
# To add a new variable according to the sodium content in food
 USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)
 HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm =TRUE)
 str(HighSodium)

# To convert the datatype from logicals to numerics
 HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm =TRUE))
 str(HighSodium)
 
# Adding variable to the dataset
 USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm =TRUE))
 str(USDA)

 USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm =TRUE))
 USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm =TRUE))
 USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm =TRUE))
 str(USDA)

 table(USDA$HighSodium)

# Food containing high sodium and high fat
 table(USDA$HighSodium, USDA$HighFat)
 
# Variation of iron on highprotein content variable
 tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)

# Variation of maximum of Vitamin C on highprotein content variable
 tapply(USDA$VitaminC, USDA$HighProtein, max, na.rm=TRUE)

 tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)

# Food with high carbohydrates generally have more Vitamin C content
 tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)


