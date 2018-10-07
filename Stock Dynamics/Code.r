# Setting working directory
 setwd("C:/Users/Mansi/Documents/Assignments/Analytics Edge/Analytics edge (GitHub)/Stock Dynamics")

# Reading the datasets for the companies to study their stock price
 IBM<-read.csv(paste("IBMStock.csv"))
 GE<-read.csv(paste("GEStock.csv"))
 CocaCola<-read.csv(paste("CocaColaStock.csv"))
 Boeing<-read.csv(paste("BoeingStock.csv"))
 PnG<-read.csv(paste("ProcterGambleStock.csv"))

# Reading the structure of one of the dataset
 str(IBM)

# Date variable is stored as factor variable 
 IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
 GE$Date = as.Date(GE$Date, "%m/%d/%y")
 CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
 Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
 PnG$Date = as.Date(PnG$Date, "%m/%d/%y")

# Summarising statistics
 summary(IBM)
 
# Minimum stock price of General Electric (GE) over this time period
 summary(GE)
   
# Maximum stock price of Coca-Cola over this time period
 summary(CocaCola)
 
# Median stock price of Boeing over this time period
 summary(Boeing)
  
# Standard deviation of the stock price of Procter & Gamble over this time period
 sd(PnG$StockPrice)

# Stock prices to visualize trends in stock prices during this time period
 plot(CocaCola$Date, CocaCola$StockPrice, type="l")

# Line for Procter and Gamble
 lines(PnG$Date, PnG$StockPrice, lty=2)
 plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
 lines(PnG$Date, PnG$StockPrice, col="blue")
 abline(v=as.Date(c("2000-03-01")), lwd=2)

# Stock prices changed from 1995-2005 for all five companies
 plot(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
 plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
 plot(PnG$Date[301:432], PnG$StockPrice[301:432], type="l", col="green", ylim=c(0,210))
 plot(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="purple", ylim=c(0,210))
 plot(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="orange", ylim=c(0,210))
 abline(v=as.Date(c("1997-11-01")), lwd=2)
 
# Stocks tend to be higher or lower during certain months
 months(IBM$Date)
  
# Average stock price for IBM
 mean(IBM$StockPrice)

# Monthly Averages
 tapply(IBM$StockPrice, months(IBM$Date), mean)

 
