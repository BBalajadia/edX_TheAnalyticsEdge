###############################################
#     Bryan R. Balajadia                      #
#     MITx:15.071x                            #
#     Unit 1: An Introduction to Analytics    #
#     Assignment 1.2 -  Stock Dynamics        #
###############################################

# Set working directory
setwd("C:/Users/136241/Desktop/MITx_Analytics_Edge/A12_Stock_Dynamics")

# Read data into R
IBM <- read.csv("IBMStock.csv", sep =",", header = TRUE)
GE <- read.csv("GEStock.csv", sep =",", header = TRUE)
CocaCola <- read.csv("CocaColaStock.csv", sep =",", header = TRUE)
ProcterGamble <- read.csv("ProcterGambleStock.csv", sep =",", header = TRUE)
Boeing <- read.csv("BoeingStock.csv", sep =",", header = TRUE)


# Convert dates
IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date <- as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")

#------------------------------------------------------------
# Problem 1.1 - Summary Statistics 
# Our five datasets all have the same number of observations. 
# How many observations are there in each data set?
#------------------------------------------------------------

str(IBM)
str(GE)
str(CocaCola)
str(ProcterGamble)
str(Boeing)

#------------------------------------------------------------
# Problem 1.2 - Summary Statistics 
# What is the earliest year in our datasets?
#------------------------------------------------------------

min.datasets <- c(min(IBM$Date), min(GE$Date), min(CocaCola$Date), 
                  min(ProcterGamble$Date), min(Boeing$Date))
i <- which.min(min.datasets)
as.numeric(format(min.datasets[i], "%Y"))

#------------------------------------------------------------
# Problem 1.3 - Summary Statistics
# What is the latest year in our datasets?
#------------------------------------------------------------

max.datasets <- c(max(IBM$Date), max(GE$Date), max(CocaCola$Date), 
                  max(ProcterGamble$Date), max(Boeing$Date))
i <- which.max(max.datasets)
as.numeric(format(max.datasets[i], "%Y"))

#------------------------------------------------------------
# Problem 1.4 - Summary Statistics
# What is the mean stock price of IBM over this time period?
#------------------------------------------------------------

mean(IBM$StockPrice)

#------------------------------------------------------------
# Problem 1.5 - Summary Statistics
# What is the minimum stock price of General Electric (GE) 
# over this time period?
#------------------------------------------------------------

min(GE$StockPrice)

#------------------------------------------------------------
# Problem 1.6 - Summary Statistics
# What is the maximum stock price of Coca-Cola 
# over this time period?
#------------------------------------------------------------

max(CocaCola$StockPrice)

#------------------------------------------------------------
# Problem 1.7 - Summary Statistics
# What is the median stock price of Boeing 
# over this time period?
#------------------------------------------------------------

median(Boeing$StockPrice)

#------------------------------------------------------------
# Problem 1.8 - Summary Statistics
# What is the standard deviation of the stock price of 
# Procter & Gamble over this time period? 
#------------------------------------------------------------

sd(ProcterGamble$StockPrice)

#------------------------------------------------------------
# Problem 2.1 - Visualizing Stock Dynamics
# Around what year did Coca-Cola has its highest 
# stock price in this time period? The lowest stock price?
#------------------------------------------------------------

plot(CocaCola$Date, CocaCola$StockPrice, col = "red")

#------------------------------------------------------------
# Problem 2.2 - Visualizing Stock Dynamics
# In March of 2000, the technology bubble burst, 
# and a stock market crash occurred. According to this plot, 
# which company's stock dropped more? 
#------------------------------------------------------------

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

#------------------------------------------------------------
# Problem 3.1-3.4 - Visualizing Stock Dynamics 1995-2005
# 3.1 Which stock fell the most right after the technology 
#   bubble burst in March 2000?
# 3.2 Which stock reaches the highest value in the time 
#   period 1995-2005?
# 3.3 Comparing September 1997 to November 1997, which 
#   companies saw a decreasing trend in their stock price? 
# 3.4 In the last two years of this time period 
#   (2004 and 2005) which stock seems to be performing 
#   the best, in terms of increasing stock price?
#------------------------------------------------------------

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="black", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="yellow green", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="purple", ylim=c(0,210))
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-30")), lwd=2)
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)

#------------------------------------------------------------
# Problem 4.1 - Monthly Trends
# For IBM, compare the monthly averages to the overall 
# average stock price. In which months has IBM historically 
# had a higher stock price (on average)? Select all that apply.
#------------------------------------------------------------
 
IBM$Month <- months(IBM$Date)
IBM.mean <- mean (IBM$StockPrice)

IBM %>% 
  group_by(Month) %>% 
  summarise (average = mean(StockPrice),
             difference = average - IBM.mean,
             higher = ifelse(difference >0, "Yes", "No"))

#------------------------------------------------------------
# Problem 4.2 - Monthly Trends
# General Electric and Coca-Cola both have their highest 
# average stock price in the same month. Which month is this?
#------------------------------------------------------------

GE$Month <- months(GE$Date)
GE.mean <- mean (GE$StockPrice)

GE.monthlymean <- GE %>% 
                group_by(Month) %>% 
                summarise (average = mean(StockPrice))
i <- which.max(GE.monthlymean$average)
GE.monthlymean$Month[i]

#------------------------------------------------------------
# Problem 4.3 - Monthly Trends
# For the months of December and January, every company's 
# average stock is higher in one month and lower in the 
# other. In which month are the stock prices lower?
#------------------------------------------------------------

GE.monthlymean




