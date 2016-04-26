###################################################################################
#     Bryan R. Balajadia                                                          #
#     MITx:15.071x                                                                #
#     Unit 1: An Introduction to Analytics                                        #
#     Assignment 1.3 -  Demographics and Employment in The United States          #
###################################################################################


# Set working directory
setwd("C:/Users/136241/Desktop/MITx_Analytics_Edge/A13_Demographics_Emp")

# Read data into R
CPS <- read.csv("CPSData.csv", sep =",", header = TRUE)


#------------------------------------------------------------
# Problem 1.1 - Loading and Summarizing the Dataset
# How many interviewees are in the CPSData.csv dataset?
#------------------------------------------------------------

str(CPS)

#------------------------------------------------------------
# Problem 1.2 - Loading and Summarizing the Dataset
# Among the interviewees with a value reported for the 
# Industry variable, what is the most common industry of 
# employment?
#------------------------------------------------------------


sort(summary(CPS$Industry), decreasing = TRUE)

#------------------------------------------------------------
# Problem 1.3 - Loading and Summarizing the Dataset
# Which state has the fewest interviewees?
#------------------------------------------------------------

sort(table(CPS$State))

#------------------------------------------------------------
# Problem 1.4 - Loading and Summarizing the Dataset
# What proportion of interviewees are citizens of 
# the United States?
#------------------------------------------------------------

table(CPS$Citizenship)

#------------------------------------------------------------
# Problem 1.5 - Loading and Summarizing the Dataset
# For which races are there at least 250 interviewees in 
# the CPS dataset of Hispanic ethnicity?
#------------------------------------------------------------

table(CPS$Race, CPS$Hispanic)

#------------------------------------------------------------
# Problem 2.1 - Evaluating Missing Values
# Which variables have at least one interviewee 
# with a missing (NA) value?
#------------------------------------------------------------

summary(CPS)

#------------------------------------------------------------
# Problem 2.2 - Evaluating Missing Values
# Often when evaluating a new dataset, we try to identify 
# if there is a pattern in the missing values in the dataset. 
# We will try to determine if there is a pattern in the 
# missing values of the Married variable. 
# The function is.na(CPS$Married) returns a vector of 
# TRUE/FALSE values for whether the Married variable is missing. 
# We can see the breakdown of whether Married is missing 
# based on the reported value of the Region variable with 
# the function table(CPS$Region, is.na(CPS$Married)). 
# Which is the most accurate:
#------------------------------------------------------------

# The married variable being NA is related to Age value for the interviewee
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

#------------------------------------------------------------
# Problem 2.3 - Evaluating Missing Values
# How many states had all interviewees living in a 
# non-metropolitan area? 
# How many states had all interviewees living in a 
# metropolitan area? treat the District of Columbia as a state.
#------------------------------------------------------------

table(CPS$State, is.na(CPS$MetroAreaCode))

#------------------------------------------------------------
# Problem 2.4 - Evaluating Missing Values
# Which region of the United States has the largest 
# proportion of interviewees living in a non-metropolitan area?
#------------------------------------------------------------

table(CPS$Region, is.na(CPS$MetroAreaCode))

#------------------------------------------------------------
# Problem 2.5 - Evaluating Missing Values
# Which state has a proportion of interviewees living in a 
# non-metropolitan area closest to 30%?
# Which state has the largest proportion of non-metropolitan 
# interviewees, ignoring states where all interviewees were non-metropolitan?
#------------------------------------------------------------

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

#------------------------------------------------------------
# Problem 3.1 - Integrating Metropolitan Area Data
# How many observations (codes for metropolitan areas) 
# are there in MetroAreaMap?
# How many observations (codes for countries) are there in CountryMap?
#------------------------------------------------------------

# Read data into R
MetroAreaMap <- read.csv("MetroAreaCodes.csv", sep =",", header = TRUE)
CountryMap <- read.csv("CountryCodes.csv", sep =",", header = TRUE)

nrow(MetroAreaMap)
nrow(CountryMap)

#------------------------------------------------------------
# Problem 3.2 - Integrating Metropolitan Area Data
# Update CPS data frame
#------------------------------------------------------------

# left outer join
CPS <- merge(CPS, MetroAreaMap, 
             by.x = "MetroAreaCode", by.y = "Code", all.x = TRUE)

summary(CPS)

#------------------------------------------------------------
# Problem 3.3 - Integrating Metropolitan Area Data
# Which of the following metropolitan areas has the largest number of interviewees?
#------------------------------------------------------------

table(CPS$MetroArea)

#------------------------------------------------------------
# Problem 3.4 - Integrating Metropolitan Area Data
# Which metropolitan area has the highest proportion of interviewees 
# of Hispanic ethnicity?
#------------------------------------------------------------

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#------------------------------------------------------------
# Problem 3.5 - Integrating Metropolitan Area Data
# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE 
# vector of whether an interviewee is Asian, determine the 
# number of metropolitan areas in the United States from which 
# at least 20% of interviewees are Asian.
#------------------------------------------------------------

sort(tapply(CPS$Race == "Asian"), CPS$MetroArea, mean)

#------------------------------------------------------------
# Problem 3.6 - Integrating Metropolitan Area Data
# determine which metropolitan area has the smallest proportion 
# of interviewees who have received no high school diploma.
#------------------------------------------------------------

sort(tapply(CPS$Education == "No high school diploma"), CPS$MetroArea, mean, na.tm = TRUE)

#------------------------------------------------------------
# Problem 4.1 - Integrating Country of Birth Data
# Update CPS data frame
#------------------------------------------------------------

# left outer join
CPS <- merge(CPS, CountryMap, 
             by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)

summary(CPS)

#------------------------------------------------------------
# Problem 4.2 - Integrating Country of Birth Data
# Among all interviewees born outside of North America, 
# which country was the most common place of birth?
#------------------------------------------------------------

sort(table(CPS$Country))

#------------------------------------------------------------
# Problem 4.3 - Integrating Country of Birth Data
# What proportion of the interviewees from the 
# "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area 
# have a country of birth that is not the United States? 
# For this computation, don't include people from this metropolitan area 
# who have a missing country of birth.
#------------------------------------------------------------

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",
      CPS$Country != "United States")

#------------------------------------------------------------
# Problem 4.4 - Integrating Country of Birth Data
# Which metropolitan area has the largest number (note -- not proportion) 
# of interviewees with a country of birth in India? 
# In Brazil?
# In Somalia?
#------------------------------------------------------------

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm = TRUE))

