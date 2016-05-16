###############################################################
#     Bryan R. Balajadia                                      #
#     MITx:15.071x                                            #
#     Unit 4: Trees                                           #
#     Assignment 4.1 -  understanding why people vote         #
###############################################################

# Set working directory
setwd("C:/Users/136241/Desktop/MITx_Analytics_Edge")

# Read in dataset
gerber <- read.csv("gerber.csv", sep =",", header = TRUE)

# Look at structure
str(gerber)

# to check what proportion of people in this dataset voted
prop.table(table(gerber$voting))

# to find which 'treatment groups' had the largest percentage of people who actually voted 
# with(gerber, table(voting, civicduty))[2,2]/sum(with(gerber, table(voting, civicduty))[,2])
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# Building a Logistic Regression model
gerberLogReg <- glm(voting ~ civicduty + hawthorne + self + neighbors, 
                data=gerber, family="binomial") 
summary(gerberLogReg) 

# to find accuracy using a threshold of 0.3
prediction <- predict(gerberLogReg, type="response") 
c.matrix <- table(gerber$voting, prediction > 0.3) 
(c.matrix[1,1] + c.matrix[2,2]) / nrow(gerber) 

# to find accuracy using a threshold of 0.5
c.matrix <- table(gerber$voting, prediction > 0.5) 
(c.matrix[1,1]) / nrow(gerber) 

# to compute the AUC
library(ROCR)
ROCRpred <- prediction(prediction, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Building a tree model
library(rpart)
library(rpart.plot)

CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodelControl = rpart(voting ~ control, data=gerber, cp=0.0) 
prp(CARTmodelControl, digits=6) 
abs(0.296638-0.34) 

CARTmodelSex <- rpart(voting ~ control + sex, data=gerber, cp=0.0) 
prp(CARTmodelSex, digits=6) 
abs(0.290456-0.334176)
abs(0.302795-0.345818) 

# Logistic Regression  using 'Control' and 'Sex'
gerberLogReg2 <- glm(voting ~ control + sex, 
                    data=gerber, family="binomial") 
summary(gerberLogReg2) 

Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerberLogReg2, newdata=Possibilities, type="response")
abs(0.2908065 - 0.290456)

# Logistic regression with interaction terms
gerberLogReg3 <- glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(gerberLogReg3) 

predict(gerberLogReg3, newdata=Possibilities, type="response")
abs(0.2904558 - 0.290456)



