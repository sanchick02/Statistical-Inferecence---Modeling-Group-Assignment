install.packages("tidyverse")

# Import dataset
mydata <- read.csv("SiM_Data_Final.csv")
View(mydata)

# Identify missing value
sum(is.na(mydata))

ncol(mydata)
nrow(mydata)
head(mydata)
colnames(mydata)
dim(mydata)

# Summary statistics
summary(mydata)

# Bar Chart
# Gender Distribution
gender <- table(mydata$gender)
barplot(gender, main="Gender Distribution", xlab="Gender (1: Female, 2: Male)", ylab="Frequency", col = rainbow(2))

# CVD Distribution
CVD <- table(mydata$cardio)
barplot(CVD, main="Cardiovascular Disease Distribution", xlab="Cardiovascular Disease (0: Absence, 1: Presence)", ylab="Frequency", col = rainbow(2))

# Histogram
# Height Distribution
hist(mydata$height, main="Height distribution", xlab="Height", ylab="Frequency", col = "rosybrown1")

# Weight Distribution
hist(mydata$weight, main="Weight distribution", xlab="Weight", ylab="Frequency", col = "skyblue")


# Scatter plot
# Scatter plot for ap_hi and CVD
plot(mydata$ap_hi, mydata$cardio, main = "ap_hi VS CVD", xlab = "ap_hi", ylab = "Cardiovascular Disease")
cor(mydata$ap_hi, mydata$cardio)

# Scatter plot for ap_lo and CVD
plot(mydata$ap_lo, mydata$cardio, main = "ap_lo VS CVD", xlab = "ap_lo", ylab = "Cardiovascular Disease")
cor(mydata$ap_lo, mydata$cardio)

# Boxplot
library(ggplot2)

# BMI Distribution
boxplot(mydata$bmi, main = 'BMI Distribution')

# Age Distribution
boxplot(mydata$age, main = 'Age Distribution')

# heat map 
install.packages("ggcorrplot")
library(ggcorrplot)
correlation_matrix <- cor(mydata[1:14])  # Exclude categorical data
ggcorrplot(correlation_matrix, lab = TRUE)

# Statistical Analysis

# Check data structure
str(data)


# Develop a binary logistic regression model using glm function
riskmodel<-glm(cardio~age+gender+height+weight+ap_hi+ap_lo+cholesterol+gluc+smoke+alco+active+bmi,
               family=binomial,data=mydata) 
summary(riskmodel) 


# rerun model by including significant independent variables only
riskmodel<-glm(cardio~weight+ap_hi+cholesterol+smoke,
               family=binomial,data=mydata) 
summary(riskmodel) 


# load library ggplot2
library(ggplot2)

# Plot Predicted data and data points
ggplot(mydata, aes(x=weight+ap_hi+cholesterol+smoke, y=cardio)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=binomial))


# use  odds ratio to measure  association between independent and dependent variables
coef(riskmodel)
exp(coef(riskmodel)) 
exp(confint(riskmodel))
cbind(coef(riskmodel),odds_ratio=exp(coef(riskmodel)),exp(confint(riskmodel))) 


# predict the probability (risk) of the final model using the fitted function
mydata$predrisk<-round(fitted(riskmodel),2)
head(mydata,n=10)  


# measure the goodness of fit of the fitted model
classificationtable<-table(mydata$cardio,mydata$predrisk > 0.5)
classificationtable 


# calculate sensitivity and specificity values in R
sensitivity<-(classificationtable[2,2]/(classificationtable[2,2]+classificationtable[2,1]))*100
sensitivity
specificity<-(classificationtable[1,1]/(classificationtable[1,1]+classificationtable[1,2]))*100
specificity 


# ROC-curve using pROC library
logit_P = predict(riskmodel , newdata = mydata, type = 'response' )

library(pROC)
roc_score=roc(mydata$cardio, logit_P) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")
