install.packages("tidyverse")

# Import dataset
mydata <- read.csv("~/SiM/data_sim.csv")
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
boxplot(mydata$age_years, main = 'Age Distribution')

# heat map 
install.packages("ggcorrplot")
library(ggcorrplot)
correlation_matrix <- cor(mydata[1:15])  # Exclude categorical data
ggcorrplot(correlation_matrix, lab = TRUE)

