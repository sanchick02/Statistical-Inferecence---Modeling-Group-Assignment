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

# Cholesterol Distribution
cholesterol <- table(mydata$cholesterol)
barplot(cholesterol, main="Cholesterol Level Distribution", xlab="Cholesterol Level", ylab="Frequency", col = rainbow(3))

# Glucose Distribution
glucose <- table(mydata$gluc)
barplot(glucose, main="Glucose Level Distribution", xlab="Glucose Level", ylab="Frequency", col = rainbow(3))

# Smoking Distribution
smoking <- table(mydata$smoke)
barplot(smoking, main="Smoking Status Distribution", xlab="Smoking Status (0: Non-smoker, 1: Smoker)", ylab="Frequency", col = rainbow(2))

# Alcohol Distribution
alcohol <- table(mydata$alco)
barplot(alcohol, main="Alcohol Intake Distribution", xlab="Alcohol Intake (0: Does not consume, 1: Consumes)", ylab="Frequency", col = rainbow(2))

# Blood Pressure Category Distribution
bp_category <- table(mydata$bp_category)
barplot(bp_category, main="Blood Pressure Category Distribution", xlab="Blood Pressure Category", ylab="Frequency", col = rainbow(4))

# Active Distribution
active <- table(mydata$active)
barplot(active, main="Physical Activity Distribution", xlab="Physical activity (0: Not physically active, 1: Physically active)", ylab="Frequency", col = rainbow(2))

# CVD Distribution
CVD <- table(mydata$cardio)
barplot(CVD, main="Cardiovascular Disease Distribution", xlab="Cardiovascular Disease (0: Absence, 1: Presence)", ylab="Frequency", col = rainbow(2))

# Histogram
# Age Distribution
hist(mydata$age_years, main="Age distribution", xlab="Age", ylab="Frequency", col = "mediumpurple1")

# Height Distribution
hist(mydata$height, main="Height distribution", xlab="Height", ylab="Frequency", col = "rosybrown1")

# Weight Distribution
hist(mydata$weight, main="Weight distribution", xlab="Weight", ylab="Frequency", col = "skyblue")

# Boxplot
library(ggplot2)

# Boxplot for BMI vs Cardiovascular
ggplot(data = mydata, mapping = aes(x = cardio, y = bmi)) + geom_boxplot(mapping = aes(group = cut_width(cardio, 0.1)))

# Boxplot for ap_hi & ap_lo
ggplot(data = mydata, mapping = aes(x = ap_hi, y = ap_lo)) + geom_boxplot(mapping = aes(group = cut_width(cardio, 0.1)))

# Scatter plot
# Scatter plot for cholesterol and CVD
plot(mydata$cholesterol, mydata$cardio, main = "Cholesterol Level VS CVD", xlab = "Cholesterol Level", ylab = "Cardiovascular Disease")
cor(mydata$cholesterol, mydata$cardio)

# Scatter plot for glucose and CVD
plot(mydata$gluc, mydata$cardio, main = "Glucose Level VS CVD", xlab = "Glucose Level", ylab = "Cardiovascular Disease")
cor(mydata$gluc, mydata$cardio)

# Scatter plot for smoke and CVD
plot(mydata$smoke, mydata$cardio, main = "Smoking Status VS CVD", xlab = "Smoking Status", ylab = "Cardiovascular Disease")
cor(mydata$smoke, mydata$cardio)

# Scatter plot for alcohol and CVD
plot(mydata$alco, mydata$cardio, main = "Alcohol Intake VS CVD", xlab = "Alcohol Intake", ylab = "Cardiovascular Disease")
cor(mydata$alco, mydata$cardio)

# Scatter plot for Active and CVD
plot(mydata$active, mydata$cardio, main = "Physical Activity VS CVD", xlab = "Physical Activity", ylab = "Cardiovascular Disease")
cor(mydata$active, mydata$cardio)

# Scatter plot for BMI and CVD
plot(mydata$bmi, mydata$cardio, main = "BMI VS CVD", xlab = "BMI", ylab = "Cardiovascular Disease")
cor(mydata$bmi, mydata$cardio)

# heat map 1
data = as.matrix(mydata[1:15])
head(data)
install.packages("pheatmap")
library("pheatmap")
pheatmap(data, cluster_rows = FALSE, cluster_cols = FALSE)


# heat map 2
install.packages("plotly")                                    
library("plotly") 
plot_ly(z = data, type = "heatmap")
