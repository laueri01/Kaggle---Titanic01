# Data exploration/checking

setwd("C:/Users/Eric/Desktop/kaggle")

trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

head(trainData)

# View(trainData)
# View(testData)

# Test plot data
plot(density(trainData$Age, na.rm = TRUE), 
     main = "Titanic Passenger by Age Group Density", col="red")

plot(density(trainData$Fare, na.rm = TRUE), 
     main = "Titanic Passenger by Fare Paid Density", col="red",
     xlab = "Fare Paid")
#
hist(trainData$Pclass , xlim =c(1,3), breaks=seq(0.55, 3.55, by=.125),
     main = "Titanic Passenger by Class of service Density", col="red",
     xlab = "Class", ylab = "Passanger Counts")

# Male/Female : Survived/Deceased
counts <- table(trainData$Survived, trainData$Sex)

barplot (counts, beside = TRUE, xlab = "Gender", ylab = "Number of People", 
        main = "Survived and deceased between male and female", 
        col=c("red", "green"), ylim = c(0, 500), legend = c("Deceased", "Survived"))

DMale <- (counts[2] / (counts[1] + counts[2])) *100
DFemale <- (counts[4] / (counts[3] + counts[4])) *100
cat(DMale ,"% of the Male die and ", DFemale ,"% of female die")

# 
Pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(Pclass_survival, beside = TRUE, xlab = "Cabin Class", ylab = "Number of People", 
        ylim = c(0, 500), main = "Survived and Deceased by Passenger Class", 
        col=c("red", "green"), legend = c("Deceased", "Survived"))

DiePclass1 <- (Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2]))*100
DiePclass2 <- (Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4]))*100
DiePclass3 <- (Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6]))*100
cat(DiePclass1, "% of 3rd class, ", DiePclass2, "% of 2nd class, ", DiePclass3, "% of 1st Class die")

