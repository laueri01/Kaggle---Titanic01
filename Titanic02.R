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


# March 20, 2016 Data cleaning
# From the training data, remove Passenger ID, Ticket, Fare, Cabin and Embar 

trainData = trainData[-c(1,9,10,11,12)]

# View(trainData)

# In Sex field sub "female" with 1 and "male" with 0 for modeling
trainData$Sex = gsub("female", 1, trainData$Sex)
trainData$Sex = gsub("male", 0, trainData$Sex)

# padding missing data
# Titles Master, Mr, Mrs, miss, dr, rev, col, capt, Lady, countess, Mme. Mlle., Don, Jonkheer. Major., Sir., 
# Replace name field with Title

master_vector = grep("Master.",trainData$Name, fixed=TRUE)
miss_vector = grep("Miss.", trainData$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", trainData$Name, fixed=TRUE)
mr_vector = grep("Mr.", trainData$Name, fixed=TRUE)
dr_vector = grep("Dr.", trainData$Name, fixed=TRUE)


trainData[master_vector,]$Name <- "Master"
trainData[miss_vector,]$Name <- "Miss"
trainData[mrs_vector,]$Name <- "Mrs"
trainData[mr_vector,]$Name <- "Mr"
trainData[dr_vector,]$Name <- "Dr"


# Calculate the average age for each title

master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

cat("Average age Master:",master_age, " Miss:",
    miss_age, " Mrs:",mrs_age, " Mr:",mr_age, " Dr:",dr_age)

# Replace NA field with calculated average age
for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,5])) {
    if (trainData$Name[i] == "Master") {
      trainData$Age[i] = master_age
    } else if (trainData$Name[i] == "Miss") {
      trainData$Age[i] = miss_age
    } else if (trainData$Name[i] == "Mrs") {
      trainData$Age[i] = mrs_age
    } else if (trainData$Name[i] == "Mr") {
      trainData$Age[i] = mr_age
    } else if (trainData$Name[i] == "Dr") {
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}




# Create new variables to improve the model

# classify Child as less than 12 years old

# Remove 
#trainData = trainData[-c(8)]

trainData["Child"] = NA
for (i in 1:nrow(trainData)) {
  if (trainData$Age[i] <= 12) {
    trainData$Child[i] = 1
  } else {
    trainData$Child[i] = 2
  }
}


# Family
# Classify Family (count) are parent with siblings 
trainData["Family"] = NA

for(i in 1:nrow(trainData)) {
  x = trainData$SibSp[i]
  y = trainData$Parch[i]
  # Siblings add Childs add 1 for non-zero
  trainData$Family[i] = x + y + 1
}


# Mother
# classify Mother to 1 are Mrs with child
trainData["Mother"] = NA
for(i in 1:nrow(trainData)) {
  if(trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    trainData$Mother[i] = 1
  } else {
    trainData$Mother[i] = 2
  }
}

# Prepare test data set
# reset test data set
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

PassengerId = testData[1]
testData = testData[-c(1, 8:11)]

testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("^male", 0, testData$Sex)

# Replace long name with just the title
test_master_vector = grep("Master.",testData$Name, fixed = TRUE)
test_miss_vector = grep("Miss.", testData$Name, fixed = TRUE)
test_mrs_vector = grep("Mrs.", testData$Name, fixed = TRUE)
test_mr_vector = grep("Mr.", testData$Name, fixed = TRUE)
test_dr_vector = grep("Dr.", testData$Name, fixed = TRUE)

testData[test_master_vector,]$Name <-"Master"
testData[test_miss_vector,]$Name <-"Miss"
testData[test_mrs_vector,]$Name <-"Mrs"
testData[test_mr_vector,]$Name <-"Mr"
testData[test_dr_vector,]$Name <-"Dr"

# Manual Replacement
testData[89,2] <- "Miss"


# Replace missing in age with average age of the grounp
test_master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm =TRUE),digits=2)
test_miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm =TRUE),digits=2)
test_mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm =TRUE),digits=2)
test_mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm =TRUE),digits=2)
test_dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm =TRUE),digits=2)

# cat(test_master_age, test_miss_age, test_mrs_age, test_mr_age, test_dr_age)

# Reset Age to NA testData$Age = ""

for (i in 1:nrow(testData)) {
  if (is.na(testData[i,4])) {
    if (testData$Name[i] == "Master") {
      testData$Age[i] = test_master_age
    } else if (testData$Name[i] == "Miss") {
      testData$Age[i] = test_miss_age
    } else if (testData$Name[i] == "Mrs") {
      testData$Age[i] = test_mrs_age
    } else if (testData$Name[i] == "Mr") {
      testData$Age[i] = test_mr_age
    } else if (testData$Name[i] == "Dr") {
      testData$Age[i] = test_dr_age
    } else {
      print(paste("Other title at: ", i, sep=""))
      print(paste("Other title: ", testData[i,2], sep=""))
    }
  }
}

#manual replacement for other title if req

# new variables

testData["Child"] = NA
for (i in 1:nrow(testData)) {
  if (testData$Age[i] <= 12) {
    testData$Child[i] = 1
  } else {
    testData$Child[i] = 2
  }
}

testData["Family"] = NA
for(i in 1:nrow(testData)) {
  testData$Family[i] = testData$SibSp[i] + testData$Parch[i] + 1
}

testData["Mother"] = NA
for(i in 1:nrow(testData)) {
  if(testData$Name[i] == "Mrs" & testData$Parch[i] > 0) {
    testData$Mother[i] = 1
  } else {
    testData$Mother[i] = 2
  }
}

# ^^^^^^^^^^^================== above tested =============================^^^^^^^^^

