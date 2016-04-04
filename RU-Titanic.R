# Titanic Project scripts
## insert test line

# data browsing 

setwd("C:/Users/Eric/Desktop/kaggle")

trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

# Also review data in Spreadsheet.

head(trainData)

plot(density(trainData$Age, na.rm = TRUE))
plot(density(trainData$Fare, na.rm = TRUE))

# numeric fields review, determine if meaningful
summary(trainData$Survived)
summary(trainData$Pclass)
summary(trainData$Age)
summary(trainData$SibSp)
summary(trainData$Parch)
summary(trainData$Fare)


# Survival Rate by Sex Plot #4

counts <- table(trainData$Survived, trainData$Sex)

maleD <- round(counts[2] / (counts[1] + counts[2]) * 100, digits = 2)
FmaleD <- round(counts[4] / (counts[3] + counts[4]) * 100, digits = 2)
t <- paste(maleD,"% of male deceased versus Female deceased of ", FmaleD, "%")

barplot(counts, beside = TRUE, xlab = "Gender", ylab = "Number of People", 
        main = t, 
        col=c("red", "green"), 
        ylim = c(0, 600), 
        legend = c("Deceased", "Survived"))
        
#Survival Rate by Passenger Class Barplot #3

Pclass_survival <- table(trainData$Survived, trainData$Pclass)
PC1 <- round(Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2])*100, digits=2)
PC2 <- round(Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4])*100, digits=2)
PC3 <- round(Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6])*100, digits=2)

t1 <- paste("Deceased:", PC1,"% of 3rd class, ", PC2, "% of 2nd class, ", PC3, "% of 1st class")

barplot(Pclass_survival, beside = TRUE, 
        xlab = "Cabin Class", ylab = "Number of People", 
        ylim = c(0, 500), 
        main = t1,
        col=c("red", "green"), legend = c("Deceased", "Survived"))

#Survival by Age #1

boxplot(trainData$Age ~ trainData$Survived,
        main ="Passenger Survived/Deceased by Age",
        xlab="",
        ylab="Age",
        names = c("Deceased","Survived"),
        col= c("red","green"))


# Passenger cabin class by age
boxplot(trainData$Age ~ trainData$Pclass,
        main ="Passenger Cabin Class by Age",
        xlab="Cabin Class",
        ylab="Age",
        names = c("1st","2nd", "3rd"),
        col= c("blue","green", "brown"))








# Plot: Male and Female counts #2







# March 18, 2016 Data cleaning
# From the training data, remove Passenger ID, Ticket, Fare, Cabin and Embar 

trainData = trainData[-c(1,9,10,11,12)]
head(trainData)

View(test)

# In Sex field sub "female" with 1 and "male" with 0 for modeling
trainData$Sex = gsub("female", 1, trainData$Sex)
trainData$Sex = gsub("male", 0, trainData$Sex)






# padding missing data
# Titles Master, Mr, Mrs, miss, dr, rev, col, capt, Lady, countess, Mme. Mlle., Don, Jonkheer. Major., Sir., 

master_vector = grep("Master.",trainData$Name, fixed=TRUE)
miss_vector = grep("Miss.", trainData$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", trainData$Name, fixed=TRUE)
mr_vector = grep("Mr.", trainData$Name, fixed=TRUE)
dr_vector = grep("Dr.", trainData$Name, fixed=TRUE)

for(i in master_vector) {
  trainData$Name[i] = "Master"
}
for(i in miss_vector) {
  trainData$Name[i] = "Miss"
}
for(i in mrs_vector) {
  trainData$Name[i] = "Mrs"
}
for(i in mr_vector) {
  trainData$Name[i] = "Mr"
}
for(i in dr_vector) {
  trainData$Name[i] = "Dr"
}


#View(testData)

# replace missing age with average age of the group (ie. Mr. Miss)
# Calculate average age for each Title
master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

## cat ("Average age by title:", "master:", master_age, " ", "miss: ", miss_age, "Mrs:", mrs_age, "mr:", mr_age, "dr:", dr_age)

# replace missing ages
for (i in 1:nrow(trainData)) {

  ## cat ("name:",trainData$Name[i], "age:", trainData$Age[i] )
  ## print(" ")
  
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
      print(trainData$Name)
    }
  }
}


# Create new variable to improve the model
# Child
trainData["Child"]
for (i in 1:nrow(trainData)) {
  if (trainData$Age[i] <= 12) {
    trainData$Child[i] = 1
  } else {
    trainData$Child[i] = 2
  }
}

# Family
trainData["Family"] = NA

for(i in 1:nrow(trainData)) {
  x = trainData$SibSp[i]
  y = trainData$Parch[i]
  trainData$Family[i] = x + y + 1
}


# Mother
trainData["Mother"] ="NA"
for(i in 1:nrow(trainData)) {
  if(trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    trainData$Mother[i] = 1
  } else {
    trainData$Mother[i] = 2
  }
}


# testdata prep

PassengerId = testData[1]
testData = testData[-c(1, 8:11)]

testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("^male", 0, testData$Sex)

test_master_vector = grep("Master.",testData$Name)
test_miss_vector = grep("Miss.", testData$Name)
test_mrs_vector = grep("Mrs.", testData$Name)
test_mr_vector = grep("Mr.", testData$Name)
test_dr_vector = grep("Dr.", testData$Name)

for(i in test_master_vector) {
  testData[i, 2] = "Master"
}
for(i in test_miss_vector) {
  testData[i, 2] = "Miss"
}
for(i in test_mrs_vector) {
  testData[i, 2] = "Mrs"
}
for(i in test_mr_vector) {
  testData[i, 2] = "Mr"
}
for(i in test_dr_vector) {
  testData[i, 2] = "Dr"
}

test_master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testData)) {
  if (is.na(testData[i,4])) {
    if (testData[i, 2] == "Master") {
      testData[i, 4] = test_master_age
    } else if (testData[i, 2] == "Miss") {
      testData[i, 4] = test_miss_age
    } else if (testData[i, 2] == "Mrs") {
      testData[i, 4] = test_mrs_age
    } else if (testData[i, 2] == "Mr") {
      testData[i, 4] = test_mr_age
    } else if (testData[i, 2] == "Dr") {
      testData[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", testData[i,2], sep=""))
    }
  }
}

#We do a manual replacement here, because we weren't able to programmatically figure out the title.
#We figured out it was 89 because the above print statement should have warned us.
#testData["Lifeboat"]
#testData["Embark"]

testData[89, 4] = test_miss_age

testData["Child"] = NA

for (i in 1:nrow(testData)) {
  if (testData[i, 4] <= 12) {
    testData[i, 7] = 1
  } else {
    testData[i, 7] = 1
  }
}

testData["Family"] = NA

for(i in 1:nrow(testData)) {
  testData[i, 8] = testData[i, 5] + testData[i, 6] + 1
}

testData["Mother"] = NA

for(i in 1:nrow(testData)) {
  if(testData[i, 2] == "Mrs" & testData[i, 6] > 0) {
    testData[i, 9] = 1
  } else {
    testData[i, 9] = 2
  }
}


train.glm <- glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, family = binomial, 
                 data = trainData)

summary(train.glm)


p.hats <- predict.glm(train.glm, newdata = testData, type = "response")

survival <- vector()
for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}






