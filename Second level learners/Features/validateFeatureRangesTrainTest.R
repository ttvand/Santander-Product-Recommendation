# Logic to check that the feature range is similar for the train and test data

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Second level learners/Features")

# Load the required libraries
library(data.table)

# Target date
targetDate <- "12-11-2016"

# Load the train, validation and test features
train <- readRDS(file.path(getwd(), targetDate, "trainFixedLag5",
                           "Back11Lag5.rds"))
test <- readRDS(file.path(getwd(), targetDate, "testFixedLag5",
                          "Lag5.rds"))

# Drop training specific columns from train and test
trainSpecificCols <- setdiff(names(train), names(test))
train <- train[, -trainSpecificCols, with=FALSE]
testSpecificCols <- setdiff(names(test), names(train))
test <- test[, -testSpecificCols, with=FALSE]

# Reorder the test columns so that they match the train columns
# test <- test[, names(train), with=FALSE]
which(names(train) != names(test))

# Mean analysis
trainMeans <- sapply(train, mean, na.rm=TRUE)
testMeans <- sapply(test, mean, na.rm=TRUE)

# Inspect most extreme ratio differences for train-test
meanRatioTT <- sort(trainMeans/testMeans)
plot(meanRatioTT)
head(meanRatioTT)
tail(meanRatioTT)

# Inspect most extreme ratio differences for train-validation
meanRatioTV <- sort(trainMeans/validationMeans)
plot(meanRatioTV)
head(meanRatioTV)
tail(meanRatioTV)

# Median analysis
trainMedians <- sapply(train, median, na.rm=TRUE)
testMedians <- sapply(test, median, na.rm=TRUE)
validationMedians <- sapply(validation, median, na.rm=TRUE)

# Inspect most extreme ratio differences for train-test
medianRatioTT <- sort(trainMedians/testMedians)
plot(medianRatioTT)
head(medianRatioTT)
tail(medianRatioTT)

# Inspect most extreme ratio differences train-validation
medianRatioTV <- sort(trainMedians/validationMedians)
plot(medianRatioTV)
head(medianRatioTV)
tail(medianRatioTV)

# NA fraction analysis
trainNAFraction <- sapply(train, function(x) mean(is.na(x)))
testNAFraction <- sapply(test, function(x) mean(is.na(x)))
validationNAFraction <- sapply(validation, function(x) mean(is.na(x)))

# Similar NA ratios for train and validation
plot(trainNAFraction, validationNAFraction)
abline(0, 1, col="blue")

# There are columns which are missing for test and available for validation
plot(validationNAFraction, testNAFraction)
abline(0, 1, col="blue")

# And those are the target columns, no surprise there!
which(testNAFraction>0.8 & validationNAFraction<0.2)
