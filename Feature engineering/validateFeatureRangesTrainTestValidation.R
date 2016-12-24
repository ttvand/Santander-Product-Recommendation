# Logic to check that the feature range is similar for the train, validation
# and test data

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Feature engineering")

# Load the required libraries
library(data.table)

# Target date
targetDate <- "12-11-2016"

# Load the train, validation and test features
train <- readRDS(file.path(getwd(), targetDate,
                           "trainBack3-1SmallOrdered features.rds"))
validation <- readRDS(file.path(getwd(), targetDate,
                                "validationBack0-0SmallOrdered features.rds"))
test <- readRDS(file.path(getwd(), targetDate,
                          "testSmallOrdered features.rds"))

# Verify that lags 1 of test align with the validation data
targetCols <- names(validation)[(-23:0) + ncol(validation)]
testLags <- test[ncodpers %in% validation$ncodpers,
                 paste0(targetCols, "Lag1"), with=FALSE]
validationVals <- validation[ncodpers %in% test$ncodpers, targetCols,
                             with=FALSE]
all(as.matrix(testLags) == as.matrix(validationVals))

# Verify that lags 1 of validation align with the last month of the train data
lastTrainDate <- max(train$lastDate)
validationLags <- validation[ncodpers %in% train[lastDate == lastTrainDate,
                                                 ncodpers],
                             paste0(targetCols, "Lag1"), with=FALSE]
trainVals <- train[lastDate == lastTrainDate &
                     ncodpers %in% validation$ncodpers, targetCols, with=FALSE]
all(as.matrix(validationLags) == as.matrix(trainVals))

# Drop training specific columns from train
trainSpecificCols <- setdiff(names(train), names(test))
train <- train[, -trainSpecificCols, with=FALSE]
validation <- validation[, -trainSpecificCols, with=FALSE]

# Reorder the test columns so that they match the train columns
# test <- test[, names(train), with=FALSE]
which(names(train) != names(test))

# Mean analysis
trainMeans <- sapply(train, mean, na.rm=TRUE)
validationMeans <- sapply(validation, mean, na.rm=TRUE)
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
