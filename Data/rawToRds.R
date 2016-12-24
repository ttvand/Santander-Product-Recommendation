# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Data")

# Load the required libraries
library(data.table)
library(bit64)

# Limited size of the small fraction of the data
# Used to test parts of the machine learning process quickly
smallFraction <- 0.1

# Set the random seed in order to obtain reproducible results
set.seed(14)


###############################################################

# Read the raw data
train <- fread("train_ver2.csv")
test <- fread("test_ver2.csv")
sampleSubmission <- fread("sample_submission.csv")

# Add the ncodpers key to train, test and the sample submission
setkey(train, ncodpers)
setkey(test, ncodpers)
setkey(sampleSubmission, ncodpers)

# Solve the warning messages due to incorrectly guessed types on 
# Train columns 11, 12 and 16
# Test column 11
table(train[[11]]) # No issue
table(train[[12]]) # There is an actual issue with the conversion
table(train[[16]]) # No issue
table(test[[11]]) # No issue
table(test[[12]]) # Check to verify that there is an issue in train[[12]]
table(test[[16]]) # Check to verify that there is no issue in train[[16]]

# Convert indrel_1mes to categorical and make sure that the column names
# are identical in train and test
numIdsTrain <- !train[[12]] %in% c("", "P")
train[[12]][numIdsTrain] <- as.numeric(train[[12]][numIdsTrain])
table(train[[12]])

# Convert indrel_1mes to categorical in test
test[[12]] <- as.character(test[[12]])

# Convert the dates columns to dates
train$fecha_dato <- as.Date(train$fecha_dato, format="%Y-%m-%d")
test$fecha_dato <- as.Date(test$fecha_dato, format="%m/%d/%Y")
train$fecha_alta <- as.Date(train$fecha_alta, format="%Y-%m-%d")
test$fecha_alta <- as.Date(test$fecha_alta, format="%m/%d/%Y")
train$ult_fec_cli_1t <- as.Date(train$ult_fec_cli_1t, format="%Y-%m-%d")
test$ult_fec_cli_1t <- as.Date(test$ult_fec_cli_1t, format="%m/%d/%Y")

# Convert the numerical entry levels in train to one common numeric encoding
# (e.g. 007 -> 7)
table(train$canal_entrada)
train$canal_entrada[!is.na(as.numeric(train$canal_entrada))] <-
  suppressWarnings(
    as.numeric(train$canal_entrada[!is.na(as.numeric(train$canal_entrada))])
  )
table(train$canal_entrada)

# Make sure that the types in train and test are identical for the non
# predictive columns
nbTestCol <- ncol(test)
sameTrainTestClass <- sapply(test, class) == sapply(train[, 1:nbTestCol,
                                                          with=FALSE], class)
table(sameTrainTestClass)
# Train and test columns should be of the same class, otherwise debug
if(any(!sameTrainTestClass)) browser() 

# Extract a small fraction of the data using a random subset of ncodpers ids
# and using the first fraction of the data (this way there are unmatched
# train - test customers which represents the real test analysis task better)
# The customer id is correlated with the age of the customers and the time at
# the bank
uniqueCustomers <- sort(unique(train$ncodpers))
randomCustomers <- sample(uniqueCustomers,
                          round(length(uniqueCustomers)*smallFraction))
trainSmallRandom <- train[ncodpers %in% randomCustomers,]
testSmallRandom <- test[ncodpers %in% randomCustomers,]

# Extract the first fraction based on the quantile of the ids
cutoffNcodpers <- uniqueCustomers[round(length(uniqueCustomers)*smallFraction)]
trainSmallOrdered <- train[ncodpers <= cutoffNcodpers,]
testSmallOrdered <- test[ncodpers <= cutoffNcodpers,]

# Write the raw data to rds format
saveRDS(train, file.path(getwd(), "train.rds"))
saveRDS(test, file.path(getwd(), "test.rds"))

# Write the raw subsetted data to rds format
saveRDS(trainSmallRandom, file.path(getwd(), "trainSmallRandom.rds"))
saveRDS(testSmallRandom, file.path(getwd(), "testSmallRandom.rds"))
saveRDS(trainSmallOrdered, file.path(getwd(), "trainSmallOrdered.rds"))
saveRDS(testSmallOrdered, file.path(getwd(), "testSmallOrdered.rds"))