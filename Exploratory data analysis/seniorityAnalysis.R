#####################################################
# Study the difference in "peaks" of seniority < 60 #
# seniority is coded as antiguedad                  #
#####################################################
# Reference:
# browseURL(paste0("https://www.kaggle.com/sudalairajkumar/",
# "santander-product-recommendation/just-another-visualisation/discussion"))

# Main explanation: There is an uneven distribution in the start date and
# this shifts month by month which makes the train data seniority look more
# flat since 1.5 years of data is trained
# No change in recorded seniority until July/August 2015!

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)

# Data extension (option to process a fraction of the data)
dataExtension <- c("", "SmallRandom", "SmallOrdered")[1]

# Read the raw data
train <- readRDS(paste0("Data/train", dataExtension, ".rds"))
test <- readRDS(paste0("Data/test", dataExtension, ".rds"))

# Set negative seniorities to NA
train[antiguedad < 0, antiguedad:=NA]
test[antiguedad < 0, antiguedad:=NA]

# Plot the seniority (antiguedad) distribution
hist(train$antiguedad)
hist(test$antiguedad)

# Zoom in on the seniority
hist(train[antiguedad < 60, antiguedad], 1e2)
hist(test[antiguedad < 60, antiguedad], 1e2)

# Study the difference in seniority between test and the last train record
# almost 99 % is 1 as expected, close to a % was not updated (0) and some
# outliers
lastTrainMonth <- train[fecha_dato=="2016-05-28", ]
matchId <- match(test$ncodpers, lastTrainMonth$ncodpers)
table(test$antiguedad - lastTrainMonth[matchId, antiguedad], useNA = "ifany")

hist(lastTrainMonth[antiguedad < 60, antiguedad], 1e2)
hist(test[antiguedad < 60, antiguedad], 1e2)

# Study the distribution for a specific train month
hist(train[fecha_dato=="2016-05-28", antiguedad], 1e2)
