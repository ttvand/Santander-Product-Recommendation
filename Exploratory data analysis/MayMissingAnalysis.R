# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)
library(stringr)

# Read the raw data
train <- readRDS("Data/train.rds")

# List the target month as a variable
targetMonth <- 11

# Find ncodpers that have data for April and June 2015, but not for May 2015
back2Month <- paste0("2015-", str_pad(targetMonth-2, 2, pad = "0"), "-28")
back1Month <- paste0("2015-", str_pad(targetMonth-1, 2, pad = "0"), "-28")
back0Month <- paste0("2015-", str_pad(targetMonth-0, 2, pad = "0"), "-28")
targetNcodpers <- setdiff(intersect(
  train[fecha_dato == back2Month, ncodpers],
  train[fecha_dato == back0Month, ncodpers]),
  train[fecha_dato == back1Month, ncodpers])
targetTrain <- train[ncodpers %in% targetNcodpers, ]
targetTrain <- targetTrain[fecha_dato == back2Month |
                             fecha_dato == back0Month, ]
posFlanksProducts <-
  as.matrix(targetTrain[fecha_dato == back2Month, -(1:24),
                        with=FALSE]) == 0 &
  as.matrix(targetTrain[fecha_dato == back0Month, -(1:24),
                        with=FALSE]) == 1
nbPosFlanks <- sum(posFlanksProducts)
negFlanksProducts <-
  as.matrix(targetTrain[fecha_dato == back2Month, -(1:24),
                        with=FALSE]) == 1 &
  as.matrix(targetTrain[fecha_dato == back0Month, -(1:24),
                        with=FALSE]) == 0
nbNegFlanks <- sum(negFlanksProducts)
plot(colSums(posFlanksProducts), pch=16, col="green")
points(colSums(negFlanksProducts), col="red")
