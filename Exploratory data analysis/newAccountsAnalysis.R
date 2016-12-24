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
targetMonth <- 7

# Find ncodpers that have data for the target month but not for the month
# before that
back1Month <- paste0("2015-", str_pad(targetMonth-1, 2, pad = "0"), "-28")
back0Month <- paste0("2015-", str_pad(targetMonth-0, 2, pad = "0"), "-28")
targetNcodpers <- setdiff(train[fecha_dato == back0Month, ncodpers],
                          train[fecha_dato == back1Month, ncodpers])
targetTrain <- train[ncodpers %in% targetNcodpers &
                       fecha_dato == back0Month, ]
posFlanksProducts <-
  as.matrix(targetTrain[fecha_dato == back0Month, -(1:24),
                        with=FALSE]) == 1
nbPosFlanks <- sum(posFlanksProducts, na.rm = TRUE)
plot(colSums(posFlanksProducts, na.rm = TRUE), pch=16, col="green")
