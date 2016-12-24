# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)

# Read the raw data
train <- readRDS("Data/train.rds")
predictors <- colnames(train)[-(1:2)]
nbPredictors <- length(predictors)

####################################
# Study the stagnant train columns #
####################################

# List the number of changes for all users by month by target variable
allMonths <- sort(unique(train$fecha_dato))
nbTransitionMonths <- length(allMonths)-1
monthProductChanges <- NULL
for(i in 1:nbTransitionMonths){
  # Show progress message
  cat("Processing month", i, "of", nbTransitionMonths, "\n")
  
  beginDate <- allMonths[i]
  endDate <- allMonths[i+1]
  monthUsers <- intersect(train[fecha_dato == beginDate, ncodpers],
                          train[fecha_dato == endDate, ncodpers])
  for(j in 1:nbPredictors){
    predictor <- predictors[j]
    monthPredChanges <- sum(train[ncodpers %in% monthUsers &
                                    fecha_dato == beginDate, j + 2,
                                  with=FALSE] !=
                              train[ncodpers %in% monthUsers &
                                      fecha_dato == endDate, j + 2,
                                    with=FALSE],
                            na.rm=T)
    monthProductChanges <- rbind(monthProductChanges,
                                 data.table(month = endDate,
                                            predictor = predictor,
                                            monthPredChanges = monthPredChanges))
  }
}