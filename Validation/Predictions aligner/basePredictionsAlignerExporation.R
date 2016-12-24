#############################################################################
# Comparison of the base predictions with the self predictions before       #
# combining them linearly                                                   #
# Main question: how to combine probabilities to maximize the expected MAP? #
#############################################################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(ggplot2)

# Submission date and file name of the compared predictions
submissionDate <- "18-12-2016"
selfFolder <- "self validation May16 10 Folds"
selfFn <- "Back0.rds"
validationExtension <-
  "xgboost weighted trainAll 20, validation May16 positive flanks.rds"

# Studied target variable
inspectProduct <- "ind_recibo_ult1"


###########################################################################

# Load the raw compared predictions
allPredsSelf <- readRDS(file.path(getwd(), "Validation", submissionDate,
                                  selfFolder, selfFn))

# Load the base model predictions for the inspected product
baseModelPredictions <- 
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste(gsub(" positive.*$", "", validationExtension),
                          "positive flank base model predictions.rds")))
baseModelPredictions <-
  baseModelPredictions[, grepl(inspectProduct, names(baseModelPredictions)),
                       with=FALSE]
selfPredictions <- allPredsSelf[product == inspectProduct, prediction]
positiveRows <- selfPredictions>0
baseModelPredictions <- baseModelPredictions[positiveRows]
selfPredictions <- selfPredictions[positiveRows]

# Find the best approach to align the base model predictions to the self 
# predictions
combinedPredictions <- cbind(selfPredictions, baseModelPredictions)

# Inspect monthly predictions over time - spend remaining time on this!!
# Some very interesting patterns!!!!!
row <- 115
plot(as.matrix(combinedPredictions[row,])[1,]);row <- row + 1
