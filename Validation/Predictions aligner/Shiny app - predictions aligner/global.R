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
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(plotly)

# Submission date and file name of the compared predictions
submissionDate <- "18-12-2016"
selfFolder <- "self validation May16 10 Folds"
selfFn <- "Back0.rds"
validationExtension <-
  "xgboost weighted trainAll 20, validation May16 positive flanks.rds"

# Studied target variable
# inspectProduct <- "ind_recibo_ult1"

# Set the target date
targetDate <- "12-11-2016"

# Last considered lags for lag selection and relative consideration cutoff
lastLagsWeightSelection <- 3
validLagRelCutoff <- 2


###########################################################################

# Load the raw compared predictions
allPredsSelf <- readRDS(file.path(getwd(), "Validation", submissionDate,
                                  selfFolder, selfFn))

# Load the base model predictions for the inspected product
baseModelPredictions <- 
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste(gsub(" positive.*$", "", validationExtension),
                          "positive flank base model predictions.rds")))

# Load the base model weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights first validation.rds"))

# Extract the products from the data columns
products <-
  unique(grep("_ult1$", gsub("Lag.*$", "", names(baseModelPredictions)),
              value = TRUE))
productsSimple <- gsub("^ind_|_ult1$", "", products)
defaultProduct <- "recibo"

# baseModelPredictions <-
#   baseModelPredictions[, grepl(inspectProduct, names(baseModelPredictions)),
#                        with=FALSE]
# selfPredictions <- allPredsSelf[product == inspectProduct, prediction]
# positiveRows <- selfPredictions>0
# baseModelPredictions <- baseModelPredictions[positiveRows]
# selfPredictions <- selfPredictions[positiveRows]
# 
# # Find the best approach to align the base model predictions to the self 
# # predictions
# combinedPredictions <- cbind(selfPredictions, baseModelPredictions)
