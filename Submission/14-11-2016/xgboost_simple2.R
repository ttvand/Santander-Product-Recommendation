################################
# Simple XGBOOST predictions 2 #
################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)

# Submission date and file name
submissionDate <- "14-11-2016"
submissionFile <- "xgboost simple 2 back 0-3 excludeOwned.csv"

# Target date 
targetDate <- "12-11-2016"

# Target train models
targetTrainModels <- "trainBack3-1SmallOrdered"
# targetTrainModels <- "trainBack3-0"

# Target features file name
testFn <- "test features.rds"

# Option to predict a subset of the test data
predictSubset <- FALSE
predictFirst <- 1e5


######################################################################

# Path to the xgboost models
modelsPath <- file.path(getwd(), "First level learners", targetDate,
                        targetTrainModels)

# Load the base models
baseModels <- list.files(modelsPath)
nbBaseModels <- length(baseModels)
baseModelNames <- gsub("[.]rds$", "", baseModels)
models <- lapply(baseModels, function(x) readRDS(file.path(modelsPath, x)))

# Load the test data
testData <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                              testFn))

# Optionally subset the test data
if(predictSubset){
  testData <- testData[1:predictFirst]
}

# Load the validation data in order to know how to rearrange the target columns
trainFn <- "trainBack0-0SmallOrdered features.rds"
colOrderData <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                  trainFn))

# Reorder the train models based on the target indicator columns in the 
# validation data
targetCols <- grep("^ind_.*_ult1$", names(colOrderData), value=TRUE)
modelOrder <- match(targetCols, baseModelNames)
models <- models[modelOrder]
baseModelNames <- baseModelNames[modelOrder]

# Verify the correct reordering of the base models
if(!all(baseModelNames == targetCols)) browser()

# Loop over all base models
for(i in 1:nbBaseModels){
  # Extract the target column
  targetVar <- targetCols[i]
  targetModel <- models[[i]]
  
  # Show progress message
  cat("Generating test predictions for model", i, "of", nbBaseModels, "\n")
  
  # Extract predictors data from the features data
  predictorData <- testData[, targetModel$predictors, with=FALSE]
  
  # Convert the predictor data to a matrix
  predictorDataM <- data.matrix(predictorData)
  
  # Calculate the test predictions
  predictions <- predict(targetModel$model, predictorDataM)
  
  # Set the predictions to -1 for products that are already owned
  alreadyOwned <- (testData[[paste0(targetVar, "Lag1")]] == 1)
  predictions[alreadyOwned] <- -runif(length(alreadyOwned))
  
  # Add the predictions to the data table with all target predictions
  predictionsDT <- data.table(ncodpers = testData$ncodpers,
                              prediction = predictions, product = targetVar)
  if(i==1){
    allPredictions <- predictionsDT
  } else{
    allPredictions <- rbindlist(list(allPredictions, predictionsDT), fill=TRUE)
  }
}

# Order the predicted probabilities for all products by client
setkey(allPredictions, ncodpers)
allPredictions[,order_predict := match(1:length(prediction),
                                       order(-prediction)), by=ncodpers]
allPredictions <- allPredictions[order(ncodpers, -prediction), ]

# Make sure that the order of the predictions is unique for each client
orderCount <- allPredictions[, .N, .(ncodpers, order_predict)]
if(max(orderCount$N)>1) browser()

# Show the confidence in the top prediction
hist(allPredictions[order_predict==1, prediction])

# Combine the top seven products to a string vector
productString <- paste(allPredictions[order_predict==1, product],
                       allPredictions[order_predict==2, product],
                       allPredictions[order_predict==3, product],
                       allPredictions[order_predict==4, product],
                       allPredictions[order_predict==5, product],
                       allPredictions[order_predict==6, product],
                       allPredictions[order_predict==7, product])

# Check for ties in the ordering (should not occur)
if(length(productString) != nrow(testData)) browser()

# Add the id and top 7 to the submission file
submission <- data.frame(ncodpers = testData$ncodpers,
                         added_products = productString)

# Extract template submission file
paddedSubmission <- fread("Data/sample_submission.csv")

# Set the place id to an empty character string
paddedSubmission[, added_products := ""]

# Replace the matched ids in padded submission by the combined submission file
matchIds <- match(submission$ncodpers, paddedSubmission$ncodpers)
paddedSubmission[matchIds, added_products := submission$added_products]

# Write the padded submission to a csv file
write.csv(paddedSubmission, file.path(getwd(), "Submission", submissionDate,
                                      submissionFile),
          row.names = FALSE)

# Display the successful submission message
cat("Submission file created successfully!\n",
    nrow(submission)," records were predicted (",
    round(nrow(submission)/nrow(paddedSubmission)*100,2),"%)\n", sep="")