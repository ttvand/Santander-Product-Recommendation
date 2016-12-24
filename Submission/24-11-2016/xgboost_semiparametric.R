####################################
# XGBOOST semiparametric learner 1 #
####################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)

# Submission date and file name
submissionDate <- "24-11-2016"
submissionFile <- "xgboost semiparametric try 1 exponential normalisation"

# Target date 
targetDate <- "12-11-2016"

# Target train models
# targetTrainModels <- "trainBack11-0SmallOrdered"
targetTrainModels <- "testTrainAll"

# Target features file name
testFn <- "test/Lag17 features.rds"

# Option to store the product predictions
savePredictions <- TRUE

# Prediction subfolder
predictionsFolder <- "Predictions"

# Use the relative frequency of the different products in June 2016
normalizeProdProbs <- TRUE
normalizeMode <- c("additive", "linear", "exponential")[3]
additiveNormalizeProds <- NULL #c("ind_cco_fin_ult1")
fractionPosFlankUsers <- 0.035
expectedCountPerPosFlank <- 1.25

# Option to set the prediction to ind_cco_fin_ult1 for users with no historical
# positive flanks
ccoNoPurchase <- FALSE

# Zero probability target variable names
zeroTargets <- NULL
# zeroTargets <- c("ind_deco_fin_ult1", "ind_dela_fin_ult1")
# zeroTargets <- c("ind_deco_fin_ult1", "ind_dela_fin_ult1",
# "ind_deme_fin_ult1", "ind_fond_fin_ult1")

# Option to predict a subset of the test data
predictSubset <- FALSE
# predictFirst <- 1e5

# Source the exponential normalisation
source("Common/exponentialNormaliser.R")


######################################################################

# Create predictions subfolder
# Create the target folder if it does not exist yet
predictionsPath <- file.path(getwd(), "Submission", submissionDate,
                             predictionsFolder)
dir.create(predictionsPath, showWarnings = FALSE)

# Extract clients with positive flanks
posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                               "positive flank clients.rds")
posFlankClients <- readRDS(posFlankClientsFn)

# Path to the xgboost models
modelsPath <- file.path(getwd(), "First level learners", targetDate,
                        targetTrainModels)

# Load the base models
baseModels <- list.files(modelsPath)
baseModelNames <- gsub("[.]rds$", "", baseModels)
allModels <- lapply(baseModels, function(x) readRDS(file.path(modelsPath, x)))
names(allModels) <- baseModelNames

# Load the test data
testData <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                              testFn))

# Optionally subset the test data
if(predictSubset){
  testData <- testData[1:predictFirst]
}

# Calculate which test records had at least one positive flank
testDataPosFlank <- testData$ncodpers %in% posFlankClients

# Load the validation data in order to know how to rearrange the target columns
trainFn <- "trainSmallOrdered/Back11Lag5 features.rds"
colOrderData <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                  trainFn))

# Reorder the train models based on the target indicator columns in the 
# validation data
targetCols <- grep("^ind_.*_ult1$", names(colOrderData), value=TRUE)
nbBaseModels <- length(targetCols)
modelOrder <- match(targetCols, baseModelNames)
models <- allModels[modelOrder]
baseModelNames <- baseModelNames[modelOrder]

# Verify the correct reordering of the base models
if(!all(baseModelNames == targetCols)) browser()
if(!all(names(models) == targetCols)) browser()

# Load the estimated relative map contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# Loop over all base models
probMultipliers <- rep(NA, nbBaseModels)
for(i in 1:nbBaseModels){
  # Extract the target column
  targetVar <- targetCols[i]
  targetModel <- models[[i]]
  
  # Another check that we are using the right model - better safe than sorry :)
  if(targetModel$targetVar != targetVar) browser()
  
  # Show progress message
  cat("Generating test predictions for model", i, "of", nbBaseModels, "\n")
  
  # Set the predictions to zero if the target variable is in the zeroed list
  if(targetVar %in% zeroTargets){
    predictions <- rep(0, nrow(testData))
  } else{
    # Extract predictors data from the features data
    predictorData <- testData[, targetModel$predictors, with=FALSE]
    
    # Convert the predictor data to a matrix
    predictorDataM <- data.matrix(predictorData)
    
    # Calculate the test predictions
    predictions <- predict(targetModel$model, predictorDataM)
    alreadyOwned <- is.na(testData[[paste0(targetVar, "Lag1")]]) |
      testData[[paste0(targetVar, "Lag1")]] == 1
    predictionsPrevNotOwned <- predictions[!alreadyOwned]
    
    # Optionally, multiply the predictions by the relative ratio of June 2016
    if(normalizeProdProbs){
      predictedPosFlankCount <- sum(predictionsPrevNotOwned)
      probMultiplier <- nrow(testData) * fractionPosFlankUsers *
        expectedCountPerPosFlank * countContributions[17, i] /
        predictedPosFlankCount
      probMultipliers[i] <- probMultiplier
      # if(i %in% c(3, 5, 7, 13, 18, 19, 22, 23, 24)) browser()
      if(is.finite(probMultiplier)){
        if(normalizeMode == "additive" ||
           targetVar %in% additiveNormalizeProds){
          predictions[!alreadyOwned] <- predictions[!alreadyOwned] +
            (probMultiplier-1)*mean(predictions[!alreadyOwned])
        } else{
          if(normalizeMode == "linear"){
            predictions[!alreadyOwned] <- predictions[!alreadyOwned] *
              probMultiplier
          } else{
            predictions[!alreadyOwned] <- probExponentNormaliser(
              predictions[!alreadyOwned], probMultiplier)
          }
        }
      }
    }
  }
  
  # Set the predictions to -1 for products that are already owned
  # predictions[alreadyOwned] <- -runif(sum(alreadyOwned))
  predictions[alreadyOwned] <- 0
  
  # The mean prediction should equal the mean map contribution if the
  # predictions are set to zero for the already owned products
  # mean(predictions)/mapContributions[17, i] 
  
  # Add the predictions to the data table with all target predictions
  predictionsDT <- data.table(ncodpers = testData$ncodpers,
                              prediction = predictions, product = targetVar)
  if(i==1){
    allPredictions <- predictionsDT
  } else{
    allPredictions <- rbindlist(list(allPredictions, predictionsDT), fill=TRUE)
  }
}

# Option to set the prediction to ind_cco_fin_ult1 for users with no historical
# positive flanks
if(ccoNoPurchase){
  allPredictions[!ncodpers %in% posFlankClients & product == "ind_cco_fin_ult1",
                 prediction := 10]
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

# Calculate the top predicted products counts
topPredictions <- allPredictions[order_predict==1, .N, product]
topPredictions <- topPredictions[order(-N)]
topPredictionsPosFlanks <- allPredictions[order_predict==1 &
                                            ncodpers %in% posFlankClients,
                                          .N, product]
topPredictionsPosFlanks <- topPredictionsPosFlanks[order(-N)]

# Study the ranking of specific products
productRankDelaFin <- allPredictions[product=="ind_dela_fin_ult1", .N,
                                     order_predict]
productRankDelaFin <- productRankDelaFin[order(order_predict),]

productRankDecoFin <- allPredictions[product=="ind_deco_fin_ult1", .N,
                                     order_predict]
productRankDecoFin <- productRankDecoFin[order(order_predict),]

productRankTjcrFin <- allPredictions[product=="ind_tjcr_fin_ult1", .N,
                                     order_predict]
productRankTjcrFin <- productRankTjcrFin[order(order_predict),]

productRankRecaFin <- allPredictions[product=="ind_reca_fin_ult1", .N,
                                     order_predict]
productRankRecaFin <- productRankRecaFin[order(order_predict),]

# Verify that the mean prediction aligns with the relative June 15 ratio
meanPreds <- allPredictions[prediction>0,
                            .(meanProb = mean(prediction)), product]
meanPreds <- meanPreds[order(-meanProb), ]

# Verify that the mean prediction aligns with the relative June 15 ratio
newProdPredictions <- rep(1, nrow(testData))
allPredictions[, totalProb := prediction * rep(newProdPredictions,
                                               each = nbBaseModels)]
meanProductProbs <- allPredictions[, .(meanCondProb = mean(prediction),
                                       meanProb = mean(totalProb)), product]
meanProductProbs <- meanProductProbs[order(-meanCondProb), ]

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

# Set the added products to an empty character string
paddedSubmission[, added_products := ""]

# Replace the matched ids in padded submission by the combined submission file
matchIds <- match(submission$ncodpers, paddedSubmission$ncodpers)
paddedSubmission[matchIds, added_products := submission$added_products]

# Write the padded submission to a csv file
write.csv(paddedSubmission, file.path(getwd(), "Submission", submissionDate,
                                      paste0(submissionFile, ".csv")),
          row.names = FALSE)

# Save the predictions to the predictions folder
if(savePredictions){
  saveRDS(allPredictions, file=file.path(predictionsPath,
                                         paste0(submissionFile, ".rds")))
}

# Display the successful submission message
cat("Submission file created successfully!\n",
    nrow(submission)," records were predicted (",
    round(nrow(submission)/nrow(paddedSubmission)*100,2),"%)\n", sep="")