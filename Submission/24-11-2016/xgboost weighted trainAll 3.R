####################################################################
# XGBOOST weighted predictions 3 - using all observations and thus #
# dropping the marginal distributions from the calculations        #
####################################################################

# When training on all data we are modeling the joint probability so there
# is no need to calculate the predicted count as marginal * P(prod|posFlank)

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
loadFile <- "xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, linear normalisation joint"
submissionFile <- "xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint"

# Target date 
targetDate <- "12-11-2016"

# Target train model folders
# trainModelsFolder <- "trainSmallOrderedTrainAll"
trainModelsFolder <- "trainTrainAll"

# Target feature files folder
testFeaturesFolder <- "test"

# Option to store the product predictions
loadPredictions <- TRUE
savePredictions <- TRUE
savePredictionsBeforeNormalisation <- TRUE

# Use the relative frequency of the different products in June 2016
normalizeProdProbs <- TRUE
normalizeMode <- c("additive", "linear", "exponential")[3]
additiveNormalizeProds <- NULL #c("ind_cco_fin_ult1")
fractionPosFlankUsers <- 0.035
expectedCountPerPosFlank <- 1.25

# List the month back weights - give more weight to models from 12 months back
monthsBackModels <- 0:11
# monthsBackModelsWeights <- rev(c(12, 0.1*(5:15)))
monthsBackModelsWeights <- rev(c(12, 0.1*(15:25)))
# monthsBackModelsWeights <- rev(c(12, rep(3, 11)))
weightSum <- sum(monthsBackModelsWeights)
monthsBackLags <- 16:5

# Option to predict a subset of the test data
predictSubset <- FALSE
# predictFirst <- 1e5

# Weight health checks
nbLags <- length(monthsBackModelsWeights)
if(nbLags != length(monthsBackModels) ||
   nbLags != length(monthsBackLags)) browser()

# Prediction subfolder
predictionsFolder <- "Predictions"

# Option to set the prediction to ind_cco_fin_ult1 for users with no historical
# positive flanks
ccoNoPurchase <- FALSE

# Zero probability target variable names
zeroTargets <- NULL
# zeroTargets <- c("ind_deco_fin_ult1", "ind_dela_fin_ult1")
# zeroTargets <- c("ind_deco_fin_ult1", "ind_dela_fin_ult1",
# "ind_deme_fin_ult1", "ind_fond_fin_ult1")

# Source the exponential normalisation
source("Common/exponentialNormaliser.R")


######################################################################

# Create predictions subfolder
# Create the target folder if it does not exist yet
predictionsPath <- file.path(getwd(), "Submission", submissionDate,
                             predictionsFolder)
dir.create(predictionsPath, showWarnings = FALSE)
if(loadPredictions){
  rawPredictionsPath <- file.path(predictionsPath,
                                  paste0("prevNorm", loadFile, ".rds"))
} else{
  rawPredictionsPath <- file.path(predictionsPath,
                                  paste0("prevNorm", submissionFile, ".rds"))
}

# Extract clients with positive flanks
posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                               "positive flank clients.rds")
posFlankClients <- readRDS(posFlankClientsFn)

# Path to the xgboost train models
modelsBasePath <- file.path(getwd(), "First level learners", targetDate,
                            trainModelsFolder)
modelGroups <- list.dirs(modelsBasePath)[-1]
nbModelGroups <- length(modelGroups)

# Construct a data table with information on the base models: the number of
# months back, the weight, the target variable and the path to the model
baseModelInfo <- NULL
baseModels <- list()
for(i in 1:nbModelGroups){
  # List the files in the considered model group
  modelGroup <- modelGroups[i]
  slashPositions <- gregexpr("\\/", modelGroup)[[1]]
  modelGroupExtension <- substring(modelGroup,
                                   1 + slashPositions[length(slashPositions)])
  modelGroupFiles <- list.files(modelGroup)
  nbModels <- length(modelGroupFiles)
  monthsBack <- as.numeric(substring(gsub("Lag.*$", "", modelGroupExtension),
                                     5))
  lag <- as.numeric(gsub("^.*Lag", "", modelGroupExtension))
  relativeWeight <- monthsBackModelsWeights[match(monthsBack, monthsBackModels)]
  
  # Loop over all models
  for(j in 1:nbModels){
    modelInfo <- readRDS(file.path(modelGroup, modelGroupFiles[j]))
    baseModelInfo <- rbind(baseModelInfo,
                           data.table(
                             modelGroupExtension = modelGroupExtension,
                             targetProduct = modelInfo$targetVar,
                             monthsBack = monthsBack,
                             modelLag = lag,
                             relativeWeight = relativeWeight)
    )
    baseModels <- c(baseModels, list(modelInfo))
  }
}
baseModelInfo[, modelId := 1:nrow(baseModelInfo)]
baseModelInfo <- baseModelInfo[order(monthsBack), ]

# Extract the base model names
baseModelNames <- baseModelInfo[monthsBack==0, targetProduct]
# baseModels <- list.files(modelsPath)
# baseModelNames <- gsub("[.]rds$", "", baseModels)
# allModels <- lapply(baseModels, function(x) readRDS(file.path(modelsPath, x)))
# names(allModels) <- baseModelNames

# Load the test data with lag five
testDataLag <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                 testFeaturesFolder, "Lag5 features.rds"))

# Optionally subset the test data
if(predictSubset){
  testDataLag <- testDataLag[1:predictFirst]
}

# Calculate which test records had at least one positive flank
testDataPosFlank <- testDataLag$ncodpers %in% posFlankClients

# Load the validation data in order to know how to rearrange the target columns
trainFn <- "trainSmallOrdered/Back11Lag5 features.rds"
colOrderData <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                  trainFn))
targetCols <- grep("^ind_.*_ult1$", names(colOrderData), value=TRUE)
nbBaseModels <- length(targetCols)

# Load the estimated relative count contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# Optionally load the predictions before normalisation if they are available
if(loadPredictions && file.exists(rawPredictionsPath)){
  allPredictions <- readRDS(rawPredictionsPath)
} else{
  # Loop over all lags and base models
  allPredictions <- NULL
  for(lagId in 1:nbLags){
    # Show progress message
    cat("\nGenerating positive flank predictions for lag", lagId, "of", nbLags,
        "@", as.character(Sys.time()), "\n\n")
    
    # Set the lag weight and the number of train months back
    lag <- monthsBackLags[lagId]
    weight <- monthsBackModelsWeights[lagId]
    # monthsBack <- monthsBackModels[lagId]
    
    # Load the test data with the appropriate lag
    testDataLag <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                     testFeaturesFolder,
                                     paste0("Lag", lag, " features.rds")))
    
    # Optionally subset the test data
    if(predictSubset){
      testDataLag <- testDataLag[1:predictFirst]
    }
    
    for(i in 1:nbBaseModels){
      # Extract the target column
      targetVar <- targetCols[i]
      targetModelId <- baseModelInfo[targetProduct==targetVar &
                                       modelLag==lag, modelId]
      targetModel <- baseModels[[targetModelId]]
      
      # Another check that we are using the right model - better safe than sorry :)
      if(targetModel$targetVar != targetVar) browser()
      
      # Show progress message
      cat("Generating test predictions for model", i, "of", nbBaseModels, "\n")
      
      # Set the predictions to zero if the target variable is in the zeroed list
      if(targetVar %in% zeroTargets){
        predictions <- rep(0, nrow(testDataLag))
      } else{
        # Extract predictors data from the features data
        predictorData <- testDataLag[, targetModel$predictors, with=FALSE]
        
        # Convert the predictor data to a matrix
        predictorDataM <- data.matrix(predictorData)
        
        # Calculate the test predictions
        predictions <- predict(targetModel$model, predictorDataM)
        alreadyOwned <- is.na(testDataLag[[paste0(targetVar, "Lag1")]]) |
          testDataLag[[paste0(targetVar, "Lag1")]] == 1
        predictionsPrevNotOwned <- predictions[!alreadyOwned]
      }
      
      # Set the predictions to 0 for products that are already owned
      # predictions[alreadyOwned] <- -runif(sum(alreadyOwned))
      predictions[alreadyOwned] <- 0
      
      # The mean prediction should equal the mean map contribution if the
      # predictions are set to zero for the already owned products
      # mean(predictions)/mapContributions[17, i] 
      
      # Add the predictions to the data table with all target predictions
      predictionsDT <- data.table(ncodpers = testDataLag$ncodpers,
                                  weightedPrediction = predictions*weight,
                                  product = targetVar)
      if(targetVar %in% allPredictions$product){
        allPredictions[product==targetVar, weightedPrediction:=
                         weightedPrediction + predictionsDT$weightedPrediction]
      } else{
        allPredictions <- rbind(allPredictions, predictionsDT)
      }
    }
  }
  
  # Divide the weighted summed predictions by the weight sum
  allPredictions[, prediction := weightedPrediction / weightSum]
  allPredictions[, weightedPrediction := NULL]
  # meanConditionalProb <- mean(allPredictions$prediction)*24
  
  # Save the predictions to the predictions folder before normalisation
  if(savePredictionsBeforeNormalisation){
    saveRDS(allPredictions, file=rawPredictionsPath)
  }
}

# Optionally, multiply the predictions by the relative count ratio of June 2016
probMultipliers <- rep(NA, nbBaseModels)
if(normalizeProdProbs){
  for(i in 1:nbBaseModels){
    # Extract the target column
    targetVar <- targetCols[i]
    
    # Look up if the target variable was already owned
    alreadyOwned <- is.na(testDataLag[[paste0(targetVar, "Lag1")]]) |
      testDataLag[[paste0(targetVar, "Lag1")]] == 1
    predictions <- allPredictions[product==targetVar, prediction]
    predictionsPrevNotOwned <- predictions[!alreadyOwned]
    if(suppressWarnings(max(predictions[alreadyOwned]))>0) browser()
    
    # Normalize the predicted probabilities
    predictedPosFlankCount <- sum(predictionsPrevNotOwned)
    probMultiplier <- nrow(testDataLag) * fractionPosFlankUsers *
      expectedCountPerPosFlank * countContributions[17, i] /
      predictedPosFlankCount
    probMultipliers[i] <- probMultiplier
    if(i %in% c(3, 5, 7, 13, 18, 19, 22, 23, 24)) browser()
    if(is.finite(probMultiplier)){
      if(normalizeMode == "additive" || targetVar %in% additiveNormalizeProds){
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
      # Update the predictions in allPredictions
      allPredictions[product==targetVar, prediction:=predictions]
    }
  }
}

# Option to set the prediction to ind_cco_fin_ult1 for users with no historical
# positive flanks AND cco_fin at zero in the previous time period
if(ccoNoPurchase){
  allPredictions[!ncodpers %in% posFlankClients &
                   ncodpers %in% testDataLag[ind_cco_fin_ult1Lag1==0, ncodpers] &
                   product == "ind_cco_fin_ult1",
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
newProdPredictions <- rep(1, nrow(testDataLag))
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
if(length(productString) != nrow(testDataLag)) browser()

# Add the id and top 7 to the submission file
submission <- data.frame(ncodpers = testDataLag$ncodpers,
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
    round(nrow(submission)/nrow(paddedSubmission)*100,2), "%)\n", sep="")