#######################
# XGBOOST weighted 17 #
#######################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)
library(stringr)

# Submission date and file name
submissionDate <- "15-12-2016"
loadFile <- "xgboost weighted trainAll 16 10 folds 200 rounds, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint"
submissionFile <- "xgboost weighted trainAll 17, reca jun 15 52 weight, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint"

# Target date 
targetDate <- "12-11-2016"

# Target train model folders
trainModelsFolder <- "trainTrainAll Top 100 monthProduct 200 rounds"
trainAll <- grepl("TrainAll", trainModelsFolder)

# Target feature files folder
# testFeaturesFolder <- "test"
testFeaturesFolder <- "testNoStagnantRemoval"

# Option to store the product predictions
loadPredictions <- FALSE # If loadPredictions TRUE... 
loadBaseModelPredictions <- TRUE # ... loadBaseModelPredictions is ignored
savePredictions <- TRUE
saveBaseModelPredictions <- TRUE
savePredictionsBeforeNormalisation <- TRUE

# Linear target variable multipliers
linearMultipliers <- rep(1, 24)
linearMultipliers[18] <- 1

# Option to drop models that were trained on a subset of the data
dropFoldModels <- TRUE
foldRelativeWeight <- 0.9

# Option to drop bootstrap models
dropBootModels <- FALSE
onlyBootModels <- FALSE # Ignored if dropBootModels is TRUE

# Use the relative frequency of the different products in June 2016
normalizeProdProbs <- TRUE
normalizeMode <- c("additive", "linear", "exponential")[3]
additiveNormalizeProds <- NULL #c("ind_cco_fin_ult1")
fractionPosFlankUsers <- 0.035114
expectedCountPerPosFlank <- 1.25

# Marginal normalisation approach - not considered if trainAll
marginalNormalisation <- c("linear", "exponential")[2]

# List the total product weights over all months
weightSum <- 1 # sum(monthsBackModelsWeights)

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

# Option to predict a subset of the test data
predictSubset <- FALSE
# predictSubsetCount <- 5e4

# Prediction subfolder
predictionsFolder <- "Predictions"

# Zero probability target variable names
zeroTargets <- NULL
# zeroTargets <- c("ind_deco_fin_ult1", "ind_dela_fin_ult1")
# zeroTargets <- c("ind_deco_fin_ult1", "ind_dela_fin_ult1",
# "ind_deme_fin_ult1", "ind_fond_fin_ult1")

# Source the exponential normalisation and weights extraction
source("Common/exponentialNormaliser.R")
source("Common/getModelWeights.R")

# Load the target product weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights first.rds"))


######################################################################

# Create predictions subfolder
# Create the target folder if it does not exist yet
predictionsPath <- file.path(getwd(), "Submission", submissionDate,
                             predictionsFolder)
dir.create(predictionsPath, showWarnings = FALSE)

# Create model predictions subfolder
if(saveBaseModelPredictions){
  baseModelPredictionsPath <- file.path(predictionsPath, submissionFile)
  dir.create(baseModelPredictionsPath, showWarnings = FALSE)
}
if(loadBaseModelPredictions){
  baseModelPredictionsPath <- file.path(predictionsPath, loadFile)
}

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
modelGroups <- modelGroups[!grepl("Manual tuning", modelGroups)]
modelGroups <- modelGroups[!grepl("no fold BU", modelGroups)]#[-c(6,7)]
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
  modelGroupFiles <- modelGroupFiles[!grepl("no fold BU", modelGroupFiles)]
  
  # Option to drop folds of model group files (trained on a subset of the
  # train data)
  if(dropFoldModels){
    modelGroupFiles <- modelGroupFiles[!grepl("Fold", modelGroupFiles)]
  }
  
  # Option to drop bootstrap model replicates
  if(dropBootModels){
    modelGroupFiles <- modelGroupFiles[!grepl("Boot", modelGroupFiles)]
  } else{
    if(onlyBootModels){
      modelGroupFiles <- modelGroupFiles[grepl("Boot", modelGroupFiles)]
    }
  }
  
  nbModels <- length(modelGroupFiles)
  monthsBack <- suppressWarnings(
    as.numeric(substring(gsub("Lag.*$", "", modelGroupExtension), 5)))
  lag <- suppressWarnings(as.numeric(gsub("^.*Lag", "", modelGroupExtension)))
  # relativeWeightOrig <- monthsBackModelsWeights[match(monthsBack,
  #                                                     monthsBackModels)]
  # weightDate <- monthsBackWeightDates[match(monthsBack, monthsBackModels)]
  
  # Loop over all models
  if(nbModels>0){
    for(j in 1:nbModels){
      modelGroupFile <- modelGroupFiles[j]
      shortFn <- gsub(".rds$", "", modelGroupFile)
      modelInfo <- readRDS(file.path(modelGroup, modelGroupFile))
      targetProduct <- modelInfo$targetVar
      
      # Load the product - month weight
      relativeWeight <- getModelWeights(monthsBack, targetProduct,
                                        dateTargetWeights)
      
      # Calculate the fold model weight
      isFold <- grepl("Fold", modelGroupFile)
      # Adjust fold weights because some models didn't store the fifth fold
      prodMonthFiles <- modelGroupFiles[grepl(targetProduct, modelGroupFiles)]
      nbFoldsProd <- sum(grepl("Fold", prodMonthFiles))
      prodMonthFiles <- modelGroupFiles[grepl(targetProduct, modelGroupFiles)]
      nbFoldsProd <- sum(grepl("Fold", prodMonthFiles))
      foldBaseWeight <- foldRelativeWeight * 4 / nbFoldsProd
      if(!is.finite(foldBaseWeight)){
        foldBaseWeight <- 0
      }
      productMonthSum <- 1 + nbFoldsProd*foldBaseWeight
      if(isFold){
        # Adjust fold weights because some models didn't store the fifth fold
        foldModelWeight <- foldBaseWeight/productMonthSum
      } else{
        foldModelWeight <- 1/productMonthSum
      }
      
      # Append the model info
      baseModelInfo <- rbind(baseModelInfo,
                             data.table(
                               modelGroupExtension = modelGroupExtension,
                               shortFn = shortFn,
                               targetProduct = targetProduct,
                               monthsBack = monthsBack,
                               modelLag = lag,
                               relativeWeight = relativeWeight * foldModelWeight)
      )
      baseModels <- c(baseModels, list(modelInfo))
    }
  }
}
baseModelInfo[, modelId := 1:nrow(baseModelInfo)]

# Extract the number of marginal/joint/conditional lags and months back
# Set the base model info to default settings when the base models are 
# trained over multiple month periods
if(all(is.na(baseModelInfo$modelLag))){
  nbGroups <- length(unique(baseModelInfo$modelGroupExtension))
  baseModelInfo <- baseModelInfo[order(targetProduct), ]
  # baseModelInfo$monthsBack <- -(1:nbGroups)
  baseModelInfo$modelLag <- 5
  baseModelInfo$relativeWeight <- 1
  monthsBackLags <- rep(defaultTestLag, nbGroups)
  nbMarginalLags <- length(monthsBackLags)
  nbConditionalLags <- 1
} else{
  monthsBackLags <- rev(sort(unique(baseModelInfo$modelLag)))
  nbMarginalLags <- length(monthsBackLags)
  nbConditionalLags <- length(monthsBackLags)
}

# Normalize the base model weights (necessary since some weights might be set
# to zero)
uniqueBaseModels <- sort(unique(baseModelInfo$targetProduct))
for(i in 1:length(uniqueBaseModels)){
  productIds <- baseModelInfo$targetProduct==uniqueBaseModels[i]
  productWeightSum <- baseModelInfo[productIds, sum(relativeWeight)]
  normalizeWeightRatio <- weightSum/productWeightSum
  baseModelInfo[productIds, relativeWeight := relativeWeight*
                  normalizeWeightRatio]
}

baseModelInfo <- baseModelInfo[order(monthsBack), ]

# Extract the base model names
baseModelNames <- unique(baseModelInfo[monthsBack==0, targetProduct])
# baseModels <- list.files(modelsPath)
# baseModelNames <- gsub("[.]rds$", "", baseModels)
# allModels <- lapply(baseModels, function(x) readRDS(file.path(modelsPath, x)))
# names(allModels) <- baseModelNames

# Load the test data with lag one
testDataLag <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                 testFeaturesFolder, "Lag1 features.rds"))

# Optionally subset the test data
if(predictSubset){
  predictSubsetIds <- sort(sample(1:nrow(testDataLag), predictSubsetCount))
  testDataLag <- testDataLag[predictSubsetIds]
}

# Calculate which test records had at least one positive flank
testDataPosFlank <- testDataLag$ncodpers %in% posFlankClients

# Load the validation data in order to know how to rearrange the target columns
trainFn <- "train/Back15Lag1 features.rds"
colOrderData <- readRDS(file.path(getwd(), "Feature engineering",
                                  targetDate, trainFn))
targetCols <- grep("^ind_.*_ult1$", names(colOrderData), value=TRUE)
rm(colOrderData)
gc()
nbBaseModels <- length(targetCols)

# Load the estimated relative count contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# Predict if there will be any positive flanks
if(!trainAll){
  posFlankModelInfo <- baseModelInfo[targetProduct=="hasNewProduct"]
  newProdPredictions <- rep(0, nrow(testDataLag))
  if(nrow(posFlankModelInfo) != nbMarginalLags) browser()
  for(i in 1:nbMarginalLags){
    # Show progress message
    cat("Generating new product predictions for lag", i, "of", nbMarginalLags,
        "\n")
    
    lag <- posFlankModelInfo[i, modelLag]
    weight <- posFlankModelInfo[i, relativeWeight]
    newProdModel <- baseModels[[posFlankModelInfo[i, modelId]]]
    
    # Load the test data with the appropriate lag
    testDataLag <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                     testFeaturesFolder,
                                     paste0("Lag", lag, " features.rds")))
    
    # Optionally subset the test data
    if(predictSubset){
      testDataLag <- testDataLag[predictSubsetIds]
    }
    
    predictorData <- testDataLag[, newProdModel$predictors, with=FALSE]
    predictorDataM <- data.matrix(predictorData)
    rm(predictorData)
    gc()
    newProdPredictionsLag <- predict(newProdModel$model, predictorDataM)
    newProdPredictions <- newProdPredictions + newProdPredictionsLag*weight
  }
  
  # Rescale the weighted sum to the [0, 1] interval
  newProdPredictions <- newProdPredictions/weightSum
  
  # Calculate the mean predictions depending on the May 2015 flag
  meanGroupPredsMayFlag <-
    c(mean(newProdPredictions[testDataLag$hasMay15Data==0]),
      mean(newProdPredictions[testDataLag$hasMay15Data==1]))
  
  # Calculate the mean predictions depending on the hasAnyPosFlank flag
  meanGroupPredsPosFlank <- c(mean(newProdPredictions[!testDataPosFlank]),
                              mean(newProdPredictions[testDataPosFlank]))
  
  # Compare the number of expected positive flanks versus the extrapolated
  # public leaderboard counts
  expectedPosFlanks <- sum(newProdPredictions)
  leaderboardPosFlanks <- fractionPosFlankUsers*nrow(testDataLag)
  normalisedProbRatio <- leaderboardPosFlanks/expectedPosFlanks
  cat("Expected/leaderboard positive flank ratio",
      round(1/normalisedProbRatio, 2), "\n")
  
  # Normalize the marginal probabilities such that the expected number of
  # products with a positive flanks matches the extrapolated public leaderboard
  # count
  if(marginalNormalisation == "linear"){
    newProdPredictions <- newProdPredictions * normalisedProbRatio
  } else{
    newProdPredictions <- probExponentNormaliser(newProdPredictions,
                                                 normalisedProbRatio)
  }
} else{
  newProdPredictions <- rep(1, nrow(testDataLag))
}

# Optionally load the predictions before normalisation if they are available
if(loadPredictions && file.exists(rawPredictionsPath)){
  allPredictions <- readRDS(rawPredictionsPath)
} else{
  # Loop over all lags and base models
  allPredictions <- NULL
  for(lagId in 1:nbConditionalLags){
    # Show progress message
    cat("\nGenerating positive flank predictions for lag", lagId, "of",
        nbConditionalLags, "@", as.character(Sys.time()), "\n\n")
    
    # Set the lag weight and the number of train months back
    lag <- monthsBackLags[lagId]
    # monthsBack <- monthsBackModels[lagId]
    
    # Calculate if the test data lag should be loaded
    loadTestDataLag <- TRUE
    baseModelPredPaths <- file.path(baseModelPredictionsPath,
                                    paste0(targetCols, " Lag ", lag, ".rds"))
    if(loadBaseModelPredictions && exists("baseModelPredictionsPath")){
      loadTestDataLag <- !all(sapply(baseModelPredPaths, file.exists))
    }
    
    # Load the test data with the appropriate lag
    if(loadTestDataLag){
      testDataLag <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                                       testFeaturesFolder,
                                       paste0("Lag", lag, " features.rds")))
      
      # Optionally subset the test data
      if(predictSubset){
        testDataLag <- testDataLag[predictSubsetIds]
      }
    }
    
    for(i in 1:nbBaseModels){
      # Extract the target column
      targetVar <- targetCols[i]
      targetModelIds <- baseModelInfo[targetProduct==targetVar &
                                        modelLag==lag, modelId]
      
      # Show progress message
      cat("Generating test predictions for model", i, "of", nbBaseModels, "\n")
      
      # Optionally, load the base model predictions
      if(exists("baseModelPredictionsPath")){
        baseModelPredPath <- file.path(baseModelPredictionsPath,
                                       paste0(targetVar, " Lag ", lag, ".rds"))
      } else{
        baseModelPredPath <- ""
      }
      foldWeights <- baseModelInfo[modelId %in% targetModelIds,
                                   relativeWeight]
      weight <- sum(foldWeights)
      loadFileExists <- file.exists(baseModelPredPath)
      if(loadBaseModelPredictions && loadFileExists){
        predictionsDT <- readRDS(baseModelPredPath)
      } else{
        # Set the predictions to zero if the target variable is in the zeroed
        # list
        if(targetVar %in% zeroTargets || weight <= 0){
          predictions <- rep(0, nrow(testDataLag))
        } else{
          nbTargetModelFolds <- length(targetModelIds)
          foldPredictions <- rep(0, nrow(testDataLag))
          
          alreadyOwned <- is.na(testDataLag[[paste0(targetVar, "Lag1")]]) |
            testDataLag[[paste0(targetVar, "Lag1")]] == 1
          
          # Extract predictors data from the features data
          predictorData <-
            testDataLag[!alreadyOwned,
                        baseModels[[targetModelIds[1]]]$predictors, with=FALSE]
          
          # Convert the predictor data to a matrix
          predictorDataM <- data.matrix(predictorData)
          rm(predictorData)
          gc()
          for(fold in 1:nbTargetModelFolds){
            targetModelId <- targetModelIds[fold]
            
            # Loop over all folds and sum the predictions
            targetModel <- baseModels[[targetModelId]]
            
            # Extract the model weight
            weightFold <- foldWeights[fold]
            # if(weight == 0) browser()
            
            # Another check that we are using the right model
            # Better safe than sorry :)
            if(targetModel$targetVar != targetVar) browser()
            
            # Calculate the test predictions
            predictionsPrevNotOwnedFold <- predict(targetModel$model,
                                                   predictorDataM)
            foldPredictions[!alreadyOwned] <- foldPredictions[!alreadyOwned] +
              predictionsPrevNotOwnedFold*weightFold
          }
          # if(targetVar == "ind_reca_fin_ult1") browser()
          predictions <- foldPredictions/weight
          
          # Set the predictions to 0 for products that are already owned
          # predictions[alreadyOwned] <- -runif(sum(alreadyOwned))
          predictions[alreadyOwned] <- 0
        }
        
        # The mean prediction should equal the mean map contribution if the
        # predictions are set to zero for the already owned products
        # mean(predictions)/mapContributions[17, i] 
        
        # Add the predictions to the data table with all target predictions
        predictionsDT <- data.table(ncodpers = testDataLag$ncodpers,
                                    predictions = predictions,
                                    product = targetVar)
      }
      
      predictionsDT[, weightedPrediction :=
                      predictionsDT$predictions*weight]
      
      # if(targetVar == "ind_reca_fin_ult1") browser()
      # c(lag, sum(predictionsDT$predictions), sum(testDataLag[[19+24*(16-lag)]], na.rm=T))
      
      # if(linearMultipliers[i] != 1) browser()
      
      if(targetVar %in% allPredictions$product){
        allPredictions[product==targetVar, weightedPrediction:=
                         weightedPrediction +
                         (predictionsDT$weightedPrediction *
                            linearMultipliers[i])]
      } else{
        allPredictions <- rbind(allPredictions, predictionsDT)
        # Linear multiplication of the base predictions
        allPredictions[product == targetVar, predictions :=
                         predictions * linearMultipliers[i]]
        allPredictions[product == targetVar, weightedPrediction :=
                         weightedPrediction * linearMultipliers[i]]
      }
      
      # Save the base model predictions
      if(saveBaseModelPredictions && (!loadBaseModelPredictions ||
                                      (loadBaseModelPredictions &&
                                       !loadFileExists))){
        predictionsDT[, weightedPrediction:=NULL]
        saveRDS(predictionsDT, baseModelPredPath)
      }
    }
  }
  
  # Divide the weighted summed predictions by the weight sum
  allPredictions[, prediction := weightedPrediction / weightSum]
  allPredictions[, weightedPrediction := NULL]
  allPredictions[, predictions := NULL]
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
    # Show progress message
    cat("Normalizing product predictions", i, "of", nbBaseModels, "\n")
    
    # Extract the target column
    targetVar <- targetCols[i]
    
    # Look up if the target variable was already owned
    alreadyOwned <- is.na(testDataLag[[paste0(targetVar, "Lag1")]]) |
      testDataLag[[paste0(targetVar, "Lag1")]] == 1
    predictions <- allPredictions[product==targetVar, prediction]
    predictionsPrevNotOwned <- predictions[!alreadyOwned]
    if(suppressWarnings(max(predictions[alreadyOwned]))>0) browser()
    
    # Normalize the predicted probabilities
    predictedPosFlankCount <- sum(predictionsPrevNotOwned *
                                    newProdPredictions[!alreadyOwned])
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
            predictions[!alreadyOwned], probMultiplier,
            weights=newProdPredictions[!alreadyOwned])
        }
      }
      # Update the predictions in allPredictions
      allPredictions[product==targetVar, prediction:=predictions]
    }
  }
}

# Order the predicted probabilities for all products by client
setkey(allPredictions, ncodpers)
allPredictions[,order_predict := match(1:length(prediction),
                                       order(-prediction)), by=ncodpers]
allPredictions <- allPredictions[order(ncodpers, -prediction), ]

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
if(nomPensAboveNominaBothNotOwned){
  # Find users where the rank of nomina < rank of nom pens and both prob not
  # zero
  ncodpers <- unique(allPredictions$ncodpers)
  nominaProb <- allPredictions[product == "ind_nomina_ult1", prediction]
  nominaProbRank <- allPredictions[product == "ind_nomina_ult1", order_predict]
  nomPensProb <- allPredictions[product == "ind_nom_pens_ult1", prediction]
  nomPensProbRank <- allPredictions[product == "ind_nom_pens_ult1", order_predict]
  swapIds <- nominaProb>0 & nomPensProb>0 & nominaProb>nomPensProb
  swapNcodPers <- ncodpers[swapIds]
  allPredictions[ncodpers %in% swapNcodPers & product == "ind_nomina_ult1",
                 order_predict := nomPensProbRank[swapIds]]
  allPredictions[ncodpers %in% swapNcodPers & product == "ind_nom_pens_ult1",
                 order_predict := nominaProbRank[swapIds]]
}

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
allPredictions[, totalProb := prediction * rep(newProdPredictions,
                                               each = nbBaseModels)]
meanProductProbs <- allPredictions[, .(meanCondProb = mean(prediction),
                                       meanProb = mean(totalProb),
                                       totalProb = sum(totalProb)), product]
meanProductProbs <- meanProductProbs[order(-meanProb), ]

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