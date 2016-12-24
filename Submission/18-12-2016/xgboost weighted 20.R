#######################
# XGBOOST weighted 20 #
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
library(matrixStats)

# Submission date and file name
submissionDate <- "18-12-2016"
loadFile <- "xgboost weighted trainAll 16 10 folds 200 rounds, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint"
submissionFile <- "xgboost weighted trainAll 20, reca lin mult 0.5"

# Target date 
targetDate <- "12-11-2016"

# Target train model folders
trainModelsFolder <- "trainTrainAll Top 100 monthProduct 200 rounds 10 Folds"
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

# MAP boosting options - all other ignored if mapBoosting is FALSE
mapBoosting <- FALSE
maxDiffNominaNomPensMapBoosting <- Inf # Typical value: Inf; Inf means no limit
maxRelDiffNominaNomPensMapBoosting <- 0.5 # Typical value: 0.8; 1 means no action
averageNominaNomPensProbsMAPBoosting <- TRUE # Min mode is equal to close gaps!
averageNominaNomPensProbsMAPBoostingMethod <- c("Average", "Min")[1]
consideredMapTopPredictions <- 1:6
swapRelativeCutoff <- 1 # Typical value: 1; 0 means no swaps
swapWithCno <- FALSE
nbMapTopPredictions <- length(consideredMapTopPredictions)

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

# mapBoosting => nomPensAboveNominaBothNotOwned
if(mapBoosting && !nomPensAboveNominaBothNotOwned){
  stop("MAP boosting requires nomPensAboveNominaBothNotOwned to be TRUE")
}

# Linear target variable multipliers
linearMultipliers <- rep(1, 24)
linearMultipliers[18] <- 1/2
# linearMultipliers[18] <- 2

# Powers of the generalized means
generalizedMeanPowers <- rep(1, 24)
# generalizedMeanPowers[3] <- 2

# Fold combination options
dropFoldModels <- TRUE
medianModelPrediction <- FALSE
# foldRelativeWeight <- 0.9

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

# Source the exponential normalisation and weights extraction as well as the
# MAP calculation (used for MAP boosting)
source("Common/exponentialNormaliser.R")
source("Common/getModelWeights.R")
source("Common/apk.R")

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
      prodMonthFiles <- modelGroupFiles[grepl(targetProduct, modelGroupFiles)]
      nbFoldsProd <- sum(grepl("Fold", prodMonthFiles))
      
      # Calculate the total model fold weight
      modelGroupFilesTarget <-
        modelGroupFiles[grepl(targetProduct, modelGroupFiles) &
                          grepl("Fold", modelGroupFiles)]
      nbFoldsModTarget <- as.numeric(gsub("^.* of | -.*$", "",
                                          modelGroupFilesTarget))
      totalModelFoldWeight <- 1 + sum(1 - 1/nbFoldsModTarget)
      
      if(isFold){
        nbFoldsMod <- as.numeric(gsub("^.* of | -.*$", "", modelGroupFile))
        foldModelWeight <- (1 - 1/nbFoldsMod)/totalModelFoldWeight
      } else{
        foldModelWeight <- 1/totalModelFoldWeight
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
          
          # Reserve space for the base predictions when using the median
          # fold prediction
          if(medianModelPrediction){
            baseFoldPredictions <- matrix(NA, nrow=nrow(predictorDataM),
                                          ncol = nbTargetModelFolds)
          }
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
            if(medianModelPrediction){
              baseFoldPredictions[, fold] <- predictionsPrevNotOwnedFold
            } else{
              foldPredictions[!alreadyOwned] <- foldPredictions[!alreadyOwned] +
                predictionsPrevNotOwnedFold*weightFold
            }
          }
          # if(targetVar == "ind_reca_fin_ult1") browser()
          if(medianModelPrediction){
            predictions <- rep(0, nrow(testDataLag))
            predictions[!alreadyOwned] <- rowMedians(baseFoldPredictions)
          } else{
            predictions <- foldPredictions/weight
          }
          # Set the predictions to 0 for products that are already owned
          # predictions[alreadyOwned] <- -runif(sum(alreadyOwned))
          predictions[alreadyOwned] <- 0
        }
        
        # Stop the submission generation if there are missing values in the
        # predictions
        if(any(is.na(predictions))) browser()
        
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
                            linearMultipliers[i]) ^ generalizedMeanPowers[i]]
      } else{
        allPredictions <- rbind(allPredictions, predictionsDT)
        # Linear multiplication of the base predictions
        allPredictions[product == targetVar, predictions :=
                         (predictions * linearMultipliers[i]) ^
                         generalizedMeanPowers[i]]
        allPredictions[product == targetVar, weightedPrediction :=
                         (weightedPrediction * linearMultipliers[i]) ^
                         generalizedMeanPowers[i]]
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
  allPredictions[, prediction := (weightedPrediction / weightSum) ^
                   (1/rep(generalizedMeanPowers, each=nrow(testDataLag)))]
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
  swapNcodpers <- ncodpers[swapIds]
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                 order_predict := nomPensProbRank[swapIds]]
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                 order_predict := nominaProbRank[swapIds]]
  
  # Also swap the predictions for logic down the MAP boosting pipeline!
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                 prediction := nomPensProb[swapIds]]
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                 prediction := nominaProb[swapIds]]
}

# Optionally, perform MAP boosting
if(mapBoosting){
  # Extract the considered ids for map boosting:
  # - Both nomina and nom pens previously not owned
  # - Predicted probability difference < maxDiffNominaNomPensMapBoosting
  nominaPreds <- allPredictions[product == "ind_nomina_ult1", prediction]
  nomPensPreds <- allPredictions[product == "ind_nom_pens_ult1", prediction]
  
  # Average the predictions if both are greater than zero and the absolute
  # difference is less than maxDiffNominaNomPensMapBoosting
  averagedIds <- nominaPreds>0 & nomPensPreds>0 & 
    abs(nominaPreds - nomPensPreds) < maxDiffNominaNomPensMapBoosting &
    (nominaPreds/nomPensPreds) > maxRelDiffNominaNomPensMapBoosting &
    (nominaPreds/nomPensPreds) < (1/maxRelDiffNominaNomPensMapBoosting)
  avNcodPers <- ncodpers[averagedIds]
  if(averageNominaNomPensProbsMAPBoosting){
    if(averageNominaNomPensProbsMAPBoostingMethod == "Average"){
      averageProbs <- (nominaPreds[averagedIds] +
                         nomPensPreds[averagedIds])/2
    } else{
      if(averageNominaNomPensProbsMAPBoostingMethod == "Min"){
        averageProbs <- pmin(nominaPreds[averagedIds],
                             nomPensPreds[averagedIds])
      } else{
        stop("averageNominaNomPensProbsMAPBoostingMethod does not exist")
      }
    }
    allPredictions[ncodpers %in% avNcodPers & product == "ind_nomina_ult1",
                   prediction := averageProbs]
    allPredictions[ncodpers %in% avNcodPers & product == "ind_nom_pens_ult1",
                   prediction := averageProbs * (1 + 1e-10)]
    
    # Recalculate the order of the predictions
    allPredictions[, order_predict := match(1:length(prediction),
                                            order(-prediction)), by=ncodpers]
    allPredictions <- allPredictions[order(ncodpers, -prediction), ]
  }
  
  # # Swap the prediction ranks if the expected MAP is greater for the non
  # # nomina - nom pens products
  # orderTable <- table(
  #   allPredictions[ncodpers %in% avNcodPers & product == "ind_nomina_ult1",
  #                  order_predict],
  #   allPredictions[ncodpers %in% avNcodPers & product == "ind_nom_pens_ult1",
  #                  order_predict]
  # )
  
  # Calculate the mean nomina - nom pens probabilities for all ncodpers
  meanNomNomPensProb <-
    allPredictions[product %in% c("ind_nomina_ult1", "ind_nom_pens_ult1"),
                   .(meanPred = mean(prediction)), ncodpers]
  
  # Swap the ranks according to the expected MAP 
  # For now: only consider pairs where rank nom_pen + 1 == rank nomina
  allSwapped <- c()
  for(i in 1:nbMapTopPredictions){
    nomPensRank <- consideredMapTopPredictions[i]
    nomRank <- nomPensRank + 1
    # List the considered swap ncodpers
    consideredNcodpers <-
      intersect(
        avNcodPers,
        intersect(allPredictions[product == "ind_nomina_ult1" &
                                   order_predict == nomRank, ncodpers],
                  allPredictions[product == "ind_nom_pens_ult1" &
                                   order_predict == nomPensRank, ncodpers])
      )
    
    # Don't swap when the third ranked product is cno_fin
    if(!swapWithCno){ 
      consideredNcodpers <-
        setdiff(consideredNcodpers,
                allPredictions[ncodpers %in% consideredNcodpers &
                                 product == "ind_cno_fin_ult1" & 
                                 order_predict == nomRank + 1, ncodpers])
    }
    
    # Calculate the probability swap ratio (expected MAP loss if wrong)
    pairRightMap <- map7(matrix(c(c(rep(0, nomPensRank-1), rep(1, 2),
                                    rep(0, 7-nomPensRank))[1:7], 2), nrow=1))
    pairWrongMap <- map7(matrix(c(c(rep(0, nomPensRank), rep(1, 2),
                                    rep(0, 6-nomPensRank))[1:7], 2), nrow=1))
    singleRightMap <- map7(matrix(c(c(rep(0, nomPensRank-1), 1,
                                      rep(0, 7-nomPensRank))[1:7], 1),
                                  nrow=1))
    singleWrongMap <- map7(matrix(c(c(rep(0, 1 + nomPensRank), 1,
                                      rep(0, 6-nomPensRank))[1:7], 1),
                                  nrow=1))
    
    # Lower the ranks of nomina and nom pens according to the expected MAP
    meanNomNomPensProbCons <-
      meanNomNomPensProb[ncodpers %in% consideredNcodpers, meanPred]
    meanSingleProbCons <-
      allPredictions[ncodpers %in% consideredNcodpers &
                       order_predict == nomPensRank + 2,
                     prediction]
    swapNcodpersIds <-
      (meanNomNomPensProbCons * (1-meanSingleProbCons) * pairRightMap +
         meanSingleProbCons * (1-meanNomNomPensProbCons) * singleWrongMap)/
      (meanSingleProbCons * (1-meanNomNomPensProbCons) * singleRightMap + 
         meanNomNomPensProbCons * (1-meanSingleProbCons) * pairWrongMap) <
      swapRelativeCutoff
    swapNcodpers <- consideredNcodpers[swapNcodpersIds]
    allSwapped <- unique(c(allSwapped, swapNcodpers))
    allPredictions[order_predict == nomPensRank + 2 &
                     ncodpers %in% swapNcodpers,
                   order_predict := as.integer(order_predict - 2)]
    allPredictions[product %in% c("ind_nomina_ult1", "ind_nom_pens_ult1") &
                     ncodpers %in% swapNcodpers,
                   order_predict := as.integer(order_predict + 1)]
  }
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
# No longer accurate when MAP boosting or swapping nomina/nomPens
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