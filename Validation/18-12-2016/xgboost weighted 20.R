################################################
# XGBOOST weighted 20 - Validation on May 2016 #
################################################

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
submissionDate <- "18-12-2016"
loadFile <- "xgboost weighted trainAll 20, validation May16" #"xgboost weighted trainAll 17, validation May16" # "xgboost weighted trainAll 17, validation May16"
submissionFile <- "xgboost weighted trainAll 20, validation May16"

# Target date 
targetDate <- "12-11-2016"

# Target train model folders
trainModelsFolder <- "trainTrainAll Top 100 monthProduct 200 rounds 10 Folds"
trainAll <- grepl("TrainAll", trainModelsFolder)

# Target feature files folder
validationFeaturesFolder <- "validation Back 0"
# validationFeaturesFolder <- "validation Back 1"

# Option to store the product predictions
loadPredictions <- FALSE # If loadPredictions TRUE... 
loadAllPredictions <- FALSE # Or if loadAllPredictions TRUE... 
loadBaseModelPredictions <- TRUE # ... loadBaseModelPredictions is ignored
savePredictions <- FALSE
savePredictionsPosFlanks <- TRUE
saveBaseModelPredictions <- TRUE
saveAllPredictionsPosFlanks <- TRUE
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
excludeNormalizeProds <- NULL #c("ind_recibo_ult1")
additiveNormalizeProds <- NULL #c("ind_cco_fin_ult1")
fractionPosFlankUsers <- 0.03008105
# expectedCountPerPosFlank <- 1.286
expectedCountPerPosFlank <- 1.25

# List the total product weights over all months
weightSum <- 1 # sum(monthsBackModelsWeights)

# MAP boosting options - all other ignored if mapBoosting is FALSE
mapBoosting <- FALSE
maxDiffNominaNomPensMapBoosting <- Inf
maxRelDiffNominaNomPensMapBoosting <- 0.8
closeGapsMAPBoosting <- TRUE
averageNominaNomPensProbsMAPBoosting <- FALSE
averageWeightedMAPAveragingMode <- c("Weighted", "Most recent")[1]
averageNominaNomPensProbsMAPBoostingMethod <- c("Average", "Min")[2]
consideredMapTopPredictions <- 1:6
swapRelativeCutoff <- 0
nbMapTopPredictions <- length(consideredMapTopPredictions)

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

if(mapBoosting && !nomPensAboveNominaBothNotOwned){
  stop("MAP boosting requires nomPensAboveNominaBothNotOwned to be TRUE")
}

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

# Source the exponential normalisation and weights extraction and the average
# precision calculation
source("Common/exponentialNormaliser.R")
source("Common/getModelWeights.R")
source("Common/apk.R")

# Load the target product weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights first validation.rds"))


######################################################################

# Create predictions subfolder
# Create the target folder if it does not exist yet
predictionsPath <- file.path(getwd(), "Validation", submissionDate,
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
modelGroups <- modelGroups[!grepl("Back0Lag16", modelGroups)] # Validation!
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
                               relativeWeight = relativeWeight *
                                 foldModelWeight)
      )
      baseModels <- c(baseModels, list(modelInfo))
    }
  }
}
baseModelInfo[, modelId := 1:nrow(baseModelInfo)]

# Extract the number of marginal/joint/conditional lags and months back
# Set the base model info to default settings when the base models are 
# trained over multiple month periods
monthsBackLags <- rev(sort(unique(baseModelInfo$modelLag)))
nbConditionalLags <- length(monthsBackLags)

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
validationDataLag <- readRDS(file.path(getwd(), "Feature engineering",
                                       targetDate, validationFeaturesFolder,
                                       "Lag1 features.rds"))

# Optionally subset the test data
if(predictSubset){
  predictSubsetIds <- sort(sample(1:nrow(validationDataLag),
                                  predictSubsetCount))
  validationDataLag <- validationDataLag[predictSubsetIds]
}

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

newProdPredictions <- rep(1, nrow(validationDataLag))

savePredictionsPath <- file.path(predictionsPath,
                                 paste0(submissionFile, ".rds"))
if(loadAllPredictions && file.exists(savePredictionsPath)){
  allPredictions <- readRDS(savePredictionsPath)
} else{
  # Optionally load the predictions before normalisation if they are available
  if(loadPredictions && file.exists(rawPredictionsPath)){
    allPredictions <- readRDS(rawPredictionsPath)
  } else{
    if(saveAllPredictionsPosFlanks){
      posFlankIds <- validationDataLag$hasNewProduct
      allPredictionsPosFlanks <- NULL
    }
    
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
      loadvalidationDataLag <- TRUE
      baseModelPredPaths <- file.path(baseModelPredictionsPath,
                                      paste0(targetCols, " Lag ", lag, ".rds"))
      if(loadBaseModelPredictions && exists("baseModelPredictionsPath")){
        loadvalidationDataLag <- !all(sapply(baseModelPredPaths, file.exists))
      }
      
      # Load the test data with the appropriate lag
      if(loadvalidationDataLag){
        validationDataLag <-
          readRDS(file.path(getwd(), "Feature engineering", targetDate,
                            validationFeaturesFolder, paste0("Lag", lag,
                                                             " features.rds")))
        
        # Optionally subset the test data
        if(predictSubset){
          validationDataLag <- validationDataLag[predictSubsetIds]
        }
      }
      
      for(i in 1:nbBaseModels){
        # Extract the target column
        targetVar <- targetCols[i]
        targetModelIds <- baseModelInfo[targetProduct==targetVar &
                                          modelLag==lag, modelId]
        
        # Show progress message
        cat("Generating test predictions for model", i, "of", nbBaseModels,
            "\n")
        
        # Optionally, load the base model predictions
        if(exists("baseModelPredictionsPath")){
          baseModelPredPath <- file.path(baseModelPredictionsPath,
                                         paste0(targetVar, " Lag ", lag,
                                                ".rds"))
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
            predictions <- rep(0, nrow(validationDataLag))
          } else{
            nbTargetModelFolds <- length(targetModelIds)
            foldPredictions <- rep(0, nrow(validationDataLag))
            
            alreadyOwned <-
              is.na(validationDataLag[[paste0(targetVar, "Lag1")]]) |
              validationDataLag[[paste0(targetVar, "Lag1")]] == 1
            
            # Extract predictors data from the features data
            predictorData <-
              validationDataLag[!alreadyOwned,
                                baseModels[[targetModelIds[1]]]$predictors,
                                with=FALSE]
            
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
          predictionsDT <- data.table(ncodpers = validationDataLag$ncodpers,
                                      predictions = predictions,
                                      product = targetVar)
          
          if(saveAllPredictionsPosFlanks){
            targetColName <- paste0(targetVar, "Lag", lag)
            allPredictionsPosFlanks[[targetColName]] <- predictions[posFlankIds]
          }
        }
        
        predictionsDT[, weightedPrediction :=
                        predictionsDT$predictions*weight]
        
        # if(targetVar == "ind_reca_fin_ult1") browser()
        # c(lag, sum(predictionsDT$predictions), sum(validationDataLag[[19+24*(16-lag)]], na.rm=T))
        
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
    
    if(saveAllPredictionsPosFlanks && nrow(allPredictionsPosFlanks)>0){
      predictionsPosFlanks <- setDT(allPredictionsPosFlanks)
      predictionsPosFlanks <- cbind(validationDataLag$ncodpers[posFlankIds],
                                    predictionsPosFlanks)
      colnames(predictionsPosFlanks)[1] <- "ncodpers"
      saveRDS(predictionsPosFlanks,
              file.path(predictionsPath,
                        paste(submissionFile,
                              "positive flank base model predictions.rds")))
    }
    
    # Save the predictions to the predictions folder before normalisation
    if(savePredictionsBeforeNormalisation){
      saveRDS(allPredictions, file=rawPredictionsPath)
    }
  }
  
  # Optionally, save the predictions related to the positive flanks before
  # normalisation
  if(savePredictionsPosFlanks){
    posFlankNcodpers <- validationDataLag[hasNewProduct == TRUE, ncodpers]
    allPredictionsPosFlanks <- allPredictions[ncodpers %in% posFlankNcodpers, ]
    saveRDS(allPredictionsPosFlanks,
            file.path(predictionsPath,
                      paste("prevNorm", submissionFile, "positive flanks.rds")))
  }
  
  # Optionally, multiply the predictions by the relative count ratio of June
  # 2016
  probMultipliers <- rep(NA, nbBaseModels)
  names(probMultipliers) <- targetCols
  if(normalizeProdProbs){
    for(i in 1:nbBaseModels){
      # Show progress message
      cat("Normalizing product predictions", i, "of", nbBaseModels, "\n")
      
      # Extract the target column
      targetVar <- targetCols[i]
      
      # Look up if the target variable was already owned
      alreadyOwned <- is.na(validationDataLag[[paste0(targetVar, "Lag1")]]) |
        validationDataLag[[paste0(targetVar, "Lag1")]] == 1
      predictions <- allPredictions[product==targetVar, prediction]
      predictionsPrevNotOwned <- predictions[!alreadyOwned]
      if(suppressWarnings(max(predictions[alreadyOwned]))>0) browser()
      
      # Normalize the predicted probabilities
      predictedPosFlankCount <- sum(predictionsPrevNotOwned *
                                      newProdPredictions[!alreadyOwned])
      probMultiplier <- nrow(validationDataLag) * fractionPosFlankUsers *
        expectedCountPerPosFlank * countContributions[16, i] /
        predictedPosFlankCount
      probMultipliers[i] <- probMultiplier
      # if(i %in% c(3, 5, 7, 13, 18, 19, 22, 23, 24)) browser()
      if(is.finite(probMultiplier) && ! targetVar %in% excludeNormalizeProds){
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
              predictions[!alreadyOwned], probMultiplier,
              weights=newProdPredictions[!alreadyOwned], nbIt = 30)
          }
        }
        # Update the predictions in allPredictions
        allPredictions[product==targetVar, prediction:=predictions]
      }
    }
  }
  
  # Optionally, save the predictions related to the positive flanks after
  # normalisation
  if(savePredictionsPosFlanks){
    allPredictionsPosFlanks <- allPredictions[ncodpers %in% posFlankNcodpers, ]
    saveRDS(allPredictionsPosFlanks,
            file.path(predictionsPath,
                      paste("postNorm", submissionFile, "positive flanks.rds")))
    saveRDS(probMultipliers,
            file.path(predictionsPath,
                      paste(submissionFile, "probMultipliers.rds")))
  }
  
  # Order the predicted probabilities for all products by client
  setkey(allPredictions, ncodpers)
  allPredictions[,order_predict := match(1:length(prediction),
                                         order(-prediction)), by=ncodpers]
  allPredictions <- allPredictions[order(ncodpers, -prediction), ]
  
  # Swap nomina and nom pens in rank if they are both not owned in the previous
  # period and if the rank of nomina > rank of nom_pens
  ncodpers <- unique(allPredictions$ncodpers)
  if(nomPensAboveNominaBothNotOwned){
    # Find users where the rank of nomina < rank of nom pens and both prob not
    # zero
    nominaProb <- allPredictions[product == "ind_nomina_ult1", prediction]
    nominaProbRank <- allPredictions[product == "ind_nomina_ult1",
                                     order_predict]
    nomPensProb <- allPredictions[product == "ind_nom_pens_ult1", prediction]
    nomPensProbRank <- allPredictions[product == "ind_nom_pens_ult1",
                                      order_predict]
    swapIds <- nominaProb>0 & nomPensProb>0 & nominaProb>nomPensProb
    swapNcodpers <- ncodpers[swapIds]
    allPredictions[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                   order_predict := nomPensProbRank[swapIds]]
    allPredictions[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                   order_predict := nominaProbRank[swapIds]]
    
    # Also swap the predictions for logic down the pipeline!
    allPredictions[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                   prediction := nomPensProb[swapIds]]
    allPredictions[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                   prediction := nominaProb[swapIds]]
  }
  
  if(mapBoosting){
    # Extract the considered ids for map boosting:
    # - Both nomina and nom pens previously not owned
    # - Predicted probability difference for most recent lag <
    #   maxDiffNominaNomPensMapBoosting
    nominaPreds <- allPredictions[product == "ind_nomina_ult1", prediction]
    nomPensPreds <- allPredictions[product == "ind_nom_pens_ult1", prediction]
    
    # Read in the predictions of the most recent lag for nomina and nom pens
    mostRecentLag <- max(monthsBackLags)
    mostRecentLagPathNomina <-
      file.path(baseModelPredictionsPath, paste0("ind_nomina_ult1 Lag ",
                                                 mostRecentLag, ".rds"))
    mostRecentLagNomina <- readRDS(mostRecentLagPathNomina)$predictions
    mostRecentLagPathNomPens <-
      file.path(baseModelPredictionsPath, paste0("ind_nom_pens_ult1 Lag ",
                                                 mostRecentLag, ".rds"))
    mostRecentLagNomPens <- readRDS(mostRecentLagPathNomPens)$predictions
    
    if(averageWeightedMAPAveragingMode == "Weighted"){
      mostRecentLagNomina <- nominaPreds
      mostRecentLagNomPens <- nomPensPreds
    }
    
    # Average the predictions if both are greater than zero and the absolute
    # difference is less than maxDiffNominaNomPensMapBoosting
    averagedIds <- mostRecentLagNomina>0 & mostRecentLagNomPens>0 & 
      abs(mostRecentLagNomina - mostRecentLagNomPens) <
      maxDiffNominaNomPensMapBoosting &
      (mostRecentLagNomina/mostRecentLagNomPens) >
      maxRelDiffNominaNomPensMapBoosting &
      (mostRecentLagNomina/mostRecentLagNomPens) <
      (1/maxRelDiffNominaNomPensMapBoosting)
    avNcodPers <- ncodpers[averagedIds]
    if(averageNominaNomPensProbsMAPBoosting){
      if(averageNominaNomPensProbsMAPBoostingMethod == "Average"){
        averageProbs <- (mostRecentLagNomina[averagedIds] +
                           mostRecentLagNomPens[averagedIds])/2
      } else{
        if(averageNominaNomPensProbsMAPBoostingMethod == "Min"){
          averageProbs <- pmin(mostRecentLagNomina[averagedIds],
                               mostRecentLagNomPens[averagedIds])
        } else{
          stop("averageNominaNomPensProbsMAPBoostingMethod does not exist")
        }
      }
      allPredictions[ncodpers %in% avNcodPers & product == "ind_nomina_ult1",
                     prediction := averageProbs]
      allPredictions[ncodpers %in% avNcodPers & product == "ind_nom_pens_ult1",
                     prediction := averageProbs * (1 + 1e-10)]
      
      # Recalculate the order of the predictions
      allPredictions[,order_predict := match(1:length(prediction),
                                             order(-prediction)), by=ncodpers]
      allPredictions <- allPredictions[order(ncodpers, -prediction), ]
    }
    
    # Lower the ranks 
    if(closeGapsMAPBoosting){
      consideredIds <- nominaPreds>0 & nomPensPreds>0 & 
        abs(nominaPreds - nomPensPreds) < maxDiffNominaNomPensMapBoosting &
        (nominaPreds/nomPensPreds) > maxRelDiffNominaNomPensMapBoosting &
        (nominaPreds/nomPensPreds) < (1/maxRelDiffNominaNomPensMapBoosting)
      closeGapNcodpers <- ncodpers[consideredIds]
      nominaRanks <- allPredictions[ncodpers %in% closeGapNcodpers &
                                      product == "ind_nomina_ult1",
                                    order_predict]
      nomPensRanks <- allPredictions[ncodpers %in% closeGapNcodpers &
                                       product == "ind_nom_pens_ult1",
                                     order_predict]
      # table(nominaRanks, nomPensRanks)
      minProb <- pmin(allPredictions[ncodpers %in% closeGapNcodpers &
                                       product == "ind_nomina_ult1",
                                     prediction],
                      allPredictions[ncodpers %in% closeGapNcodpers &
                                       product == "ind_nom_pens_ult1",
                                     prediction])
      
      # Update the probabilities 
      allPredictions[ncodpers %in% closeGapNcodpers &
                       product == "ind_nomina_ult1", prediction := minProb]
      allPredictions[ncodpers %in% closeGapNcodpers &
                       product == "ind_nom_pens_ult1",
                     prediction := minProb * (1 + 1e-10)]
      
      # Recalculate the order of the predictions
      allPredictions[,order_predict := match(1:length(prediction),
                                             order(-prediction)), by=ncodpers]
      allPredictions <- allPredictions[order(ncodpers, -prediction), ]
    }
    
    # Swap the prediction ranks if the expected MAP is greater for the non
    # nomina - nom pens products
    orderTable <- table(
      allPredictions[ncodpers %in% avNcodPers & product == "ind_nomina_ult1",
                     order_predict],
      allPredictions[ncodpers %in% avNcodPers & product == "ind_nom_pens_ult1",
                     order_predict]
    )
    
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
      consideredNcodpers <-
        setdiff(consideredNcodpers,
                allPredictions[ncodpers %in% consideredNcodpers &
                                 product == "ind_cno_fin_ult1" & 
                                 order_predict == nomRank + 1, ncodpers])
      
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
      nonZeroSwappedNcodpers <-
        intersect(swapNcodpers, unique(validationDataLag[hasNewProduct == TRUE,
                                                         ncodpers]))
      allSwapped <- unique(c(allSwapped, nonZeroSwappedNcodpers))
      # View(allPredictions[ncodpers %in% nonZeroSwappedNcodpers &
      #                       order_predict < 8, ])
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
  allPredictions[, totalProb := prediction * rep(newProdPredictions,
                                                 each = nbBaseModels)]
  meanProductProbs <- allPredictions[, .(meanCondProb = mean(prediction),
                                         meanProb = mean(totalProb),
                                         totalProb = sum(totalProb)), product]
  meanProductProbs <- meanProductProbs[order(-meanProb), ]
  
  # Add the positive flanks to allPredictions
  allPredictions$posFlank <- NA
  for(i in 1:nbBaseModels){
    targetVar <- targetCols[i]
    targetRows <- which(allPredictions$product == targetVar)
    allPredictions$posFlank[targetRows] <- 0
    posFlankIds <-
      which(!is.na(validationDataLag[[targetVar]]) &
              validationDataLag[[targetVar]] == 1 &
              !is.na(validationDataLag[[paste0(targetVar, "Lag1")]]) &
              validationDataLag[[paste0(targetVar, "Lag1")]] == 0)
    allPredictions$posFlank[targetRows[posFlankIds]] <- 1
  }
  
  # Save the predictions to the predictions folder
  if(savePredictions){
    saveRDS(allPredictions, file=savePredictionsPath)
  }
}

# Calculate the number of new products for each client
newProducts <- allPredictions[, .(nbNewProducts = sum(posFlank)), ncodpers]
setkey(newProducts, ncodpers)
allPredictions <- newProducts[allPredictions]

# Show the confidence in the top prediction
hist(allPredictions[order_predict==1, prediction])

# Calculate the log losses for all products


# # Plot the log losses for all products
# plot(logLosses)
# 
# # Plot the mean predictions for true ones for all products
# plot(meanPredsTrue0, pch=16, col="red", ylim=c(0,1))
# points(meanPredsTrue1, pch=16, col="green")

# Set up the matrix that can be used to calculate the MAP@7
mapData <- cbind(allPredictions[order_predict==1, posFlank],
                 allPredictions[order_predict==2, posFlank],
                 allPredictions[order_predict==3, posFlank],
                 allPredictions[order_predict==4, posFlank],
                 allPredictions[order_predict==5, posFlank],
                 allPredictions[order_predict==6, posFlank],
                 allPredictions[order_predict==7, posFlank],
                 newProducts$nbNewProducts)

# Set up the submission file
submission <- cbind(allPredictions[order_predict==1, product],
                    allPredictions[order_predict==2, product],
                    allPredictions[order_predict==3, product],
                    allPredictions[order_predict==4, product],
                    allPredictions[order_predict==5, product],
                    allPredictions[order_predict==6, product],
                    allPredictions[order_predict==7, product])

# Don't consider predictions where there was no positive flank
mapData <- mapData[validationDataLag$hasNewProduct, ]
submission <- submission[validationDataLag$hasNewProduct, ]

# Calculate the mean average precision@7
map <- map7(mapData, returnScores = TRUE)
mapData <- cbind(mapData, map)
mapZeroPadded <- rep(0, nrow(validationDataLag))
mapZeroPadded[validationDataLag$hasNewProduct] <- map
validationDataLag <- validationDataLag[hasNewProduct == TRUE, ]

# Display the mean MAP
cat("Mean MAP@7:", paste0(round(mean(map)*100, 3), "%"), "\n")

# Study the true and false positives
truePosSummary <-
  allPredictions[posFlank==1, .(meanPredTP = mean(prediction),
                                medianPredTP = median(prediction),
                                meanRank = mean(order_predict), .N),
                 product]
falsePosSummary <-
  allPredictions[posFlank==0 & prediction>0,
                 .(meanPredFP = mean(prediction),
                   medianPredFP = median(prediction),
                   mean(order_predict), .N),
                 product]
truePosSummary[, meanPredFP := falsePosSummary$meanPredFP[
  match(truePosSummary$product, falsePosSummary$product)]]
truePosSummary[, medianPredFP := falsePosSummary$medianPredFP[
  match(truePosSummary$product, falsePosSummary$product)]]
truePosSummary[, medianPredRatio := medianPredTP/medianPredFP]
truePosSummary[, meanPredRatio := meanPredTP/meanPredFP]
truePosSummary <- truePosSummary[order(-N),]

# Study the mean map by true product
allPredictions[, map := rep(mapZeroPadded, each=24)]
meanMapSummary <- allPredictions[posFlank==1,
                                 .(meanMap = mean(map), .N), product]
meanMapSummary <- meanMapSummary[order(-N),]

# Study the rank of nomina and nom pens where there was at least one positive
# flank
allPredPosFlank <- allPredictions[ncodpers %in% validationDataLag$ncodpers, ]
targetNcodPers <- intersect(allPredPosFlank[product == "ind_nomina_ult1" &
                                              prediction > 0, ncodpers],
                            allPredPosFlank[product == "ind_nom_pens_ult1" &
                                              prediction > 0, ncodpers])
table(allPredPosFlank[ncodpers %in% targetNcodPers &
                        product == "ind_nom_pens_ult1", order_predict],
      allPredPosFlank[ncodpers %in% targetNcodPers &
                        product == "ind_nomina_ult1", order_predict])
gapNcodPersId <-
  (allPredPosFlank[ncodpers %in% targetNcodPers &
                     product == "ind_nom_pens_ult1", order_predict] -
     allPredPosFlank[ncodpers %in% targetNcodPers &
                       product == "ind_nomina_ult1", order_predict] == -2) &
  (allPredPosFlank[ncodpers %in% targetNcodPers &
                     product == "ind_nom_pens_ult1", order_predict] == 1)
gapNcodPers <- targetNcodPers[gapNcodPersId]
submissionGap <- submission[match(gapNcodPers, validationDataLag$ncodpers), ]
mapGap <- mapData[match(gapNcodPers, validationDataLag$ncodpers), ]
allPredsGap <- allPredictions[ncodpers %in% gapNcodPers & order_predict < 4, ]