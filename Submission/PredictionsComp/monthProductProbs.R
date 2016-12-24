# Logic to extract the predictions of all base models for all product - month
# pairs. This data set is the starting point for the weight setting app and
# weight modification logic

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(ggplot2)

# Only generate base predictions?
onlyPreds <- TRUE

# Submission date and model name
# submissionDate <- "09-12-2016"
# modelName <- "xgboost weighted trainAll 14, linear increase jun15 times6 back 15-0 no zeroing, exponential normalisation joint"
submissionDate <- "20-12-2016"
modelName <- "xgboost weighted trainAll 22, 10 folds, 20 folds Top"

modelFolder <- "trainTrainAll Top 100 monthProduct 200 rounds 10 Folds"
targetDate <- "12-11-2016"

# List the base products and target variables
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
targetVars <- paste0("ind_", baseProducts, "_ult1")
nbTargetVars <- length(targetVars)

# Random subset fraction for faster analysis
randomSubsetSize <- 0.1

# Public leaderboard probing data
fractionPosFlankUsers <- 0.035114
expectedCountPerPosFlank <- 1.25

# Source the exponential normalisation
source("Common/exponentialNormaliser.R")


######################################################################

# Load the first level base model weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights first.rds"))

# Load the estimated relative count contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# List all base models
basePath <- file.path(getwd(), "Submission", submissionDate, "Predictions",
                      modelName)
baseModels <- list.files(basePath)
baseModelLags <- as.numeric(gsub(".*Lag |.rds$", "", baseModels))
modelLags <- sort(unique(baseModelLags))
nbLags <- length(modelLags)

# Combine the base model predictions
combinedPreds <- list()
probMultipliers <- rep(NA, nbTargetVars)
names(probMultipliers) <- targetVars
for(targetId in 1:nbTargetVars){
  # Show progress message
  cat("Processing target variable", targetId, "of", nbTargetVars, "\n")
  
  targetVar <- targetVars[targetId]
  weightedTargetPredictions <- rep(0, 929615)
  for(lagId in 1:nbLags){
    lag <- modelLags[lagId]
    
    # if(targetVar == "ind_cco_fin_ult1" && lag==3) browser()
    
    modelPredsName <- paste(targetVar, "Lag", lag, sep="_")
    predictionsDT <- readRDS(file.path(basePath, paste0(targetVar, " Lag ",
                                                        lag, ".rds")))
    if(length(combinedPreds) == 0){
      combinedPreds[["ncodpers"]] <- predictionsDT$ncodpers
    }
    combinedPreds[[modelPredsName]] <- predictionsDT$predictions
    relativeModelWeight <-
      dateTargetWeights[modelLag == lag & targetProduct == targetVar,
                        relativeWeight]
    weightedTargetPredictions <- weightedTargetPredictions +
      relativeModelWeight * predictionsDT$predictions
  }
  
  # Add the weighted predictions for the target variable
  weightedTargetPredsName <- paste(targetVar, "Weighted", sep="_")
  combinedPreds[[weightedTargetPredsName]] <- weightedTargetPredictions
  
  # Add the normalized weighted predictions for the target variable
  predictedPosFlankCount <- sum(weightedTargetPredictions)
  probMultiplier <- nrow(predictionsDT) * fractionPosFlankUsers *
    expectedCountPerPosFlank * countContributions[17, targetId] /
    predictedPosFlankCount
  probMultipliers[targetId] <- probMultiplier
  weightedNormTargetPredictions <- probExponentNormaliser(
    weightedTargetPredictions, probMultiplier)
  weightedNormTargetPredsName <- paste(targetVar, "Weighted_Norm_Exp", sep="_")
  combinedPreds[[weightedNormTargetPredsName]] <- weightedNormTargetPredictions
}

# Convert the base model predictions to a datatable
combinedPreds <- setDT(combinedPreds)

###############################################################
# Calculate the weighted predictions and all 24 product ranks #
###############################################################

if(!onlyPreds){
  weightedExtensions <- c("", "_Norm_Exp")
  for(i in 1:length(weightedExtensions)){
    weightedExtension <- weightedExtensions[i]
    productRanks <-
      melt(combinedPreds[, c(1, grep(paste0("Weighted", weightedExtension, "$"),
                                     names(combinedPreds))),
                         with=FALSE], id = "ncodpers")
    productRanks[, variable := gsub("^ind_|_ult1.*$", "", variable)]
    setkey(productRanks, ncodpers)
    productRanks[, order_predict := match(1:length(value),
                                          order(-value)), by=ncodpers]
    names(productRanks)[2:3] <- c("product", "prediction")
    
    # Add the individual product ranks
    for(targetId in 1:nbTargetVars){
      rankTargetVar <- baseProducts[targetId]
      targetRanks <- productRanks[product == rankTargetVar, order_predict]
      rankTargetName <- paste(paste0(rankTargetVar, weightedExtension), "Rank",
                              sep="_")
      combinedPreds[[rankTargetName]] <- targetRanks
    }
    
    # Calculate the top rank products
    maxRankIds <- apply(combinedPreds[, -(1:(ncol(combinedPreds)-24)),
                                      with = FALSE], 1, which.min)
    targetTopProds <- gsub("ind_|_ult1", "", targetVars[maxRankIds])
    topProdTargetName <- paste(paste0("Weighted", weightedExtension),
                                "TopRank", sep="_")
    combinedPreds[[topProdTargetName]] <- targetTopProds
  }
  
  # Remove large temporary objects to clear memory
  rm(productRanks)
  gc()
}

# Store the base model predictions
saveFolder <- file.path(getwd(), "Submission", "PredictionsComp",
                        submissionDate)
dir.create(saveFolder, showWarnings = FALSE)
saveRDS(combinedPreds, file.path(saveFolder, paste0(modelName, ".rds")))

if(!onlyPreds){
  # Save a random subset of the data
  randomIds <- sample(1:nrow(combinedPreds),
                      round(nrow(combinedPreds)*randomSubsetSize))
  combinedPredsSubset <- combinedPreds[randomIds, ]
  saveRDS(combinedPredsSubset, file.path(saveFolder, paste0(modelName,
                                                            " subset.rds")))
  # Store the probability multipliers
  saveRDS(probMultipliers, file.path(saveFolder, paste0("probMultipliers ",
                                                        modelName, ".rds")))
  
  
  ##########################################################
  # Extract and store the base model confidences over time #
  ##########################################################
  
  # List the available model groups
  modelGroupsFolder <- file.path(getwd(), "First level learners", targetDate,
                                 modelFolder)
  modelGroups <- list.dirs(modelGroupsFolder)[-1]
  modelGroups <- modelGroups[!grepl("no fold BU", modelGroups)]
  nbModelGroups <- length(modelGroups)
  
  # Loop over the model groups and add feature importance information
  foldPredRatios <- NULL
  for(i in 1:nbModelGroups){
    # List the fold files in the considered model group
    modelGroup <- modelGroups[i]
    slashPositions <- gregexpr("\\/", modelGroup)[[1]]
    modelGroupExtension <- substring(modelGroup,
                                     1 + slashPositions[length(slashPositions)])
    modelGroupFiles <- list.files(modelGroup)
    modelGroupFiles <- modelGroupFiles[grepl("Fold", modelGroupFiles)]
    nbModels <- length(modelGroupFiles)
    
    monthsBack <- as.numeric(substring(gsub("Lag.*$", "", modelGroupExtension),
                                       5))
    lag <- as.numeric(gsub("^.*Lag", "", modelGroupExtension))
    
    # Loop over all fold models
    for(j in 1:nbModels){
      modelInfo <- readRDS(file.path(modelGroup, modelGroupFiles[j]))
      importanceMatrix <- modelInfo$importanceMatrix
      if(is.null(modelInfo$foldPredRatio)){
        foldPredRatio <- NA
      } else{
        foldPredRatio <- modelInfo$foldPredRatio
      }
      foldPredRatios <- rbind(foldPredRatios,
                              data.table(
                                targetVar = modelInfo$targetVar,
                                monthsBack = monthsBack,
                                lag = lag,
                                foldPredRatio = foldPredRatio)
      )
    }
  }
  
  # Calculate and store the mean fold prediction ratios
  meanFoldPredRatios <- foldPredRatios[, .(meanFoldRatio = 
                                             mean(foldPredRatio, na.rm=T)),
                                       .(targetVar, monthsBack, lag)]
  meanFoldPredRatios <- meanFoldPredRatios[order(targetVar, -monthsBack), ]
  saveRDS(meanFoldPredRatios, file.path(saveFolder, paste0("meanFoldPredRatios ",
                                                           modelName, ".rds")))
}