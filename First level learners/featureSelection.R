# Study the feature importance by product over time and by product overall

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Target date
targetDate <- "12-11-2016"

# Target model group
# modelGroup <- "train"
modelGroup <- "trainTrainAll"
# modelGroup <- "trainSmallOrdered"

# Consider the topNFeatures
topNFeatures <- 10

# Exponential decay in rank importance
rankImpExponent <- -0.75

# Considered K folds
K <- 5

# Source the weights extraction
source("Common/getModelWeights.R")

# Load the target product weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights",
                                       targetDate, "model weights first.rds"))

# Use the june 16 ranks to weigh features among the different products
# jun16Counts <- c(0, 0, 9673, 9, 2497, 48, 473, 201, 118, 0, 0, 98, 
#                  2065, 114, 20, 14, 6, 3243, 4123, 289, 0, 4472, 4585, 8622)
jun16Counts <- c(0, 0, 9704, 9, 2505, 48, 474, 201, 119, 0, 0, 99, 
                 2072, 114, 20, 14, 6, 3254, 4137, 290, 0, 4487, 4600, 8651)

# List the base products
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
targetVars <- paste0("ind_", baseProducts, "_ult1")
nbTargetVars <- length(targetVars)

# List the excluded predictors
dropPredictors <- c(
  "trainWeight" # Not an explicit feature
  , "hasNewProduct", "nbNewProducts", "hasAnyPosFlank" # Not for training
  , "ncodpers" # Not excluding the customer id is dangerous
  # , "hasMay15Data", "hasJune15Data" # Study if these impact local validation
  , "lastDate" # This information is used by assigning different weights
  , "gapsFrac", "dataMonths", "monthsFrac", "nbLagRecords " # , Leakage?
  # , "gaps" # , Leakage?
  , "grossIncome" # Numerical predictors with better transformed versions
  , "seniorityDensity" # No sensible interpretation
  , paste0(targetVars, "MAPRatioJune15") # Used for tuning conditional predictions
  , paste0(targetVars, "RelMAP") # Used for tuning conditional predictions
  # , paste0(targetVars, "MAPRatioJune15") # Used for tuning conditional predictions
  , "familyId"
  # , names(trainOrig)[(which(grepl("ind_", names(trainOrig)))[1]): ncol(trainOrig)]
  # , names(trainOrig)[1:(which(grepl("ind_", names(trainOrig)))[1] - 1)]
  , targetVars # Including this would make the competition too easy :)
)

# List all possible features
featureSample <- readRDS(file.path(getwd(), "Feature engineering",
                                   "12-11-2016", "train",
                                   "Back0Lag16 features.rds"))
possibleFeatures <- setdiff(colnames(featureSample), dropPredictors)


##########################################################################

# List the available model groups
modelGroupsFolder <- file.path(getwd(), "First level learners", targetDate,
                               modelGroup)
modelGroups <- list.dirs(modelGroupsFolder)[-1]
modelGroups <- modelGroups[!grepl("Manual tuning", modelGroups)]
modelGroups <- modelGroups[!grepl("no fold BU", modelGroups)]
nbModelGroups <- length(modelGroups)
if(nbModelGroups==0){
  modelGroups <- modelGroupsFolder
  nbModelGroups <- 1
}

# Loop over the model groups and add feature importance information
featureImportance <- NULL
for(i in 1:nbModelGroups){
  # List the files in the considered model group
  modelGroup <- modelGroups[i]
  slashPositions <- gregexpr("\\/", modelGroup)[[1]]
  modelGroupExtension <- substring(modelGroup,
                                   1 + slashPositions[length(slashPositions)])
  modelGroupFiles <- list.files(modelGroup)
  modelGroupFiles <- modelGroupFiles[!grepl("no fold BU", modelGroupFiles)]
  nbModels <- length(modelGroupFiles)
  
  monthsBack <- as.numeric(substring(gsub("Lag.*$", "", modelGroupExtension),
                                     5))
  lag <- as.numeric(gsub("^.*Lag", "", modelGroupExtension))
  
  # Loop over all models
  if(nbModels>0){
    for(j in 1:nbModels){
      # Extract whether it is a model fold or a fully trained model
      modelGroupFile <- modelGroupFiles[j]
      isFold <- grepl("Fold", modelGroupFile)
      if(isFold){
        fold <- as.numeric(gsub("^.* Fold | - .*$", "", modelGroupFile))
      } else{
        fold <- NA
      }
      
      modelInfo <- readRDS(file.path(modelGroup, modelGroupFile))
      importanceMatrix <- modelInfo$importanceMatrix
      # Load the product - month weight
      targetVar <- modelInfo$targetVar
      relativeWeight <- getModelWeights(monthsBack, targetVar,
                                        dateTargetWeights)
      
      foldWeight <- ifelse(isFold, 1 - 1/K, 1)
      if(isFold){
        # Adjust fold weights because some models didn't store the fifth fold
        prodMonthFiles <- modelGroupFiles[grepl(targetVar, modelGroupFiles)]
        nbFoldsProd <- sum(grepl("Fold", prodMonthFiles))
        foldWeight <- foldWeight * 4 / nbFoldsProd
      }
      featureRankWeight <- foldWeight *
        ((1:nrow(importanceMatrix))^rankImpExponent)
      featureWeight <- relativeWeight * featureRankWeight
      jun16W <- jun16Counts[match(targetVar, targetVars)]
      featureImportance <- rbind(featureImportance,
                                 data.table(
                                   modelGroupExtension = modelGroupExtension,
                                   targetVar = targetVar,
                                   relativeWeight = relativeWeight,
                                   rank = 1:nrow(importanceMatrix),
                                   monthsBack = monthsBack,
                                   lag = lag,
                                   feature = importanceMatrix$Feature,
                                   isFold = isFold,
                                   fold = fold,
                                   featureRankWeight = featureRankWeight,
                                   featureWeight = featureWeight,
                                   jun16W = jun16W,
                                   overallWeight = featureWeight*jun16W
                                 )
      )
    }
  }
}

# Extract all available features in the models and append
allFeatures <- sort(unique(featureImportance$feature))
nonModeledFeatures <- setdiff(possibleFeatures, allFeatures)
allFeatures <- c(allFeatures, nonModeledFeatures)

# Rank the features by product
overallProductFeatureRanks <-
  featureImportance[,sum(featureWeight),
                    .(feature, targetVar)]
names(overallProductFeatureRanks)[3] <- "weightSum"
overallProductFeatureRanks <-
  overallProductFeatureRanks[order(targetVar, -weightSum)]
overallProductFeatureRanks[,feature_rank := match(1:length(weightSum),
                                                  order(-weightSum)), by=targetVar]

# Calculate the general product ranking (used as default ranking for features
# with a summed weight of 0)
generalRank <- featureImportance[, .(overallWeightSum =
                                       sum(overallWeight)), feature]
generalRank <- generalRank[order(-overallWeightSum)]
sortedFeatures <- generalRank$feature
sortedFeatures <- c(sortedFeatures, nonModeledFeatures)

# Append unused features to all products
for(i in 1:nbTargetVars){
  targetVarLoop <- targetVars[i]
  overallFeatTargetVar <- overallProductFeatureRanks[targetVar==targetVarLoop,
                                                     feature]
  missingFeatures <- setdiff(sortedFeatures, overallFeatTargetVar)
  feature_ranks <- rev(rev(1:length(allFeatures))[1:length(missingFeatures)])
  
  # Append the missing features to overallProductFeatureRanks
  overallProductFeatureRanks <- rbind(overallProductFeatureRanks,
                                      data.table(feature = missingFeatures,
                                                 targetVar = targetVarLoop,
                                                 weightSum = 0,
                                                 feature_rank = feature_ranks))
}

# Order the feature ranks by product
overallProductFeatureRanks <- overallProductFeatureRanks[order(targetVar,
                                                               feature_rank)]

# Rank the features by product and month
productFeatureRanksMonths <-
  featureImportance[,sum(featureRankWeight),
                    .(monthsBack, targetVar, feature)]
names(productFeatureRanksMonths)[4] <- "weightSum"
productFeatureRanksMonths <-
  productFeatureRanksMonths[order(-monthsBack, targetVar, -weightSum)]
productFeatureRanksMonths[,feature_rank := match(1:length(weightSum),
                                                 order(-weightSum)),
                          by=.(monthsBack, targetVar)]

# Append unused features to all product-month combinations of
# productFeatureRanksMonths
monthsBacks <- sort(unique(featureImportance$monthsBack))
nbMonthsBack <- length(monthsBacks)
for(i in 1:nbMonthsBack){
  monthsBackLoop <- monthsBacks[i]
  for(j in 1:nbTargetVars){
    targetVarLoop <- targetVars[j]
    overallFeatTargetVar <-
      productFeatureRanksMonths[targetVar==targetVarLoop &
                                  monthsBack == monthsBackLoop, feature]
    sortedFeatures <-
      overallProductFeatureRanks[targetVar==targetVarLoop, feature]
    if(length(sortedFeatures) != length(allFeatures)) browser()
    missingFeatures <- setdiff(sortedFeatures, overallFeatTargetVar)
    feature_ranks <- rev(rev(1:length(allFeatures))[1:length(missingFeatures)])
    productFeatureRanksMonths <- rbind(productFeatureRanksMonths,
                                       data.table(monthsBack = monthsBackLoop,
                                                  targetVar = targetVarLoop,
                                                  feature = missingFeatures,
                                                  weightSum = 0,
                                                  feature_rank = feature_ranks))
  }
}

# Order the feature ranks by month, product and rank
productFeatureRanksMonths <- productFeatureRanksMonths[
  order(-monthsBack, targetVar, feature_rank)]

# Store the ordered product feature ranks
saveRDS(overallProductFeatureRanks,
        file.path(getwd(), "first level learners", targetDate,
                  "product feature order.rds"))

# Store the ordered product-month feature ranks
saveRDS(productFeatureRanksMonths,
        file.path(getwd(), "first level learners", targetDate,
                  "product month feature order.rds"))