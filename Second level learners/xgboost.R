# XGBOOST second level learner

# Clear the workspace
rm(list=ls()); gc()

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)
library(Matrix)
library(Ckmeans.1d.dp)
library(beepr)
library(ggplot2)
library(stringr)

# Set the random seed in order to obtain reproducible results
set.seed(14)

# Target date
targetDate <- "12-11-2016"

# Train features folder
trainModelFolder <- "train Top 100 monthProduct 200 rounds"
saveFolderExtension <- "" # Useful to distinguish different hyperparameter sets

# Overwrite existing models?
overwrite <- FALSE

# Feature selection options
featureSelectionFirst <- TRUE
topFeaturesFirst <- 100
featureSelectionFirstMode <- c("monthProduct", "product")[1]

# List the considered target variable ids. Id 0 relates to modeling hasNewProd
targetIds <- 0:24
# targetIds <- c(3, 5, 7, 13, 18, 19, 22, 23, 24)
# targetIds <- 18

# Exclude train records without no new products
# I expect slightly better results without excluding at a high computational
# cost.
# My assumption was correct :D.
excludeNoNewProducts <- FALSE # This is the most important toggle variable!
excludeString <- ifelse(excludeNoNewProducts, "",  "TrainAll")

# Xgboost hyperparameters
nrounds <- 1e2
# Different hyperparameter settings:
# Set 1 is used for a limited number of positive flanks (<1000).
# Set 2 is a deeper model and is used to model >=1000 positive flanks
hyperparSetSimple <- list(nrounds = nrounds, etaC = 10, subsample = 1,
                          colsample_bytree = 0.5, max.depth = 6,
                          min_child_weight = 0, gamma = 0)
hyperparSetExtended <- list(nrounds = nrounds, etaC = 10, subsample = 1,
                            colsample_bytree = 0.5, max.depth = 8,
                            min_child_weight = 0, gamma = 0.1)

# cco_fin (3): Prediction ratio TP/FP Jun15: 16.48 (17.33) (cco_tuning)
# cno_fin (5): Prediction ratio TP/FP Jun15: 160.68 
# ecue_fin (13): Prediction ratio TP/FP Jun15: 319.25
# reca_fin (18): Prediction ratio TP/FP Jun15: 9.25 (9.37) (cco_tuning)
# tjcr_fin (19): Prediction ratio TP/FP Jun15: 38.68
# nomina (22): Prediction ratio TP/FP Jun15: 85.78
# nom_pens (23): Prediction ratio TP/FP Jun15: 122.44
# recibo (24): Prediction ratio TP/FP Jun15: 23.79 (24.23) (cco_tuning)

# cco_fin (3): Prediction ratio TP/FP Dec15: 40.38(43.28) (cco_tuning)
# cno_fin (5): Prediction ratio TP/FP Dec15:  
# ecue_fin (13): Prediction ratio TP/FP Dec15: 
# reca_fin (18): Prediction ratio TP/FP Dec15: 3.68(3.67) (cco_tuning)
# tjcr_fin (19): Prediction ratio TP/FP Dec15: 
# nomina (22): Prediction ratio TP/FP Dec15: 
# nom_pens (23): Prediction ratio TP/FP Dec15: 
# recibo (24): Prediction ratio TP/FP Dec15: 

# K in K-fold cross validation. Setting K to one results in building a single
# model. K-fold cross validation is used in the first phase of generating 
# base models. The second phase is the out of training time validation. 
baseK <- 5
if(baseK <= 1) browser()
K <- 1
saveBaseModels <- TRUE
saveCommonModel <- TRUE

# File name of the stacking folds
useStackingFolds <- TRUE
stackingIdsFn <- "second level ncodpers 5 folds.rds"

# Show variable importance plot
showVariableImportance <- FALSE

# Execution time range in 24 hour notation (option to start fit at certain hour)
timeRange <- c(0, 24)

# List the base products and target variables (id 0 === hasNewProduct)
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
targetVars <- paste0("ind_", baseProducts, "_ult1")


############################################################################

# List the feature files in the train model folder
featuresPath <- file.path(getwd(), "Second level learners", "Features",
                          targetDate,  trainModelFolder)
featureFiles <- list.files(featuresPath)[-(1:2)] #[-(1:11)])
featureFiles <- featureFiles[grepl("Back.*.rds$", featureFiles)]
trainFnBases <- gsub(".rds$", "", featureFiles)
nbFeatureFiles <- length(featureFiles)
trainFeaturePaths <- paste(featuresPath, featureFiles, sep="/")

# Don't train a model for hasNewProduct when training on all records
if(!excludeNoNewProducts){
  targetIds <- targetIds[targetIds!=0]
}

# Optionally inspect variable importance for models of interest
inspectVarImpTopModels <- FALSE
inspectIds <- c(3, 5, 7, 13, 18, 19, 22, 23, 24)

# Low priority ids - only model using one tenth of the rounds
# lowPriorityIds <- c(1, 2, 4, 10, 11, 15, 16, 17, 21)
lowPriorityIds <- NULL

nbTargetIds <- length(targetIds)

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

# Source the common weights extraction and hyperpar description logic
source("Common/getModelWeights.R")
source("Common/getHyperParDescr.R")


########################################################################

# Wait until the time period is valid
while(TRUE){
  if(as.numeric(format(Sys.time(),"%H")) >= timeRange[1] &&
     as.numeric(format(Sys.time(),"%H")) <= timeRange[2]){
    break
  }
  
  # Display sleep message
  cat(paste0("Invalid time range, sleeping for five minutes @"),
      as.character(Sys.time()), "\n")
  
  # Sleep since the features file is not available
  Sys.sleep(300) 
}

# Load the target product weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights second.rds"))

# Load the feature order when using feature selection
if(featureSelectionFirst){
  if(featureSelectionFirstMode == "monthProduct"){
    featureOrdersFirst <-
      readRDS(file.path(getwd(), "first level learners", targetDate,
                        "product month feature order.rds"))
  } else{
    featureOrdersFirst <-
      readRDS(file.path(getwd(), "first level learners", targetDate,
                        "product feature order.rds"))
  }
}

# Load the stacking fold ids
if(useStackingFolds && K>1){
  stackingFoldsPath <- file.path(getwd(), "Second level learners", targetDate,
                                 stackingIdsFn)
  stackingFolds <- readRDS(stackingFoldsPath)
}

# Generate all the training models
for(modelGroupId in 1:nbFeatureFiles){
  # Display progress message
  cat("Learning xgboost models for month", modelGroupId, "of",
      nbFeatureFiles , "@", as.character(Sys.time()), "\n\n")
  
  # Read the training data
  trainOrig <- readRDS(trainFeaturePaths[modelGroupId])
  
  # Calculate the predictors (all columns minus the dropped predictors)
  predictors <- setdiff(names(trainOrig), unique(dropPredictors))
  predictorsOrig <- predictors
  
  # Create the target folder if it does not exist yet
  saveDir <- file.path(getwd(), "Second level learners", "Models", targetDate)
  dir.create(saveDir, showWarnings = FALSE)
  
  # Create the model subfolder if it does not exist yet
  baseModelDir <- file.path(saveDir, paste0(trainModelFolder, excludeString,
                                            saveFolderExtension))
  dir.create(baseModelDir, showWarnings = FALSE)
  
  # Create the model group save folder
  if(nbFeatureFiles==1){
    saveModelDir <- baseModelDir
  } else{
    saveModelDir <- file.path(baseModelDir, trainFnBases[modelGroupId])
    dir.create(saveModelDir, showWarnings = FALSE)
  }
  
  ###########################################################################
  
  # Exclude records without no new products if the according flag is TRUE
  if(excludeNoNewProducts && min(targetIds)>0){
    trainOrig <- trainOrig[hasNewProduct == TRUE, ]
  }
  
  # Extract clients that have any positive flanks in the training period
  posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                                 "positive flank clients.rds")
  posFlankClients <- readRDS(posFlankClientsFn)
  
  # Reserve space to store the mean fraction of positive flanks in the train
  # data
  meanPosFlanks <- rep(NA, nbTargetIds)
  
  # Loop over the target variables and repeat the model generation process
  for(targetIndex in 1:nbTargetIds){
    # Extract the relevant target variable
    targetId <- targetIds[targetIndex]
    
    if(targetId==0){
      targetVar <- "hasNewProduct"
    } else{
      targetVar <- targetVars[targetId]
    }
    
    # Display progress message
    cat("Learning xgboost models for target variable", targetVar, targetIndex,
        "of", nbTargetIds , "@", as.character(Sys.time()), "\n")
    
    # Load the observation weights
    observationWeights <- getModelWeights(trainOrig$lastDate, targetVar,
                                          dateTargetWeights)
    trainOrig[, trainWeight := observationWeights]
    
    # Skip the modeling effort if overwrite is FALSE and the model does not
    # exist yet
    savePath <- file.path(saveModelDir, paste0(targetVar, ".rds"))
    saveDirFiles <- list.files(saveModelDir)
    # if(overwrite || !file.exists(savePath))
    if(overwrite || !any(grepl(targetVar, saveDirFiles))){
      
      # Optionally restrict the features to the bare minimum to study the
      # misalignment between the test and validation MAP
      predictors <- predictorsOrig
      
      # Optionally, restrict the features to the loaded top features
      if(featureSelectionFirst){
        targetVarLoop <- targetVar
        monthsBackLoop <- 12*2016 + 5 - 12*year(trainOrig$lastDate[1]) -
          month(trainOrig$lastDate[1])
        if(featureSelectionFirstMode == "monthProduct"){
          featureOrder <-
            featureOrdersFirst[targetVar == targetVarLoop &
                            monthsBack == monthsBackLoop, feature]
        } else{
          featureOrder <- featureOrdersFirst[targetVar == targetVarLoop, feature]
        }
        featureOrder <- featureOrder[featureOrder %in% predictors]
        if(topFeaturesFirst < length(featureOrder)){
          excludedFeatures <- featureOrder[-(1:topFeaturesFirst)]
          predictors <- setdiff(predictors, excludedFeatures)
        }
      }
      
      # Copy the original train data to train
      if(excludeNoNewProducts){
        train <- trainOrig[targetId==0 | (hasNewProduct == TRUE), ]
      } else{
        train <- trainOrig
      }
      gc()
      
      # Exclude records where the target variable is missing or already 1
      if(targetId>0){
        dropTarget <- is.na(train[[targetVar]]) |
          (is.na(train[[paste0(targetVar, "Lag1")]])) |
          (train[[paste0(targetVar, "Lag1")]] == 1)
        train <- train[!dropTarget, ]
      }
      
      # Add the mean number of positive flanks in all of the train data
      meanPosFlanks[targetIndex] <- sum(train[[targetVar]])/nrow(trainOrig)
      
      # Generate the random folds for the K-fold cross validation
      nbModels <- ifelse(K==1, 1, ifelse(saveCommonModel, K+1, K))
      foldIds <- vector(mode = "list", length = nbModels)
      allNcodpers <- sort(unique(train$ncodpers))
      if(K>1){
        if(useStackingFolds){
          for(j in 1:K){
            stackingFold <- stackingFolds[[j]]
            foldIds[[j]] <- stackingFold[stackingFold %in% allNcodpers]
          }
        } else{
          folds <- sample(cut(seq(1, length(allNcodpers)), breaks = K,
                              labels = FALSE))
          for(j in 1:K){
            foldIds[[j]] <- allNcodpers[folds==j]
          }
        }
      }
      
      # Reserve space for the cross validation predictions
      allPredictions <- rep(NA, nrow(train))
      
      # Loop over all folds and generate the xgboost learners
      for(i in 1:nbModels){
        # Extract the excluded person ids
        excludeIds <- foldIds[[i]]
        
        # Display progress message
        cat("Learning xgboost model for fold", i, "of", nbModels , "@",
            as.character(Sys.time()), "\n")
        
        # Extract predictors data and labels from the features data
        features <- train[!ncodpers %in% excludeIds,]
        predictorData <- features[, predictors, with=FALSE]
        labels <- features[, targetVar, with=FALSE][[1]]
        trainWeights <- features[, "trainWeight", with=FALSE][[1]]
        
        # Set the weights to 1 if they are all zero
        if(all(trainWeights==0)){
          trainWeights <- rep(1, length(trainWeights))
        }
        
        # Convert the predictor data to a matrix
        predictorData <- data.matrix(predictorData)
        
        # Select the desired hyperparameter set
        if(sum(labels)>=1e3){
          hyperpar <- hyperparSetExtended
        } else{
          hyperpar <- hyperparSetSimple
        }
        
        # Adjust the number of rounds based on the number of excluded records
        foldRounds <- hyperpar$nrounds *
          (1 + ifelse(i==nbModels & (saveCommonModel | K==1), 1/baseK, 0))
        if(targetId %in% lowPriorityIds){
          foldRounds <- round(foldRounds/10)
        } else{
          foldRounds <- round(foldRounds)
        }
        
        # Learn the model
        model <- xgboost(data = predictorData, label = labels
                         , eta = hyperpar$etaC/foldRounds, nrounds = foldRounds
                         , subsample = hyperpar$subsample
                         , colsample_bytree = hyperpar$colsample_bytree
                         , max.depth = hyperpar$max.depth
                         , min_child_weight = hyperpar$min_child_weight
                         , gamma = hyperpar$gamma
                         , objective = "reg:logistic"
                         , eval_metric = "logloss"
                         , missing = NA
                         # verbose = 0 is the silent mode, 1 is the default
                         , verbose = 0
                         # as.numeric(targetId==0 || !excludeNoNewProducts)
                         , save_period = NULL # Don't save a temp model file
                         , weight = (trainWeights/mean(trainWeights))
        )
        
        # Predict the out of bag items
        oobFeatures <- train[train$ncodpers %in% excludeIds,]
        predictorDataOob <- oobFeatures[, predictors, with=FALSE]
        predictorDataOob <- data.matrix(predictorDataOob)
        if(nrow(predictorDataOob)>0){
          predOob <- predict(model, predictorDataOob, missing=NA)
          allPredictions[train$ncodpers %in% excludeIds] <- predOob
        }
        
        # Calculate the log loss for the fold
        if(nrow(predictorDataOob)>0){
          foldPredIds <- train$ncodpers %in% excludeIds
          analyzedPreds <- allPredictions[foldPredIds]
          analyzedLabels <- train[[targetVar]][foldPredIds]
          foldLL <- -(sum(log(analyzedPreds[analyzedLabels==1])) + 
                        sum(log(1-analyzedPreds[analyzedLabels==0])))
          cat("Fold log loss:", foldLL, "\n")
          foldTP <- sum(analyzedLabels==1)
          foldPredRatio <- round(mean(analyzedPreds[analyzedLabels==1])/
                                   mean(analyzedPreds[analyzedLabels==0]), 2)
          cat("Fold prediction ratio TP/FP:", foldPredRatio, "\n")
        } else{
          foldPredRatio <- NA
          foldTP <- NA
        }
        
        # Assess feature importance
        assessFeatureImportance <- showVariableImportance ||
          (inspectVarImpTopModels && targetId %in% inspectIds)
        if(assessFeatureImportance){
          importanceMatrix <- xgb.importance(predictors, model = model)
          p <- xgb.plot.importance(importanceMatrix)
          print(p)
        }
        
        if((i==nbModels || saveBaseModels) && !assessFeatureImportance){
          importanceMatrix <- xgb.importance(predictors, model = model)
        }
        
        # Optionally, store the base models
        # Store the overall model
        if(saveBaseModels && i<(K+1) && (!(i==1 && K==1))){
          hyperParDescr <- getHyperParDescr(hyperpar)
          saveBasePath <- file.path(saveModelDir,
                                    paste0(targetVar, " Fold ", i, " - ",
                                           hyperParDescr, ".rds"))
          saveRDS(list(targetVar=targetVar, model=model, predictors=predictors,
                       hyperpar=hyperpar, importanceMatrix=importanceMatrix,
                       timeStamp = as.character(Sys.time()),
                       foldPredRatio = foldPredRatio, foldTP = foldTP),
                  saveBasePath)
        }
      }
      
      # Assess the out of bag log loss 
      nbPosFlanks <- sum(train[[targetVar]])
      foldPredRatio <- NA
      foldTP <- NA
      if(any(!is.na(allPredictions))){
        newProductIds <- !is.na(allPredictions) & (train$hasNewProduct | 
                                                     (targetId == 0) |
                                                     !excludeNoNewProducts)
        analyzeRecordIds <- newProductIds & (train$trainWeight ==
                                               max(train$trainWeight))
        analyzedPreds <- allPredictions[analyzeRecordIds]
        analyzedLabels <- train[[targetVar]][analyzeRecordIds]
        nbPosFlanks <- sum(analyzedLabels)
        boxplot(log(analyzedPreds) ~ analyzedLabels,
                main=paste0(targetIndex, " - ", targetVar, " (", nbPosFlanks,
                            ")"))
        meanLL <- -(sum(log(analyzedPreds[analyzedLabels==1])) + 
                      sum(log(1-analyzedPreds[analyzedLabels==0]))) /
          length(analyzedPreds)
        cat("Out of bag mean log loss for", paste0(targetVar, ":"), meanLL,
            "\n")
        foldTP <- sum(analyzedLabels==1)
        foldPredRatio <- round(mean(analyzedPreds[analyzedLabels==1])/
                                 mean(analyzedPreds[analyzedLabels==0]), 2)
        cat("Prediction ratio TP/FP:", foldPredRatio, "\n")
        cat("(Mean predictions, mean target)",
            c(mean(allPredictions[analyzeRecordIds]), mean(train[[targetVar]][
              analyzeRecordIds])), "\n")
      }
      cat("\n")
      
      # Store the overall model
      if(K==1 || saveCommonModel){
        saveRDS(list(targetVar=targetVar, model=model, predictors=predictors,
                     hyperpar=hyperpar, importanceMatrix=importanceMatrix,
                     timeStamp=as.character(Sys.time()),
                     foldPredRatio=foldPredRatio, foldTP=foldTP),
                savePath)
      }
    }
  }
  
  # # Plot the mean fraction of positive flanks for the train data
  # plot(meanPosFlanks)
  # targetVars[order(meanPosFlanks, decreasing=TRUE)]
}

# Play a positive sound to celebrate that the model was learnt :)
beep(sound = "fanfare")