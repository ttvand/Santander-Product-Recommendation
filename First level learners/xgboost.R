# XGBOOST first level learner

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
trainModelFolder <- "train"
saveFolderExtension <- " Top 100 monthProduct 200 rounds 20 Folds" 
# saveFolderExtension <- "" # Useful for stacking and ensembling
testModel <- grepl("test", trainModelFolder)

# Overwrite existing models?
overwrite <- FALSE

# Feature selection options
featureSelection <- TRUE
topFeatures <- 100
featureSelectionMode <- c("monthProduct", "product")[1]

# List the considered target variable ids. Id 0 relates to modeling hasNewProd
targetIds <- 0:24
# targetIds <- c(3, 5, 18, 19, 22, 23, 24)
# targetIds <- 3
# targetIds <- 22:23

# Exclude train records without no new products
# I expect slightly better results without excluding at a high computational
# cost.
# My assumption was correct :D.
excludeNoNewProducts <- FALSE # This is the most important toggle variable!
jointModelNoNewProducts <- FALSE # Ignored if excludeNoNewProducts is FALSE
excludeNoPosFlanks <- FALSE
excludeString <- ifelse(excludeNoNewProducts, "", 
                        ifelse(excludeNoPosFlanks, "PosFlankCusts", "TrainAll"))

# Option to undersample nom_pens positive flanks where nomina does not have
# a positive flank
underSampleNomPensNoNomina <- FALSE # Keep FALSE!
maxMonthNomPensNoNomina <- 150

# Xgboost hyperparameters
nrounds <- 2e2
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
baseK <- 20
if(baseK <= 1) browser()
K <- 20
saveBaseModels <- TRUE
skipCommonModel <- TRUE
# skipCommonSecondLoop <- TRUE

# Bootstrap options
bootstrap <- FALSE
nbBoots <- ifelse(bootstrap, 5, 1)
extraBootstrapDepth <- ifelse(bootstrap, 1, 0)
if(bootstrap && K>1){
  stop("Please Tom, don't combine bootstrap with cross validation")
}

# File name of the stacking folds
useStackingFolds <- TRUE
stackingIdsFn <- paste("first level ncodpers", baseK, "folds.rds")

# Maximum number of allowed train records to handle memory constraints
maxTrainRecords <- Inf #3e6

# Show variable importance plot
showVariableImportance <- FALSE

# Show ggplot of the mayFlag versus the mean label
showMeanLabelByMayFlag <- FALSE

# Execution time range in 24 hour notation (option to start fit at certain hour)
timeRange <- c(0, 24)

# Option to consider a simple model:
# - History on the target variable
# - Lag1 of all other target variables
# - No other features
simpleModeling <- FALSE

# Option to drop product related features when modeling the positive flank
# probabilities
dropProductFeaturesPosFlankProd <- FALSE

# Option to only consider own lag behavior
dropOtherIndFeatures <- FALSE

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
featuresPath <- file.path(getwd(), "Feature engineering", targetDate, 
                          trainModelFolder)
featureFiles <- list.files(featuresPath)[c(3, 10)] #[-c(1,2)] #[-(1:11)])
featureFiles <- featureFiles[grepl(paste0(ifelse(testModel, "Lag17 ", ""),
                                          "features.rds$"), featureFiles)]
trainFnBases <- gsub(" features.rds$", "", featureFiles)
batchFeatures <- all(grepl("batch", trainFnBases, ignore.case = TRUE))
nbFeatureFiles <- length(featureFiles)
trainFeaturePaths <- paste(featuresPath, featureFiles, sep="/")

# Don't train a model for hasNewProduct when training on all records
if(!excludeNoNewProducts || testModel){
  targetIds <- targetIds[targetIds!=0]
}

# List the first non zero target id
firstNonZeroTarget <- which(targetIds!=0)[1]

# Optionally inspect variable importance for models of interest
inspectVarImpTopModels <- FALSE
inspectIds <- c(3, 5, 7, 13, 18, 19, 22, 23, 24)

# Low priority ids - only model using one tenth of the rounds
# lowPriorityIds <- c(1, 2, 4, 10, 11, 15, 16, 17, 21)
lowPriorityIds <- NULL

nbTargetIds <- length(targetIds)

# # List the month weights
# weightDates <- as.Date(paste(c(rep(2015, 9), rep(2016, 6)),
#                              str_pad(c(4:12, 1:6), 2, pad='0'), 28, sep="-"))
# dateWeights <- c(1.2, 1.3, 13, 0.1*(15:25), 1)
# dateWeights <- c(1, (1+1e-5), rep(1, length(weightDates) - 2))
# dateWeights <- c(0, 1, rep(0, length(weightDates) - 2))

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
                                       "model weights first.rds"))

# Load the feature order when using feature selection
if(featureSelection){
  if(featureSelectionMode == "monthProduct"){
    featureOrders <-
      readRDS(file.path(getwd(), "first level learners", targetDate,
                        "product month feature order.rds"))
  } else{
    featureOrders <-
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
  
  # Add the target values for test models (semi parametric learning: the best
  # submission is assumed to be the ground truth)
  if(testModel){
    # Load the best submission
    submission <- readRDS(bestSubmissionFn)
    submissionProds <- sort(unique(submission$product))
    for(i in 1:length(submissionProds)){
      targetSubmissionProduct <- submissionProds[i]
      submissionProdRows <- submission[product == targetSubmissionProduct, ]
      trainOrig[[targetSubmissionProduct]] <-
        submissionProdRows[match(trainOrig$ncodpers,
                                 submissionProdRows$ncodpers),
                           totalProb]
      
    }
  }
  
  # Calculate the predictors (all columns minus the dropped predictors)
  predictors <- setdiff(names(trainOrig), unique(dropPredictors))
  predictorsOrig <- predictors
  
  # # Calculate the train record weights
  # observationWeights <- getModelWeights(trainOrig$lastDate, targetVar,
  #                                  dateTargetWeights)
  # trainOrig[, trainWeight := observationWeights]
  # trainOrig[, trainWeight := dateWeights[match(trainOrig$lastDate,
  # weightDates)]]
  
  # Create the target folder if it does not exist yet
  saveDir <- file.path(getwd(), "First level learners", targetDate)
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
  if(excludeNoPosFlanks && min(targetIds)>0){
    trainOrig <- trainOrig[ncodpers %in% posFlankClients, ]
  }
  
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
    
    # Merge all feature files if we reach the first non zero target and if
    # both excludeNoNewProducts and jointModelNoNewProducts are TRUE
    if(!is.null(firstNonZeroTarget) && targetIndex==firstNonZeroTarget &&
       excludeNoNewProducts && jointModelNoNewProducts && !testModel &&
       nbFeatureFiles>1){
      if(modelGroupId==1){
        # Combine the new product training data
        trainOrig <- trainOrig[hasNewProduct == TRUE, ]
        trainOrig[, trainWeight := NULL]
        for(j in 2:nbFeatureFiles){
          trainBatch <- readRDS(trainFeaturePaths[j])
          trainOrig <- rbind(trainOrig, trainBatch[hasNewProduct == TRUE, ])
        }
      } else{
        break # Don't recalculate the same models
      }
    }
    
    # Load the observation weights
    observationWeights <- getModelWeights(trainOrig$lastDate, targetVar,
                                          dateTargetWeights)
    trainOrig[, trainWeight := observationWeights]
    
    # Skip the modeling effort if overwrite is FALSE and the model does not
    # exist yet
    # savePath <- file.path(saveModelDir, paste0(targetVar, ".rds"))
    saveDirFiles <- list.files(saveModelDir)
    # if(overwrite || !file.exists(savePath))
    if(overwrite || !any(grepl(targetVar, saveDirFiles) & (
      !bootstrap | grepl("Boot", saveDirFiles)))){
      
      # Optionally restrict the features to the bare minimum to study the
      # misalignment between the test and validation MAP
      predictors <- predictorsOrig
      if(simpleModeling){
        consideredFeatures <- c(grep(targetVar, predictors, ignore.case=TRUE,
                                     value = TRUE), paste0(targetVars, "Lag1"))
        predictors <- unique(consideredFeatures)
      }
      if(targetId == 0 && dropProductFeaturesPosFlankProd){
        # Drop predictors related to specific products
        predictors <- predictors[!grepl("ult1", predictors)]
      }
      if(dropOtherIndFeatures && targetId != 0){
        predictors <- predictors[!grepl("ult1", predictors) |
                                   grepl(baseProducts[targetId], predictors)]
      }
      
      # Optionally, restrict the features to the loaded top features
      if(featureSelection){
        targetVarLoop <- targetVar
        monthsBackLoop <- 12*2016 + 5 - 12*year(trainOrig$lastDate[1]) -
          month(trainOrig$lastDate[1])
        if(featureSelectionMode == "monthProduct"){
          featureOrder <-
            featureOrders[targetVar == targetVarLoop &
                            monthsBack == monthsBackLoop, feature]
        } else{
          featureOrder <- featureOrders[targetVar == targetVarLoop, feature]
        }
        featureOrder <- featureOrder[featureOrder %in% predictors]
        if(topFeatures < length(featureOrder)){
          excludedFeatures <- featureOrder[-(1:topFeatures)]
          predictors <- setdiff(predictors, excludedFeatures)
        }
      }
      
      # Iterate over the different bootstrap ids
      for(bootId in 1:nbBoots){
        # Calculate the bootstrap extension
        bootExtension <- ifelse(bootstrap, paste0(" - Boot ", bootId), "")
        
        # Show a progress message if more than one bootstrap replicate is
        # being processed
        if(nbBoots>1){
          cat("Bootstrap replicate", bootId, "of", nbBoots, "@",
              as.character(Sys.time()), "\n")
        }
        
        # Copy the original train data to train
        if(excludeNoNewProducts){
          train <- trainOrig[targetId==0 | (hasNewProduct == TRUE), ]
        } else{
          if(excludeNoPosFlanks){
            train <- trainOrig[targetId==0 | (ncodpers %in% posFlankClients), ]
          } else{
            train <- trainOrig
          }
        }
        
        # Sample down on train if the target id is 0 (memory issues)
        if(targetId==0 && nrow(train)>maxTrainRecords){
          train <- train[sample(1:nrow(train), maxTrainRecords)]
        }
        gc()
        
        # Exclude records where the target variable is missing or already 1
        if(targetId>0){
          dropTarget <- is.na(train[[targetVar]]) |
            (train[[paste0(targetVar, "Lag1")]] == 1)
          train <- train[!dropTarget, ]
        }
        
        # Optionally, under sample the positive flanks of nom_pens where 
        # nomina does not show a positive flank
        if(underSampleNomPensNoNomina && targetVar=="ind_nom_pens_ult1"){
          posFlankIdsNpNoNom <- which(train[[paste0(targetVar, "Lag1")]] == 0 &
                                        train[[targetVar]] == 1 &
                                        !is.na(train[["ind_nomina_ult1"]]) &
                                        (train[["ind_nomina_ult1"]] == 0 |
                                           train[["ind_nomina_ult1Lag1"]] == 1))
          if(length(posFlankIdsNpNoNom)>maxMonthNomPensNoNomina){
            # set.seed(14)
            keepTarget <- sample(posFlankIdsNpNoNom, maxMonthNomPensNoNomina)
            dropTarget <- setdiff(posFlankIdsNpNoNom, keepTarget)
            train <- train[-dropTarget, ]
          }
        }
        
        # Plot the true class versus the presence of the May 15 flag
        nbPosFlanks <- sum(train[[targetVar]])
        plotTitle <- paste0(targetId, " - ", targetVar, " (", nbPosFlanks, ")")
        if(showMeanLabelByMayFlag){
          plotData <- data.frame(MayFlag = train$hasMay15Data,
                                 JuneFlag = train$hasJune15Data,
                                 labels = as.numeric(train[[targetVar]]))
          p <- ggplot(plotData, aes(x=MayFlag, y=labels, fill=MayFlag)) +
            stat_summary(fun.y="mean", geom="bar") +
            ggtitle(plotTitle)
          print(p)
        }
        
        # Add the mean number of positive flanks in all of the train data
        meanPosFlanks[targetIndex] <- sum(train[[targetVar]])/nrow(trainOrig)
        
        # Sample with replacement from the train data when using bootstrapping
        if(bootstrap){
          train <- train[sample(1:nrow(train), nrow(train), replace = TRUE), ]
        }
        
        # Generate the random folds for the K-fold cross validation
        nbModels <- ifelse(K==1, 1, ifelse(skipCommonModel, K, K+1))
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
          if(nbModels>1){
            cat("Learning xgboost model for fold", i, "of", nbModels , "@",
                as.character(Sys.time()), "\n")
          }
          
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
          
          # Add the bootstrap max depth to the hyperparameters
          hyperpar$max.depth <- hyperpar$max.depth + extraBootstrapDepth
          
          # Adjust the number of rounds based on the number of excluded records
          foldRounds <- hyperpar$nrounds *
            (1 + ifelse(i==nbModels & (K==1 | !skipCommonModel), 1/baseK, 0))
          if(targetId %in% lowPriorityIds){
            foldRounds <- round(foldRounds/10)
          } else{
            foldRounds <- round(foldRounds)
          }
          
          # Learn the model
          model <- xgboost(data = predictorData, label = labels
                           , eta = hyperpar$etaC/foldRounds
                           , nrounds = foldRounds
                           , subsample = hyperpar$subsample
                           , colsample_bytree = hyperpar$colsample_bytree
                           , max.depth =
                             hyperpar$max.depth
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
          if(saveBaseModels && i<(K+1) && (!(i==1 && K==1))){
            hyperParDescr <- getHyperParDescr(hyperpar)
            saveBasePath <- file.path(saveModelDir,
                                      paste0(targetVar, " Fold ", i, " of ",
                                             baseK, " - ", hyperParDescr,
                                             bootExtension, ".rds"))
            saveRDS(list(targetVar=targetVar, model=model,
                         predictors=predictors, hyperpar=hyperpar,
                         importanceMatrix=importanceMatrix,
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
          if(testModel){
            newProductIds <- !is.na(allPredictions)
          } else{
            newProductIds <- !is.na(allPredictions) & (train$hasNewProduct | 
                                                         (targetId == 0) |
                                                         !excludeNoNewProducts)
          }
          analyzeRecordIds <- newProductIds &
            (train$trainWeight == max(train$trainWeight))
          analyzedPreds <- allPredictions[analyzeRecordIds]
          analyzedLabels <- train[[targetVar]][analyzeRecordIds]
          nbPosFlanks <- sum(analyzedLabels)
          if(testModel){
            predLabelCorr <- cor(analyzedPreds, analyzedLabels)
            plot(analyzedPreds, analyzedLabels,
                 main = paste("Predicted vs label correlation:",
                              round(predLabelCorr, 3)))
          } else{
            boxplot(analyzedPreds ~ analyzedLabels,
                    main=paste0(targetIndex, " - ", targetVar, " (",
                                nbPosFlanks, ")"))
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
                c(mean(allPredictions[analyzeRecordIds]),
                  mean(train[[targetVar]][analyzeRecordIds])), "\n")
          }
        }
        cat("\n")
        
        # Store the overall model
        if(K==1 || !skipCommonModel){
          savePath <- file.path(saveModelDir, paste0(targetVar,
                                                     bootExtension, ".rds"))
          saveRDS(list(targetVar=targetVar, model=model, predictors=predictors,
                       hyperpar=hyperpar, importanceMatrix=importanceMatrix,
                       timeStamp=as.character(Sys.time()),
                       foldPredRatio=foldPredRatio, foldTP=foldTP),
                  savePath)
        }
      }
    }
  }
}

# Play a positive sound to celebrate that the models were learned :)
beep(sound = "fanfare")