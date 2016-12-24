#########################################################################
# XGBOOST validation using same month model folds - fast idea iteration #
# This can only be used for some validation techniques since I only     #
# consider positive flanks                                              #
# Main conclusion: MAP boosting works!! (Predictions close to unbiased) #
#########################################################################

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
submissionDate <- "19-12-2016"
# loadFileFolder <- "xgboost weighted trainAll 17, validation May16"
targetK <- 10
submissionFileFolder <- paste("self validation", targetK, "Folds")

# Only consider positive flanks in the analysis?
# This is way faster but can not be used for all efforts
onlyPosFlankAnalysis <- TRUE

# Processed months back
# consideredMonthsBack <- 11:11
consideredMonthsBack <- 11 # 0 refers to May 16
nbMonthsBack <- length(consideredMonthsBack)

# Probability multipliers
useProbMultipliers <- FALSE
probMultipliers <- rep(1, 24)
# probMultipliers[18] <- 1.3

# Target date 
targetDate <- "12-11-2016"

# Target train model folders
trainModelsFolder <- "trainTrainAll Top 100 monthProduct 200 rounds 10 Folds"

# Option to store the product predictions
savePredictions <- TRUE

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

if(mapBoosting && !nomPensAboveNominaBothNotOwned){
  stop("MAP boosting requires nomPensAboveNominaBothNotOwned to be TRUE")
}

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


######################################################################

# Create the save folder
# Create the target folder if it does not exist yet
if(savePredictions){
  saveFolder <- file.path(getwd(), "Validation", submissionDate,
                          submissionFileFolder)
  dir.create(saveFolder, showWarnings = FALSE)
}

# Extract clients with positive flanks
posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                               "positive flank clients.rds")
posFlankClients <- readRDS(posFlankClientsFn)

# Load fold ids
foldsPath <- file.path(getwd(), "Second level learners", targetDate,
                       paste("first level ncodpers", targetK, "folds.rds"))
foldNcodpers <- readRDS(foldsPath)

# Keep the track of the average map of all months
monthsMAPs <- rep(NA, nbMonthsBack)

# Loop over the processed months back and generate the out of fold predictions
for(monthsBackId in 1:nbMonthsBack){
  monthsBack <- consideredMonthsBack[monthsBackId]
  
  # Show progress message
  cat("\nProcessing month", monthsBackId, "of", nbMonthsBack, "back",
      monthsBack, "\n")
  
  # Compose the path to the xgboost train models
  modelsBasePath <- file.path(getwd(), "First level learners", targetDate,
                              trainModelsFolder)
  modelGroups <- list.dirs(modelsBasePath)[-1]
  modelGroup <- modelGroups[grepl(paste0("Back", monthsBack, "Lag"),
                                  modelGroups)]
  
  # List the files in the considered model group that are folds
  modelGroupFiles <- list.files(modelGroup)
  modelGroupFiles <- modelGroupFiles[grepl("Fold", modelGroupFiles)]
  
  # Load the features and drop feature records without positive flanks
  featuresOrig <-
    readRDS(file.path(getwd(), "Feature engineering", targetDate, "train",
                      paste0("Back", monthsBack, "Lag", 16 - monthsBack,
                             " features.rds")))
  if(onlyPosFlankAnalysis){
    featuresOrig <- featuresOrig[hasNewProduct == TRUE, ]
  }
  targetVars <- names(featuresOrig)[-(1:(ncol(featuresOrig)-24))]
  nbTargetVars <- length(targetVars)
  
  # Loop over all base models and folds
  allPredictions <- NULL
  for(targetId in 1:nbTargetVars){
    # # Show progress message
    # cat("Processing target variable", targetId, "of", nbTargetVars, "\n")
    
    # Extract the target variable and target models
    targetVar <- targetVars[targetId]
    targetModels <- modelGroupFiles[grepl(targetVar, modelGroupFiles)]
    targetModelFolds <- as.numeric(gsub("^.* Fold | of .*$", "", targetModels))
    nbFolds <- length(targetModels)
    predictions <- rep(0, nrow(featuresOrig))
    
    # Calculate the ids of the users that owned the product in the previous
    # month
    prevOwned <- (is.na(featuresOrig[[targetVar]])) |
      (is.na(featuresOrig[[paste0(targetVar, "Lag1")]])) |
      (featuresOrig[[paste0(targetVar, "Lag1")]] == 1)
    
    # Loop over the folds and generate the relevant predictions
    for(fold in 1:nbFolds){
      targetModelId <- match(fold, targetModelFolds)
      targetModel <- readRDS(file.path(modelGroup, targetModels[targetModelId]))
      targetNcodpers <- intersect(foldNcodpers[[fold]],
                                  featuresOrig$ncodpers[!prevOwned])
      targetRows <- match(targetNcodpers, featuresOrig$ncodpers)
      
      # Check that we are using the right model
      if(targetModel$targetVar != targetVar) browser()
      
      # Generate the predictions for the fold ncodpers
      foldFeatures <- data.matrix(
        featuresOrig[targetRows, targetModel$predictors, with=FALSE])
      foldPredictions <- predict(targetModel$model, foldFeatures, missing=NA)
      predictions[targetRows] <- foldPredictions
    }
    
    # Optionally, apply a probability multiplier
    if(useProbMultipliers){
      predictions <- predictions * probMultipliers[targetId]
    }
    
    # Append the predictions
    predictionsDT <- data.table(ncodpers = featuresOrig$ncodpers,
                                product = targetVar,
                                prediction = predictions)
    
    allPredictions <- rbind(allPredictions, predictionsDT)
  }
  
  predSumProds <- allPredictions[, .(predSum = sum(prediction)), product]
  predSumProds <- predSumProds[order(-predSum), ]
  
  # Optionally, save the base model predictions
  if(savePredictions){
    # Add the positive flanks to allPredictions
    allPredictions$posFlank <- NA
    for(i in 1:nbTargetVars){
      targetVar <- targetVars[i]
      targetRows <- which(allPredictions$product == targetVar)
      allPredictions$posFlank[targetRows] <- 0
      posFlankIds <-
        which(!is.na(featuresOrig[[targetVar]]) &
                featuresOrig[[targetVar]] == 1 &
                !is.na(featuresOrig[[paste0(targetVar, "Lag1")]]) &
                featuresOrig[[paste0(targetVar, "Lag1")]] == 0)
      allPredictions$posFlank[targetRows[posFlankIds]] <- 1
    }
    
    posFlankNcodpers <- featuresOrig[hasNewProduct == TRUE, ncodpers]
    saveRDS(allPredictions[ncodpers %in% posFlankNcodpers, ],
            file.path(saveFolder, paste0("Back", monthsBack, ".rds")))
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
      nonZeroSwappedNcodpers <-
        intersect(swapNcodpers, unique(featuresOrig[hasNewProduct == TRUE,
                                                    ncodpers]))
      allSwapped <- unique(c(allSwapped, nonZeroSwappedNcodpers))
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
  
  # Add the positive flanks to allPredictions
  allPredictions$posFlank <- NA
  for(i in 1:nbTargetVars){
    targetVar <- targetVars[i]
    targetRows <- which(allPredictions$product == targetVar)
    allPredictions$posFlank[targetRows] <- 0
    posFlankIds <-
      which(!is.na(featuresOrig[[targetVar]]) &
              featuresOrig[[targetVar]] == 1 &
              !is.na(featuresOrig[[paste0(targetVar, "Lag1")]]) &
              featuresOrig[[paste0(targetVar, "Lag1")]] == 0)
    allPredictions$posFlank[targetRows[posFlankIds]] <- 1
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
  
  # Calculate the mean average precision@7
  map <- map7(mapData, returnScores = TRUE)
  mapData <- cbind(mapData, map)
  
  # Display the mean MAP
  cat("Mean MAP@7:", paste0(round(mean(map)*100, 3), "%"), "\n")
  monthsMAPs[monthsBackId] <- mean(map)
  
  # Study the mean map by true product
  allPredictions[, map := rep(map, each=24)]
  meanMapSummary <- allPredictions[posFlank==1,
                                   .(meanMap = mean(map), .N), product]
  meanMapSummary <- meanMapSummary[order(-N),]
  
  # Study the rank of nomina and nom pens where there was at least one positive
  # flank
  allPredPosFlank <- allPredictions
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
  submissionGap <- submission[match(gapNcodPers, featuresOrig$ncodpers), ]
  mapGap <- mapData[match(gapNcodPers, featuresOrig$ncodpers), ]
  allPredsGap <- allPredictions[ncodpers %in% gapNcodPers & order_predict < 4, ]
}

# Display the mean overall MAP
cat("\nMean overall MAP@7:", paste0(round(mean(monthsMAPs)*100, 3), "%"), "\n")