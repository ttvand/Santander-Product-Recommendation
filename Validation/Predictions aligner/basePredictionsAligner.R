##############################################################################
# Combine the base model predictions in a smart way: only use weighted       #
# average of the base models that are not too different from the last couple #
# of lags for selected target products
##############################################################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Submission date and file name of the compared predictions
submissionDate <- "19-12-2016"
selfFolder <- "self validation May16 10 Folds"
selfFn <- "Back0.rds"
validationExtension <- "xgboost weighted trainAll 21, validation May16"

# Modified target variables
adjustedProducts <- NULL #ind_ecue_fin_ult1"
adjustedProducts <- c("ind_ahor_fin_ult1", "ind_aval_fin_ult1", "ind_cco_fin_ult1", 
                      "ind_cder_fin_ult1", "ind_cno_fin_ult1", "ind_ctju_fin_ult1", 
                      "ind_ctma_fin_ult1", "ind_ctop_fin_ult1", "ind_ctpp_fin_ult1", 
                      "ind_deco_fin_ult1", "ind_deme_fin_ult1", "ind_dela_fin_ult1", 
                      "ind_ecue_fin_ult1", "ind_fond_fin_ult1", "ind_hip_fin_ult1", 
                      "ind_plan_fin_ult1", "ind_pres_fin_ult1", "ind_reca_fin_ult1", 
                      "ind_tjcr_fin_ult1", "ind_valo_fin_ult1", "ind_viv_fin_ult1", 
                      "ind_nomina_ult1", "ind_nom_pens_ult1", "ind_recibo_ult1")

adjustedProducts <- c("ind_cco_fin_ult1", "ind_cno_fin_ult1", "ind_ctju_fin_ult1", 
                      "ind_ctma_fin_ult1",  "ind_ecue_fin_ult1", "ind_tjcr_fin_ult1",
                      "ind_nomina_ult1", "ind_nom_pens_ult1", "ind_recibo_ult1")
# adjustedProducts <- "ind_recibo_ult1"
adjustedProducts <- NULL

# Last considered lags for lag selection and relative consideration cutoff
lastLagsWeightSelection <- 3
validLagRelCutoff <- 4
minConsideredLagsCutoff <- 4

# MAP boosting options - all other ignored if mapBoosting is FALSE
mapBoosting <- FALSE
maxDiffNominaNomPensMapBoosting <- Inf # Typical value: Inf; Inf means no limit
maxRelDiffNominaNomPensMapBoosting <- 0.5 # Typical value: 0.8; 1 means no action
averageNominaNomPensProbsMAPBoosting <- TRUE # Min mode is equal to close gaps!
averageNominaNomPensProbsMAPBoostingMethod <- c("Average", "Min")[1]
consideredMapTopPredictions <- 1:6
swapRelativeCutoff <- 1 # Typical value: 1; 0 means no swaps
swapProducts <- c("ind_cco_fin_ult1", "ind_cno_fin_ult1", "ind_ctju_fin_ult1", 
                  "ind_ctma_fin_ult1",  "ind_ecue_fin_ult1", "ind_tjcr_fin_ult1",
                  "ind_recibo_ult1")
nbMapTopPredictions <- length(consideredMapTopPredictions)

# Public leaderboard probing data
fractionPosFlankUsers <- 0.03008105
expectedCountPerPosFlank <- 1.25

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

# Set the target date
targetDate <- "12-11-2016"

# Source the exponential normalisation and the average precision calculation
source("Common/exponentialNormaliser.R")
source("Common/apk.R")


###########################################################################

# Load the raw compared predictions (only actual positive flanks)
allPredsSelf <- readRDS(file.path(getwd(), "Validation", submissionDate,
                                  selfFolder, selfFn))

# Load the base model predictions
baseModelPredictions <- 
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste(validationExtension, "base preds.rds")))

# Load the base model weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights first validation.rds"))

# Load the estimated relative count contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# List the targetProducts and calculate the normalized, weighted predictions
# for all products
targetVars <- paste0(unique(gsub("_ult1.*$", "",
                                 names(baseModelPredictions)))[-1], "_ult1")
nbTargetVars <- length(targetVars)
allPredictions <- NULL
probMultipliers <- rep(NA, nbTargetVars)
names(probMultipliers) <- targetVars
for(targetId in 1:nbTargetVars){
  # Show a progress message
  cat("Processing target variable", targetId, "of", nbTargetVars, "\n")
  
  # Extract the target variable
  targetVar <- targetVars[targetId]
  
  # Extract the base model columns and weights
  baseColumns <- names(baseModelPredictions)[
    grepl(targetVar, names(baseModelPredictions)) &
      !grepl("Weighted", names(baseModelPredictions))]
  baseModels <- baseModelPredictions[, baseColumns, with=FALSE]
  baseWeightsData <- dateTargetWeights[targetProduct == targetVar, ]
  baseWeights <- baseWeightsData[, rev(relativeWeight)]
  
  weightedTargetPredictions <- rep(0, nrow(baseModels))
  if(targetVar %in% adjustedProducts){
    ##############################################
    # Generate the advanced weighted predictions #
    ##############################################
    
    # Calculate the mean of the last lags
    nbLags <- length(baseWeights)
    meanLagCols <- nbLags - ((lastLagsWeightSelection-1):0)
    meanLastLags <- rowMeans(baseModels[, meanLagCols, with=FALSE])
    nonZeroRows <- which(meanLastLags>0)
    meanLastLags <- meanLastLags[nonZeroRows]
    baseNonZeroPreds <- baseModels[nonZeroRows, ]
    validLagIds <-
      baseNonZeroPreds > rep(meanLastLags, nbLags)/validLagRelCutoff &
      baseNonZeroPreds < rep(meanLastLags, nbLags)*validLagRelCutoff
    
    # If less than minConsideredLagsCutoff lags valid => use all
    useAllLagIds <- which(rowSums(validLagIds) < minConsideredLagsCutoff)
    validLagIds[useAllLagIds, ] <- TRUE
    
    # Calculate the weighted row sums
    adjustedLagWeights <- validLagIds * rep(baseWeights,
                                            each = length(nonZeroRows))
    weightedRowSums <- rowSums(adjustedLagWeights)
    adjustedProb <- rowSums(adjustedLagWeights*baseNonZeroPreds)/
      weightedRowSums
    weightedTargetPredictions[nonZeroRows] <- adjustedProb
    # browser()
  }  
  else{
    # Generate the weighted predictions
    nonZeroRows <- rowSums(baseModels)>0
    weightedTargetPredictions[nonZeroRows] <-
      rowSums(baseModels[nonZeroRows, ] *
                rep(baseWeights, each = sum(nonZeroRows)))
  }
  
  # Normalise the predictions
  predictedPosFlankCount <- sum(weightedTargetPredictions)
  probMultiplier <- nrow(baseModels) * fractionPosFlankUsers *
    expectedCountPerPosFlank * countContributions[16, targetId] /
    predictedPosFlankCount
  probMultipliers[targetId] <- probMultiplier
  weightedNormTargetPredictions <- probExponentNormaliser(
    weightedTargetPredictions, probMultiplier)
  
  # Append the predictions to allPredictions - only retain positive flank
  # predictions!
  predictionsDT <- data.table(ncodpers = baseModelPredictions$ncodpers,
                              product = targetVar,
                              prediction = weightedNormTargetPredictions)
  predictionsDT <- predictionsDT[ncodpers %in% allPredsSelf$ncodpers]
  predictionsDT$posFlank <- allPredsSelf[product == targetVar, posFlank]
  allPredictions <- rbind(allPredictions, predictionsDT)
}

# Calculate the order of the post normalisation predictions
allPredictions[, order_predict := match(1:length(prediction),
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
    
    # Only swap with considered swap products
    consideredNcodpers <-
      allPredictions[ncodpers %in% consideredNcodpers &
                       product %in% swapProducts & 
                       order_predict == nomRank + 1, ncodpers]
    
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
    nonZeroSwappedNcodpers <- swapNcodpers
    allSwapped <- unique(c(allSwapped, nonZeroSwappedNcodpers))
    allPredictions[order_predict == nomPensRank + 2 &
                     ncodpers %in% swapNcodpers,
                   order_predict := as.integer(order_predict - 2)]
    allPredictions[product %in% c("ind_nomina_ult1", "ind_nom_pens_ult1") &
                     ncodpers %in% swapNcodpers,
                   order_predict := as.integer(order_predict + 1)]
  }
}

# Calculate the number of new products for each client
newProducts <- allPredsSelf[, .(nbNewProducts = sum(posFlank)), ncodpers]
setkey(newProducts, ncodpers)

# Assess the MAP of the altered post normalisation predictions
mapData <- cbind(allPredictions[order_predict==1, posFlank],
                 allPredictions[order_predict==2, posFlank],
                 allPredictions[order_predict==3, posFlank],
                 allPredictions[order_predict==4, posFlank],
                 allPredictions[order_predict==5, posFlank],
                 allPredictions[order_predict==6, posFlank],
                 allPredictions[order_predict==7, posFlank],
                 newProducts$nbNewProducts)

# Calculate and display the mean MAP
map <- map7(mapData, returnScores = TRUE)
cat("Mean MAP@7:", paste0(round(mean(map)*100, 3), "%"), "\n")