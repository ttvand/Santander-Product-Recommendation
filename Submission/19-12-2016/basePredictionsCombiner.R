##############################################################################
# Combine the base model predictions in a smart way: only use weighted       #
# average of the base models that are not too different from the last couple #
# of lags for selected target products                                       #
# Reasoning: trends might indicate decreasing popularity for certain subsets #
# in the customer base                                                       #
##############################################################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Base models date, submission date and file name of the compared predictions
baseModelsDate <- "17-12-2016"
submissionDate <- "19-12-2016"
loadPredsFile <- "xgboost weighted trainAll 16 10 folds 200 rounds, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.rds"

# Set the submission file name
submissionFile <- "confidence rescaler cco 1.2, cno 1.1, nom and nomPens 1.2, weights submission 117, no MAP boosting"

# Save option (final combined and processed probabilities)
savePredictions <- TRUE

# Raw predictions confidence rescaler: lower probabilities of uncertain
# predictions and keep the same probability multiplier
# Apply to cno(5), reca(18), nomina(22) and nom_pens(23)
# Don't apply to cco(3), ctju(6), ctpp(9) ecue(13), fond(14), tjcr(19), valo(20)
# and recibo(24)
# Maybe apply to ctma(7~0.9), ctop(8 ~ 0.6), dela(12 ~ 1.2)
rescaleConfidences <- TRUE
confidenceRescalers <- rep(1, 24)
confidenceRescalers[3] <- 1.2
confidenceRescalers[5] <- 1.1
# confidenceRescalers[18] <- 0.8
confidenceRescalers[22:23] <- 1.2

# MAP boosting options - all other ignored if mapBoosting is FALSE
mapBoosting <- FALSE
maxDiffNominaNomPensMapBoosting <- Inf # Typical value: Inf; Inf means no limit
maxRelDiffNominaNomPensMapBoosting <- 0.5 # Typical value: 0.8; 1 means no action
averageNominaNomPensProbsMAPBoosting <- TRUE # Min mode is equal to close gaps!
averageNominaNomPensProbsMAPBoostingMethod <- c("Average", "Min")[1]
consideredMapTopPredictions <- 1:6
swapRelativeCutoff <- 1 # Typical value: 1; 0 means no swaps
excludeSwapProducts <- c("ind_cno_fin_ult1")
nbMapTopPredictions <- length(consideredMapTopPredictions)

# Public leaderboard probing data
fractionPosFlankUsers <- 0.035114
expectedCountPerPosFlank <- 1.25

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

# Modified target variables
# adjustedProducts <- c("ind_ahor_fin_ult1", "ind_aval_fin_ult1", "ind_cco_fin_ult1", 
#                       "ind_cder_fin_ult1", "ind_cno_fin_ult1", "ind_ctju_fin_ult1", 
#                       "ind_ctma_fin_ult1", "ind_ctop_fin_ult1", "ind_ctpp_fin_ult1", 
#                       "ind_deco_fin_ult1", "ind_deme_fin_ult1", "ind_dela_fin_ult1", 
#                       "ind_ecue_fin_ult1", "ind_fond_fin_ult1", "ind_hip_fin_ult1", 
#                       "ind_plan_fin_ult1", "ind_pres_fin_ult1", "ind_reca_fin_ult1", 
#                       "ind_tjcr_fin_ult1", "ind_valo_fin_ult1", "ind_viv_fin_ult1", 
#                       "ind_nomina_ult1", "ind_nom_pens_ult1", "ind_recibo_ult1")
# adjustedProducts <- c("ind_cco_fin_ult1", "ind_cno_fin_ult1", "ind_ctju_fin_ult1", 
#                       "ind_ctma_fin_ult1",  "ind_ecue_fin_ult1", "ind_tjcr_fin_ult1",
#                       "ind_nomina_ult1", "ind_nom_pens_ult1", "ind_recibo_ult1")
# adjustedProducts <- c("ind_cno_fin_ult1", "ind_ecue_fin_ult1", "ind_tjcr_fin_ult1")
adjustedProducts <- NULL
adjustedProducts <- adjustedProducts[adjustedProducts!="ind_cco_fin_ult1"]

# Last considered lags for lag selection and relative consideration cutoff
lastLagsWeightSelection <- 3
validLagRelCutoff <- 4
minConsideredLagsCutoff <- 4
alwaysKeepJun15 <- FALSE

# Set the target date
targetDate <- "12-11-2016"

# Source the exponential normalisation
source("Common/exponentialNormaliser.R")


###########################################################################

# Load the base model predictions
baseModelPredictions <- 
  readRDS(file.path(getwd(), "Submission", "PredictionsComp", baseModelsDate,
                    loadPredsFile))

# Load the base model weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights first.rds"))

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
    
    if(alwaysKeepJun15){
      colLags <- as.numeric(gsub(".*_Lag_", "", names(baseModels)))
      JunColumn <- which(colLags == 5)
      validLagIds[, JunColumn] <- TRUE
    }
    
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
    expectedCountPerPosFlank * countContributions[17, targetId] /
    predictedPosFlankCount
  probMultipliers[targetId] <- probMultiplier
  confidenceRescaler <- ifelse(rescaleConfidences,
                               confidenceRescalers[targetId], 1)
  # if(confidenceRescaler != 1) browser()
  weightedNormTargetPredictions <- probExponentNormaliser(
    weightedTargetPredictions/confidenceRescaler, probMultiplier)
  
  # Append the predictions to allPredictions - only retain positive flank
  # predictions!
  predictionsDT <- data.table(ncodpers = baseModelPredictions$ncodpers,
                              product = targetVar,
                              prediction = weightedNormTargetPredictions)
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
                       !product %in% excludeSwapProducts & 
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

# Make sure that the order of the predictions is unique for each client
orderCount <- allPredictions[, .N, .(ncodpers, order_predict)]
if(max(orderCount$N)>1) browser()

# Show the confidence in the top prediction
hist(allPredictions[order_predict==1, prediction])

# Extract clients with positive flanks
posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                               "positive flank clients.rds")
posFlankClients <- readRDS(posFlankClientsFn)

# Calculate the top predicted products counts
topPredictions <- allPredictions[order_predict==1, .N, product]
topPredictions <- topPredictions[order(-N)]
topPredictionsPosFlanks <- allPredictions[order_predict==1 &
                                            ncodpers %in% posFlankClients,
                                          .N, product]
topPredictionsPosFlanks <- topPredictionsPosFlanks[order(-N)]

# Study the ranking of specific products
productRankCcoFin <- allPredictions[product=="ind_cco_fin_ult1", .N,
                                     order_predict]
productRankCcoFin <- productRankCcoFin[order(order_predict), ]

productRankDelaFin <- allPredictions[product=="ind_dela_fin_ult1", .N,
                                     order_predict]
productRankDelaFin <- productRankDelaFin[order(order_predict), ]

productRankDecoFin <- allPredictions[product=="ind_deco_fin_ult1", .N,
                                     order_predict]
productRankDecoFin <- productRankDecoFin[order(order_predict), ]

productRankTjcrFin <- allPredictions[product=="ind_tjcr_fin_ult1", .N,
                                     order_predict]
productRankTjcrFin <- productRankTjcrFin[order(order_predict), ]

productRankRecaFin <- allPredictions[product=="ind_reca_fin_ult1", .N,
                                     order_predict]
productRankRecaFin <- productRankRecaFin[order(order_predict), ]

# Verify that the mean prediction aligns with the relative June 15 ratio
# No longer accurate when MAP boosting or swapping nomina/nomPens
newProdPredictions <- rep(1, nrow(baseModels))
allPredictions[, totalProb := prediction * rep(newProdPredictions,
                                               each = nbTargetVars)]
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
if(length(productString) != nrow(baseModels)) browser()

# Add the id and top 7 to the submission file
submission <- data.frame(ncodpers = baseModelPredictions$ncodpers,
                         added_products = productString)

# Extract template submission file
paddedSubmission <- fread("Data/sample_submission.csv")

# Set the added products to an empty character string
paddedSubmission[, added_products := ""]

# Replace the matched ids in padded submission by the combined submission file
matchIds <- match(submission$ncodpers, paddedSubmission$ncodpers)
paddedSubmission[matchIds, added_products := submission$added_products]

# Write the padded submission to a csv file
predictionsPath <- file.path(getwd(), "Submission", submissionDate)
write.csv(paddedSubmission, file.path(predictionsPath,
                                      paste0(submissionFile, ".csv")),
          row.names = FALSE)

# Save the predictions to the predictions folder
if(savePredictions){
  saveRDS(allPredictions, file.path(predictionsPath, "Predictions",
                                         paste0(submissionFile, ".rds")))
}

# Display the successful submission message
cat("Submission file created successfully!\n",
    nrow(submission)," records were predicted (",
    round(nrow(submission)/nrow(paddedSubmission)*100,2), "%)\n", sep="")
