###############################################################################
# Analysis to determine the optimal base product probability alignment in     #
# order to maximize the expected MAP by incorporating the uncertainty of the  #
# product predictions                                                         #
# Main question: how to transform probabilities to maximize the expected MAP? #
###############################################################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(ggplot2)

# Submission date and file name of the compared predictions
submissionDate <- "19-12-2016"
monthsBackAnalysis <- 11 # 0 refers to May 16

# Calculated variables based on monthsBackAnalysis
studiedMonth <- month.abb[1 + ((4-monthsBackAnalysis) %% 12)]
studiedYear <- 16 - as.numeric(monthsBackAnalysis>4)
selfFolder <- "self validation 10 Folds"
selfFn <- paste0("Back", monthsBackAnalysis,".rds")
validationExtension <- paste0(
  "xgboost weighted trainAll 21, validation ", studiedMonth, studiedYear,
  " positive flanks.rds")

#########################################################################
# Raw predictions confidence rescaler: lower probabilities of uncertain #
# predictions and keep the same probability multiplier                  #
# Apply to cno(5), reca(18), nomina(22) and nom_pens(23)                #
# Don't apply to cco(3), ctju(6), ctpp(9) ecue(13), fond(14), tjcr(19), #
# valo(20) and recibo(24)                                               #
# Maybe apply to ctma(7~0.9), ctop(8 ~ 0.6), dela(12 ~ 1.2)             #
#########################################################################

# # Mimic the test set product importance distribution for certain products?
# testProductsDistribution <- "ind_reca_fin_ult1"
# testProductsDistribution <- NULL

rescaleMode <- c("before norm", "exponent", "after norm")[1] # Before norm seems a bit better
confidenceRescalers <- rep(1, 24)
confidenceRescalers[3] <- 0.8
# confidenceRescalers[5] <- 1.1
# My intuition tells me this number of reca_fin should be > 1!
# confidenceRescalers[18] <- 1.2
# confidenceRescalers[22:23] <- 1.2
# confidenceRescalers[19] <- 0.9
# confidenceRescalers[9] <- 0.8
# confidenceRescalers[13] <- 0.7

# MAP boosting options - all other ignored if mapBoosting is FALSE
mapBoosting <- FALSE
maxDiffNominaNomPensMapBoosting <- Inf # Typical value: Inf; Inf means no limit
maxRelDiffNominaNomPensMapBoosting <- 0.7 # Typical value: 0.8; 1 means no action
averageNominaNomPensProbsMAPBoosting <- TRUE # Min mode is equal to close gaps!
averageNominaNomPensProbsMAPBoostingMethod <- c("Average", "Min")[1]
consideredMapTopPredictions <- 1:6
swapRelativeCutoff <- 1 # Typical value: 1; 0 means no swaps
excludeSwapProducts <- c("ind_cno_fin_ult1")
nbMapTopPredictions <- length(consideredMapTopPredictions)

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

# Target date 
targetDate <- "12-11-2016"

# Source the exponential normalisation and average precision calculation
source("Common/exponentialNormaliser.R")
source("Common/apk.R")


###########################################################################

# Load the raw compared predictions
allPredsSelf <- readRDS(file.path(getwd(), "Validation", submissionDate,
                                  selfFolder, selfFn))
allPredsValPrevNorm <-
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste("prevNorm", validationExtension)))
allPredsValPrevNorm$posFlank <- allPredsSelf$posFlank
allPredsValPostNorm <-
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste("postNorm", validationExtension)))
allPredsValPostNorm$posFlank <- allPredsSelf$posFlank

# Load the raw predictions of all data before normalisation
allPredictions <- 
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste0("prevNorm",
                           gsub(" positive flanks", "", validationExtension))))

# Load the estimated relative count contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# Load the probability multipliers
probMultipliers <- 
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste(gsub(" positive.*$", "", validationExtension),
                          "probMultipliers.rds")))

# Extract all considered target variables
targetVars <- unique(allPredsSelf$product)
nbTargetVars <- length(targetVars)

# Incorporate uncertainy of the predictions into the normalisation
allNcodpers <- unique(allPredictions$ncodpers)
targetPredIds <- match(unique(allPredsSelf$ncodpers), allNcodpers)
for(i in 1:nbTargetVars){
  targetVar <- targetVars[i]
  
  mimicTestDistr <- FALSE #targetVar %in% testProductsDistribution
  confidenceRescaler <- confidenceRescalers[i]
  if(confidenceRescaler!=1){
    
    probMultiplier <- probMultipliers[i]
    if(mimicTestDistr){
      # Rescale the probability multiplier
      probMultiplier <- probMultiplier * countContributions[17, i]/
        countContributions[16 - monthsBackAnalysis, i] 
    }
    
    predOrig <- allPredictions[product==targetVar, prediction]
    
    if(rescaleMode == "before norm"){
      rescaledPrediction <- probExponentNormaliser(predOrig/confidenceRescaler,
                                                   probMultiplier, nbIt = 30)
    } else{
      if(rescaleMode == "exponent"){
        rescaledPrediction <- probExponentNormaliser(predOrig, probMultiplier/
                                                       confidenceRescaler,
                                                     nbIt = 30)
      } else{
      rescaledPrediction <- probExponentNormaliser(predOrig, probMultiplier,
                                                   nbIt = 30)/confidenceRescaler
      }
    }
    allPredsValPostNorm[product==targetVar,
                        prediction := rescaledPrediction[targetPredIds]]
  } else{
    if(mimicTestDistr){
      # Rescale the probability multiplier
      probMultiplier <- probMultiplier * countContributions[17, i]/
        countContributions[16 - monthsBackAnalysis, i]
      
      # Rescale the predictions
      predOrig <- allPredictions[product==targetVar, prediction]
      rescaledPrediction <- probExponentNormaliser(predOrig, probMultiplier,
                                                   nbIt = 30)
      allPredsValPostNorm[product==targetVar,
                          prediction := rescaledPrediction[targetPredIds]]
    }
  }
}

if(nomPensAboveNominaBothNotOwned){
  # Calculate the order of the post normalisation predictions
  allPredsValPostNorm[, order_predict := match(1:length(prediction),
                                               order(-prediction)), by=ncodpers]
  allPredsValPostNorm <- allPredsValPostNorm[order(ncodpers, -prediction), ]
  
  ncodpers <- unique(allPredsValPostNorm$ncodpers)
  # Find users where the rank of nomina < rank of nom pens and both prob not
  # zero
  nominaProb <- allPredsValPostNorm[product == "ind_nomina_ult1", prediction]
  nominaProbRank <- allPredsValPostNorm[product == "ind_nomina_ult1",
                                        order_predict]
  nomPensProb <- allPredsValPostNorm[product == "ind_nom_pens_ult1", prediction]
  nomPensProbRank <- allPredsValPostNorm[product == "ind_nom_pens_ult1",
                                         order_predict]
  swapIds <- nominaProb>0 & nomPensProb>0 & nominaProb>nomPensProb
  swapNcodpers <- ncodpers[swapIds]
  allPredsValPostNorm[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                      order_predict := nomPensProbRank[swapIds]]
  allPredsValPostNorm[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                      order_predict := nominaProbRank[swapIds]]
  
  # Also swap the predictions for logic down the pipeline!
  allPredsValPostNorm[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                      prediction := nomPensProb[swapIds]]
  allPredsValPostNorm[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                      prediction := nominaProb[swapIds]]
}

# Optionally, perform MAP boosting
if(mapBoosting){
  allPredictions <- allPredsValPostNorm
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
    allSwapped <- unique(c(allSwapped, swapNcodpers))
    allPredictions[order_predict == nomPensRank + 2 &
                     ncodpers %in% swapNcodpers,
                   order_predict := as.integer(order_predict - 2)]
    allPredictions[product %in% c("ind_nomina_ult1", "ind_nom_pens_ult1") &
                     ncodpers %in% swapNcodpers,
                   order_predict := as.integer(order_predict + 1)]
  }
  allPredsValPostNorm <- allPredictions 
}

# Calculate the order of the post normalisation predictions
allPredsValPostNorm[, order_predict := match(1:length(prediction),
                                             order(-prediction)), by=ncodpers]
allPredsValPostNorm <- allPredsValPostNorm[order(ncodpers, -prediction), ]

# Calculate the number of new products for each client
newProducts <- allPredsValPostNorm[, .(nbNewProducts = sum(posFlank)), ncodpers]
setkey(newProducts, ncodpers)

# Assess the MAP of the altered post normalisation predictions
mapData <- cbind(allPredsValPostNorm[order_predict==1, posFlank],
                 allPredsValPostNorm[order_predict==2, posFlank],
                 allPredsValPostNorm[order_predict==3, posFlank],
                 allPredsValPostNorm[order_predict==4, posFlank],
                 allPredsValPostNorm[order_predict==5, posFlank],
                 allPredsValPostNorm[order_predict==6, posFlank],
                 allPredsValPostNorm[order_predict==7, posFlank],
                 newProducts$nbNewProducts)

# Combine the top seven products to a string vector
productString <- paste(allPredsValPostNorm[order_predict==1, product],
                       allPredsValPostNorm[order_predict==2, product],
                       allPredsValPostNorm[order_predict==3, product],
                       allPredsValPostNorm[order_predict==4, product],
                       allPredsValPostNorm[order_predict==5, product],
                       allPredsValPostNorm[order_predict==6, product],
                       allPredsValPostNorm[order_predict==7, product])

# Calculate and display the mean MAP
map <- map7(mapData, returnScores = TRUE)
cat("Mean MAP@7:", paste0(round(mean(map)*100, 3), "%"), "\n")

# Study the true and false positives
truePosSummary <-
  allPredsValPostNorm[posFlank==1, .(meanPredTP = mean(prediction),
                                     medianPredTP = median(prediction),
                                     meanRank = mean(order_predict), .N),
                      product]
falsePosSummary <-
  allPredsValPostNorm[posFlank==0 & prediction>0,
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
