###############################################################################
# Comparison of the base predictions before and after normalisation of the    #
# validation data with the TRUE to reality out of fold predictions of the     #
# models that are trained on the month itself                                 #
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
selfFolder <- "self validation May16 10 Folds"
selfFn <- "Back0.rds"
validationExtension <-
  "xgboost weighted trainAll 21, validation May16 positive flanks.rds"

# List the target variables with confident predictions
confidentPredTargetsBase <- c("cco_fin", "cno_fin", "ctju_fin", "ctma_fin",
                              "ecue_fin", "tjcr_fin", "nomina", "nom_pens",
                              "recibo")
confidentPredTargetsBase <- NULL
confidentPredTargets <- paste0("ind_", confidentPredTargetsBase, "_ult1")

# Low priority ids, not considered in the correlationanalysis
lowPriorityIds <- c(1, 2, 4, 10, 11, 15, 16, 17, 21)

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

# Source the average precision calculation
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

# Study the relation between the inspected target predictions
# inspectTarget <- "ind_cco_fin_ult1"
# inspectTarget <- "ind_nomina_ult1"
# inspectTarget <- "ind_recibo_ult1"
inspectTarget <- "ind_reca_fin_ult1"
plotData <-
  data.table(RowId = 1:(nrow(allPredsSelf)/24),
             Self = allPredsSelf[product == inspectTarget,
                                         prediction],
             PrevNorm = allPredsValPrevNorm[product == inspectTarget,
                                                prediction],
             PostNorm = allPredsValPostNorm[product == inspectTarget,
                                                prediction])
plotData <- as.data.frame(plotData[RowId < 1e4, ])
p <- ggplot(plotData, aes(x = PrevNorm, y = Self)) +
  geom_point()
print(p)
q <- ggplot(plotData, aes(x=PostNorm, y = Self)) +
  geom_point()
print(q)

# Load the probability multipliers
probMultipliers <- 
  readRDS(file.path(getwd(), "Validation", submissionDate, "Predictions",
                    paste(gsub(" positive.*$", "", validationExtension),
                          "probMultipliers.rds")))

# Extract all considered target variable
targetVars <- unique(allPredsSelf$product)[-lowPriorityIds]
nbTargetVars <- length(targetVars)

# Calculate the correlations between the self predictions on one end and the
# pre and post predictions on the other end for all products
corAnalysis <- matrix(NA, nrow=4, ncol=nbTargetVars, dimnames = list(
  c("Prob multiplier", "Prob exponent", "Prev norm", "Post norm"), targetVars))
for(i in 1:nbTargetVars){
  targetVar <- targetVars[i]
  selfPreds <- allPredsSelf[product == targetVar &
                              prediction>0, prediction]
  valPredsPrevNorm <- allPredsValPrevNorm[product == targetVar &
                                            prediction>0, prediction]
  valPredsPostNorm <- allPredsValPostNorm[product == targetVar &
                                            prediction>0, prediction]
  probExponent <- log(valPredsPostNorm[1])/log(valPredsPrevNorm[1])
  corAnalysis[1, i] <- probMultipliers[names(probMultipliers) == targetVar]
  corAnalysis[2, i] <- probExponent
  corAnalysis[3, i] <- cor(selfPreds, valPredsPrevNorm)
  corAnalysis[4, i] <- cor(selfPreds, valPredsPostNorm)
}

# Consider linear transformations for the confident predictions instead of
# the default exponential transformation
if(length(confidentPredTargetsBase)>0){
  nbConfidentPredTargets <- length(confidentPredTargets)
  for(i in 1:nbConfidentPredTargets){
    targetVar <- confidentPredTargets[i]
    probMultiplier <- probMultipliers[names(probMultipliers) == targetVar]
    if(length(probMultiplier)==0) stop("Confident target var not matched")
    targetPreds <- allPredsValPrevNorm[product == targetVar, prediction] *
      probMultiplier
    allPredsValPostNorm[product == targetVar, prediction := targetPreds]
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

# Calculate and display the mean MAP
map <- map7(mapData, returnScores = TRUE)
cat("Mean MAP@7:", paste0(round(mean(map)*100, 3), "%"), "\n")