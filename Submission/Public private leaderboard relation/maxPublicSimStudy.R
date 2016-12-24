# Script to estimate the maximum true public leaderboard score if the best
# submission is considered to be optimal. This number is less than the
# fraction of users with a positive flank since there is inherent variation
# in new product transitions

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Number of simulations
nbSims <- 1e1

# Fix the seed in order to obtain reproducible results
set.seed(14)

# Public leaderboard information
bestPublicScore <- 0.035114
publicFraction <- 0.3
testRecords <- 929615
publicRecords <- round(testRecords * publicFraction)
publicPosClients <- round(bestPublicScore * publicFraction * testRecords)

# Load the best submission to date
# bestSubmissionFn <- file.path(getwd(), "Submission",
# "24-11-2016/Predictions/xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint.rds")
bestSubmissionFn <- file.path(getwd(), "Submission",
                              "28-11-2016/Predictions/xgboost weighted posFlanks 6, more weight dec15 other 0 cco_fin linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.rds")

# Target date 
targetDate <- "12-11-2016"

# Considered products for conditional adjustments
adjustProd <- c("ind_nomina_ult1", "ind_nom_pens_ult1", "ind_cno_fin_ult1")
adjustAll <- FALSE

# Source the average precision calculation
source("Common/apk.R")

# Max top ranks to be considered for conditional probability adjustments
maxCondAdj <- 7


########################################################################

# Load the positive flanks conditional proabilities data
condProbPath <- file.path(getwd(), "Feature engineering", targetDate,
                          "product conditional positive flanks.rds")
condProbs <- readRDS(condProbPath)

# Load the submission
submission <- readRDS(bestSubmissionFn)
clients <- sort(unique(submission$ncodpers))

# Loop over all simulations
maxMapFracSims <- rep(NA, nbSims)
for(i in 1:nbSims){
  # Show progress message
  cat("Simulating random public leaderboard sample", i, "of", nbSims, "\n")
  
  # Take a random sample from the clients, representing the sampled public
  # leaderboard
  sampledNcodpers <- sample(clients, publicRecords)
  publicPred <- submission[ncodpers %in% sampledNcodpers, ]
  
  # Simulate a ground truth by sequentially adjusting the conditional
  # probabilities.
  nbPers <- length(unique(publicPred$ncodpers))
  publicPred[order_predict==1, posFlank := prediction>runif(nbPers)]
  for(j in 2:maxCondAdj){
    predictionsOrderJ <- publicPred[order_predict==j, prediction]
    botProd <- publicPred[order_predict==j, product]
    for(k in 1:(j-1)){
      # Adjust probability of j given k
      topProd <- publicPred[order_predict==k, product]
      adjustId <- (topProd %in% adjustProd | adjustAll) & (botProd %in% adjustProd)
      posId <- publicPred[order_predict==k, posFlank][adjustId]
      adjustProb <- rep(0, sum(adjustId))
      condMatchIds <- match(paste0(botProd[adjustId], topProd[adjustId]),
                            condProbs$jointKey)
      adjustProb[posId] <- condProbs[condMatchIds[posId], condProb]
      adjustProb[!posId] <- predictionsOrderJ[adjustId][!posId] * 
        (sum(predictionsOrderJ[adjustId])-(sum(adjustProb)))/
        sum(predictionsOrderJ[adjustId][!posId])
      predictionsOrderJ[adjustId] <- adjustProb
      publicPred[order_predict==j, totalProb := predictionsOrderJ]
    }
    publicPred[order_predict==j, posFlank := totalProb>runif(nbPers)]
  }
  publicPred[order_predict>maxCondAdj, posFlank :=
               prediction>runif(nbPers * (24-maxCondAdj))]
  
  # Calculate the number of users with a positive flank
  posFlankUsers <- unique(sort(publicPred[posFlank == TRUE, ncodpers]))
  fractionPosFlankUsers <- length(posFlankUsers)/publicRecords
  publicPredPos <- publicPred[ncodpers %in% posFlankUsers, ]
  setkey(publicPredPos, ncodpers)
  newProductsCount <- publicPredPos[, .(posFlankCount = sum(posFlank)), ncodpers]
  
  # Set up the matrix that can be used to calculate the MAP@7
  mapData <- cbind(publicPredPos[order_predict==1, posFlank],
                   publicPredPos[order_predict==2, posFlank],
                   publicPredPos[order_predict==3, posFlank],
                   publicPredPos[order_predict==4, posFlank],
                   publicPredPos[order_predict==5, posFlank],
                   publicPredPos[order_predict==6, posFlank],
                   publicPredPos[order_predict==7, posFlank],
                   newProductsCount$posFlankCount)
  
  # Calculate the mean average precision
  map <- map7(mapData, returnScores = TRUE)
  mapData <- cbind(mapData, map)
  maxMapFracSims[i] <- mean(map)
  newProdCount <- publicPred[posFlank==TRUE, .N, product]
}

# Inspect the simulated distribution of the maximum map fraction
hist(maxMapFracSims)
