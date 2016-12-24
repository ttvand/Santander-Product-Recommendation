# Logic to maximize the MAP, assuming the joint probabilities of the 
# submission as the ground truth in combination with the conditional
# probability of all product positive flanks given all other product positive
# flanks.

# The current approach is a dummy approach and should be improved significantly

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Conditional probability multiplier alpha (1 means no change)
alpha <- 0.8

# Submission file
submissionFile <- paste0("map booster cond prob alpha ", alpha,
                         " xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint")

# Option to store the modified raw predictions
savePredictions <- TRUE

# Path to the best submission
bestSubmissionPredictionsPath <- file.path(getwd(), "Submission",
                                           "24-11-2016/Predictions/xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint.rds")

# Target date
targetDate <- "12-11-2016"

# Target submission date
submissionDate <- "26-11-2016"

nbConsideredTopProds <- 2


##########################################################################

# Read the best submission predictions
allPredictions <- readRDS(bestSubmissionPredictionsPath)

# Load the positive flanks conditional proabilities data
condProbPath <- file.path(getwd(), "Feature engineering", targetDate,
                          "product conditional positive flanks.rds")
condProbs <- readRDS(condProbPath)

# Extract the top product
topProds <- allPredictions[order_predict==1, .(ncodpers, topProduct = product,
                                               rank1Prob = totalProb)]
setkey(topProds, ncodpers)
setkey(allPredictions, ncodpers)
allPredictions <- allPredictions[topProds]

# Extract the second top product
rank2Prods <- allPredictions[order_predict==2,
                             .(ncodpers, rank2Product = product,
                               rank2Prob = totalProb)]
setkey(rank2Prods, ncodpers)
setkey(allPredictions, ncodpers)
allPredictions <- allPredictions[rank2Prods]

# Loop over all unique top products and recalculate the total proabilities
topProdsUnique <- sort(unique(topProds$topProduct))
nbTopProds <- length(topProdsUnique)

# Loop over all top products and calculate the conditional probability of the
# second ranked product given the top product
allPredictions[, condProbTop := 1]
for(i in 1:nbTopProds){
  topProdLoop <- topProdsUnique[i]
  rank2ProdIds <- which(allPredictions$topProduct == topProdLoop &
                          allPredictions$order_predict == 2)
  topProductIds <- which(allPredictions$topProduct == topProdLoop &
                           allPredictions$order_predict <= nbConsideredTopProds)
  condProbsTop <- condProbs[baseProduct == topProdLoop, ]
  allPredictions$condProbTop[topProductIds] <-
    rep(condProbsTop$condProb[
      match(allPredictions[rank2ProdIds, product],
            condProbsTop$conditionalProduct)], each = nbConsideredTopProds)
}


# Loop over all rank 2 products and calculate the conditional probability of
# the top ranked product given the rank 2 product
rank2ProdsUnique <- sort(unique(rank2Prods$rank2Product))
nbRank2Prods <- length(rank2ProdsUnique)
allPredictions[, condProbRank2 := 1]
for(i in 1:nbRank2Prods){
  rank2ProdLoop <- rank2ProdsUnique[i]
  topProdIds <- which(allPredictions$rank2Product == rank2ProdLoop &
                        allPredictions$order_predict == 1)
  condProbsRank2 <- condProbs[baseProduct == rank2ProdLoop, ]
  topProductIds <- which(allPredictions$rank2Product == rank2ProdLoop &
                           allPredictions$order_predict <= nbConsideredTopProds)
  allPredictions$condProbRank2[topProductIds] <-
    rep(condProbsRank2$condProb[
      match(allPredictions[topProdIds, product],
            condProbsRank2$conditionalProduct)], each = nbConsideredTopProds)
}

# Modify the joint probabilities through Bayes's theorem:
# P(A) <- alpha*P(A) + (1-alpha) * (P(B) * P(A|B) / P(B|A))
allPredictions[, modifiedTotalProb := totalProb]
allPredictions[order_predict == 1,
               modifiedTotalProb := alpha*totalProb + (1-alpha)*
                 rank2Prob * condProbRank2 / condProbTop]
allPredictions[order_predict == 2,
               modifiedTotalProb := alpha*totalProb + (1-alpha)*
                 rank1Prob * condProbTop / condProbRank2]


# Order the predicted probabilities for all products by client
setkey(allPredictions, ncodpers)
allPredictions[,order_predict2 := match(1:length(modifiedTotalProb),
                                        order(-modifiedTotalProb)), by=ncodpers]
allPredictions <- allPredictions[order(ncodpers, -modifiedTotalProb), ]

# Combine the top seven products to a string vector
productString <- paste(allPredictions[order_predict2==1, product],
                       allPredictions[order_predict2==2, product],
                       allPredictions[order_predict2==3, product],
                       allPredictions[order_predict2==4, product],
                       allPredictions[order_predict2==5, product],
                       allPredictions[order_predict2==6, product],
                       allPredictions[order_predict2==7, product])

# Add the id and top 7 to the submission file
uniqueNcodPers <- sort(unique(allPredictions$ncodpers))
submission <- data.frame(ncodpers = uniqueNcodPers,
                         added_products = productString)

# Extract template submission file
paddedSubmission <- fread("Data/sample_submission.csv")

# Set the added products to an empty character string
paddedSubmission[, added_products := ""]

# Replace the matched ids in padded submission by the combined submission file
matchIds <- match(submission$ncodpers, paddedSubmission$ncodpers)
paddedSubmission[matchIds, added_products := submission$added_products]

# Write the padded submission to a csv file
write.csv(paddedSubmission, file.path(getwd(), "Submission", submissionDate,
                                      paste0(submissionFile, ".csv")),
          row.names = FALSE)

# Save the predictions to the predictions folder
if(savePredictions){
  predictionsPath <- file.path(getwd(), "Submission", submissionDate,
                               "Predictions")
  saveRDS(allPredictions, file=file.path(predictionsPath,
                                         paste0(submissionFile, ".rds")))
}
