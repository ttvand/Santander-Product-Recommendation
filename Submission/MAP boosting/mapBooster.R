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

# Conditional probability exponent
condProbExp <- 0.2

# Submission file
submissionFile <- "map booster cond exp 0.2 xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint"

# Option to store the modified raw predictions
savePredictions <- TRUE

# Path to the best submission
bestSubmissionPredictionsPath <- file.path(getwd(), "Submission",
                                           "24-11-2016/Predictions/xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint.rds")

# Target date
targetDate <- "12-11-2016"

# Target submission date
submissionDate <- "25-11-2016"




##########################################################################

# Read the best submission predictions
allPredictions <- readRDS(bestSubmissionPredictionsPath)

# Load the positive flanks conditional proabilities data
condProbPath <- file.path(getwd(), "Feature engineering", targetDate,
                          "product conditional positive flanks.rds")
condProbs <- readRDS(condProbPath)

# Extract the top product
topProds <- allPredictions[order_predict==1, .(ncodpers, topProduct = product)]
setkey(topProds, ncodpers)
setkey(allPredictions, ncodpers)
allPredictions <- allPredictions[topProds]

# Loop over all unique top products and recalculate the total proabilities
topProdsUnique <- sort(unique(topProds$topProduct))
nbTopProds <- length(topProdsUnique)

# Loop over all top product and multiply the total probability with the
# conditional given the top raised to condProbExp
allPredictions[, condProbTop := 1]
for(i in 1:nbTopProds){
  topProdLoop <- topProdsUnique[i]
  topProdIds <- which(allPredictions$topProduct == topProdLoop &
                        allPredictions$order_predict != 1)
  condProbsTop <- condProbs[baseProduct == topProdLoop, ]
  allPredictions$condProbTop[topProdIds] <- condProbsTop$condProb[
    match(allPredictions[topProdIds, product],
          condProbsTop$conditionalProduct)]
}
allPredictions[, modifiedTotalProb := totalProb * (condProbTop^condProbExp)]

# Order the predicted probabilities for all products by client
setkey(allPredictions, ncodpers)
allPredictions[,order_predict := match(1:length(modifiedTotalProb),
                                       order(-modifiedTotalProb)), by=ncodpers]
allPredictions <- allPredictions[order(ncodpers, -modifiedTotalProb), ]

# Combine the top seven products to a string vector
productString <- paste(allPredictions[order_predict==1, product],
                       allPredictions[order_predict==2, product],
                       allPredictions[order_predict==3, product],
                       allPredictions[order_predict==4, product],
                       allPredictions[order_predict==5, product],
                       allPredictions[order_predict==6, product],
                       allPredictions[order_predict==7, product])

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
