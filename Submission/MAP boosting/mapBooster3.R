# Logic to maximize the MAP
# Simple approach: swap nomina and nom pens if the rank of nomina < rank of
# nom pens and both probabilities are not zero.
# Next boosting step: soft averaging of nomina and nom_pens?
# Maybe also incorporate cno_fin (often tied with nomina and nom_pens)

# The current approach is a dummy approach and should be improved significantly

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Submission file
submissionFile <- "map booster swap nomina nom_pens xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint"

# Option to store the modified raw predictions
savePredictions <- TRUE

# Path to the best submission
bestSubmissionPredictionsPath <- file.path(getwd(), "Submission",
                                           "24-11-2016/Predictions/xgboost weighted trainAll 3 linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint.rds")

# Target date
targetDate <- "12-11-2016"

# Target submission date
submissionDate <- "26-11-2016"


##########################################################################

# Read the best submission predictions
allPredictions <- readRDS(bestSubmissionPredictionsPath)

# Find users where the rank of nomina < rank of nom pens and both prob not
# zero
ncodpers <- unique(allPredictions$ncodpers)
nominaProb <- allPredictions[product == "ind_nomina_ult1", totalProb]
nominaProbRank <- allPredictions[product == "ind_nomina_ult1", order_predict]
nomPensProb <- allPredictions[product == "ind_nom_pens_ult1", totalProb]
nomPensProbRank <- allPredictions[product == "ind_nom_pens_ult1", order_predict]
swapIds <- nominaProb>0 & nomPensProb>0 & nominaProb>nomPensProb
swapNcodPers <- ncodpers[swapIds]
allPredictions[ncodpers %in% swapNcodPers & product == "ind_nomina_ult1",
               order_predict := nomPensProbRank[swapIds]]
allPredictions[ncodpers %in% swapNcodPers & product == "ind_nom_pens_ult1",
               order_predict := nominaProbRank[swapIds]]

# Inspect the relation between the predicted probabilities for nomina and
# nom_pens
cutoff <- 0.1
posIds <- nominaProb>0 & nomPensProb>0 & (nominaProb>cutoff |
                                            nomPensProb>cutoff)
plot(nominaProb[posIds][1:1e3], nomPensProb[posIds][1:1e3])
abline(0, 1, col="blue")
abline(h=cutoff, col="blue")
abline(v=cutoff, col="blue")

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
