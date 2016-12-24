# Simulation study of the effect of the confidence of predictions on the MAP
# Reasoning: devise a tactive how to transform variables based on the 
# confidence in the accuracy of the predictions

# Setup: 7 independent product probabilities with
# Experiment 1: all products have independent probs in rbeta(2, 8)
# Different confidences in the accuracy of the predictions. 
# Variation 1: beta distribution with fixed means and varying shape params

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(ggplot2)

# # Set the seed in order to obtain reproducible results
# set.seed(14)

# Independent positive flank probabilities
nbProducts <- 7
posFlankProbAlpha <- 2
posFlankProbBeta <- 8
varAlphas <- 0.3*(1:nbProducts)

# Option to estimate the entitlement MAP
entitlementCheck <- FALSE

# Probability multipliers (decreasing variation!)
# probMultipliers <- rep(1, 7)
probMultipliers <- 1 + (10:16)/10

# Number of simulations for each product
nbSims <- 1e6

# Source the average precision calculation
source("Common/apk.R")

###############################################################

# Generate the "base" probabilities and ground truth
baseProbs <- matrix(rbeta(nbSims * nbProducts, posFlankProbAlpha,
                          posFlankProbBeta), nrow = nbSims)
basePosFlanks <- matrix(as.numeric(runif(nbSims * nbProducts) < baseProbs),
                        nrow = nbSims)

# Option to consider the entitlement MAP: the model probabilities are equal
# to the true probabilities
if(entitlementCheck){
  modelProbs <- baseProbs
} else{
  # Simulate the modeled probabilities
  modelBetas <- rep(varAlphas, each=nbSims)*(1-baseProbs)/baseProbs
  modelProbs <- matrix(rbeta(nbSims*nbProducts, rep(varAlphas, each = nbSims),
                             modelBetas), nrow = nbSims)
  
  # MAP optimization part: modify model probabilities based on the confidence
  # of the predictions
  modelProbs <- modelProbs * rep(probMultipliers, each = nbSims)
}

# Convert the predictions and ground truth to a data table format
allPredictions <- data.table(id = 1:nbSims,
                             product = rep(paste("Product", 1:nbProducts),
                                           each=nbSims),
                             posFlank = as.vector(basePosFlanks),
                             prediction = as.vector(modelProbs))

# Calculate the order of the predictions
allPredictions[, order_predict := match(1:length(prediction),
                                        order(-prediction)), by=id]
allPredictions <- allPredictions[order(id, -prediction), ]

# Assess the MAP of the combined predictions
nbNewProducts <- rowSums(basePosFlanks)
mapData <- cbind(allPredictions[order_predict==1, posFlank],
                 allPredictions[order_predict==2, posFlank],
                 allPredictions[order_predict==3, posFlank],
                 allPredictions[order_predict==4, posFlank],
                 allPredictions[order_predict==5, posFlank],
                 allPredictions[order_predict==6, posFlank],
                 allPredictions[order_predict==7, posFlank],
                 nbNewProducts)

# Calculate and display the mean MAP
map <- map7(mapData, returnScores = TRUE)
cat("Mean MAP@7:", paste0(round(mean(map)*100, 3), "%"), "\n")