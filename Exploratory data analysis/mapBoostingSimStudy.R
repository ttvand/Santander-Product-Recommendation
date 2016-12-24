# Simulation study of single versus linked predictions
# Conclusion: Move rank of the single product up to maximize expected MAP!

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(ggplot2)

# Source the average precision calculation
source("Common/apk.R")

# Combined probabilty - play with this value!
combinedProb <- 0.5

# Single probabilities
singleProbs <- seq(combinedProb/2, combinedProb, length.out = 20)
nbSingleProbs <- length(singleProbs)

# Number of simulations for each single probability
nbSims <- 5e4


###############################################################

# Mean MAP for both approaches: 
# Approach 1: always keep original prob ordering: paired on top
# Approach 2: always put single prob on top
meanMAP <- matrix(NA, nrow=nbSingleProbs, ncol=2)

# Draw random independent samples and calculate the mean MAP for both
# approaches
zeroCols <- matrix(0, nrow = nbSims, ncol = 4)
for(i in 1:nbSingleProbs){
  # Show a progress message
  cat("Processing single probabilities", i, "of", nbSingleProbs, "\n")
  
  singleProb <- singleProbs[i]
  singleOutcomes <- as.numeric(runif(nbSims) < singleProb)
  pairedOutcomes <- as.numeric(runif(nbSims) < combinedProb)
  
  # Set up the matrices for the MAP calculation
  posFlankCount <- rowSums(cbind(singleOutcomes, 2*pairedOutcomes))
  submission1 <- cbind(pairedOutcomes, pairedOutcomes, singleOutcomes, zeroCols,
                       posFlankCount)
  submission2 <- cbind(singleOutcomes, pairedOutcomes, pairedOutcomes, zeroCols,
                       posFlankCount)
  meanMAP[i, 1] <- map7(submission1)
  meanMAP[i, 2] <- map7(submission2)
}

# Set up the plot data
plotData <- data.frame(Approach = rep(c("Keep order", "Single top"),
                                      each=nbSingleProbs),
                       SingleProb = rep(singleProbs, 2),
                       MeanMAP = c(meanMAP[, 1], meanMAP[, 2]))
p <- ggplot(plotData, aes(x = SingleProb, y = MeanMAP, colour = Approach)) +
  geom_line() + 
  ggtitle(paste("Combined probability:", combinedProb))
print(p)
