# Script to estimate the private leaderboard confidence interval based on the
# public leaderboard estimate.
# Confidence interval for  - Total score
#                          - Relative MAP contributions

# Use the binomial distribution to get an estimate of the variation in the
# total score and relative MAP scores

# Simplification: assume independence of products positive flanks

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Number of simulations
nbSims <- 1e5

# Estimate of the public leaderboard MAP contributions
mapContributions <- structure(c(8.22876380676512e-06, 1.34449917206079e-25, 0.275049370021242, 
                                9.49302405463426e-05, 0.0507875375491793, 0.00159810401330649, 
                                0.0127466760907744, 0.00557667078971381, 0.00328625242965278, 
                                0, 0, 0.00268875198789773, 0.0567625419667298, 0.00298750220877525, 
                                8.59198057807535e-05, 0.0014059431775617, 0.000158348269042084, 
                                0.0913179841815636, 0.117209669990949, 0.00786708914977484, 0.00011737228949, 
                                0.0611442118729335, 0.0620404625355661, 0.247066432665714),
                              .Names = c("ind_ahor_fin_ult1", 
                                         "ind_aval_fin_ult1", "ind_cco_fin_ult1", "ind_cder_fin_ult1", 
                                         "ind_cno_fin_ult1", "ind_ctju_fin_ult1", "ind_ctma_fin_ult1", 
                                         "ind_ctop_fin_ult1", "ind_ctpp_fin_ult1", "ind_deco_fin_ult1", 
                                         "ind_deme_fin_ult1", "ind_dela_fin_ult1", "ind_ecue_fin_ult1", 
                                         "ind_fond_fin_ult1", "ind_hip_fin_ult1", "ind_plan_fin_ult1", 
                                         "ind_pres_fin_ult1", "ind_reca_fin_ult1", "ind_tjcr_fin_ult1", 
                                         "ind_valo_fin_ult1", "ind_viv_fin_ult1", "ind_nomina_ult1", "ind_nom_pens_ult1", 
                                         "ind_recibo_ult1"))
normalizedMapContributions <- mapContributions/sum(mapContributions)
targetVars <- names(mapContributions)
nbTargetVars <- length(targetVars)

# Estimated maximum public leaderboard score
maxPublicLb <- 0.035
# Needed since the map contributions don't sum to 1 right now
trueMaxPublicLB <- maxPublicLb*sum(mapContributions)

# Number of leaderboard test records
nbRecords <- 929615

# Public leaderboard fraction
publicFraction <- 0.3

# Calculate the number of public leaderboard records with at least one
# positive flank
nbPublicRecords <- round(nbRecords*publicFraction)
nbPrivateRecords <- round(nbRecords*(1-publicFraction))
posFlankPublic <- nbPublicRecords*maxPublicLb

# Distributions estimate of the number of positive flanks conditional on
# there being at least one positive flank (used june 2015 for the estimate)
nbProductsDist <- structure(c(0.796326310102647, 0.159223242691638, 0.0398283210276727, 0.00444204333993637, 0.000180082838105529), 
                            .Names = c("1", "2", "3", "4", "5"))

# Estimate the number of positive flanks for each product in the public 
# leaderboard using the number of positive flanks distribution from above
meanTopCorrectContribution <-
  sum(nbProductsDist*(1/as.numeric(names(nbProductsDist))))
posFlanksEstimatePublic <- mapContributions * posFlankPublic /
  meanTopCorrectContribution

# Use the binomial distribution to get an estimate of the variation in the
# total score and relative MAP scores
simResults <-
  matrix(NA, nrow = nbSims, ncol = nbTargetVars + 1,
         dimnames = list(paste("Sim", 1:nbSims),
                         c("Private max LB", paste("Rel MAP", targetVars))))
sampleProb <- posFlanksEstimatePublic/nbPublicRecords
sampleProbSd <- sqrt(sampleProb*(1-sampleProb)/nbPublicRecords)
for(i in 1:nbSims){
  # Use the probability from the sample to draw a random "TRUE" probability
  # estimate using the standard normal distribution
  trueProbEstimate <- sampleProb+rnorm(nbTargetVars)*sampleProbSd
  trueProbEstimate <- pmax(0, pmin(1, trueProbEstimate))
  privateFlanksSim <- sapply(trueProbEstimate,
                             function(x) rbinom(1, nbPrivateRecords, x))
  simResults[i,1] <- sum(privateFlanksSim) * meanTopCorrectContribution /
    nbPrivateRecords
  simResults[i,-1] <- privateFlanksSim / (sum(privateFlanksSim))
}

# Study some target variable distributions of interest (18, 3, 24, 19)
targetVarIds <- c(18, 3, 24, 19)
for(i in 1:length(targetVarIds)){
  targetVarId <- targetVarIds[i]
  hist(simResults[, 1+targetVarId], col="grey", 25,
       main = paste0(targetVarId, " - ", targetVars[targetVarId]))
  abline(v=normalizedMapContributions[targetVarId], col="red")
  Sys.sleep(4)
}

# Show the simulated private LB distribution
hist(simResults[,1], col="grey", breaks = 25,
     main="Private leaderboard max score distribution")
abline(v=trueMaxPublicLB, col="red")
