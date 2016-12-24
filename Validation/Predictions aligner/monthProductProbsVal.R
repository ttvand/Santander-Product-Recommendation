# Logic to extract the predictions of all base models for all product - month
# pairs. This data set is the starting point for the base model combination
# logic

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(ggplot2)

# Submission date and model name
# submissionDate <- "09-12-2016"
# modelName <- "xgboost weighted trainAll 14, linear increase jun15 times6 back 15-0 no zeroing, exponential normalisation joint"
submissionDate <- "19-12-2016"
monthsBackAnalysis <- 11 # 0 refers to May 16
studiedMonth <- month.abb[1 + ((4-monthsBackAnalysis) %% 12)]
studiedYear <- 16 - as.numeric(monthsBackAnalysis>4)
modelName <- paste0("xgboost weighted trainAll 21, validation ", studiedMonth,
                    studiedYear)

# Set the target date
targetDate <- "12-11-2016"

# List the base products and target variables
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
targetVars <- paste0("ind_", baseProducts, "_ult1")
nbTargetVars <- length(targetVars)

# Public leaderboard probing data
allFracPosFlankUsers <- 
  c(0.0300810542775529, 0.0289534271735105, 0.0301772443883491, 
    0.0394507885017319, 0.0288999152850054, 0.0372489477979507, 0.0314951076760619, 
    0.0372911577070477, 0.0338312772111975, 0.0293664235761944, 0.0414074437246231, 
    0.0530032468823725, 0.0337554824997811, 0.0406830722293673, 0.0403091375057191, 
    0.0381778895085452)
fractionPosFlankUsers <- allFracPosFlankUsers[monthsBackAnalysis + 1]
expectedCountPerPosFlank <- 1.25

# Source the exponential normalisation
source("Common/exponentialNormaliser.R")


######################################################################

# Load the first level base model weights
dateTargetWeights <- readRDS(file.path(getwd(), "Model weights", targetDate,
                                       "model weights first validation.rds"))

# Load the estimated relative count contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# List all base models
basePath <- file.path(getwd(), "Validation", submissionDate, "Predictions",
                      modelName)
baseModels <- list.files(basePath)
baseModelLags <- as.numeric(gsub(".*Lag |.rds$", "", baseModels))
modelLags <- sort(unique(baseModelLags))
nbLags <- length(modelLags)

# Combine the base model predictions
combinedPreds <- list()
probMultipliers <- rep(NA, nbTargetVars)
names(probMultipliers) <- targetVars
for(targetId in 1:nbTargetVars){
  # Show progress message
  cat("Processing target variable", targetId, "of", nbTargetVars, "\n")
  
  targetVar <- targetVars[targetId]
  weightedTargetPredictions <- NULL
  for(lagId in 1:nbLags){
    lag <- modelLags[lagId]
    
    # if(targetVar == "ind_cco_fin_ult1" && lag==3) browser()
    
    modelPredsName <- paste(targetVar, "Lag", lag, sep="_")
    predictionsDT <- readRDS(file.path(basePath, paste0(targetVar, " Lag ",
                                                        lag, ".rds")))
    if(length(combinedPreds) == 0){
      combinedPreds[["ncodpers"]] <- predictionsDT$ncodpers
    }
    combinedPreds[[modelPredsName]] <- predictionsDT$predictions
    relativeModelWeight <-
      dateTargetWeights[modelLag == lag & targetProduct == targetVar,
                        relativeWeight]
    if(is.null(weightedTargetPredictions)){
      weightedTargetPredictions <- rep(0, nrow(predictionsDT))
    }
    weightedTargetPredictions <- weightedTargetPredictions +
      relativeModelWeight * predictionsDT$predictions
  }
  
  # Add the weighted predictions for the target variable
  weightedTargetPredsName <- paste(targetVar, "Weighted", sep="_")
  combinedPreds[[weightedTargetPredsName]] <- weightedTargetPredictions
  
  # Add the normalized weighted predictions for the target variable
  predictedPosFlankCount <- sum(weightedTargetPredictions)
  probMultiplier <- nrow(predictionsDT) * fractionPosFlankUsers *
    expectedCountPerPosFlank *
    countContributions[16 - monthsBackAnalysis, targetId] /
    predictedPosFlankCount
  probMultipliers[targetId] <- probMultiplier
  weightedNormTargetPredictions <- probExponentNormaliser(
    weightedTargetPredictions, probMultiplier)
  weightedNormTargetPredsName <- paste(targetVar, "Weighted_Norm_Exp", sep="_")
  combinedPreds[[weightedNormTargetPredsName]] <- weightedNormTargetPredictions
}

# Convert the base model predictions to a datatable
combinedPreds <- setDT(combinedPreds)

# Store the base model predictions
saveFolder <- file.path(getwd(), "Validation", submissionDate, "Predictions")
dir.create(saveFolder, showWarnings = FALSE)
saveRDS(combinedPreds, file.path(saveFolder, paste(modelName,
                                                   "base preds.rds")))

# Store the probability multipliers
saveRDS(probMultipliers, file.path(saveFolder, paste0("probMultipliers ",
                                                      modelName, ".rds")))