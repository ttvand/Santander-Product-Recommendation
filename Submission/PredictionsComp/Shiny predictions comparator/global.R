# # Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/")

# Load required libraries
library(shiny)
library(data.table)
library(DT)
library(shinythemes)
library(plotly)
library(reshape2)

# Option to process a subset of the test predictions to speed up the
# calculations
processSubset <- TRUE
subsetExtension <- ifelse(processSubset, " subset", "")

# Option to drop low priority ids
dropLowPriorityIds <- TRUE
lowPriorityIds <- c(1, 2, 4, 10, 11, 15, 16, 17, 21)

# Submission date and model name
# submissionDate <- "10-12-2016"
# modelName <- "xgboost weighted trainAll 15 boot, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint"
# submissionDate <- "09-12-2016"
# modelName <- "xgboost weighted trainAll 14, linear increase jun15 times6 back 15-0 no zeroing, exponential normalisation joint"
# submissionDate <- "14-12-2016"
# modelName <- "xgboost weighted trainAll 16 10 folds 200 rounds, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint"
submissionDate <- "17-12-2016"
modelName <- "xgboost weighted trainAll 16 10 folds 200 rounds, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint"

# Public leaderboard parameters
fractionPosFlankUsers <- 0.035114
expectedCountPerPosFlank <- 1.25

# Target date
targetDate <- "12-11-2016"

# Monthly counts path
monthlyCountsPath <- file.path(getwd(), "Feature engineering", targetDate,
                               "monthlyTransitionCounts.rds")

# Minimum scatter plot threshold
minScatterThreshold <- 0.02

# Maximum number of considered data points when plotting the predictions by
# month
maxRowsPredsByMonthPlot <- 5e4

# Top ranks to show in a separate color in the lag base model comparison plot
showTopRanksScatter <- 3


#######################################################################

# Load the raw predictions data
baseFolder <- file.path(getwd(), "Submission", "PredictionsComp",
                        submissionDate)
basePredictions <- readRDS(file.path(baseFolder,
                                     paste0(modelName, subsetExtension,
                                            ".rds")))

# Calculate the summed base predictions
basePredSums <- colSums(basePredictions[
  , !grepl("Rank|ncodpers", names(basePredictions)), with=FALSE])

# Extract the products from the data columns
productsSimpleCols <- gsub("ind_|_ult1.*$|_Rank|_Norm_Exp", "", names(basePredSums))
productsSimple <- unique(productsSimpleCols)
standardProd <- "cco_fin" #"reca_fin"

# Load the model confidences over time (cross validation TP/FN ratio)
meanFoldPredRatios <- readRDS(file.path(baseFolder,
                                        paste0("meanFoldPredRatios ", modelName,
                                               ".rds")))

# Load the normalisation probability multiplier
probMultipliers <- readRDS(file.path(baseFolder,
                                        paste0("probMultipliers ", modelName,
                                               ".rds")))

# Load the relative MAP contributions
mapContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        "monthlyMAPContributions.rds"))

# Extract clients with positive flanks
posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                               "positive flank clients.rds")
posFlankClients <- readRDS(posFlankClientsFn)

# Load all train counts
allCounts <- readRDS(monthlyCountsPath)
trainCounts <- as.vector(allCounts) * nrow(basePredictions)/929615
names(trainCounts) <- names(basePredSums)[!grepl("Weighted|Rank",
                                                 names(basePredSums))]

# Load the estimated relative count contributions
countContributions <- readRDS(file.path(getwd(), "Feature engineering",
                                        targetDate,
                                        # "monthlyMAPContributions.rds"))
                                        "monthlyRelativeProductCounts.rds"))

# Calculate the extrapolated public leaderboard number of positive test flanks
publicPosFlanks <- nrow(basePredictions) * fractionPosFlankUsers *
  expectedCountPerPosFlank * countContributions[17, ]

# Subset the precalculated data when dropping low priority ids
if(dropLowPriorityIds){
  productsSimple <- productsSimple[-lowPriorityIds]
  publicPosFlanks <- publicPosFlanks[-lowPriorityIds]
}