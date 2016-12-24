# Study and store the number of new products over time

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)

# Target date 
targetDate <- "12-11-2016"

# Time between products rate plots in seconds
showAllProductPlots <- TRUE
sleepTime <- 2

# Save parameters
loadCounts <- TRUE
saveCounts <- TRUE
saveFn <- "monthlyTransitionCounts.rds"
savePath <- file.path(getwd(), "Feature engineering", targetDate,
                      saveFn)


#######################################################################

if(loadCounts && file.exists(savePath)){
  allCounts <- readRDS(savePath)
  targetCols <- colnames(allCounts)
  nbTargetCols <- length(targetCols)
  sortedDates <- as.Date(rownames(allCounts))
} else{
  
  # Read the raw data
  train <- readRDS("Data/train.rds")
  
  # List the target columns
  targetCols <- names(train)[-(1:24)]
  nbTargetCols <- length(targetCols)
  
  # List all unique sorted dates
  sortedDates <- sort(unique(train$fecha_dato))[-1]
  nbDates <- length(sortedDates)
  
  # Count the number of records for all months
  monthsCounts <- train[fecha_dato %in% sortedDates, .N, fecha_dato]
  
  # Vector to store the number of positive flanks
  nbPosFlanks <- rep(NA, nbTargetCols)
  names(nbPosFlanks) <- targetCols
  
  # Store all monthly counts
  allCounts <- matrix(0, nrow = nbDates, ncol = nbTargetCols, dimnames = list(
    as.character(sortedDates), targetCols))
  
  # Calculate all monthly counts
  for(i in 1:nbTargetCols){
    targetCol <- targetCols[i]
    
    # Show progress message
    cat("Processing target variable", i, "of", nbTargetCols, "@",
        as.character(Sys.time()), "\n")
    
    # Positive flank rows calculation
    posId <- 1 + which(diff(train$ncodpers) == 0 &
                         diff(train$fecha_dato, unit="days") < 32 &
                         train[[targetCol]][-nrow(train)]==0 &
                         train[[targetCol]][-1]==1)
    posFlanksCount <- train[posId, .N, fecha_dato]
    matchId <- match(posFlanksCount$fecha_dato, sortedDates)
    allCounts[matchId,i] <- posFlanksCount$N
  }
  
  # Store the counts
  if(saveCounts){
    saveRDS(allCounts, savePath)
  }
}

# Study new products metrics for all target columns
if(showAllProductPlots){
  # Relative ratios analysis (evolution of product over time)
  for(i in 1:nbTargetCols){
    targetCol <- targetCols[i]
    
    # Look up the positive flank counts and plot relative monthly fractions
    counts <- allCounts[,i]
    meanCounts <- counts/sum(counts)
    plot(sortedDates, meanCounts, ylim = c(0, max(meanCounts)),
         main=paste0(i, " - RR ", targetCol, " (", sum(counts), ")"), pch=16, 
         col="black")
    points(sortedDates[5], meanCounts[5], pch=16, col="green")
    
    Sys.sleep(sleepTime)
  }
  
  # Time fraction analysis (overall evolution over time)
  for(i in 1:nbTargetCols){
    targetCol <- targetCols[i]
    
    # Look up the positive flank counts and plot relative overall fractions
    counts <- allCounts[,i]
    meanCounts <- counts/rowSums(allCounts)
    plot(sortedDates, meanCounts,
         ylim = c(0,max(meanCounts[is.finite(meanCounts)])),
         main=paste0(i, " - Overall frac ", targetCol, " (", sum(counts), ")"),
         pch=16, col="black")
    points(sortedDates[5], meanCounts[5], pch=16, col="green")
    
    Sys.sleep(sleepTime)
  }
}

# Calculate the relative yearly new rate of june versus the train months (first
# four months)
relativeRatiosJune <- rep(NA, nbTargetCols)
names(relativeRatiosJune) <- targetCols
relativeRatiosMay <- relativeRatiosJune
juneCounts <- relativeRatiosJune
relativeRatiosJune <- allCounts[5,] / colMeans(allCounts[1:4,])
relativeRatiosMay <- allCounts[4,] / colMeans(allCounts[1:3,])

# Plot the total number of positive flanks
nbPosFlanks <- colSums(allCounts)
plot(nbPosFlanks)
sort(nbPosFlanks, decreasing = TRUE)

# Plot the relative ratio of May versus the other months
plot(relativeRatiosMay)
abline(h=1, col="green")

# Plot the relative ratio of June versus the other months
plot(relativeRatiosJune)
abline(h=1, col="blue")

# Study the product frequencies in June 2015
juneCounts <- allCounts[5,]
plot(juneCounts)