# Study and store the relative MAP contribution over time
# The extrapolated MAP contribution is also calculated for June 2016 (test data)

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)
library(forecast)

# Target date 
targetDate <- "12-11-2016"

# Time between products rate plots in seconds
showAllProductPlots <- !TRUE
sleepTime <- 2

# Monthly counts path
loadFn <- "monthlyTransitionCounts.rds"
loadPath <- file.path(getwd(), "Feature engineering", targetDate,
                      loadFn)

# Save parameters
loadContributions <- TRUE
saveContributions <- TRUE
saveFnMonthlyMAP <- "monthlyMAPContributions.rds"
saveFnRelProdCounts <- "monthlyRelativeProductCounts.rds"
savePathMonthlyMAP <- file.path(getwd(), "Feature engineering", targetDate,
                                saveFnMonthlyMAP)
savePathRelProdCounts <- file.path(getwd(), "Feature engineering", targetDate,
                                   saveFnRelProdCounts)


#######################################################################

# Load the counts data
allCounts <- readRDS(loadPath)

if(loadContributions && file.exists(savePathMonthlyMAP)){
  relativeContributions <- readRDS(savePathMonthlyMAP)
  targetCols <- colnames(relativeContributions)
  nbTargetCols <- length(targetCols)
  sortedDates <- as.Date(rownames(relativeContributions))
} else{
  # Read the raw data
  train <- readRDS("Data/train.rds")
  
  # List the target columns
  targetCols <- names(train)[-(1:24)]
  nbTargetCols <- length(targetCols)
  
  # List all unique sorted dates
  sortedDates <- sort(unique(train$fecha_dato))
  nbDates <- length(sortedDates) - 1
  
  # Store all monthly relative MAP contributions
  relativeContributions <- matrix(0, nrow = nbDates, ncol = nbTargetCols,
                                  dimnames = list(as.character(sortedDates[-1]),
                                                  targetCols))
  
  # Calculate all monthly counts and calculate relative MAP contributions
  productsPerChangeUser <- rep(NA, nbDates)
  for(i in 1:nbDates){
    # Show progress message
    cat("Processing target date", i, "of", nbDates, "@",
        as.character(Sys.time()), "\n")
    
    targetNcodPers <-
      unique(intersect(train[fecha_dato==sortedDates[i+1], ncodpers],
                       train[fecha_dato==sortedDates[i], ncodpers]))
    
    secondLastMonth <- as.matrix(train[fecha_dato==sortedDates[i] &
                                         ncodpers %in% targetNcodPers,
                                       targetCols, with=FALSE])
    lastMonth <- as.matrix(train[fecha_dato==sortedDates[i+1] &
                                   ncodpers %in% targetNcodPers,
                                 targetCols, with=FALSE])
    productChange <- (lastMonth - secondLastMonth) == 1
    productChangesMonth <- colSums(productChange, na.rm = TRUE)
    userProducts <- rowSums(productChange, na.rm = TRUE)
    
    # Loop over all products to calculate their relative MAP contributions
    for(j in 1:nbTargetCols){
      relativeContributions[i,j] <-
        sum(productChange[userProducts>0, j] / 
              userProducts[userProducts>0], na.rm = TRUE) /
        sum(userProducts>0)
    }
    productsPerChangeUser[i] <- sum(userProducts)/sum(userProducts>0)
  }
  
  # Drop the first date for the plots
  sortedDates <- sortedDates[-1]
  
  # Plot the productsPerChangeUser over time
  # plot(productsPerChangeUser)
}

# Study the relative MAP contributions
if(showAllProductPlots){
  for(i in 1:nbTargetCols){
    targetCol <- targetCols[i]
    
    # Look up the relative monthly MAP contributions
    contributions <- relativeContributions[,i]
    plot(sortedDates, contributions, ylim = c(0, max(contributions)),
         main=paste0(i, " - relative contribution - ", targetCol), pch=16, 
         col="black")
    points(sortedDates[5], contributions[5], pch=16, col="green")
    
    Sys.sleep(sleepTime)
  }
}

# Use time series models to extrapolate the map contributions for the test data
# Target column ids of specific interest: 3, 5, 7, 13, 18, 19, 22, 23 and 24
# Columns with outliers in monthly trend: 3, 5
# Leaderboard probing test order: 3, 24, 19, 23, 5, 13, 22, 7
testContributions <- matrix(NA, nrow = nbTargetCols, ncol = 6,
                            dimnames = list(targetCols,
                                            c("arima", "ses", "holt",
                                              "mean", "adjusted",
                                              "lb probing")))
for(i in 1:nbTargetCols){
  contributions <- ts(relativeContributions[,i], frequency = 12)
  arimaFit <- auto.arima(contributions, seasonal = TRUE, approximation = TRUE)
  testContributions[i, 1:3] <- pmax(0,
                                    c(as.numeric(forecast(arimaFit, h=1)$mean),
                                      as.numeric(ses(contributions, h=1)$mean),
                                      as.numeric(holt(contributions, h=1)$mean)
                                    )
  )
  testContributions[i,4] <- mean(testContributions[i, 1:3])
}

# Compare the count/contribution ratio over time
# Products that appear new along with other products tend to have a lower MAP
# contribution ratio. Product 5 tends to have a positive flank more often than
# other products but it is especially apparent for products 22 and 23
meanCountContributionRatio <- colSums(allCounts) /
  colSums(relativeContributions[-17,])
plot(meanCountContributionRatio)

# Zoom in on the ratio for products 5, 22 and 23
plot(allCounts[,3]/rowSums(allCounts)/relativeContributions[-17,3])
plot(allCounts[,5]/rowSums(allCounts)/relativeContributions[-17,5])
plot(allCounts[,7]/rowSums(allCounts)/relativeContributions[-17,7])
plot(allCounts[,12]/rowSums(allCounts)/relativeContributions[-17,12])
plot(allCounts[,13]/rowSums(allCounts)/relativeContributions[-17,13])
plot(allCounts[,18]/rowSums(allCounts)/relativeContributions[-17,18])
plot(allCounts[,19]/rowSums(allCounts)/relativeContributions[-17,19])
plot(allCounts[,20]/rowSums(allCounts)/relativeContributions[-17,20])
plot(allCounts[,22]/rowSums(allCounts)/relativeContributions[-17,22])
plot(allCounts[,23]/rowSums(allCounts)/relativeContributions[-17,23])
plot(allCounts[,24]/rowSums(allCounts)/relativeContributions[-17,24])

meanCountContributionRatio <- rep(0.87, 24)
for(i in 1:24){
  countContributionRatio <- mean(
    allCounts[,i]/rowSums(allCounts)/relativeContributions[-17,i])
  if(!is.na(countContributionRatio)){
    meanCountContributionRatio[i] <- countContributionRatio
  }
}
# Manual adjustment nom_pens
# meanCountContributionRatio[23] <- 1.01 * meanCountContributionRatio[22]
meanCountContributionRatio[23] <- 1.003 * meanCountContributionRatio[22]  

# Add the probed public leaderboard MAP contributions
testContributions[18,6] <- 0.0032092
testContributions[3,6] <- 0.0096681
testContributions[24,6] <- 0.0086845
testContributions[19,6] <- 0.0041178
testContributions[23,6] <- 0.0021801
testContributions[5,6] <- 0.0017839
testContributions[13,6] <- 0.0019961
testContributions[22,6] <- 0.0021478
testContributions[7,6] <- 0.0004488
testContributions[12,6] <- 0.0000933
testContributions[10,6] <- 0
testContributions[20,6] <- 0.000278
testContributions[8,6] <- 0.0001949
testContributions[9,6] <- 0.0001142
testContributions[14,6] <- 0.000104
testContributions[11,6] <- 0
testContributions[21,6] <- 0
testContributions[1,6] <- 0 # To be verified (not really :))
testContributions[2,6] <- 0 # To be verified (not really :))
testContributions[4,6] <- 0.000009
testContributions[6,6] <- 0.0000502
testContributions[15,6] <- 0.0000161
testContributions[16,6] <- 0.0000126
testContributions[17,6] <- 0.0000054
testContributions[,6] <- testContributions[,6]/sum(testContributions[,6])

# testContributions[18,6] <- 0.0917
# testContributions[3,6] <- 0.2762
# testContributions[24,6] <- 0.2481
# testContributions[19,6] <- 0.1177
# testContributions[23,6] <- 0.0623
# testContributions[5,6] <- 0.0510
# testContributions[13,6] <- 0.0570
# testContributions[22,6] <- 0.0614
# testContributions[7,6] <- 0.0128
# testContributions[12,6] <- 0.0027
# testContributions[10,6] <- 0
# testContributions[20,6] <- 0.0079
# testContributions[8,6] <- 0.0056
# testContributions[9,6] <- 0.0033
# testContributions[14,6] <- 0.003
# testContributions[11,6] <- 0
# testContributions[21,6] <- 0
# testContributions[4,6] <- 0.0003
# testContributions[6,6] <- 0.0014
# testContributions[15,6] <- 0.0005
# testContributions[16,6] <- 0.0004
# testContributions[17,6] <- 0.0002


# Copy the mean contributions to the manual adjusted test contributions as a 
# starting point and make adjustments if needed for the target columns of
# interest (3, 5, 7, 13, 18, 19, 22, 23 and 24)
testContributions[,5] <- testContributions[,4]

# Calculate the maximum plot ranges for the relative MAP contributions
ymaxPlots <- apply(rbind(relativeContributions, testContributions[,6]), 2, max,
                   na.rm = TRUE)

# Study target column 3 for a possible manual adjustment
studiedCol <- 3
# testContributions[studiedCol, 5] <- 0.18 # No manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 5 for a possible manual adjustment
studiedCol <- 5
testContributions[studiedCol, 5] <- 0.05 # Manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 7 for a possible manual adjustment
studiedCol <- 7
# testContributions[studiedCol, 5] <- 0.05 # No manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 13 for a possible manual adjustment
studiedCol <- 13
# testContributions[studiedCol, 5] <- 0.07 # Manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 18 for a possible manual adjustment
studiedCol <- 18
testContributions[studiedCol, 5] <- 0.09 # Manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 19 for a possible manual adjustment
studiedCol <- 19
testContributions[studiedCol, 5] <- 0.12 # Manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 22 for a possible manual adjustment
studiedCol <- 22
# testContributions[studiedCol, 5] <- 0.12 # No manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 23 for a possible manual adjustment
studiedCol <- 23
# testContributions[studiedCol, 5] <- 0.12 # No manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Study target column 24 for a possible manual adjustment
studiedCol <- 24
testContributions[studiedCol, 5] <- 0.30 # Manual adjustment
contributions <- c(relativeContributions[1:16, studiedCol],
                   testContributions[studiedCol,5])
plot(contributions, col="black", ylim=c(0, ymaxPlots[studiedCol]))
points(5, contributions[5], pch=16, col="green")
points(17, testContributions[studiedCol, 4], pch=16, col="grey")
points(17, contributions[17], pch=16, col="red")
points(17, testContributions[studiedCol, 6], pch=16, col="purple")

# Normalize the relative test contributions
testContributions[,5] <- testContributions[,5]/sum(testContributions[,5])

# Estimate and normalize the test contributions
testContributions[is.na(testContributions[,6]),6] <-
  testContributions[is.na(testContributions[,6]),5]
testContributions[,6] <- testContributions[,6] / sum(testContributions[,6])
# dput(round(testContributions[,6] * 929615 * 0.035114 * 1.123))

# Store the relative product map contributions
if(saveContributions){
  if(!loadContributions){
    relativeContributions <- rbind(relativeContributions, testContributions[,6])
  } else{
    relativeContributions[17,] <- testContributions[,6]
  }
  rownames(relativeContributions)[17] <- "2016-06-28"
  saveRDS(relativeContributions, savePathMonthlyMAP)
  
  # Calculate and store the extrapolated count contributions
  countContributions <- allCounts/rep(rowSums(allCounts), ncol(allCounts))
  countContributionsJun16 <- relativeContributions[17,] *
    meanCountContributionRatio
  countContributionsJun16 <- countContributionsJun16/
    sum(countContributionsJun16)
  # dput(round(countContributionsJun16 * 929615 * 0.035114 * 1.25))
  
  countContributions <- rbind(countContributions, countContributionsJun16)
  rownames(countContributions)[17] <- "2016-06-28"
  saveRDS(countContributions, savePathRelProdCounts)
}