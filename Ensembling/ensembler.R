##############################################################
# Calculate a weighted average of the base submissions using #
# the mutual agreement and public leaderboard feedback       #
##############################################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)
library(readxl)

# File name of the submission inventory
inventoryFn <- "Submission Inventory.xlsx"
pathSheet <- "Model paths"

# File name of the agreement file
agreementFn <- "agreementLast26.rds"

# Average method
averageMethod <- c("Probability", "Rank")[1]

# Submission subset selection and weight selection parameters
# Ensemble 1 - independent four good submissions after the third baseline
submissionSubset <- c(14, 15, 17, 19)
perfectAgreementScore <- 0.98
baselinePublicScore <- 0.030985
baselinePublicCutoff <- 0.03098 # No effect in this ensemble
scoreExponent <- 1

# # Ensemble 2 - All 26 models after the third baseline
# submissionSubset <- 1:26
# perfectAgreementScore <- 1
# baselinePublicCutoff <- 0.03098
# baselinePublicScore <- 0.030975
# scoreExponent <- 2

# Target date 
targetDate <- "12-11-2016"

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE

#############################################################################

# Load the submission inventory
inventory <- read_excel(file.path(getwd(), "Ensembling", inventoryFn),
                        sheet = pathSheet)

# Extract the number of base submissions
nbSubmissions <- nrow(inventory)

# Load the submission agreement
submissionAgreement <- readRDS(file.path(getwd(), "Ensembling", agreementFn))
meanAgreements <- rowMeans(submissionAgreement, na.rm = TRUE)

# Study the diversity of the models
d <- as.dist(1-submissionAgreement) # euclidean distances between the rows
fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Submission agreement",	type="n")
textCol <- ifelse(1:nbSubmissions %in% submissionSubset, "red", "black")
text(x, y, labels = gsub("Submission ", "", rownames(submissionAgreement)),
     cex=.7, col = textCol)

# Plot the mean agreement versus the public leaderboard score
# plot(meanAgreements, inventory$`Public score`)
plot(meanAgreements, inventory$`Public score`,
     xlab="Mean agreement", ylab="Public score", type="n")
text(meanAgreements, inventory$`Public score`,
     labels = gsub("Submission ", "", rownames(submissionAgreement)),
     cex=.7, col = textCol)

# Subset the considered submissions
inventory <- inventory[submissionSubset, ]
publicAboveCutoff <- inventory$`Public score`>baselinePublicCutoff
submissionSubset <- submissionSubset[publicAboveCutoff]
inventory <- inventory[publicAboveCutoff, ]
submissionAgreement <- submissionAgreement[submissionSubset, submissionSubset]
submissionIds <- submissionSubset
nbSubmissions <- length(submissionIds)
meanAgreements <- rowMeans(submissionAgreement, na.rm = TRUE)

# Calculate the base submission weights
submissionWeights <- (inventory$`Public score` - baselinePublicScore)^
  scoreExponent * (perfectAgreementScore-meanAgreements)
submissionWeights <- submissionWeights/mean(submissionWeights)
weightSum <- sum(submissionWeights)

# Set up the file path to the submissions
submissionPaths <- file.path(getwd(), "Submission",
                             inventory$`Predictions folder`, "Predictions",
                             paste0(inventory$`Predictions extension`, ".rds"))

if(averageMethod == "Rank"){
  # Take the weighted average of the ranks of the submissions
  allPredictions <- readRDS(submissionPaths[[1]])
  allPredictions <- allPredictions[order(ncodpers, product)]
  allPredictions[, totalRank := 1/order_predict*submissionWeights[1]/weightSum*
                   as.numeric(totalProb>0)]
  
  for(i in 2:nbSubmissions){
    # Show progress message
    cat("Adding submission", i, "of", nbSubmissions,
        "to the rank ensemble\n")
    
    rawSubmission <- readRDS(submissionPaths[[i]])
    rawSubmission <- rawSubmission[order(ncodpers, product)]
    allPredictions[, totalRank := totalRank + 1/order_predict *
                     submissionWeights[i]/weightSum *
                     as.numeric(totalProb>0)]
  }
  allPredictions[, totalProb := totalRank]
} else{
  # Take the weighted average of the probabilities of the submissions
  allPredictions <- readRDS(submissionPaths[[1]])
  allPredictions <- allPredictions[order(ncodpers, product)]
  allPredictions[, totalProb := totalProb*submissionWeights[1]/weightSum]
  
  for(i in 2:nbSubmissions){
    # Show progress message
    cat("Adding submission", i, "of", nbSubmissions,
        "to the probability ensemble\n")
    
    rawSubmission <- readRDS(submissionPaths[[i]])
    rawSubmission <- rawSubmission[order(ncodpers, product)]
    allPredictions[, totalProb := totalProb + submissionWeights[i]/weightSum*
                     rawSubmission$totalProb]
  }
}

# Order the predicted probabilities for all products by client
setkey(allPredictions, ncodpers)
allPredictions[, order_predict := match(1:length(totalProb),
                                        order(-totalProb)), by=ncodpers]
allPredictions <- allPredictions[order(ncodpers, -totalProb), ]

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
if(nomPensAboveNominaBothNotOwned){
  # Find users where the rank of nomina < rank of nom pens and both prob not
  # zero
  ncodpers <- unique(allPredictions$ncodpers)
  nominaProb <- allPredictions[product == "ind_nomina_ult1", totalProb]
  nominaProbRank <- allPredictions[product == "ind_nomina_ult1", order_predict]
  nomPensProb <- allPredictions[product == "ind_nom_pens_ult1", totalProb]
  nomPensProbRank <- allPredictions[product == "ind_nom_pens_ult1", order_predict]
  swapIds <- nominaProb>0 & nomPensProb>0 & nominaProb>nomPensProb
  swapNcodpers <- ncodpers[swapIds]
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                 order_predict := nomPensProbRank[swapIds]]
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                 order_predict := nominaProbRank[swapIds]]
  
  # Also swap the predictions for logic down the MAP boosting pipeline!
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nomina_ult1",
                 totalProb := nomPensProb[swapIds]]
  allPredictions[ncodpers %in% swapNcodpers & product == "ind_nom_pens_ult1",
                 totalProb := nominaProb[swapIds]]
}

# Order the predicted probabilities for all products by client
setkey(allPredictions, ncodpers)
allPredictions[, order_predict := match(1:length(totalProb),
                                        order(-totalProb)), by=ncodpers]
allPredictions <- allPredictions[order(ncodpers, -totalProb), ]

# Make sure that the order of the predictions is unique for each client
orderCount <- allPredictions[, .N, .(ncodpers, order_predict)]
if(max(orderCount$N)>1) browser()

# Show the confidence in the top prediction
hist(allPredictions[order_predict==1, totalProb])

# Extract clients with positive flanks
posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                               "positive flank clients.rds")
posFlankClients <- readRDS(posFlankClientsFn)

# Calculate the top predicted products counts
topPredictions <- allPredictions[order_predict==1, .N, product]
topPredictions <- topPredictions[order(-N)]
topPredictionsPosFlanks <- allPredictions[order_predict==1 &
                                            ncodpers %in% posFlankClients,
                                          .N, product]
topPredictionsPosFlanks <- topPredictionsPosFlanks[order(-N)]

# Verify that the mean prediction aligns with the relative June 15 ratio
meanProductProbs <- allPredictions[, .(meanOrigProb = mean(prediction),
                                       meanProb = mean(totalProb),
                                       totalProb = sum(totalProb)), product]
meanProductProbs <- meanProductProbs[order(-meanProb), ]

# Combine the top seven products to a string vector
productString <- paste(allPredictions[order_predict==1, product],
                       allPredictions[order_predict==2, product],
                       allPredictions[order_predict==3, product],
                       allPredictions[order_predict==4, product],
                       allPredictions[order_predict==5, product],
                       allPredictions[order_predict==6, product],
                       allPredictions[order_predict==7, product])

# Add the id and top 7 to the submission file
submission <- data.frame(ncodpers = unique(allPredictions$ncodpers),
                         added_products = productString)

# Extract template submission file
paddedSubmission <- fread("Data/sample_submission.csv")

# Set the added products to an empty character string
paddedSubmission[, added_products := ""]

# Replace the matched ids in padded submission by the combined submission file
matchIds <- match(submission$ncodpers, paddedSubmission$ncodpers)
paddedSubmission[matchIds, added_products := submission$added_products]

# Write the padded submission to a csv and rds file
saveBase <- gsub("[:]", "-", as.character(Sys.time()))
write.csv(paddedSubmission,
          file.path(getwd(), "Ensembling",
                    paste0(saveBase, ".csv")),
          row.names = FALSE)
saveRDS(allPredictions, file.path(getwd(), "Ensembling",
                  paste0(saveBase, ".rds")))

# Display the successful submission message
cat("Submission file created successfully!\n",
    nrow(submission)," records were predicted (",
    round(nrow(submission)/nrow(paddedSubmission)*100,2),"%)\n", sep="")