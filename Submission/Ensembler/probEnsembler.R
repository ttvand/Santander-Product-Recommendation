# Logic to ensemble different submissions

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Path to the submissions
ensembleDict <- list()
# ensembleDict <- c(ensembleDict,
#                   list(list(fn = file.path(getwd(), "Submission",
#                                            "28-11-2016/Predictions/xgboost weighted posFlanks 6, more weight dec15 other 0 cco_fin linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.rds"),
#                             weight = 10)))
# ensembleDict <- c(ensembleDict,
#                   list(list(fn = file.path(getwd(), "Submission",
#                                            "30-11-2016/Predictions/xgboost weighted posFlanks 8, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation marginal, linear normalisation conditional.rds"),
#                             weight = 4)))
# ensembleDict <- c(ensembleDict,
#                   list(list(fn = file.path(getwd(), "Submission",
#                                            "30-11-2016/Predictions/xgboost weighted trainAll 8, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.rds"),
#                             weight = 7)))
ensembleDict <- c(ensembleDict,
                  list(list(fn = file.path(getwd(), "Ensembling",
                                           "2016-12-21 22-57-46 - Main ensemble 1.rds"),
                            weight = 1)))
ensembleDict <- c(ensembleDict,
                  list(list(fn = file.path(getwd(), "Ensembling",
                                           "2016-12-21 22-54-57 - Main ensemble 2.rds"),
                            weight = 1)))

# Target date 
targetDate <- "12-11-2016"

# Restrict the analysis to products with any positive flank?
restrictPosFlankCustomers <- TRUE

baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
allProducts <- paste0("ind_", baseProducts, "_ult1")

# Swap nomina and nom pens in rank if they are both not owned in the previous
# period and if the rank of nomina > rank of nom_pens
nomPensAboveNominaBothNotOwned <- TRUE


###############################################################

# Count the number of submissions
nbSubmissions <- length(ensembleDict)

# Load the submissions
submissionNames <- lapply(ensembleDict, function(x) x[[1]])
rawSubmissions <- lapply(submissionNames, function(x) readRDS(x))

# Take the weighted average of the predictions
weightSum <- sum(sapply(ensembleDict, function(x) x$weight))
allPredictions <- rawSubmissions[[1]]
allPredictions <- allPredictions[order(ncodpers, product)]
allPredictions[, totalProb := totalProb*ensembleDict[[1]]$weight/weightSum]

for(i in 2:nbSubmissions){
  rawSubmission <- rawSubmissions[[i]]
  rawSubmission <- rawSubmission[order(ncodpers, product)]
  allPredictions[, totalProb := totalProb + ensembleDict[[i]]$weight/weightSum*
                   rawSubmission$totalProb]
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
allPredictions[,order_predict := match(1:length(totalProb),
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
newProdPredictions <- 1
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
submission <- data.frame(ncodpers = unique(rawSubmissions[[1]]$ncodpers),
                         added_products = productString)

# Extract template submission file
paddedSubmission <- fread("Data/sample_submission.csv")

# Set the added products to an empty character string
paddedSubmission[, added_products := ""]

# Replace the matched ids in padded submission by the combined submission file
matchIds <- match(submission$ncodpers, paddedSubmission$ncodpers)
paddedSubmission[matchIds, added_products := submission$added_products]

# Write the padded submission to a csv file
write.csv(paddedSubmission,
          file.path(getwd(), "Submission", "Ensembler",
                    paste0(gsub("[:]", "-", as.character(Sys.time())), ".csv")),
          row.names = FALSE)

# Display the successful submission message
cat("Submission file created successfully!\n",
    nrow(submission)," records were predicted (",
    round(nrow(submission)/nrow(paddedSubmission)*100,2),"%)\n", sep="")
