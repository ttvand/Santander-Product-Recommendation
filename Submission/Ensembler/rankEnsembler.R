# Logic to ensemble different submissions

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Path to the submissions
ensembleDict <- list()
ensembleDict <- c(ensembleDict,
                  list(list(fn = file.path(getwd(), "Submission",
                                           "17-11-2016/xgboost simple 6 back 11-11 Zero deco_fin dela_fin.csv"),
                            weight = 0.0300757)))
ensembleDict <- c(ensembleDict,
                  list(list(fn = file.path(getwd(), "Submission",
                                           "19-11-2016/xgboost simple 8 back 11-11 trainAll normalize June16.csv"),
                            weight = 0.0300359)))
ensembleDict <- c(ensembleDict,
                  list(list(fn = file.path(getwd(), "Submission",
                                           "20-11-2016/xgboost 10 back 11-11 Zero deco_fin dela_fin normalize 3-22-23.csv"),
                            weight = 0.0300033)))

# Weight exponent
weightExp <- 1/(1.123)

# Relative difference cutoff
relDiffCutoff <- 0.5

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


###############################################################

# Count the number of submissions
nbSubmissions <- length(ensembleDict)

# Load the submissions
submissionNames <- lapply(ensembleDict, function(x) x[[1]])
rawSubmissions <- lapply(submissionNames, function(x) fread(x))

# Calculate the number of predicted new product rows
nbProducts <- nrow(rawSubmissions[[1]])

# Add the rank of the products to the submission data tables
productRanks <- NULL
for(i in 1:nbSubmissions){
  allProdRanks <- strsplit(paste(rawSubmissions[[i]][[2]], collapse = " "),
                           split = " ")[[1]]
  submissionName <- submissionNames[i]
  weights <- ensembleDict[[i]][[2]] * (1/(1:7))^weightExp
  for(j in 1:7){
    prodRankJ <- factor(allProdRanks[7*(0:(nbProducts-1))+j],
                        levels = allProducts)
    productRanks <- rbind(productRanks,
                          data.table(ncodpers = rawSubmissions[[1]]$ncodpers,
                                     product = prodRankJ,
                                     rank = j,
                                     weight = weights[j],
                                     submission = submissionName,
                                     submissionId = i
                          ))
  }
}

# Optionally omit predictions for customers that have no positive train flanks
# for the top predictions comparison
topPredictions <- productRanks[rank==1,]
if(restrictPosFlankCustomers){
  posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                                 "positive flank clients.rds")
  posFlankClients <- readRDS(posFlankClientsFn)
  for(i in 1:nbSubmissions){
    topPredictions <- topPredictions[ncodpers %in% posFlankClients,]
  }
}

# Compare the submission top predictions
topCompAgreement <- matrix(1, nrow=nbSubmissions, ncol=nbSubmissions)
for(i in 1:(nbSubmissions-1)){
  topI <- topPredictions[submissionId == i, ]
  for(j in (i+1):nbSubmissions){
    topJ <- topPredictions[submissionId == j, ]
    topCompAgreement[i, j] <- mean(topI$product == topJ$product)
    topCompAgreement[j, i] <- topCompAgreement[i, j]
  }
}

# Display the maximum top product alignment
cat("Maximum top product alignment:",
    round(max(topCompAgreement[topCompAgreement<1]), 3), "\n")

# Generate the ensembled submission
combinedProductRanks <- productRanks[, .(weightSum = sum(weight)) ,
                                     .(ncodpers, product)]
combinedProductRanks[,order_predict := match(1:length(weightSum),
                                             order(-weightSum)), by=ncodpers]
combinedProductRanks <- combinedProductRanks[order(ncodpers, -weightSum), ]

# Make sure that the order of the predictions is unique for each client
orderCount <- combinedProductRanks[, .N, .(ncodpers, order_predict)]
if(max(orderCount$N)>1) browser()

# Combine the top seven products to a string vector
productString <- paste(combinedProductRanks[order_predict==1, product],
                       combinedProductRanks[order_predict==2, product],
                       combinedProductRanks[order_predict==3, product],
                       combinedProductRanks[order_predict==4, product],
                       combinedProductRanks[order_predict==5, product],
                       combinedProductRanks[order_predict==6, product],
                       combinedProductRanks[order_predict==7, product])

# Check for ties in the ordering (should not occur)
if(length(productString) != nbProducts) browser()

# Add the id and top 7 to the submission file
submission <- data.frame(ncodpers = rawSubmissions[[1]]$ncodpers,
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