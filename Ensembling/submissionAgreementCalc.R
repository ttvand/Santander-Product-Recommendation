#################################################################
# Calculate agreement of submissions based on the product ranks #
#################################################################

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

# Save extension of the agreement
agreementExtension <- "Last26"

# No positive flanks relative weight
noPosFlanksWeight <- 0.2

# Maximum compared rank
maxComparedRank <- 10

# List all the possible products
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
allProducts <- paste0("ind_", baseProducts, "_ult1")

# Target date
targetDate <- "12-11-2016"


#############################################################################

# Load the submission inventory
inventory <- read_excel(file.path(getwd(), "Ensembling", inventoryFn),
                        sheet = pathSheet)

# Extract the number of base submissions
nbSubmissions <- nrow(inventory)

# Load the rank submissions
submissions <- vector(mode = "list", length = nbSubmissions)
for(i in 1:nbSubmissions){
  # Show a progress message
  cat("Extracting submission", i, "of", nbSubmissions, "\n")
  
  # Read the raw predictions
  predictions <-
    readRDS(file.path(getwd(), "Submission", inventory$`Predictions folder`[i],
                      "Predictions",
                      paste0(inventory$`Predictions extension`[i], ".rds")))
  predictions <- predictions[, c(1, 2, 4), with=FALSE]
  
  # Make sure that ncodpers is ordered
  if(min(diff(predictions$ncodpers)) < 0) browser()
  
  # Order by ncodpers and then by product and append the ranks
  predictionsRank <- predictions[order(ncodpers, product)]
  if(i==1){
    predictionRanks <- predictionsRank
  } else{
    predictionRanks <- cbind(predictionRanks, predictionsRank[[3]])
  }
  names(predictionRanks)[2+i] <- paste0("RankSubmission", i)
  
  # # Read the raw csv of the ranks
  # submission <-
  #   fread(file.path(getwd(), "Submission", inventory$`Predictions folder`[i],
  #                   paste0(inventory$`Predictions extension`[i], ".csv")))
  # 
  # # Store ncodpers when loading the first submission
  # if(i==1){
  #   ncodpers <- submission$ncodpers
  # }
  # 
  # # Make sure that ncodpers is ordered
  # if(min(diff(submission$ncodpers)) <= 0) browser()
  # 
  # # Calculate the number of predicted new product rows
  # nbProducts <- nrow(submission)
  # 
  # # Extract the top 7 products for the submission
  # prods <- strsplit(paste(submission[[2]], collapse = " "), split = " ")[[1]]
  # for(j in 1:7){
  #   submission[[paste0("Rank", j)]] <- factor(prods[7*(0:(nbProducts-1))+j],
  #                                             levels = allProducts)
  # }
  # 
  # # Drop ncodpers and added_products since this is redundant information
  # submission[, c("ncodpers", "added_products") := NULL]
  # submissions[[i]] <- submission
}

# Read the positive flanks data
posFlanks <- readRDS(file.path(getwd(), "Feature engineering", targetDate,
                               "product positive flanks.rds"))

# Count the number of positive flanks for all ncodpers on separate months
testNcodpers <- unique(predictions$ncodpers)
posFlanksUserMonth <- posFlanks[ncodpers %in% testNcodpers,
                                .(count = length(unique(fecha_dato))),
                                ncodpers]

# Calculate the relative importance weight for each ncodpers and give a default
# weight to users without historical flanks
ncodpersWeight <- rep(noPosFlanksWeight, length(testNcodpers))
matchIds <- match(posFlanksUserMonth$ncodpers, testNcodpers)
ncodpersWeight[matchIds] <- posFlanksUserMonth$count

# Calculate the agreement between submissions
submissionAgreement <-
  matrix(NA, nrow = nbSubmissions, ncol = nbSubmissions,
         dimnames = list(paste("Submission", 1:nbSubmissions),
                      paste("Submission", 1:nbSubmissions)))

# Compare all submission ranks
for(i in 1:(nbSubmissions-1)){
  # Show a progress message
  cat("Comparing first submission", i, "of", nbSubmissions-1, "\n")
  for(j in (i+1):nbSubmissions){
    comparedRanks <- predictionRanks[predictionRanks[[2+i]] <= maxComparedRank,
                                     2 + c(i,j), with=FALSE]
    agreement <- sum(1/comparedRanks[[1]] *
                       1/(1+abs(comparedRanks[[1]]-comparedRanks[[2]])) *
                       rep(ncodpersWeight, each = maxComparedRank)) /
      (sum(ncodpersWeight) * sum(1/(1:maxComparedRank)))
    submissionAgreement[i, j] <- agreement
    submissionAgreement[j, i] <- agreement
  }
}

# Store the submission agreement
saveRDS(submissionAgreement, file.path(getwd(), "Ensembling", 
                                       paste0("agreement", agreementExtension,
                                              ".rds")))