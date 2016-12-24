# Script to generate dummy submissions where all predicted products are
# set to a single product

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)


################################################################

# List all target variables
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
targetVars <- paste0("ind_", baseProducts, "_ult1")
nbTargetVars <- length(targetVars)

# Extract template submission file
sampleSubmission <- fread("Data/sample_submission.csv")

# Generate all 24 dummy csv files
for(i in 1:nbTargetVars){
  dummyTable <- sampleSubmission
  dummyTable[, added_products :=  targetVars[i]]
  write.csv(dummyTable, file.path(getwd(), "Submission", "Leaderboard probing",
                                  "Submission files",
                                  paste0("all_", baseProducts[i], ".csv")),
            row.names = FALSE)
}