# Generate a data table with all positive flank conditional probabilities,
# given the transition of all other product positive flanks

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)
library(stringr)

# Target date
targetDate <- "12-11-2016"

# List the base products
baseProducts <- factor(c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                         "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                         "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                         "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                         "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                         "viv_fin", "nomina", "nom_pens", "recibo"
)
)
nbProducts <- length(baseProducts)

# List the month weights
weightDates <- as.Date(paste(c(rep(2015, 11), rep(2016, 6)),
                             str_pad(c(2:12, 1:6), 2, pad='0'), 28, sep="-"))
dateWeights <- c(1, 1.1, 1.2, 1.3, 14, 0.1*(15:25), 1)


###################################################################

# Read the positive flanks data
positiveFlanksFn <- file.path(getwd(), targetDate,
                              "product positive flanks.rds")
positiveFlanks <- readRDS(positiveFlanksFn)

# Calculate the record weights
positiveFlanks[, weight := dateWeights[match(positiveFlanks$fecha_dato,
                                             weightDates)]]

# Calculate the conditional probabilities
condProbs <- NULL
nonCondProbs <- NULL
userMonthCounts <- positiveFlanks[, .(nbProd = .N), .(ncodpers, fecha_dato)]
setkey(userMonthCounts, ncodpers, fecha_dato)
setkey(positiveFlanks, ncodpers, fecha_dato)
positiveFlanks <- positiveFlanks[userMonthCounts]
for(i in 1:nbProducts){
  baseProduct <- baseProducts[i]
  consFlanksUserMonths <- positiveFlanks[newProdSimple == baseProduct,
                                         .(ncodpers, fecha_dato, weight)]
  otherFlanksUser <- positiveFlanks[consFlanksUserMonths]
  otherFlanksUser <- otherFlanksUser[newProdSimple != baseProduct, ]
  totalWeight <- sum(consFlanksUserMonths$weight)
  for(j in 1:nbProducts){
    conditionalProduct <- baseProducts[j]
    condProb <- otherFlanksUser[newProdSimple == conditionalProduct,
                                sum(weight)]/totalWeight
    if(!is.na(condProb) && !is.finite(condProb)) browser()
    condProbs <- rbind(condProbs,
                       data.table(baseProduct = baseProduct,
                                  conditionalProduct = conditionalProduct,
                                  condProb = condProb))
  }
}

# Store the conditional positive flanks information
condProbs[, baseProduct := paste0("ind_", baseProduct, "_ult1")]
condProbs[, conditionalProduct := paste0("ind_", conditionalProduct, "_ult1")]
condProbs[, jointKey := paste0(baseProduct, conditionalProduct)]

# Manual adjustment nomina - nom_pens
condProbs[jointKey == "ind_nom_pens_ult1ind_nomina_ult1", condProb := 0.97]

saveRDS(condProbs, file.path(getwd(), targetDate,
                             "product conditional positive flanks.rds"))