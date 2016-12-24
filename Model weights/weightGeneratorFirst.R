#####################################################
# Model weight generator for the first level models #
#####################################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Model weights")

# Load the required libraries
library(data.table)
library(stringr)

# Target date 
targetDate <- "12-11-2016"

# List the month back weights - give more weight to models from 12 months back
monthsBackModels <- 0:15
nbMonths <- length(monthsBackModels)
# monthsBackModelsWeights <- rev(c(12, 0.1*(5:15)))
monthsBackWeightDates <- rev(as.Date(paste(c(rep(2015, 11), rep(2016, 5)),
                                           str_pad(c(2:12, 1:5), 2, pad='0'),
                                           28, sep="-")))
# Target weight sum and default weights
monthsBackModelsWeights <- rev(c(1e-10, 1e-10, 1.2, 1.3, 13, 0.1*(15:25)))
# monthsBackModelsWeights <- rev(c(12, rep(3, 11)))
weightSum <- 1 # 37.5 # sum(monthsBackModelsWeights)
monthsBackLags <- 16:1

# List the base products
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
targetVars <- c("hasNewProduct", paste0("ind_", baseProducts, "_ult1"))
nbModels <- length(targetVars)

# List month product combinations where the weight are overriden
monthProductWeightOverride <- NULL
# monthProductWeightOverride <-
#   rbind(monthProductWeightOverride, data.frame(product = "ind_nom_pens_ult1",
#                                                month = as.Date(c("2015-04-28",
#                                                                  "2015-06-28",
#                                                                  "2015-07-28",
#                                                                  "2016-01-28",
#                                                                  "2016-02-28",
#                                                                  "2016-04-28"
#                                                                  )),
#                                                weight = 0)
#   )
monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_cco_fin_ult1",
                                               month = as.Date(c("2015-12-28"
                                               )),
                                               weight = 9)
  )
monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_cco_fin_ult1",
                                               month = as.Date(c("2015-04-28",
                                                                 "2015-05-28",
                                                                 "2015-07-28",
                                                                 "2015-08-28",
                                                                 "2015-09-28",
                                                                 "2015-10-28",
                                                                 "2015-11-28",
                                                                 "2016-01-28",
                                                                 "2016-02-28",
                                                                 "2016-03-28",
                                                                 "2016-04-28",
                                                                 "2016-05-28"
                                               )),
                                               weight = 3e-1)
  )
monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_ctma_fin_ult1",
                                               month = as.Date(c("2015-02-28",
                                                                 "2015-03-28",
                                                                 "2015-04-28",
                                                                 "2015-05-28",
                                                                 "2015-06-28",
                                                                 "2015-07-28",
                                                                 "2015-08-28",
                                                                 "2015-09-28"
                                               )),
                                               weight = 1e-10)
  )

monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_ecue_fin_ult1",
                                               month = as.Date(c("2015-04-28"
                                               )),
                                               weight = 1e-10)
  )
monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_ecue_fin_ult1",
                                               month = as.Date(c("2015-06-28"
                                               )),
                                               weight = 1.4)
  )
monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_reca_fin_ult1",
                                               month = as.Date(c("2015-03-28"
                                               )),
                                               weight = 1e-10)
  )
monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_reca_fin_ult1",
                                               month = as.Date(c("2015-06-28"
                                               )),
                                               weight = 52)
  )
monthProductWeightOverride <-
  rbind(monthProductWeightOverride, data.frame(product = "ind_recibo_ult1",
                                               month = as.Date(c("2015-04-28"
                                               )),
                                               weight = 1e-10)
  )

######################################################################

# Set the base model weights
modelWeights <- NULL
for(i in 1:nbMonths){
  # List the files in the considered model group
  monthsBack <- monthsBackModels[i]
  lag <- monthsBackLags[i]
  relativeWeightOrig <- monthsBackModelsWeights[i]
  weightDate <- monthsBackWeightDates[i]
  
  # Loop over all models
  for(j in 1:nbModels){
    targetProduct <- targetVars[j]
    
    # Check if the month product combination is in monthProductWeightOverride
    # and if so, set the weight for the model to the override weight
    overrideId <- which(monthProductWeightOverride$product == targetProduct &
                          monthProductWeightOverride$month == weightDate)
    if(length(overrideId)>0){
      relativeWeight <- monthProductWeightOverride$weight[overrideId]
    } else{
      relativeWeight <- relativeWeightOrig
    }
    
    # Append the model info
    modelWeights <- rbind(modelWeights,
                          data.table(
                            targetProduct = targetProduct,
                            monthsBack = monthsBack,
                            modelLag = lag,
                            weightDate = weightDate,
                            relativeWeight = relativeWeight)
    )
  }
}

# Normalize the base model weights 
for(i in 1:nbModels){
  productIds <- modelWeights$targetProduct==targetVars[i]
  productWeightSum <- modelWeights[productIds, sum(relativeWeight)]
  normalizeWeightRatio <- weightSum/productWeightSum
  modelWeights[productIds, relativeWeight := relativeWeight*
                 normalizeWeightRatio]
}

# Store the model weights
saveFolder <- file.path(getwd(), targetDate)
dir.create(saveFolder, showWarnings = FALSE)
saveRDS(modelWeights, file.path(saveFolder, "model weights first.rds"))