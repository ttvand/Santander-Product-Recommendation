# Study the evolution of new products fraction over time

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(ggplot2)

baseProducts <- factor(c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                         "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                         "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                         "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                         "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                         "viv_fin", "nomina", "nom_pens", "recibo"
)
)

# Target date 
targetDate <- "12-11-2016"

# Restrict the analysis to products with any positive flank?
restrictPosFlankCustomers <- TRUE

# Construct the prober leaderboard data
probedJun16 <- data.table(newProdSimple = baseProducts,
                          fecha_dato = as.Date("2016-06-28"),
                          N = c(0, 0, 10047, 4, 1855, 59, 466, 232, 145, 0, 0, 98, 
                                2073, 4, 3, 38, 6, 3336, 4281, 364, 4, 2233, 2266, 9024))

# Load the new products data
productPosFlank <- readRDS(file.path(getwd(), "Feature engineering",
                                     targetDate, "product positive flanks.rds"))


##################################################################

# Load the training data
train <- readRDS(file.path(getwd(), "Data", "train.rds"))

# Optionally omit predictions for customers that have no positive train flanks
if(restrictPosFlankCustomers){
  posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                                 "positive flank clients.rds")
  posFlankClients <- readRDS(posFlankClientsFn)
  train <- train[ncodpers %in% posFlankClients,]
  productPosFlank <- productPosFlank[ncodpers %in% posFlankClients,]
}

# Study the mean previous value and mean positive flanks for all products
# and months
allDates <- sort(unique(train$fecha_dato))
posFlanksSummary <- NULL
for(i in 2:length(allDates)){
  for(j in 1:24){
    baseProduct <- baseProducts[j]
    nbPosFlanks <- sum(productPosFlank$newProdSimple == baseProduct &
                         productPosFlank$fecha_dato == allDates[i])
    targetNcodPers <- unique(
      intersect(train[fecha_dato==allDates[i-1], ncodpers],
                train[fecha_dato==allDates[i], ncodpers]))
    posFlanksPrev0Fraction <-
      nbPosFlanks / sum(train[fecha_dato == allDates[i-1] &
                                ncodpers %in% targetNcodPers,
                              paste0("ind_", baseProduct, "_ult1"),
                              with=FALSE] == 0)
    prev1Fraction <- mean(train[fecha_dato == allDates[i-1] &
                                  ncodpers %in% targetNcodPers,
                                paste0("ind_", baseProduct, "_ult1"),
                                with=FALSE] == 1)
    posFlanksSummary <- rbind(posFlanksSummary,
                              data.table(
                                fecha_dato = allDates[i],
                                product = baseProduct,
                                nbPosFlanks = nbPosFlanks,
                                posFlanksPrev0Fraction = posFlanksPrev0Fraction,
                                prev1Fraction = prev1Fraction)
    )
  }
}