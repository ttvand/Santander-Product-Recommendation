# Generate a data table with all positive flank records for exploratory
# data analysis

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)

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


###################################################################

# Read in the raw cleaned data
train <- readRDS(paste0("../Data/train.rds"))

# Extract clients that have any positive flanks
posFlankClients <- integer(0)
daysDiff <- diff(train$fecha_dato)
samePersonConsecutive <- (diff(train$ncodpers)) == 0 & (daysDiff>0) &
  (daysDiff < 32)

# Calculate the number of valid clients by month
dateMonths <- sort(unique(train$fecha_dato))
nbDateMonths <- length(dateMonths) - 1
validClientCounts <- rep(NA, nbDateMonths)
for(i in 1:nbDateMonths){
  validClientsMonth <-
    unique(intersect(train[fecha_dato==dateMonths[i], ncodpers],
                     train[fecha_dato==dateMonths[i+1], ncodpers]))
  validClientCounts[i] <- length(validClientsMonth)
}

cbind.dt.simple = function(...) {
  x = c(...)
  setattr(x, "class", c("data.table", "data.frame"))
  ans = .Call(data.table:::Calloccolwrapper, x, max(100L, ncol(x) + 64L), FALSE)
  .Call(data.table:::Csetnamed, ans, 0L)
}

productPosFlank <- NULL
trainCols <- 25:48
for(trainCol in trainCols){
  if(trainCol==27) browser()
  posFlankIds <- 1 + which(samePersonConsecutive &
                             (train[[trainCol]][-nrow(train)] == 0) &
                             (train[[trainCol]][-1] == 1))
  posFlankTrain <- train[posFlankIds,]
  
  # Add lag information to the positive train flanks
  posFlankLag <- train[posFlankIds-1, trainCols, with=FALSE]
  names(posFlankLag) <- paste0(names(posFlankLag), "Lag1")
  
  # Calculate the number of valid clients for the months with positive flanks
  monthMatchIds <- match(posFlankTrain$fecha_dato, dateMonths[-1])
  monthMatchCounts <- validClientCounts[monthMatchIds]
  
  # Combine all data tables to the product positive flank summary data table
  productPosFlank <- rbind(productPosFlank, cbind.dt.simple(
    data.table(newProduct = rep(colnames(train)[trainCol],
                                length(posFlankIds)),
               monthClientCount = monthMatchCounts),
    cbind.dt.simple(posFlankTrain, posFlankLag)))
}

# Remove memory consuming train
rm(train)
gc()

# Recode the new product column
productPosFlank[, ncharNewProd := nchar(newProduct)]
productPosFlank[, newProdSimple := substr(newProduct, 5, ncharNewProd - 5)]
productPosFlank[, newProdSimple := factor(newProdSimple, levels = baseProducts)]

# Add the monthly positive flank and monthly product-month counts
productPosFlankMonthCount <- productPosFlank[, .(monthPosFlankCount = .N),
                                             fecha_dato]
setkey(productPosFlankMonthCount, fecha_dato)
productPosFlankMonthProdCount <- productPosFlank[
  , .(monthProdPosFlankCount = .N), .(fecha_dato, newProdSimple)]
setkey(productPosFlankMonthProdCount, fecha_dato, newProdSimple)
monthCountData <- productPosFlankMonthProdCount[productPosFlankMonthCount]
setkey(productPosFlank, fecha_dato, newProdSimple)
productPosFlank <- productPosFlank[monthCountData]

# Format the month for the shiny app
productPosFlank[, monthFormat := format(fecha_dato, "%b-%Y")]

# Store the product positive flanks information
saveRDS(productPosFlank, file.path(getwd(), targetDate,
                                   "product positive flanks.rds"))