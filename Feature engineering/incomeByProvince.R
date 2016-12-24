# Logic to create an imputed income for all users in the train data set
# Maybe a simple model that uses features which are mostly available?

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)

# Target date
targetDate <- "12-11-2016"


###################################################################

# Read in the raw cleaned data
train <- readRDS(paste0("../Data/train.rds"))
test <- readRDS(paste0("../Data/test.rds"))

# Combine the train and test records
rawData <- rbindlist(list(train[, 1:24, with=FALSE], test), fill=TRUE)
setkey(rawData, ncodpers)

# Study the fraction of missing values in the raw data
sapply(rawData, function(x) round(mean(is.na(x)),3))
classes <- sapply(rawData, class)
rawData[, logRenta := log(renta)]

# Study the correlation of income with the numerical features
intCols <- names(classes)[classes=="integer"]

# Calculate the correlation with the integer columns
corInts <- sapply(intCols, function(x) cor(rawData$logRenta, rawData[[x]],
                                           use = "complete.obs"))

# Columns of interest: age, ncodpers id and the categorical cod_prov
sampleRows <- sample(1:nrow(rawData), 1e3)
plot(rawData$ncodpers[sampleRows], rawData$logRenta[sampleRows],
     use = "complete.obs")
plot(rawData$age[sampleRows], rawData$logRenta[sampleRows],
     use = "complete.obs")

# Study the mean income by province code
provIncomes <- rawData[, .(.N, meanLogIncome = mean(logRenta, na.rm = TRUE)),
                          cod_prov]
provIncomes <- provIncomes[order(cod_prov), ]

# Store the imputed incomes
saveRDS(provIncomes, file.path(getwd(), targetDate, "province incomes.rds"))