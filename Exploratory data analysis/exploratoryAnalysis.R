# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(bit64)

# Read the raw data
train <- readRDS("Data/train.rds")
test <- readRDS("Data/test.rds")

##########################################################
# Study the univariate distribution of all train columns #
##########################################################

# Consider columns with missing values
sapply(train, function(x) any(is.na(x)))
sapply(test, function(x) any(is.na(x)))
sapply(train, function(x) is.character(x) && any(x==""))
sapply(test, function(x) is.character(x) && any(x==""))

# Missing values in target columns ind_nomina_ult1 and ind_nom_pens_ult1
# Missing test values in indrel_1mes but not in train
# Missing test values in cod_prov and renta (also missing in train)
# Empty test values in sexo, ult_fec_cli_1t, tiprel_1mes,
# conyuemp, canal_entrada, nomprov and segmento (also missing in train)

# Month column
# More train observations in more recent months
# Exactly one date as expected in test with no missing dates
table(train$fecha_dato, useNA = "ifany") 
plot(table(train$fecha_dato, useNA = "ifany"))
table(test$fecha_dato, useNA = "ifany")

# Customer id column analysis
length(unique(train$ncodpers)) # Nearly a million train customers
# Bimodal distribution with most customers present in entire period
table(train$ncodpers, useNA = "ifany")
hist(table(train$ncodpers, useNA = "ifany")) 
# No new customers in the test data
all(test$ncodpers %in% unique(train$ncodpers)) 

# Employee index analysis - Empty values are interesting
# P is mentioned in description but is coded using an S 
table(train$ind_empleado, useNA = "ifany")
table(test$ind_empleado, useNA = "ifany") # No missing data
# About 0.2% has all predictors missing and some target values too
allMissingPred <- train[ind_empleado=="",]
missingIds <- unique(train[ind_empleado=="", ncodpers])
# 309 employees that could be imputed with given information but that will
# be taken care of in the feature engineering section.
# In that section I currently guess that the training records will be 
# excluded from learning
imputeMissing <- missingIds %in% unique(train[ind_empleado!="",ncodpers])
sum(imputeMissing)
# View(train[ncodpers %in% missingIds[imputeMissing],])

# Country of residence analysis
sort(table(train$pais_residencia, useNA = "ifany"), decreasing = TRUE)
sort(table(test$pais_residencia, useNA = "ifany"), decreasing = TRUE)

# Gender analysis
table(train$sexo, useNA = "ifany")
table(test$sexo, useNA = "ifany")
test[test$sexo=="",]

# Age analysis
hist(train$age)
# This looks doubtable but there could be value in the crooked numbers
range(train$age, na.rm = TRUE)
mean(is.na(train$age)) # Most ages available
train[,length(unique(age)), by=ncodpers] # Strange stuff - 4 ages in .5 year??
train[ncodpers=="1519143", ]

# Fecha alta analysis
mean(is.na(train$fecha_alta))
hist(table(train$fecha_alta), 1e2)
# Similar year distribution
hist(year(train$fecha_alta))
hist(year(test$fecha_alta))

# New customer index analysis
table(train$ind_nuevo, useNA = "ifany")
table(test$ind_nuevo, useNA = "ifany")
# It looks like the new customer index flag could be calculated more correctly
hist(table(train[ind_nuevo==1, ncodpers]))

# Customer seniority analysis
# Can be calculated using fecha_alta? There does not seem to be a 1 on 1 
# relation that makes sense which is weird but very high correlation (96%)
range(train$antiguedad, na.rm = TRUE) # Missing values coded as -999999?
missingAnt <- train[train$antiguedad==-999999,]
hist(train$antiguedad)
hist(test$antiguedad)
hist(train$antiguedad[train$antiguedad!=-999999])
hist(test$antiguedad[test$antiguedad!=-999999])
plot(test[test$antiguedad!=-999999, year(fecha_dato)-year(fecha_alta)],
     test[test$antiguedad!=-999999, antiguedad])
cor(test[test$antiguedad!=-999999, year(fecha_dato)-year(fecha_alta)],
    test[test$antiguedad!=-999999, antiguedad])

# End of relationship id during month analysis and change date - no unexpected
# exit dates!
table(train$indrel, useNA = "ifany")
table(test$indrel, useNA = "ifany")
trainChangeRecords <- train[train$indrel==99,]
table(train$indrel==99, is.na(train$ult_fec_cli_1t))
table(test$indrel==99, is.na(test$ult_fec_cli_1t))

# Customer type at the beginning of the month analysis
# Only 1 and 3 in test, take into account in feature engineering and
# train/validation record generation!
# No added value in indrel_1mes for test!
table(train$indrel_1mes, useNA = "ifany")
table(test$indrel_1mes, useNA = "ifany")
table(train$tiprel_1mes, useNA = "ifany")
table(train[fecha_dato == "2016-05-28", tiprel_1mes], useNA = "ifany")
table(test$tiprel_1mes, useNA = "ifany")
table(train$indrel_1mes, train$tiprel_1mes, useNA="ifany")
table(test$indrel_1mes, test$tiprel_1mes, useNA="ifany")

# Residence index analysis
# True should align with Spain but some strange cases
# These strange cases can be explained by moving people!
table(train$indresi, useNA = "ifany")
table(train$indresi, train$pais_residencia=="ES", useNA = "ifany")
trainCountryDismatch <- train[ncodpers %in%
                                train[indresi!="" & xor(pais_residencia=="ES",
                                                        indresi=="S"),
                                      ncodpers], ]
table(test$indresi, useNA = "ifany")
table(test$indresi, test$pais_residencia=="ES", useNA = "ifany")
testCountryDismatch <- test[ncodpers %in%
                              test[indresi!="" & xor(pais_residencia=="ES",
                                                     indresi=="S"),
                                   ncodpers], ]

# Foreigner index analysis
table(train$indext, useNA = "ifany")
table(train$indext, train$pais_residencia=="ES", useNA = "ifany")
table(test$indext, useNA = "ifany")
table(test$indext, test$pais_residencia=="ES", useNA = "ifany")

# Spouse index analysis
table(train$conyuemp, useNA = "ifany")
table(test$conyuemp, useNA = "ifany")

# Channel used to join analysis - interesting stuff :)
table(train$canal_entrada, useNA = "ifany")
table(test$canal_entrada, useNA = "ifany")

# Deceased index analysis
table(train$indfall, useNA = "ifany")
table(test$indfall, useNA = "ifany")
# Do dead people buy new products?
# YES! Especially direct debit and current accounts??
deceasedTrain <- train[indfall=="S", ]
sapply(deceasedTrain[, -(1:24), with=FALSE], function(x){
  newIds <- diff(x)==1 & diff(deceasedTrain$ncodpers)==0 
  sum(newIds, na.rm = TRUE)
})
deceasedDD <- deceasedTrain[ncodpers %in%
                              deceasedTrain[c(FALSE, diff(ind_recibo_ult1)==1 &
                                                diff(ncodpers)==0), ncodpers], ]

# Address type analysis - no information there
table(train$tipodom, useNA = "ifany")
table(test$tipodom, useNA = "ifany")

# Province code analysis - some missing
table(train$cod_prov, useNA = "ifany")
table(test$cod_prov, useNA = "ifany")

# Province name analysis has same information as the province code analysis
table(train$nomprov, useNA = "ifany")
table(test$nomprov, useNA = "ifany")
table(train$cod_prov, train$nomprov, useNA = "ifany")
plot(as.numeric(sort(table(train$cod_prov, useNA = "ifany"))),
     as.numeric(sort(table(train$nomprov))))
cor(as.numeric(sort(table(train$cod_prov, useNA = "ifany"))),
    as.numeric(sort(table(train$nomprov))))
cor(as.numeric(sort(table(test$cod_prov, useNA = "ifany"))),
    as.numeric(sort(table(test$nomprov))))

# Activity index analysis
table(train$ind_actividad_cliente, useNA = "ifany")
table(test$ind_actividad_cliente, useNA = "ifany")

# Gross income analysis - great for feature engineering!!! Change in time!!!
hist(train$renta, 1e2)
hist(log(train$renta), 1e2)
hist(test$renta, 1e2)
hist(log(test$renta), 1e2)

# Customer segment analysis
table(train$segmento, useNA = "ifany")
table(test$segmento, useNA = "ifany")

# Target accounts analysis
# The missing target data with actual train data should better be dropped in
# my opinion since it does not reflect the test data
which(sapply(train[, -(1:24), with=FALSE], function(x) any(is.na(x))))
targetTrainMissing <- train[is.na(ind_nomina_ult1) | is.na(ind_nom_pens_ult1),]
targetTrainMissingData <- targetTrainMissing[nomprov!="", ]
hist(sapply(train[, -(1:24), with=FALSE], mean, na.rm=TRUE), 1e2)