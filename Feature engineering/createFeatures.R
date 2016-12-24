# Logic to create and store a data table with features of the studied data
# period

# Time changes in the raw data are converted to customer specific features
# The interpretation of the features should be similar for train and test
# records. Use relative measures where relevant - use absolute measures if
# this results in better cross validation scores. The test data set should
# also be considered to make decisions since it contains out of time predictions

# TODO: Add multivariate features such as relative income for region
# Multivariate feature generation will be guided by the feature importance
# assessment
# TODO: apply monotonous transformations to the features
# TODO entry channel grouping based on exploratory analysis?!

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)
library(beepr)

# Summary type (for training, validation or testing)
summaryType <- c("train", "validation", "test")[2]

# Process only part of the data?
processFraction <- FALSE

# Months before test period to be considered in train features - default 0
# Multiple months are considered!
trainBackPeriods <- 15:0 #  13:0 trainBackPeriods 0 for train === validation!
saveByMonth <- TRUE # Always TRUE going forward
maxRecordsSaveBatch <- 2e6

# Option to limit the summary period to a fixed month window
maxSummaryMonths <- c(5, Inf)[2] # Always Inf going forward
testSummaryMonths <- 3:1 # 16:1 #16:5 #16:3 #5 #17 # If not saveByMonth: consider first as lag
validationBackMonths <- 11 # 0 is default (validate May 16). 11 is Jun 15
validationSummaryMonths <- (15-validationBackMonths):1 # Default: 15:1 for May 16; 14:1 for Apr 16, ...
summaryMonthsExtension <- ifelse(is.finite(maxSummaryMonths),
                                 paste0("FixedLag", maxSummaryMonths), "")

# Mimic the train stagnant variables in test
mimicStagnantTest <- FALSE

# Exclude records without no new products from the saved features file
excludeNoNewProducts <- FALSE # THIS SHOULD ALWAYS BE FALSE GOING FORWARD
excludeString <- ""
# ifelse(summaryType != "train" | !excludeNoNewProducts, "", "TrainAll")

# Less used settings
fractionType <- c("Random", "Ordered")[2]
fractionFlag <- ifelse(processFraction, paste0("Small", fractionType), "")
trainBackPeriods <- sort(trainBackPeriods)
if(summaryType == "validation"){
  trainBackPeriods <- rep(validationBackMonths,
                          max(c(1, length(validationSummaryMonths) *
                                  saveByMonth)))
}
if(summaryType == "test"){
  trainBackPeriods <- rep(0, max(c(1, length(testSummaryMonths) * saveByMonth)))
}
nbTrainBackPeriods <- length(trainBackPeriods)
trainBackString <- ifelse(summaryType != "test",
                          paste0("Back", trainBackPeriods[nbTrainBackPeriods],
                                 "-", trainBackPeriods[1]), "")

# Target date 
targetDate <- "12-11-2016"

# Create the target folder
folderPath <- file.path(getwd(), targetDate, paste0(summaryType, fractionFlag,
                                                    excludeString,
                                                    summaryMonthsExtension))
folderPathExtension <- ifelse(saveByMonth, "", trainBackString)
validationPathExtension <- ifelse(summaryType != "validation", "",
                                  paste(" Back", validationBackMonths))
folderPath <- paste0(folderPath, folderPathExtension, validationPathExtension)
dir.create(folderPath, showWarnings = FALSE)

# Feature mapping file name
featureMapFn <- "feature mapping.rds"

# Family incomes file name
familyIncomeFn <- "family incomes.rds"

# Mean log province incomes file name
meanProvinceIncomeFn <- "province incomes.rds"

# Client available ids file names for May and June 2015
clientsMay15Fn <- "May15 clients.rds"
clientsJune15Fn <- "June15 clients.rds"

# Client ids with at least one positive flank between Jan 15 and May 16
clientsPosFlankFn <- "positive flank clients.rds"

# Target variable constants
rawDataLags <- c(1, 2, 3, 4, 5, 6, 12)
nbRawDataLags <- length(rawDataLags)

# Count the number of products as well as the number of positive and negative 
# flanks in the flanks period
flanksPeriod <- Inf

# Months back product summary periods
monthsBackTransCounts <- c(1, 2, 3, 4, 5, 6, 12) # These must appear in rawDataLags
nbMonthsBackTransCounts <- length(monthsBackTransCounts)
monthsBackProdCounts <- c(1, 2, 3, 4, 5, 6, 12)
nbMonthsBackProdCounts <- length(monthsBackProdCounts)


###################################################################

# Read in the raw cleaned data
if(summaryType=="test"){
  trainPart <- readRDS(paste0("../Data/train", fractionFlag, ".rds"))
  testPart <- readRDS(paste0("../Data/test", fractionFlag, ".rds"))
  
  # Combine the train and test records
  rawData <- rbindlist(list(trainPart, testPart), fill=TRUE)
  setkey(rawData, ncodpers)
} else{
  # Drop the last trainBackPeriod months
  rawData <- readRDS(paste0("../Data/train", fractionFlag, ".rds"))
}

# Load the estimated relative map contributions
mapContributions <- readRDS(file.path(getwd(), targetDate,
                                      "monthlyMAPContributions.rds"))

# Store the original raw data before applying modifications
rawDataOrig <- rawData

# Keep track of the number of feature records
featureRecordsCounter <- list()

# Loop over the train back periods and calculate the relevant features
for(trainBackId in 1:nbTrainBackPeriods){
  # Set the train back period (in months since the last observation)
  trainBackPeriod <- trainBackPeriods[trainBackId]
  
  # Show progress message
  cat("Processing period", trainBackId, "of", nbTrainBackPeriods, "for",
      summaryType, "@", as.character(Sys.time()), "\n")
  
  # Set the raw data to the original raw data since rawData is modified in
  # each train back iteration
  if(summaryType=="test"){
    rawData <- rawDataOrig
  } else{
    # Drop the last trainBackPeriod months
    lastMonth <- sort(unique(rawDataOrig$fecha_dato),
                      decreasing = TRUE)[trainBackPeriod+1]
    rawData <- rawDataOrig[fecha_dato <= lastMonth,]
  }
  
  # Drop customers that don't have data in the month prior to the targeted 
  # period Or in the month of the target period itself
  dataMonths <- sort(unique(rawData$fecha_dato), decreasing = TRUE)
  dataMonthsVec <- dataMonths
  rawData <- rawData[ncodpers %in% rawData[fecha_dato==dataMonths[1],
                                           ncodpers], ]
  rawData <- rawData[ncodpers %in% rawData[fecha_dato==dataMonths[2],
                                           ncodpers], ]
  
  # Restrict the raw data optionally to the last maxSummaryMonths
  if(summaryType == "train"){
    if(is.finite(maxSummaryMonths) &&
       maxSummaryMonths < (length(dataMonths)-1)){
      rawData <- rawData[fecha_dato >= dataMonths[maxSummaryMonths + 1], ]
    }
  } else{
    periodLagMonths <- ifelse(summaryType == "validation",
                              validationSummaryMonths[trainBackId],
                              testSummaryMonths[trainBackId])
    if(is.finite(periodLagMonths) && periodLagMonths < (length(dataMonths)-1)){
      rawData <- rawData[fecha_dato >= dataMonths[periodLagMonths + 1], ]
    }
    
    # Mimic the stagnant behavior for the first seven train months Jan 15 - 
    # Jul 15 in the test set
    if(mimicStagnantTest){
      rawData[, rowId := 1:nrow(rawData)]
      lastOriginalTestMonth <- dataMonths[max(c(1, periodLagMonths-5))]
      # Copy all data from lastOriginalTestMonth to all previous test records
      
      # Some test persons miss data in the last original test month =>
      # find the earliest date greater than or equal to lastOriginalTestMonth
      # for all users
      lastOriginalTestAfterCutoff <-
        rawData[fecha_dato>=lastOriginalTestMonth,
                .(rowFirstNonStagnant = min(rowId)), ncodpers]
      copyStagnantTest <-
        rawData[lastOriginalTestAfterCutoff$rowFirstNonStagnant,
                1:24, with=FALSE]
      copyStagnantNcodpers <- copyStagnantTest$ncodpers
      
      # Extract the months that need to be overwritten with the stagnant test
      # data
      overwrittenStagMonths <-
        dataMonths[dataMonths < lastOriginalTestMonth &
                     dataMonths >= min(rawData$fecha_dato)]
      nbOverwrittenMonths <- length(overwrittenStagMonths)
      for(i in 1:nbOverwrittenMonths){
        overwrittenStagMonth <- overwrittenStagMonths[i]
        overwriteData <- rawData[fecha_dato == overwrittenStagMonth,
                                 .(ncodpers, rowId)]
        matchIds <- match(overwriteData$ncodpers, copyStagnantNcodpers)
        for(copyColId in 3:24){
          copyCol <- colnames(copyStagnantTest)[copyColId]
          rawData[rowId %in% overwriteData$rowId, (copyCol) :=
                    copyStagnantTest[[copyCol]][matchIds]]
          # rawData[rowId %in% overwriteData$rowId, 3:24, with = FALSE] <-
          #   copyStagnantTest[matchIds, 3:24, with = FALSE]
        }
      }
      rawData[, rowId:=NULL]
    }
  }
  
  # Set up the save path variables
  nbDataMonths <- length(unique(rawData$fecha_dato))
  lagPeriodExtension <- paste0("Lag", nbDataMonths - 1)
  trainBackString <- ifelse(summaryType == "train",
                            paste0("Back", trainBackPeriod), "")
  if(saveByMonth){
    saveExtension <- paste0(trainBackString, lagPeriodExtension, " ")
  } else{
    saveExtension <- ifelse(summaryType != "train",
                            paste0(lagPeriodExtension, " "), "")
  }
  
  
  #############################################
  # Section 1: Add predictor related features #
  #############################################
  
  # Add an indicator to capture the fraction of months present
  # Capture the fraction of months with gaps (non subsequent clients)
  nbMonths <- length(dataMonths)
  monthsFeatures <-
    rawData[, list(dataMonths = .N,
                   gaps = sum(diff(year(fecha_dato)*12 +
                                     month(fecha_dato)) != 1)
    ), ncodpers]
  monthsFeatures[, monthsFrac := (dataMonths-2)/(nbMonths-2)]
  monthsFeatures[, gapsFrac := (gaps)/(nbMonths-2)]
  monthsFeatures[, lastDate := dataMonthsVec[1]]
  missingFrac <- rawData[ncodpers %in% monthsFeatures[monthsFrac<1, ncodpers],]
  features <- monthsFeatures
  
  # Add a flag for training data to indicate if there are any new products
  # This will be used when building the base models
  indicatorCols <- grepl("^ind.*ult1$", names(rawData))
  if(summaryType != "test"){
    secondLastMonth <- as.matrix(rawData[fecha_dato==dataMonths[2],
                                         indicatorCols, with=FALSE])
    lastMonth <- as.matrix(rawData[fecha_dato==dataMonths[1], indicatorCols,
                                   with=FALSE])
    productChange <- (lastMonth - secondLastMonth) == 1
    features$nbNewProducts <- rowSums(productChange, na.rm = TRUE)
    features[, hasNewProduct := nbNewProducts > 0]
    
    
    # Remove target columns information for non test data for the last month
    # These records are already NA for test data
    rawData[fecha_dato==dataMonths[1], names(rawData)[-(1:24)]] <- NA
  }
  
  # Add a flag which indicates if the user had data in May and/or June 2015
  May15Clients <- readRDS(file.path(getwd(), targetDate, clientsMay15Fn))
  June15Clients <- readRDS(file.path(getwd(), targetDate, clientsJune15Fn))
  features[, hasJune15Data := ncodpers %in% June15Clients]
  features[, hasMay15Data := ncodpers %in% May15Clients]
  
  # Add a flag that indicates if the user had any positive flank in the 
  # training period
  posFlankClients <- readRDS(file.path(getwd(), targetDate, clientsPosFlankFn))
  features[, hasAnyPosFlank := ncodpers %in% posFlankClients]
  
  # Add employee index features
  employeeIndexFeatures <-
    rawData[, .(employeeIndex = unique(ind_empleado[ind_empleado!=""])),
            ncodpers]
  features <- employeeIndexFeatures[features]
  
  # Add customer residence features
  residenceFeatures <-
    rawData[, .(firstCountry = unique(pais_residencia[pais_residencia!=""])[1],
                lastCountry = rev(pais_residencia[pais_residencia!=""])[1],
                nbCountries = length(unique(pais_residencia[
                  pais_residencia!=""]))), ncodpers]
  residenceFeatures$monthsSinceMove <- NA
  changeCountryData <- rawData[ncodpers %in% residenceFeatures[nbCountries>1,
                                                               ncodpers], ]
  monthsChange <-
    changeCountryData[c(FALSE,diff(changeCountryData$ncodpers)==0 & (
      changeCountryData$pais_residencia[-1] !=
        changeCountryData$pais_residencia[-nrow(changeCountryData)])),
      (year(dataMonths[1])*12 + month(dataMonths[1])) - 
        (year(fecha_dato)*12 + month(fecha_dato)), ncodpers]
  matchId <- match(monthsChange$ncodpers, residenceFeatures$ncodpers)
  residenceFeatures$monthsSinceMove[matchId] <- monthsChange[[2]]
  
  # Add country mapping to regions
  countryMap <- 
    list(ES = "ES", PT = "ES", NL = "EU", AD = "ES", IN = "AS", US = "EU",
         FR = "EU", GB = "EU", IT = "ES", DE = "EU", MX = "AM", CL = "AM",
         CO = "AM", CH = "AM", CR = "AM", PE = "AM", JP = "AS", AT = "EU",
         AR = "AM", AE = "EU", BE = "EU", MA = "AS", CI = "AS", SE = "EU",
         BR = "AM", FI = "EU", RS = "ES", KE = "AS", RU = "AS", VE = "AM",
         CU = "AM", EC = "AM", KR = "AS", DO = "AM", AU = "EU", LU = "ES",
         GH = "AS", CZ = "ES", PA = "AM", IE = "EU", BO = "AM", CM = "AS",
         CA = "EU", GR = "ES", ZA = "AS", RO = "ES", KH = "AS", IL = "EU",
         NG = "AS", CN = "AS", DK = "EU", NZ = "EU", MM = "AS", SG = "AS",
         UY = "AM", NI = "AS", EG = "AS", GI = "ES", PH = "AS", KW = "EU",
         VN = "AS", TH = "AS", NO = "EU", GQ = "AS", BY = "ES", AO = "AS",
         UA = "EU", TR = "AS", PL = "EU", GA = "AS", GE = "EU", BG = "ES",
         HR = "ES", PR = "AM", HK = "AS", HN = "AM", BA = "AM", MD = "AS",
         SK = "ES", TN = "AS", QA = "EU", TG = "AS", SA = "EU", MR = "AS",
         DZ = "AS", LB = "AS", SV = "ES", PK = "AS", PY = "AM", LY = "AS",
         MK = "ES", EE = "ES", SN = "AS", MZ = "AS", GT = "AM", GN = "AS",
         TW = "AS", IS = "EU", LT = "ES", CD = "AS", KZ = "AS", BZ = "AS",
         CF = "AS", GM = "AS", ET = "AS", SL = "AS", GW = "AS", LV = "ES",
         OM = "EU", CG = "AS", ML = "AS", HU = "ES", AL = "ES", MT = "ES",
         BM = "AS", ZW = "AS", DJ = "AS", JM = "AM",
         MISSING = "Missing"
    )
  
  # Add region flags to the residence features
  mappedCountries <- unlist(countryMap)
  regions <- sort(unique(mappedCountries))
  nbRegions <- length(regions)
  lastCountries <- residenceFeatures$lastCountry
  lastCountries[is.na(lastCountries)] <- "MISSING"
  for(i in 1:nbRegions){
    region <- regions[i]
    regionCountries <- names(mappedCountries)[mappedCountries == region]
    regionFlag <- lastCountries %in% regionCountries
    residenceFeatures$regionFlag <- regionFlag
    names(residenceFeatures)[ncol(residenceFeatures)] <- paste0("regionFlag",
                                                                region)
  }
  
  features <- residenceFeatures[features]
  
  # Add gender features
  genderFeatures <-
    rawData[, .(firstGender = sexo[sexo!=""][1],
                nbGenders = length(unique(sexo[sexo!=""]))), ncodpers]
  features <- genderFeatures[features]
  
  # Add age features
  # Some weird age jumps -> Pick last known age as age
  ageFeatures <-
    rawData[, .(lastAge = age[!is.na(age)][sum(!is.na(age))],
                count = length(unique(age[!is.na(age)]))), ncodpers]
  ageFeatures[, count:=NULL]
  features <- ageFeatures[features]
  
  # Add features related to the time at the bank
  timeAtBankFeatures <-
    rawData[, .(lastStartDateBank = fecha_alta[!is.na(fecha_alta)][
      sum(!is.na(fecha_alta))],
      count = length(unique(fecha_alta[!is.na(fecha_alta)]))), ncodpers]
  timeAtBankFeatures[, startCustMonth := month(lastStartDateBank)]
  timeAtBankFeatures[, startCustYear := year(lastStartDateBank)]
  timeAtBankFeatures[, monthsAtBank := 
                       (year(dataMonths[1])*12 + month(dataMonths[1])) -
                       (month(lastStartDateBank) + 12*year(lastStartDateBank))]
  timeAtBankFeatures[, c("count", "lastStartDateBank") := NULL]
  features <- timeAtBankFeatures[features]
  
  # Add features related to the time at the bank
  # The new customer flag aligns with a reset in antiguedad. This sometimes
  # happens for existing customers too
  # Recode -999999 antiguedad as NA
  rawData[antiguedad==-999999, antiguedad:=NA]
  seniorityFeatures <-
    suppressWarnings(
      rawData[, .(
        maxSeniority = as.double(max(antiguedad[!is.na(antiguedad)])),
        lastSeniority = antiguedad[!is.na(antiguedad)][
          sum(!is.na(antiguedad))],
        count = length(unique(antiguedad[!is.na(antiguedad)]))), ncodpers]
    )
  
  # Add the seniority density of the last raw data month
  seniorityCounts <- table(rawData[fecha_dato == dataMonths[1], antiguedad])
  seniorityDensity <- seniorityCounts/sum(seniorityCounts)
  matchId <- match(seniorityFeatures$lastSeniority, names(seniorityDensity))
  seniorityFeatures[, seniorityDensity := seniorityDensity[matchId]]
  seniorityFeatures[!is.finite(maxSeniority), maxSeniority:=NA]
  seniorityFeatures[, count:=NULL]
  features <- seniorityFeatures[features]
  
  # Quitting customers during the month analysis (related to ult_fec_cli_1t)
  table(rawData$indrel)
  quittingCustomersFeatures <- 
    suppressWarnings(
      rawData[, .(nbQuits =
                    length(unique(ult_fec_cli_1t[
                      indrel==99 & !is.na(ult_fec_cli_1t)])),
                  daysSinceLastQuit =
                    as.numeric(difftime(dataMonths[1], max(ult_fec_cli_1t[
                      indrel==99 &!is.na(ult_fec_cli_1t)]),
                      units="days"))),
              ncodpers]
    )
  quittingCustomersFeatures[!is.finite(daysSinceLastQuit),
                            daysSinceLastQuit:=NA]
  features <- quittingCustomersFeatures[features]
  
  # Generate customer types at the beginning of the month features
  # Impute missing tiprel_1mes at dataMonths [1] (none missing in test)
  rawData[tiprel_1mes == "", tiprel_1mes := NA]
  table(rawData[fecha_dato==dataMonths[1], tiprel_1mes], useNA = "ifany")
  table(rawData[, tiprel_1mes], useNA = "ifany")
  
  # Overall customer status change features
  custTypeFeatures <-
    rawData[, .(firstCustType = tiprel_1mes[1],
                lastCustType = tiprel_1mes[length(tiprel_1mes)],
                nbCustTypes = length(unique(tiprel_1mes)),
                nbCustTypesNonMissing = length(unique(tiprel_1mes[
                  !is.na(tiprel_1mes)]))), ncodpers]
  custTypeFeatures[is.na(firstCustType), firstCustType := "missing"]
  
  # Add features if there is more than 1 non missing customer status
  custTypePrevious <-
    suppressWarnings(
      rawData[ncodpers %in% custTypeFeatures[nbCustTypesNonMissing>1, ncodpers],
              .(previousCustType = unique(rev(tiprel_1mes[
                !is.na(tiprel_1mes)]))[2]
                , lastPrevDate = max(fecha_dato[
                  !is.na(tiprel_1mes) & tiprel_1mes == unique(rev(tiprel_1mes[
                    !is.na(tiprel_1mes)]))[2]])
              ),
              ncodpers]
    )
  custTypePrevious[, monthsSincePrevCustType :=
                     (year(dataMonths[1])*12 + month(dataMonths[1])) -
                     (month(lastPrevDate) + 12*year(lastPrevDate))]
  custTypePrevious[, lastPrevDate:= NULL]
  custTypeFeatures <- custTypePrevious[custTypeFeatures]
  
  # Add the months since the last missing customer type
  lastMissingDate <- rawData[is.na(tiprel_1mes),
                             .(lastMissDate = max(fecha_dato)),
                             ncodpers]
  lastMissingDate[, monthsLastCustTypeMissing := 
                    (year(dataMonths[1])*12 + month(dataMonths[1])) -
                    (month(lastMissDate) + 12*year(lastMissDate))]
  lastMissingDate[, lastMissDate:=NULL]
  custTypeFeatures <- lastMissingDate[custTypeFeatures]
  features <- custTypeFeatures[features]
  
  # Add features related to the residence index
  residentFeatures <-
    rawData[, .(lastResidentType = indresi[length(indresi)],
                countNonMissingResident = length(unique(indresi[
                  indresi!=""]))), ncodpers]
  residentStatusChange <-
    suppressWarnings(
      rawData[ncodpers %in% residentFeatures[countNonMissingResident>1, ncodpers],
              .(previousResidentType = unique(rev(indresi[indresi!=""]))[2]
                , lastPrevDate = max(fecha_dato[indresi!="" & indresi ==
                                                  unique(rev(indresi[
                                                    indresi!=""]))[2]])
              ),
              ncodpers]
    )
  residentStatusChange[, monthsResidenceChange := 
                         (year(dataMonths[1])*12 + month(dataMonths[1])) -
                         (month(lastPrevDate) + 12*year(lastPrevDate) + 1)]
  residentStatusChange[, lastPrevDate:=NULL]
  residentFeatures <- residentStatusChange[residentFeatures]
  features <- residentFeatures[features]
  
  # Add features related to the foreigner flag
  foreignFeatures <-
    rawData[, .(lastForeignerType = indext[length(indext)],
                countNonMissingForeign = length(unique(indext[
                  indext!=""]))), ncodpers]
  foreignStatusChange <-
    suppressWarnings(
      rawData[ncodpers %in% foreignFeatures[countNonMissingForeign>1, ncodpers],
              .(previousForeignType = unique(rev(indext[indext!=""]))[2]
                , lastPrevDate = max(fecha_dato[indext!="" & indext ==
                                                  unique(rev(indext[
                                                    indext!=""]))[2]])
              ),
              ncodpers]
    )
  foreignStatusChange[, monthsForeignChange := 
                        (year(dataMonths[1])*12 + month(dataMonths[1])) -
                        (month(lastPrevDate) + 12*year(lastPrevDate) + 1)]
  foreignStatusChange[, lastPrevDate:=NULL]
  foreignFeatures <- foreignStatusChange[foreignFeatures]
  features <- foreignFeatures[features]
  
  # Add spouse index related features
  table(rawData$conyuemp)
  spouseFeatures <-
    rawData[, .(lastSpouseStatus = conyuemp[conyuemp!=""][sum(conyuemp!="")],
                count = length(unique(conyuemp[conyuemp!=""]))), ncodpers]
  spouseFeatures[, count := NULL]
  features <- spouseFeatures[features]
  
  # Add entry channel related features
  table(rawData$canal_entrada, useNA = "ifany")
  entryFeatures <-
    rawData[, .(lastEntryStatus = canal_entrada[length(canal_entrada)],
                countEntryChannels = length(unique(canal_entrada)),
                countNonMissingEntryChannels = length(unique(canal_entrada[
                  canal_entrada!=""]))), ncodpers]
  entryStatusChange <-
    suppressWarnings(
      rawData[ncodpers %in% entryFeatures[countNonMissingEntryChannels>1,
                                          ncodpers],
              .(previousEntryType = unique(rev(canal_entrada[
                canal_entrada!=""]))[2]
                , lastPrevDate = max(fecha_dato[
                  canal_entrada!="" & canal_entrada == unique(rev(canal_entrada[
                    canal_entrada!=""]))[2]])
              ),
              ncodpers]
    )
  entryStatusChange[, monthsEntryChange := 
                      (year(dataMonths[1])*12 + month(dataMonths[1])) -
                      (month(lastPrevDate) + 12*year(lastPrevDate) + 1)]
  entryStatusChange[, lastPrevDate:=NULL]
  
  entryFeatures <- entryStatusChange[entryFeatures]
  
  # Add last entry status flags
  lastEntryStatus <- entryFeatures$lastEntryStatus
  entryChannels <- c("KAT", "KFC", "KHE", "KFA", "KHK", "RED", "KHL", "KAS",
                     "KAG", "KHQ", "KHM", "KHN", "KHD", "OTHER", "MISSING") 
  nbEntryChannels <- length(entryChannels)
  lastEntryStatus[lastEntryStatus == ""] <- "MISSING"
  lastEntryStatus[!lastEntryStatus %in% entryChannels] <- "OTHER"
  for(i in 1:nbEntryChannels){
    channel <- entryChannels[i]
    entryFlag <- lastEntryStatus == channel
    entryFeatures$entryFlag <- entryFlag
    names(entryFeatures)[ncol(entryFeatures)] <- paste0("entryFlag", channel)
  }
  
  features <- entryFeatures[features]
  
  # Add diseased status related features
  table(rawData$indfall)
  deceasedFeatures <-
    rawData[, .(lastDeceasedStatus = indfall[length(indfall)],
                countDeceasedStatus = length(unique(indfall[indfall!=""]))),
            ncodpers]
  deceasedStatusChange <-
    suppressWarnings(
      rawData[ncodpers %in% deceasedFeatures[countDeceasedStatus>1, ncodpers],
              .(deceasedDate = min(fecha_dato[indfall == "S"])), ncodpers]
    )
  deceasedStatusChange[, monthsDead := 
                         (year(dataMonths[1])*12 + month(dataMonths[1])) -
                         (month(deceasedDate) + 12*year(deceasedDate))]
  deceasedStatusChange[, deceasedDate:=NULL]
  deceasedFeatures[, countDeceasedStatus := NULL]
  deceasedFeatures <- deceasedStatusChange[deceasedFeatures]
  features <- deceasedFeatures[features]
  
  # Add province related features
  table(rawData$cod_prov)
  provinceFeatures <-
    rawData[, .(lastProvince = cod_prov[!is.na(cod_prov)][
      sum(!is.na(cod_prov))],
      provinceCount = length(unique(cod_prov[
        !is.na(cod_prov)]))), ncodpers]
  provinceChange <-
    suppressWarnings(
      rawData[ncodpers %in% provinceFeatures[provinceCount>1, ncodpers],
              .(previousProvince = unique(rev(cod_prov[!is.na(cod_prov)]))[2]
                , lastPrevDate = max(fecha_dato[!is.na(cod_prov) & cod_prov ==
                                                  unique(rev(cod_prov[
                                                    !is.na(cod_prov)]))[2]])
              ),
              ncodpers]
    )
  provinceChange[, monthsProvinceChange := 
                   (year(dataMonths[1])*12 + month(dataMonths[1])) -
                   (month(lastPrevDate) + 12*year(lastPrevDate) + 1)]
  provinceChange[, lastPrevDate:=NULL]
  provinceFeatures <- provinceChange[provinceFeatures]
  
  # Add last province flags
  lastProvince <- provinceFeatures$lastProvince
  keyProvinces <- c(28, 8, 41, 46, 29, 3, 15, 11, 30, 35, 50, 47, 33)
  valProvinces <- c("MADR", "BARC", "SEVI", "VALE", "MALA", "ALIC", "CORU", 
                    "CADI", "MURC", "PALM", "ZARA", "VALL", "ASTU",
                    "OTHER", "MISSING")
  
  nbProvinces <- length(keyProvinces)
  lastProvince[is.na(lastProvince)] <- -1
  lastProvince[!lastProvince %in% keyProvinces & lastProvince !=-1] <- 0
  for(i in 1:nbProvinces){
    province <- keyProvinces[i]
    provinceFlag <- lastProvince == province
    provinceFeatures$provinceFlag <- provinceFlag
    names(provinceFeatures)[ncol(provinceFeatures)] <- paste0("provinceFlag",
                                                              valProvinces[i])
  }
  provinceFeatures[[paste0("provinceFlagOTHER")]] <- lastProvince == 0
  provinceFeatures[[paste0("provinceFlagMISSING")]] <- lastProvince < 0
  
  features <- provinceFeatures[features]
  
  # Add activity related features
  # There is finally some nice diversity in this feature! No missing values!
  # This needs to be elaborated further!
  table(rawData$ind_actividad_cliente)
  activityFeatures <-
    suppressWarnings(
      rawData[, .(activeFraction = mean(ind_actividad_cliente),
                  currentActivity = ind_actividad_cliente[
                    length(ind_actividad_cliente)],
                  lastActiveMonth = max(fecha_dato[ind_actividad_cliente==1]),
                  lastInactiveMonth = max(fecha_dato[ind_actividad_cliente==0])),
              ncodpers]
    )
  activityFeatures[!is.na(lastActiveMonth), lastActiveMonths :=
                     (year(dataMonths[1])*12 + month(dataMonths[1])) -
                     (month(lastActiveMonth) + 12*year(lastActiveMonth) - 1)]
  activityFeatures[!is.na(lastInactiveMonth), lastInactiveMonths :=
                     (year(dataMonths[1])*12 + month(dataMonths[1])) -
                     (month(lastInactiveMonth) + 12*year(lastInactiveMonth) -
                        1)]
  activityFeatures[!is.na(lastInactiveMonth), lastMonthsActive :=
                     lastInactiveMonths-1]
  activityFeatures[is.na(lastMonthsActive), lastMonthsActive := 
                     17 - trainBackPeriod]
  
  activityFeatures[, lastActiveMonth := NULL]
  activityFeatures[, lastActiveMonths := NULL]
  activityFeatures[, lastInactiveMonth := NULL]
  activityFeatures[, lastInactiveMonths := NULL]
  features <- activityFeatures[features]
  
  # Household gross income related features generation
  # The income appears to be fixed over time
  incomeFeatures <-
    rawData[, .(grossIncome = renta[!is.na(renta)][1]), ncodpers]
  incomeFeatures[, logGrossIncome := log(grossIncome)]
  incomeFeatures[, missingIncome := is.na(logGrossIncome)]
  features <- incomeFeatures[features]
  
  # Inpute missing gross incomes
  provIncomes <- readRDS(file.path(getwd(), targetDate, meanProvinceIncomeFn))
  missIncomeId <- is.na(features$logGrossIncome) &
    !is.na(features$lastProvince)
  features[missIncomeId, logGrossIncome := provIncomes$meanLogIncome[
    match(features$lastProvince[missIncomeId], provIncomes$cod_prov)]]
  
  # Add segment related features
  table(rawData$segmento)
  segmentFeatures <-
    rawData[, .(lastSegment = rev(segmento[segmento!=""])[1],
                nbSegments = length(unique(segmento[segmento!=""]))), ncodpers]
  segmentStatusChange <-
    suppressWarnings(
      rawData[ncodpers %in% segmentFeatures[nbSegments>1, ncodpers],
              .(previousSegmentType = unique(rev(segmento[segmento!=""]))[2]
                , lastPrevDate = max(fecha_dato[segmento!="" & segmento ==
                                                  unique(rev(segmento[
                                                    segmento!=""]))[2]])
              ),
              ncodpers]
    )
  segmentStatusChange[, monthsSegmentChange := 
                        (year(dataMonths[1])*12 + month(dataMonths[1])) -
                        (month(lastPrevDate) + 12*year(lastPrevDate) + 1)]
  segmentStatusChange[, lastPrevDate:=NULL]
  segmentFeatures <- segmentStatusChange[segmentFeatures]
  features <- segmentFeatures[features]
  
  # Order the columns according to the original order of the data fields
  # The order was reversed for better debugging (newest columns in front)
  setcolorder(features, c(names(features)[1],
                          rev(names(features)[-1])))
  
  ###############################
  # Add family related features #
  ###############################
  
  # Load the family income file
  familyIncomes <- readRDS(file.path(getwd(), targetDate, familyIncomeFn))
  matchId <- match(features$grossIncome, familyIncomes$renta)
  matchIndex <- matchId[!is.na(matchId)]
  features$familySize <- 1
  features$familySize[!is.na(matchId)] <- familyIncomes$familySize[matchIndex]
  features$familyId <- NA
  features$familyId[!is.na(matchId)] <- familyIncomes$familyId[matchIndex]
  
  # Calculate the observed family counts and drop one person families for
  # the considered month
  observedFamCounts <-
    features[!is.na(familyId), .(.N, totalSize = familySize[1]), familyId]
  treatedFamIds <- observedFamCounts[N>1, familyId]
  
  # Family features data table generation
  featuresFamIds <- features$familyId %in% treatedFamIds
  familyNcodPers <- features[featuresFamIds, ncodpers]
  familyFeatures <- data.table(ncodpers = familyNcodPers,
                               familyId = features[featuresFamIds,
                                                   familyId])
  familyFeatures[, personAge := features[featuresFamIds, lastAge]]
  familyFeatures[, personTimeBank := features[featuresFamIds, monthsAtBank]]
  familyFeatures[, familyObsSize :=
                   observedFamCounts[match(familyFeatures$familyId,
                                           observedFamCounts$familyId), N]]
  setkey(familyFeatures, familyId)
  
  # Add family age related features
  meanFamAges <- features[featuresFamIds,
                          .(meanFamAge = mean(lastAge)), familyId]
  setkey(meanFamAges, familyId)
  familyFeatures <- familyFeatures[meanFamAges]
  familyFeatures[, meanAgeOthersFam := (meanFamAge*familyObsSize-personAge)/
                   (familyObsSize-1)]
  familyFeatures[, meanAgeDiffFamSelf := meanAgeOthersFam-personAge]
  familyFeatures[, personAge := NULL]
  familyFeatures[, meanFamAge := NULL]
  
  # Add family time at bank related features
  meanFamTimeBank <- features[featuresFamIds,
                              .(meanFamTimeBank = mean(monthsAtBank)), familyId]
  setkey(meanFamTimeBank, familyId)
  familyFeatures <- familyFeatures[meanFamTimeBank]
  familyFeatures[, meanTimeBankOthersFam :=
                   (meanFamTimeBank*familyObsSize-personTimeBank)/
                   (familyObsSize-1)]
  familyFeatures[, meanTimeBankDiffFamSelf :=
                   meanTimeBankOthersFam-personTimeBank]
  familyFeatures[, personTimeBank := NULL]
  familyFeatures[, meanFamTimeBank := NULL]
  
  
  ##################################################
  # Section 2: Add target columns related features #
  ##################################################
  
  # Extract the indicator column names
  indColNames <- names(rawData)[-(1:24)]
  nbIndCols <- length(indColNames)
  
  # # Add the relative MAP fraction features
  # for(i in 1:nbIndCols){
  #   # Extract the target column name
  #   indCol <- indColNames[i]
  #   
  #   # Add the appropriate target column
  #   monthIdRelMAP <- which(rownames(mapContributions) == features$lastDate[1])
  #   productIdRelMAP <- which(colnames(mapContributions) == indCol)
  #   features[[paste0(indCol, "RelMAP")]] <- mapContributions[monthIdRelMAP,
  #                                                            productIdRelMAP]
  #   
  #   # Add the MAP ratio versus June 2015 (reference training period for now)
  #   features[[paste0(indCol, "MAPRatioJune15")]] <-
  #     mapContributions[monthIdRelMAP, productIdRelMAP] /
  #     mapContributions[5, productIdRelMAP]
  # }
  
  # Calculate the number of features so far (used for ending up with an
  # intuitive column ordering)
  nbUnivarCols <- ncol(features) - 1
  
  
  # # Study missing target measure patterns
  # sapply(rawData[fecha_dato < dataMonths[1], -(1:24), with=FALSE],
  #        function(x) sum(is.na(x)))
  # a <- rawData[fecha_dato < dataMonths[1] & is.na(ind_nomina_ult1),]
  
  # Add lags of target columns as features
  for(i in 1:nbIndCols){
    indCol <- indColNames[i]
    for(j in 1:nbRawDataLags){
      # Show progress message
      cat("Adding target column lag", j + (i-1)*nbRawDataLags, "of", 
          nbIndCols*nbRawDataLags, "\n")
      
      # Add the appropriate lag column
      rawDataLag <- rawDataLags[j]
      if(rawDataLag < length(dataMonths)){
        lagMonth <- dataMonths[rawDataLag+1]
        
        # Speed up the bottleneck
        # lagData <- rawData[fecha_dato==lagMonth, get(indCol), ncodpers] 
        lagData <- rawData[fecha_dato==lagMonth, c("ncodpers", indCol),
                           with=FALSE]
        names(lagData)[2] <- paste0(indCol, "Lag", rawDataLag)
        features <- lagData[features]
      }
    }
  }
  
  # Count the number of products as well as the number of positive and negative 
  # flanks in the flanks period
  for(i in 1:nbIndCols){
    # Show progress message
    cat("Calculating flank features for target column", i, "of", nbIndCols,
        "\n")
    indCol <- indColNames[i]
    indColCapFirst <- paste0(toupper(substr(indCol,1,1)), substring(indCol, 2))
    
    # Calculate the number and last occurence of the target column flanks
    # lagData <- rawData[fecha_dato >= dataMonths[flanksPeriod+1], .(
    #   nbPosFlanks = sum(diff(get(indCol)[!is.na(get(indCol))]) == 1),
    #   nbNegFlanks = sum(diff(get(indCol)[!is.na(get(indCol))]) == -1),
    #   lastPosFlank = max(fecha_dato[c(FALSE, diff(get(indCol)[
    #     !is.na(get(indCol))]) == 1)]),
    #   lastNegFlank = max(fecha_dato[c(FALSE, diff(get(indCol)[
    #     !is.na(get(indCol))]) == -1)])
    # ), ncodpers]
    maxDataMonthId <- min(c(length(dataMonths), flanksPeriod+1))
    ld <- rawData[fecha_dato >= dataMonths[maxDataMonthId],
                  c("ncodpers", "fecha_dato", indCol), with=FALSE]
    names(ld)[3] <- "indCol"
    # lagData <- ld[, .(
    #   nbPosFlanks = sum(diff(indCol[!is.na(indCol)]) == 1),
    #   nbNegFlanks = sum(diff(indCol[!is.na(indCol)]) == -1)
    # lastPosFlank = rev(fecha_dato)[as.double(which(diff(rev(indCol[
    #   !is.na(rev(indCol))])) == -1)[1])],
    # lastNegFlank = rev(fecha_dato)[as.double(which(diff(rev(indCol[
    #   !is.na(rev(indCol))])) == 1)[1])]
    # lastPosFlank = max(fecha_dato[c(FALSE, diff(indCol[
    #   !is.na(indCol)]) == 1)]),
    # lastNegFlank = max(fecha_dato[c(FALSE, diff(indCol[
    #   !is.na(indCol)]) == -1)]),
    # ), ncodpers]
    
    # Positive flanks calculation
    maxLagCol <- 1 + as.numeric(i==nbIndCols)
    lagData <- rawData[fecha_dato >= dataMonths[maxDataMonthId],
                       .(nbLagRecords = .N), ncodpers][, 1:maxLagCol, with=FALSE]
    posFlankIds <- which(diff(ld$ncodpers) == 0 & diff(ld[["indCol"]]) == 1 &
                           diff(ld$fecha_dato) < 32)
    posFlankData <- suppressWarnings(
      ld[1 + posFlankIds, .(nbPosFlanks = .N,
                            lastPosFlank = max(fecha_dato)),
         ncodpers])
    lagData <- posFlankData[lagData]
    lagData[is.na(nbPosFlanks), nbPosFlanks:=0]
    
    # Negative flanks calculation
    negFlankIds <- which(diff(ld$ncodpers) == 0 & diff(ld[["indCol"]]) == -1)
    negFlankData <- suppressWarnings(
      ld[1 + negFlankIds, .(nbNegFlanks = .N,
                            lastNegFlank = max(fecha_dato)), ncodpers]
    )
    lagData <- negFlankData[lagData]
    lagData[is.na(nbNegFlanks), nbNegFlanks:=0]
    
    # Find the second last negative flank
    usersMoreThanOneNegFlank <- lagData[nbNegFlanks>1, ncodpers]
    twoNegFlankData <- suppressWarnings(
      ld[1 + negFlankIds, ][ncodpers %in% usersMoreThanOneNegFlank,
                            .(secondLastNegFlank = sort(fecha_dato)[.N - 1]),
                            ncodpers]
    )
    lagData <- twoNegFlankData[lagData]
    
    # Calculate the number of months since the last flanks
    lagData$monthsSinceLastPosFlank <- NA
    posId <- is.finite(lagData$lastPosFlank)
    lagData$monthsSinceLastPosFlank[posId] <-
      (year(dataMonths[1])*12 + month(dataMonths[1])) - 
      (year(lagData$lastPosFlank[posId])*12 +
         month(lagData$lastPosFlank[posId]))
    lagData$monthsSinceLastNegFlank <- NA
    negId <- is.finite(lagData$lastNegFlank)
    lagData$monthsSinceLastNegFlank[negId] <-
      (year(dataMonths[1])*12 + month(dataMonths[1])) - 
      (year(lagData$lastNegFlank[negId])*12 +
         month(lagData$lastNegFlank[negId]))
    lagData$monthsSinceSecondLastNegFlank <- NA
    negId <- is.finite(lagData$secondLastNegFlank)
    lagData$monthsSinceSecondLastNegFlank[negId] <-
      (year(dataMonths[1])*12 + month(dataMonths[1])) - 
      (year(lagData$secondLastNegFlank[negId])*12 +
         month(lagData$secondLastNegFlank[negId]))
    
    # Rename the lag data columns and remove obsolete columns
    lagData[, c("lastPosFlank", "lastNegFlank", "secondLastNegFlank") := NULL]
    replaceIds <- c(1, which(grepl("nbLagRecords", names(lagData))))
    names(lagData)[-replaceIds] <- paste0(names(lagData)[-replaceIds],
                                          indColCapFirst)
    
    # Merge with existing features
    features <- lagData[features]
  }
  
  # Calculate the number of months since the last positive or negative flank
  lastPosFlankFeat <- features[, grepl("monthsSinceLastPosFlank",
                                       names(features)), with=FALSE]
  lastPosFlankFeat[, monthsSinceLastPosFlank :=
                     do.call(pmin, c(.SD, list(na.rm=TRUE)))]
  lastNegFlankFeat <- features[, grepl("monthsSinceLastNegFlank",
                                       names(features)), with=FALSE]
  lastNegFlankFeat[, monthsSinceLastNegFlank :=
                     do.call(pmin, c(.SD, list(na.rm=TRUE)))]
  features$monthsSinceLastPosFlank <- lastPosFlankFeat$monthsSinceLastPosFlank
  features$monthsSinceLastNegFlank <- lastNegFlankFeat$monthsSinceLastNegFlank
  
  # Order the columns according to the original order of the data fields
  # The order was reversed for better debugging (newest columns in front)
  excludeColIds <- c(1, ncol(features) + seq(-(nbUnivarCols+1), 0))
  setcolorder(features, c(names(features)[excludeColIds],
                          rev(names(features)[-excludeColIds])))
  
  # Add the combined number of positive and negative transition flanks in the
  # past transition flanks period
  posFlanksData <- features[, grepl("nbPosFlanks", names(features)),
                            with=FALSE]
  hist(sapply(posFlanksData, sum, na.rm=TRUE)/nrow(rawData), 1e2)
  features$nbPosFlanks <- rowSums(posFlanksData, na.rm=TRUE)
  
  negFlanksData <- features[, grepl("nbNegFlanks", names(features)),
                            with=FALSE]
  # hist(sapply(negFlanksData, sum, na.rm=TRUE)/nrow(rawData), 1e2)
  features$nbNegFlanks <- rowSums(negFlanksData, na.rm=TRUE)
  
  # Add the total number of transition counts for all monthsBackTransCounts
  for(i in 1:nbMonthsBackTransCounts){
    monthsBackTransCount <- monthsBackTransCounts[i]
    targetNcodPers <-
      intersect(features$ncodpers, rawData[
        fecha_dato==dataMonths[2 + monthsBackTransCount], ncodpers])
    if(length(targetNcodPers) == 0){
      prevMonth <- matrix(NA, nrow=0, ncol=0)
    } else{
      prevMonth <- data.matrix(rawData[ncodpers %in% targetNcodPers &
                                         fecha_dato==dataMonths[
                                           2 + monthsBackTransCount],
                                       indColNames, with=FALSE])
      nextMonth <- data.matrix(features[ncodpers %in% targetNcodPers,
                                        paste0(indColNames, "Lag",
                                               monthsBackTransCount), 
                                        with=FALSE])
    }
    
    # Set the flanks to an empty vector if there is no data available
    if(nrow(prevMonth) == 0){
      positiveFlankCounts <- logical(0)
      negativeFlankCounts <- logical(0)
    } else{
      positiveFlankCounts <- rowSums(prevMonth==0 & nextMonth==1, na.rm=TRUE)
      negativeFlankCounts <- rowSums(prevMonth==1 & nextMonth==0, na.rm=TRUE)
    }
    
    # Combine results to a data table
    monthsBackCounts <- data.table(ncodpers = targetNcodPers,
                                   positiveFlankCounts = positiveFlankCounts,
                                   negativeFlankCounts = negativeFlankCounts)
    names(monthsBackCounts)[-1] <- paste0(names(monthsBackCounts)[-1],
                                          "Back", monthsBackTransCount)
    
    # Add NA rows for clients that don't have the required data available
    # so that we can preserve the desired column order by a left join
    padNa <- setdiff(features$ncodpers, targetNcodPers)
    monthsBackCounts <- rbindlist(list(monthsBackCounts, 
                                       data.table(ncodpers = padNa)), fill=TRUE)
    setkey(monthsBackCounts, ncodpers)
    
    # Merge with existing features
    features <- features[monthsBackCounts]
  }
  
  # Add the total product counts for all monthsBackProdCounts
  for(i in 1:nbMonthsBackProdCounts){
    monthsBackProdCount <- monthsBackProdCounts[i]
    targetNcodPers <-
      intersect(features$ncodpers, rawData[
        fecha_dato==dataMonths[1 + monthsBackProdCount], ncodpers])
    
    prevMonth <- data.matrix(rawData[ncodpers %in% targetNcodPers &
                                       fecha_dato==dataMonths[
                                         1 + monthsBackProdCount],
                                     indColNames, with=FALSE])
    productCounts <- rowSums(prevMonth, na.rm=TRUE)
    
    # Combine results to a data table
    monthsBackCounts <- data.table(ncodpers = targetNcodPers,
                                   productCounts = productCounts)
    names(monthsBackCounts)[-1] <- paste0(names(monthsBackCounts)[-1],
                                          "Back", monthsBackProdCount)
    
    # Add NA rows for clients that don't have the required data available
    # so that we can preserve the desired column order by a left join
    padNa <- setdiff(features$ncodpers, targetNcodPers)
    monthsBackCounts <- rbindlist(list(monthsBackCounts, 
                                       data.table(ncodpers = padNa)), fill=TRUE)
    setkey(monthsBackCounts, ncodpers)
    
    # Merge with existing features
    features <- features[monthsBackCounts]
  }
  
  # Check which clients that had data 12 months ago
  targetNcodPers <- intersect(intersect(features$ncodpers,
                                        rawData[fecha_dato==dataMonths[13],
                                                ncodpers]),
                              rawData[fecha_dato==dataMonths[14], ncodpers])
  
  # Add a boolean flag to indicate if there was a positive transition for each 
  # of the target variable 12 months back
  for(i in 1:nbIndCols){
    # Extract the target column name
    indCol <- indColNames[i]
    indColCapFirst <- paste0(toupper(substr(indCol,1,1)), substring(indCol, 2))
    
    # Add the appropriate target column
    prevMonth <- rawDataOrig[fecha_dato == dataMonths[14] &
                               ncodpers %in% targetNcodPers, indCol,
                             with=FALSE][[1]]
    nextMonth <- rawDataOrig[fecha_dato == dataMonths[13] &
                               ncodpers %in% targetNcodPers, indCol,
                             with=FALSE][[1]]
    
    # Combine results to a data table
    yearBackTransition <-
      data.table(ncodpers = targetNcodPers, positiveFlankYearBack =
                   as.numeric((prevMonth == 0 & nextMonth==1)),
                 negativeFlankYearBack =
                   as.numeric(prevMonth == 1 & nextMonth==0))
    names(yearBackTransition)[-1] <- paste0(names(yearBackTransition)[-1],
                                            indColCapFirst)
    
    # Add NA rows for clients that don't have the required data available
    # so that we can preserve the desired column order by a left join
    padNa <- setdiff(features$ncodpers, targetNcodPers)
    yearBackTransition <- rbindlist(list(yearBackTransition, 
                                         data.table(ncodpers = padNa)), fill=TRUE)
    setkey(yearBackTransition, ncodpers)
    
    # Merge with existing features
    features <- features[yearBackTransition]
  }
  
  # Loop over all target lags and add family product related features
  featureFamMatchIds <- match(familyFeatures$ncodpers, features$ncodpers)
  for(i in 1:nbIndCols){
    # Extract the target column name
    indCol <- indColNames[i]
    indColCapFirst <- paste0(toupper(substr(indCol,1,1)), substring(indCol, 2))
    
    # Add the mean product number of positive flanks for the other family
    # members
    featuresTarget <- features[featuresFamIds, c("familyId",
                                                 paste0(c("nbPosFlanks",
                                                          "nbNegFlanks"),
                                                        indColCapFirst),
                                                 paste0(indCol, "Lag1")),
                               with=FALSE]
    names(featuresTarget)[-1] <- c("nbPosFlanks", "nbNegFlanks", "lag1")
    meanFamFlanks <- featuresTarget[, .(meanFamPF = mean(nbPosFlanks),
                                        meanFamNF = mean(nbNegFlanks),
                                        meanLag1 = mean(lag1)),
                                    familyId]
    setkey(meanFamFlanks, familyId)
    familyFeatures <- familyFeatures[meanFamFlanks]
    posFlankProdFamUser <- features[featureFamMatchIds, paste0("nbPosFlanks",
                                                               indColCapFirst),
                                    with=FALSE]
    negFlankProdFamUser <- features[featureFamMatchIds, paste0("nbNegFlanks",
                                                               indColCapFirst),
                                    with=FALSE]
    lag1ProdFamUser <- features[featureFamMatchIds, paste0(indCol, "Lag1"),
                                with=FALSE]
    familyFeatures[, meanPosFlankOthersFam :=
                     (meanFamPF*familyObsSize-posFlankProdFamUser)/
                     (familyObsSize-1)]
    familyFeatures[, meanNegFlankOthersFam :=
                     (meanFamNF*familyObsSize-negFlankProdFamUser)/
                     (familyObsSize-1)]
    familyFeatures[, meanLag1OthersFam :=
                     (meanLag1*familyObsSize-lag1ProdFamUser)/
                     (familyObsSize-1)]
    nbFamFeatures <- ncol(familyFeatures)
    names(familyFeatures)[(nbFamFeatures-2):nbFamFeatures] <-
      c(paste0("mean", c("NbPosFlanks", "NbNegFlanks"), "FamOthers",
               indColCapFirst), paste0("meanLag1FamOthers", indColCapFirst))
    
    familyFeatures[, meanFamPF := NULL]
    familyFeatures[, meanFamNF := NULL]
    familyFeatures[, meanLag1 := NULL]
  }
  
  # Add the mean total number of positive flanks for the other family members
  featuresTarget <- features[featuresFamIds, c("familyId",
                                               "nbPosFlanks",
                                               "nbNegFlanks",
                                               "positiveFlankCountsBack1",
                                               "negativeFlankCountsBack1",
                                               "productCountsBack1"
  ),
  with=FALSE]
  meanFamFlanks <-
    featuresTarget[, .(meanFamPF = mean(nbPosFlanks),
                       meanFamNF = mean(nbNegFlanks),
                       meanPosFlankCountLag1 = mean(positiveFlankCountsBack1),
                       meanNegFlankCountLag1 = mean(negativeFlankCountsBack1),
                       meanCountLag1 = mean(productCountsBack1)),
                   familyId]
  setkey(meanFamFlanks, familyId)
  familyFeatures <- familyFeatures[meanFamFlanks]
  posFlankFamUser <- features[featureFamMatchIds, "nbPosFlanks", with=FALSE]
  negFlankFamUser <- features[featureFamMatchIds, "nbNegFlanks", with=FALSE]
  lag1PosFlanksCountUser <-
    features[featureFamMatchIds, "positiveFlankCountsBack1", with=FALSE]
  lag1NegFlanksCountUser <-
    features[featureFamMatchIds, "negativeFlankCountsBack1", with=FALSE]
  lag1CountFamUser <- features[featureFamMatchIds, "productCountsBack1",
                               with=FALSE]
  familyFeatures[, meanNbPosFlankOthersFam :=
                   (meanFamPF*familyObsSize-posFlankFamUser)/ (familyObsSize-1)]
  familyFeatures[, meanNbNegFlankOthersFam :=
                   (meanFamNF*familyObsSize-negFlankFamUser)/ (familyObsSize-1)]
  familyFeatures[, meanLag1PosFlankCountsOthersFam :=
                   (meanPosFlankCountLag1*familyObsSize-lag1PosFlanksCountUser)/
                   (familyObsSize-1)]
  familyFeatures[, meanLag1NegFlankCountsOthersFam :=
                   (meanNegFlankCountLag1*familyObsSize-lag1NegFlanksCountUser)/
                   (familyObsSize-1)]
  familyFeatures[, meanLag1CountOthersFam :=
                   (meanCountLag1*familyObsSize-lag1CountFamUser)/
                   (familyObsSize-1)]
  
  # Remove temporary column names
  familyFeatures[, meanFamPF := NULL]
  familyFeatures[, meanFamNF := NULL]
  familyFeatures[, meanPosFlankCountLag1 := NULL]
  familyFeatures[, meanNegFlankCountLag1 := NULL]
  familyFeatures[, meanCountLag1 := NULL]
  
  # Add the family features to the end of features
  familyFeatures[, familyId := NULL]
  familyFeatures[, familyObsSize := NULL]
  setkey(familyFeatures, ncodpers)
  nbFamFeatures <- ncol(familyFeatures)-1 
  features <- familyFeatures[features]
  
  # Order the columns according to the original order of the data fields
  # The order was reversed for better debugging (newest columns in front)
  setcolorder(features, c(names(features)[1],
                          names(features)[(nbFamFeatures+2):ncol(features)],
                          names(features)[2:(nbFamFeatures+1)]))
  
  
  #################################################
  # Section 3: Add multivariate features          #
  # For a later stage of the competition pipeline #
  #################################################
  
  
  #########################################
  # Section 4: Add target feature columns #
  #########################################
  
  # Add target columns information
  for(i in 1:nbIndCols){
    # Extract the target column name
    indCol <- indColNames[i]
    
    # Add the appropriate target column
    targetData <- rawDataOrig[fecha_dato == dataMonths[1],
                              c("ncodpers", indCol), with=FALSE]
    setkey(targetData, ncodpers)
    targetData <- targetData[ncodpers %in% features$ncodpers, ]
    features <- features[targetData]
  }
  
  # # Inspect the number of new products and make sure they agree with the
  # # new products flag
  # posFlanksCount <- apply(features, 1, function(x){
  #   sum(x[names(f)[324:347]] == "1" &
  #         x[paste0(names(f)[324:347], "Lag1")] == "0")
  # })
  # table(features$hasNewProduct, posFlanksCount)
  
  ##################################################################
  # Section 5: Convert categorical to numeric features             #
  # Make sure to use identical dictionaries for train and test!!!! #
  # This is enforced by using a lookup of the possible values      #
  # generated by a separate script (XXX.R)                         #
  ##################################################################
  
  # Load the feature mapping file
  mapping <- readRDS(file.path(getwd(), targetDate, featureMapFn))
  nbMappedFeatures <- length(mapping)
  
  # # Store the features before converting them to their numeric representation
  # featuresCat <- features
  
  # Loop over all mapped features and convert the columns from categorical to
  # numerical
  for(i in 1:nbMappedFeatures){
    mappedCol <- names(mapping)[i]
    mappedVals <- rep(NA, nrow(features))
    keys <- mapping[[i]]$keys
    values <- mapping[[i]]$values
    nbKeys <- length(keys)
    
    # Loop over all keys and write the mapped values for the matched ids
    for(j in 1:nbKeys){
      matchIds <- features[[mappedCol]]==keys[j]
      mappedVals[matchIds] <- values[j]
    }
    
    features[[mappedCol]] <- mappedVals
  }
  
  # Verify that the generated features are unique for each client
  personCount <- features[, .N, ncodpers]
  if(max(personCount$N)>1) browser()
  
  # Save all features if there is only one month to be processed
  if(nbTrainBackPeriods==1 || saveByMonth){
    if(summaryType != "test"){
      features <- features[hasNewProduct == TRUE | !excludeNoNewProducts, ]
      # featuresCat <- featuresCat[hasNewProduct | !excludeNoNewProducts == TRUE, ]
    }
    saveRDS(features, file.path(folderPath, paste0(saveExtension,
                                                   "features.rds")))
    # saveRDS(featuresCat, file.path(folderPath, "featuresCat.rds"))
  } else{
    # Increment the feature records counter
    features <- features[hasNewProduct == TRUE | !excludeNoNewProducts, ]
    featureRecordsCounter <- c(featureRecordsCounter, list(nrow(features)))
    
    # Store the features for the generated month
    saveRDS(features,
            file.path(folderPath, paste0("month", trainBackId, ".rds")))
    # saveRDS(featuresCat[hasNewProduct == TRUE | !excludeNoNewProducts, ],
    # file.path(folderPath, paste0("month", trainBackId, "Cat.rds")))
    
    # Clear workspace of variables generated in the loop
    rm(list=setdiff(ls(),
                    c("summaryType",
                      "processFraction",
                      "fractionType",
                      "fractionFlag",
                      "trainBackPeriods",
                      "saveByMonth",
                      "maxSummaryMonths",
                      "testSummaryMonths",
                      "excludeNoNewProducts",
                      "nbTrainBackPeriods",
                      "trainBackString",
                      "targetDate",
                      "featureMapFn",
                      "familyIncomeFn",
                      "meanProvinceIncomeFn",
                      "clientsMay15Fn",
                      "clientsJune15Fn",
                      "clientsPosFlankFn",
                      "rawDataLags",
                      "nbRawDataLags",
                      "flanksPeriod",
                      "monthsBackTransCounts",
                      "nbMonthsBackTransCounts",
                      "monthsBackProdCounts",
                      "nbMonthsBackProdCounts",
                      "mapContributions",
                      "rawDataOrig",
                      "folderPath",
                      "featureRecordsCounter",
                      "maxRecordsSaveBatch")))
    gc()
  }
}


##############################################################################
# Section 6: Generate output before and after conversion to numeric features #
##############################################################################

# # Temp logic to combine monthly files without choking on memory
# featureRecordsCounter <- list()
# for(trainBackId in 1:nbTrainBackPeriods){
#   features <- readRDS(file.path(folderPath,
#                                 paste0("month", trainBackId, ".rds")))
#   featureRecordsCounter <- c(featureRecordsCounter, list(nrow(features)))
# }

# Combine the monthly features in the target date folder
if(nbTrainBackPeriods>1 && !saveByMonth){
  totalRecordsCount <- sum(unlist(featureRecordsCounter))
  nbBatches <- ceiling(totalRecordsCount/maxRecordsSaveBatch)
  if(nbBatches>1){
    batchIds <- list()
    for(trainBackId in 1:nbTrainBackPeriods){
      nbFeatures <- featureRecordsCounter[[trainBackId]]
      foldIds <- vector(mode = "list", length = nbBatches)
      folds <- sample(cut(seq(1, nbFeatures), breaks = nbBatches,
                          labels = FALSE))
      for(j in 1:nbBatches){
        foldIds[[j]] <- which(folds==j)
      }
      batchIds <- c(batchIds, list(foldIds))
    }
  }
  
  # Loop over all batches and generate the feature files
  for(batch in 1:nbBatches){
    allFeatures <- NULL
    
    # Show progress message
    cat("\nProcessing batch", batch, "of", nbBatches, "@",
        as.character(Sys.time()), "\n")
    
    # Calculate the save extension for the considered batch
    if(nbBatches==1){
      saveExtensionBatch <- "features.rds"
    } else{
      saveExtensionBatch <- paste("Batch", batch, "features.rds")
    }
    
    
    # allCatFeatures <- NULL
    
    # dropPredictors <- NULL
    # dropPredictors <- c(
    #   paste0(targetVars, "MAPRatioJune15"), # Used for tuning conditional predictions
    #   paste0(targetVars, "RelMAP") # Used for tuning conditional predictions
    # )
    
    for(trainBackId in 1:nbTrainBackPeriods){
      # Show progress message
      cat("Combining features", trainBackId, "of", nbTrainBackPeriods, "@",
          as.character(Sys.time()), "\n")
      
      features <- readRDS(file.path(folderPath,
                                    paste0("month", trainBackId, ".rds")))
      
      # Subset the features if there is more than one batch
      if(nbBatches>1){
        features <- features[batchIds[[trainBackId]][[batch]], ]
      }
      
      # features <- features[,!names(features) %in% dropPredictors, with=FALSE]
      # featuresCat <- readRDS(file.path(folderPath,
      # paste0("month", trainBackId, "Cat.rds")))
      
      allFeatures <- rbindlist(list(allFeatures, features[
        hasNewProduct == TRUE | !excludeNoNewProducts]), fill=TRUE)
      # allCatFeatures <- rbindlist(list(allCatFeatures, featuresCat), fill=TRUE)
      rm(features)
      # rm(featuresCat)
      gc()
    }
    saveRDS(allFeatures, file.path(folderPath, saveExtensionBatch))
    # saveRDS(allCatFeatures, file.path(folderPath, "featuresCat.rds"))
  }
}

# Play a positive sound to celebrate the successful feature generation
beep("fanfare")