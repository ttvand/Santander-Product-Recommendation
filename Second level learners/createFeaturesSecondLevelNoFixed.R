# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(xgboost)

# Target date
targetDate <- "12-11-2016"

# Summary type (for training or testing)
summaryType <- c("train", "test", "testNoStagnantRemoval")[3]

# Base model folder
baseModelsFolder <- "trainTrainAll Top 100 monthProduct 200 rounds"

# Save folder
saveFolder <- paste(summaryType, "Top 100 monthProduct 200 rounds")

# Specify the processed month back second level feature files
processedMonthBacks <- 15:0 # 11:0
nbProcessedMonths <- length(processedMonthBacks)

# File name of the stacking folds
stackingIdsFn <- "first level ncodpers 10 folds.rds"

# List the base products and target variables (id 0 === hasNewProduct)
baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
allTargetVars <- paste0("ind_", baseProducts, "_ult1")
nbAllTargetVars <- length(allTargetVars)
targetVars <- allTargetVars #[23:24]
nbTargetVars <- length(targetVars)

# Overwrite existing feature files?
overwrite <- TRUE


#####################################################################

# List the feature files in the full lag train model folders
featuresPathFullLag <- file.path(getwd(), "Feature engineering", targetDate, 
                                 summaryType)
featureFilesFullLag <- list.files(featuresPathFullLag)
featureFilesFullLag <- featureFilesFullLag[!grepl("BU", featureFilesFullLag)]
trainFnBasesFullLag <- gsub(" features.rds$", "", featureFilesFullLag)
trainFeaturePathsFullLag <- paste(featuresPathFullLag,
                                  featureFilesFullLag, sep="/")
if(summaryType == "train"){
  monthsBackFullLag <- as.numeric(gsub("Back|Lag.*$", "", featureFilesFullLag))
} else{
  testBackFullLag <- as.numeric(gsub("Lag| features.*$", "",
                                     featureFilesFullLag))
}

# List the path to the base model folders
modelsPath <- file.path(getwd(), "First level learners", targetDate, 
                        baseModelsFolder)
modelFolders <- list.dirs(modelsPath)
modelFolders <- modelFolders[modelFolders!=modelsPath]
modelFoldersExtension <- sapply(modelFolders,
                                function(x) substring(x, nchar(modelsPath)+2))
monthsBackModelFolders <- as.numeric(gsub("Back|Lag.*$", "",
                                          modelFoldersExtension))

# List the base features folder for the stacked data
baseStackedFeaturesFolder <- file.path(getwd(), "Second level learners",
                                       "Features", targetDate,
                                       saveFolder)
dir.create(baseStackedFeaturesFolder, showWarnings = FALSE)

# Load the stacking fold ids
stackingFoldsPath <- file.path(getwd(), "Second level learners", targetDate,
                               stackingIdsFn)
stackingFolds <- readRDS(stackingFoldsPath)
K <- length(stackingFolds)

# Loop over all the processed months and compose the stacked feature files
for(monthId in 1:nbProcessedMonths){
  # Extract the months back for the processed batch
  monthsBack <- processedMonthBacks[monthId]
  
  # Display progress message
  cat("\nCreating stacked features for month back", monthsBack, "-", 
      monthId, "of", nbProcessedMonths , "@", as.character(Sys.time()), "\n\n")
  
  # Set up the save path for the considered months back features file
  if(summaryType == "train"){
    savePath <- file.path(baseStackedFeaturesFolder,
                          paste0("Back", monthsBack, "Lag", 16-monthsBack,
                                 ".rds"))
  } else{
    testMonthLag <- 16 - monthsBack
    savePath <- file.path(baseStackedFeaturesFolder, paste0("Lag", testMonthLag,
                                                            ".rds"))
  }
  
  # Generate and store the features file if overwrite is TRUE or the file 
  # does not exist yet
  if(overwrite || !file.exists(savePath)){
    # Extract the ids of the fixed and full lag features as well as the id
    # of the processed months
    if(summaryType == "train"){
      fullLagId <- match(monthsBack, monthsBackFullLag)
    } else{
      fullLagId <- match(testMonthLag, testBackFullLag)
    }
    
    # Extract the full lag features
    fullLagFeatures <- readRDS(trainFeaturePathsFullLag[fullLagId])
    setkey(fullLagFeatures, ncodpers)
    
    # Extract the full lag ncodpers
    fullNcodpers <- fullLagFeatures$ncodpers
    
    # Reserve space for the second level features file
    secondLevelFeatures <- list(ncodpers = fullNcodpers)
    
    # Load the base fold models
    modelPathId <- match(monthsBack, monthsBackModelFolders)
    modelFolder <- modelFolders[modelPathId]
    modelFolderFiles <- list.files(modelFolder)
    modelFolderFoldIds <- grepl("Fold", modelFolderFiles)
    modelFolderFilesNoFold <- modelFolderFiles[!modelFolderFoldIds]
    modelFolderFiles <- modelFolderFiles[modelFolderFoldIds]
    modelFolderFilesFold <- as.numeric(gsub(".*Fold | -.*$", "",
                                            modelFolderFiles))
    
    for(j in 1:nbTargetVars){
      # Extract the target variable
      targetVar <- targetVars[j]
      predictions <- rep(NA, nrow(fullLagFeatures))
      
      # Calculate the ids of the users that owned the product in the
      # previous month
      prevOwned <- (summaryType == "train" &
                      is.na(fullLagFeatures[[targetVar]])) |
        (is.na(fullLagFeatures[[paste0(targetVar, "Lag1")]])) |
        (fullLagFeatures[[paste0(targetVar, "Lag1")]] == 1)
      
      if(summaryType == "train"){
        # Extract the target variable folds
        targetModelMatches <- grepl(targetVar, modelFolderFiles)
        modelFolderFilesTarget <- modelFolderFiles[targetModelMatches]
        modelFolderFilesFoldTarget <-
          modelFolderFilesFold[targetModelMatches]
        for(fold in 1:K){
          # Load the appropriate base fold model
          modelFolderFilesTargetId <- match(fold,
                                            modelFolderFilesFoldTarget)
          baseModel <- readRDS(file.path(modelFolder,
                                         modelFolderFilesTarget[
                                           modelFolderFilesTargetId]))
          
          # Make sure that the base model relates to the target variable
          if(baseModel$targetVar != targetVar) browser()
          
          # Fit the model to the fixed lag features fold ids that are not
          # previously owned
          fitFoldIds <- !prevOwned & (!is.na(match(fullNcodpers,
                                                   stackingFolds[[fold]])))
          foldFeatures <- data.matrix(
            fullLagFeatures[fitFoldIds, baseModel$predictors,
                             with=FALSE])
          foldPredictions <- predict(baseModel$model, foldFeatures,
                                     missing=NA)
          predictions[fitFoldIds] <- foldPredictions
        }
      } else{
        # Extract the target variable model match
        targetModelMatch <- grepl(targetVar, modelFolderFilesNoFold)
        
        # Load the base model
        baseModel <- readRDS(file.path(modelFolder,
                                       modelFolderFilesNoFold[
                                         targetModelMatch]))
        
        # Make sure that the base model relates to the target variable
        if(baseModel$targetVar != targetVar) browser()
        
        # Fit the model to the fixed lag features that are not previously
        # owned
        modelFeatures <- data.matrix(
          fullLagFeatures[!prevOwned, baseModel$predictors, with=FALSE])
        predictions[!prevOwned] <- predict(baseModel$model, modelFeatures,
                                           missing=NA)
      }
      
      # Append the base model predictions to the second level features file
      baseModelPredName <- paste0(targetVar, "Back", monthsBack)
      secondLevelFeatures[[baseModelPredName]] <- predictions
    }
    
    # Delete the fixed lag features and other massive loop variables
    suppressWarnings(rm(foldFeatures))
    gc()
    
    # Combine the second level features to a keyed data table
    # secondLevelFeatures <- data.table(do.call(cbind, secondLevelFeatures))
    secondLevelFeatures <- setDT(secondLevelFeatures)
    setkey(secondLevelFeatures, ncodpers)
    
    # Combine the full lag features with the second level features
    if(nrow(secondLevelFeatures) != nrow(fullLagFeatures)) browser()
    fullLagFeatures <- secondLevelFeatures[fullLagFeatures]
    
    # Store the second level features
    saveRDS(fullLagFeatures, savePath)
  }
}
