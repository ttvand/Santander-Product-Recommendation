# Study the feature importance of a set of xgboost models

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)

# Target date
targetDate <- "12-11-2016"

# Target model group
modelGroup <- "trainFixedLag5TrainAll"

# Consider the topNFeatures
topNFeatures <- 10


##########################################################################

# List the available model groups
modelGroupsFolder <- file.path(getwd(), "Second level learners", "Models",
                               targetDate, modelGroup)
modelGroups <- list.dirs(modelGroupsFolder)[-1]
nbModelGroups <- length(modelGroups)
if(nbModelGroups==0){
  modelGroups <- modelGroupsFolder
  nbModelGroups <- 1
}

# Loop over the model groups and add feature importance information
featureImportance <- NULL
for(i in 1:nbModelGroups){
  # List the files in the considered model group
  modelGroup <- modelGroups[i]
  slashPositions <- gregexpr("\\/", modelGroup)[[1]]
  modelGroupExtension <- substring(modelGroup,
                                   1 + slashPositions[length(slashPositions)])
  modelGroupFiles <- list.files(modelGroup)
  nbModels <- length(modelGroupFiles)
  
  monthsBack <- as.numeric(substring(gsub("Lag.*$", "", modelGroupExtension),
                                     5))
  lag <- as.numeric(gsub("^.*Lag", "", modelGroupExtension))
  
  # Loop over all models
  for(j in 1:nbModels){
    modelInfo <- readRDS(file.path(modelGroup, modelGroupFiles[j]))
    importanceMatrix <- modelInfo$importanceMatrix
    featureImportance <- rbind(featureImportance,
                               data.table(
                                 modelGroupExtension = modelGroupExtension,
                                 targetVar = modelInfo$targetVar,
                                 foldPredRatio = modelInfo$foldPredRatio,
                                 rank = 1:nrow(importanceMatrix),
                                 monthsBack = monthsBack,
                                 lag = lag,
                                 feature = importanceMatrix$Feature)
    )
  }
}

# Study the top features for different products over time
topFeaturesOverall <- featureImportance[targetVar == "hasNewProduct" &
                                          rank <= topNFeatures]
topFeaturesOverall <- topFeaturesOverall[order(-monthsBack)]
topFeaturesCco_fin <- featureImportance[targetVar == "ind_cco_fin_ult1" &
                                          rank <= topNFeatures]
topFeaturesCco_fin <- topFeaturesCco_fin[order(-monthsBack)]
topFeaturesCno_fin <- featureImportance[targetVar == "ind_cno_fin_ult1" &
                                          rank <= topNFeatures]
topFeaturesCno_fin <- topFeaturesCno_fin[order(-monthsBack)]
topFeaturesCtma_fin <- featureImportance[targetVar == "ind_ctma_fin_ult1" &
                                           rank <= topNFeatures]
topFeaturesCtma_fin <- topFeaturesCtma_fin[order(-monthsBack)]
topFeaturesEcue_fin <- featureImportance[targetVar == "ind_ecue_fin_ult1" &
                                           rank <= topNFeatures]
topFeaturesEcue_fin <- topFeaturesEcue_fin[order(-monthsBack)]
topFeaturesReca_fin <- featureImportance[targetVar == "ind_reca_fin_ult1" &
                                           rank <= topNFeatures]
topFeaturesReca_fin <- topFeaturesReca_fin[order(-monthsBack)]
topFeaturesTjcr_fin <- featureImportance[targetVar == "ind_tjcr_fin_ult1" &
                                           rank <= topNFeatures]
topFeaturesTjcr_fin <- topFeaturesTjcr_fin[order(-monthsBack)]
topFeaturesNomina <- featureImportance[targetVar == "ind_nomina_ult1" &
                                         rank <= topNFeatures]
topFeaturesNomina <- topFeaturesNomina[order(-monthsBack)]
topFeaturesNomPens <- featureImportance[targetVar == "ind_nom_pens_ult1" &
                                          rank <= topNFeatures]
topFeaturesNomPens <- topFeaturesNomPens[order(-monthsBack)]
topFeaturesRecibo <- featureImportance[targetVar == "ind_recibo_ult1" &
                                         rank <= topNFeatures]
topFeaturesRecibo <- topFeaturesRecibo[order(-monthsBack)]

# Study the fold prediction ratios for different products by months
foldPredCco_fin <- featureImportance[targetVar == "ind_cco_fin_ult1",
                                     foldPredRatio[1], monthsBack]
foldPredCco_fin <- foldPredCco_fin[order(-monthsBack)]
foldPredCno_fin <- featureImportance[targetVar == "ind_cno_fin_ult1",
                                     foldPredRatio[1], monthsBack]
foldPredCno_fin <- foldPredCno_fin[order(-monthsBack)]
foldPredCtma_fin <- featureImportance[targetVar == "ind_ctma_fin_ult1",
                                     foldPredRatio[1], monthsBack]
foldPredCtma_fin <- foldPredCtma_fin[order(-monthsBack)]
foldPredEcue_fin <- featureImportance[targetVar == "ind_ecue_fin_ult1",
                                      foldPredRatio[1], monthsBack]
foldPredEcue_fin <- foldPredEcue_fin[order(-monthsBack)]
foldPredReca_fin <- featureImportance[targetVar == "ind_reca_fin_ult1",
                                      foldPredRatio[1], monthsBack]
foldPredReca_fin <- foldPredReca_fin[order(-monthsBack)]
foldPredTjcr_fin <- featureImportance[targetVar == "ind_tjcr_fin_ult1",
                                      foldPredRatio[1], monthsBack]
foldPredTjcr_fin <- foldPredTjcr_fin[order(-monthsBack)]
foldPredNomina <- featureImportance[targetVar == "ind_nomina_ult1",
                                      foldPredRatio[1], monthsBack]
foldPredNomina <- foldPredNomina[order(-monthsBack)]
foldPredNom_pens <- featureImportance[targetVar == "ind_nom_pens_ult1",
                                    foldPredRatio[1], monthsBack]
foldPredNom_pens <- foldPredNom_pens[order(-monthsBack)]
foldPredRecibo <- featureImportance[targetVar == "ind_recibo_ult1",
                                      foldPredRatio[1], monthsBack]
foldPredRecibo <- foldPredRecibo[order(-monthsBack)]


# Study all top features for June 15
topFeaturesJun15 <- featureImportance[monthsBack == 11 &
                                         rank <= topNFeatures]
topFeaturesJun15 <- topFeaturesJun15[order(targetVar)]
