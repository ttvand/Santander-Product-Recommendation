# Logic to compare model predictions by month for the test data set

# Major findings:
#  - cco_fin:  Strong correlation between lag 5(jun15) and lag 11 (dec 15),
#              other predictions are poorly correlated
#              => way more weight for lags 5 and 11 (submission 66 - success)
#  - cno_fin:  All predications are highly correlated
#              => more weights to higher lags?
#  - ctma_fin: June 15 looks like a special case. No clear correlation pattern.
#              => More weight to June 15?
#  - ecue_fin: Predictions are highly correlated except lags 3, 4 and 12
#              Use similar weights and set weight for lags 3, 4 (and 12) to 0?
#  - reca_fin: Reca in June 15 is a special case!
#              => way more weight for lag 5 (and others to 0?)
#  - tjcr_fin: All predications are highly correlated
#              => more weights to higher lags? Drop lags 3 and 4?
#  - nomina:   High correlations but interesting patterns. Less correlation 
#              between lag 5 and the lags 7(aug 15)/12(jan 16)
#              => more weights to higher lags? (No improvement submission 68)
#  - nom_pens: Similar pattern as nom pens
#              => more weights to higher lags? (No improvement submission 68)
#  - recibo:   All predications are highly correlated
#              => more weights to higher lags? (submission 65 - epic FAIL)

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(plotly)

# Analyzed submission file
predictionsFolder <- "Predictions"
# submissionDate <- "27-11-2016"
submissionDate <- "06-12-2016"
# submissionFile <- "xgboost weighted posFlanks 5, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint"
submissionFile <- "xgboost weighted stacked 1, linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint"
modelsPath <- file.path(getwd(), "Submission", submissionDate,
                        predictionsFolder, submissionFile)

# Analyzed lags and base model
# analyzedLags <- 3:16
analyzedLags <- 5:16
nbLags <- length(analyzedLags)
# baseModel <- "ind_cco_fin_ult1"
# baseModel <- "ind_cno_fin_ult1"
# baseModel <- "ind_ctma_fin_ult1"
# baseModel <- "ind_ecue_fin_ult1"
baseModel <- "ind_reca_fin_ult1"
# baseModel <- "ind_tjcr_fin_ult1"
# baseModel <- "ind_nomina_ult1"
# baseModel <- "ind_nom_pens_ult1"
# baseModel <- "ind_recibo_ult1"

modelPaths <- paste0(modelsPath, "/", baseModel, " Lag ", analyzedLags, ".rds")


#########################################################################

# Load the predictions and plot the correlation matrix
predictions <- NULL
for(i in 1:nbLags){
  predictionsDT <- readRDS(modelPaths[i])
  predictions <- cbind(predictions, predictionsDT$predictions)
}

# Generate the correlation matrix
correlations <- round(cor(predictions, method="pearson"), 2)
rownames(correlations) <- paste("Lag", analyzedLags)
colnames(correlations) <- paste("Lag", analyzedLags)
correlations[lower.tri(correlations)] <- NA
melted_cormat <- melt(correlations, na.rm = TRUE)
p <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() + 
  ylab("") +
  xlab("")
print(p)
ggplotly(p)
