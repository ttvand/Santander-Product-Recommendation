# Logic to compare different submissions

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(data.table)
library(ggplot2)
library(plotly)

# Path to the submissions
# submission1Fn <- file.path(getwd(), "Submission",
# "21-11-2016/xgboost 11 back 11-0 Zero deco_fin dela_fin not normalised.csv")
# submission2Fn <- file.path(getwd(), "Submission",
# "21-11-2016/xgboost 11 back 11-0 Zero deco_fin deme_fin dela_fin fond_fin normalised linearly.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "22-11-2016/xgboost weighted back 11-0 zero prods 10, 11, 12 and 14 not normalised.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "22-11-2016/xgboost weighted back 11-0 zero prods 10, 11, 12 and 14 linear normalisation.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "23-11-2016/xgboost weighted 2 back 11-0 no zeroing, linear normalisation joint, exponential normalisation marginal.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "23-11-2016/xgboost weighted 2 uniform jun15 times6 back 11-0 no zeroing, linear normalisation joint, exponential normalisation marginal.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "23-11-2016/xgboost weighted 2 linear increase jun15 times6 back 11-0 no zeroing, linear normalisation joint, exponential normalisation marginal.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "26-11-2016/xgboost weighted posFlanks 4 linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation marginal, linear normalisation conditional.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "27-11-2016/xgboost weighted posFlanks 5 linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "28-11-2016/xgboost weighted posFlanks 6, more weight dec15 other 0 cco_fin linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "30-11-2016/xgboost weighted posFlanks 8, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation marginal, linear normalisation conditional.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "30-11-2016/xgboost weighted trainAll 8, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission2Fn <- file.path(getwd(), "Submission",
#                            "Ensembler/2016-12-01 11-10-38.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "02-12-2016/xgboost weighted trainAll 10, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "03-12-2016/xgboost weighted trainAll 11, 5 folds linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "03-12-2016/xgboost weighted trainAll 11 top 100 monthProduct, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "07-12-2016/xgboost weighted stacked 2, linear increase jun15 times6 back 11-0 7 months no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "07-12-2016/xgboost weighted stacked 2, linear increase jun15 times6 back 11-0 10 months no zeroing, exponential normalisation joint.csv")
# submission2Fn <- file.path(getwd(), "Submission",
# "08-12-2016/xgboost weighted stacked 3, linear increase jun15 times6 back 11-0 no zeroing, exponential normalisation joint.csv")
# submission2Fn <- file.path(getwd(), "Submission",
# "08-12-2016/xgboost weighted trainAll 13, linear increase jun15 times6 back 15-0 no zeroing, exponential normalisation joint.csv")
# submission2Fn <- file.path(getwd(), "Submission",
# "08-12-2016/xgboost weighted trainAll 13, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission","09-12-2016/xgboost weighted trainAll 14, linear increase jun15 times6 back 15-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "09-12-2016/xgboost weighted trainAll 14, ecue jun15 1.4 apr15 0, linear increase jun15 times6 back 15-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
#                            "09-12-2016/xgboost weighted trainAll 14 nom pens swap nomina, ecue jun15 1.4 apr15 0, linear increase jun15 times6 back 15-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
#                            "14-12-2016/xgboost weighted trainAll 16 10 folds 200 rounds, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "14-12-2016/xgboost weighted trainAll 16 10 folds 200 rounds, half cco dec15, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
#                            "15-12-2016/xgboost weighted trainAll 17, reca jun 15 52 weight, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission1Fn <- file.path(getwd(), "Submission",
#                            "16-12-2016/xgboost weighted trainAll 18, cco 0.3 weight other months, linear increase jun15 times6 back 13-0 no zeroing, exponential normalisation joint.csv")
# submission2Fn <- file.path(getwd(), "Submission",
#                            "17-12-2016/xgboost weighted trainAll 19, nomina nom pens Jun15 weight 1.4.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "18-12-2016/Submission 108, weights recibo Apr 15 0.csv")
# submission1Fn <- file.path(getwd(), "Submission",
# "19-12-2016/confidence rescaler cno, reca, nomina and nom pens, weights submission 117, no MAP boosting - Personal rescale weights.csv")
# submission2Fn <- file.path(getwd(), "Submission",
#                            "19-12-2016/confidence rescaler only reca, weights submission 117, no MAP boosting.csv")
# submission1Fn <- file.path(getwd(), "Submission",
#                            "19-12-2016/confidence rescaler cco 1.2, weights submission 117, no MAP boosting.csv")
# submission1Fn <- file.path(getwd(), "Ensembling",
#                            "2016-12-21 14-39-26 - Main ensemble 1.csv")
submission1Fn <- file.path(getwd(), "Ensembling",
                           "2016-12-21 16-24-31 - Main ensemble 2.csv")
submission2Fn <- file.path(getwd(), "Ensembling",
                           "2016-12-21 23-02-19 - Final ensemble of main ensembles 1 and 2.csv")


# Relative difference cutoff and minimum count to display products
relDiffCutoff <- 0.5
minCountDifference <- 1e3
maxPlotRange <- 8e4

# Target date 
targetDate <- "12-11-2016"

# Restrict the analysis to products with any positive flank?
restrictPosFlankCustomers <- TRUE

baseProducts <- c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                  "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                  "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                  "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                  "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                  "viv_fin", "nomina", "nom_pens", "recibo"
)
allProducts <- paste0("ind_", baseProducts, "_ult1")

# Drop low priority ids from the product comparison plots?
dropLowPriorityIds <- TRUE
lowPriorityIds <- c(1, 2, 4, 10, 11, 15, 16, 17, 21)


###############################################################

# Load the submissions
submission1 <- fread(submission1Fn)
submission2 <- fread(submission2Fn)

# Don't consider records where one of the two predictions is missing
dropIds <- which(submission1$added_products == "" |
                   submission2$added_products == "")
if(length(dropIds)>0){
  submission1 <- submission1[-dropIds]
  submission2 <- submission2[-dropIds]
}

# Optionally omit predictions for customers that have no positive train flanks
if(restrictPosFlankCustomers){
  posFlankClientsFn <- file.path(getwd(), "Feature engineering", targetDate,
                                 "positive flank clients.rds")
  posFlankClients <- readRDS(posFlankClientsFn)
  submission1 <- submission1[ncodpers %in% posFlankClients,]
  submission2 <- submission2[ncodpers %in% posFlankClients,]
}

# Calculate the number of predicted new product rows
nbProducts <- nrow(submission1)

# Add the rank of the products to the submission data tables
prods1 <- strsplit(paste(submission1[[2]], collapse = " "), split = " ")[[1]]
prods2 <- strsplit(paste(submission2[[2]], collapse = " "), split = " ")[[1]]
for(i in 1:7){
  submission1[[paste0("Rank", i)]] <- factor(prods1[7*(0:(nbProducts-1))+i],
                                             levels = allProducts)
  submission2[[paste0("Rank", i)]] <- factor(prods2[7*(0:(nbProducts-1))+i],
                                             levels = allProducts)
}

# Compare the submissions
topProds1 <- submission1[, .N, Rank1]
topProds1 <- topProds1[order(-N)]
topProds2 <- submission2[, .N, Rank1]
topProds2 <- topProds2[order(-N)]
topProds1[, Fraction := round(100*N/sum(topProds1$N), 2)]
topProds2[, Fraction := round(100*N/sum(topProds2$N), 2)]
top1 <- as.numeric(table(submission1$Rank1))
top2 <- as.numeric(table(submission2$Rank1))
plot(top1, top2)
abline(0, 1, col="blue")
plot(top1, top2, xlim = c(0, maxPlotRange), ylim = c(0, maxPlotRange))
abline(0, 1, col="blue")
difProds <- allProducts[(top1>minCountDifference | top2>minCountDifference) &
                          ((top1/top2 > (1 + relDiffCutoff)) |
                             (top2/top1 > (1 + relDiffCutoff)))]

# Analyze the second product counts when the top product is nomina since it is
# very rare not to buy nom pens when you buy nomina
submission1[Rank1 == "ind_nomina_ult1", .N, Rank5]

if(length(difProds) > 0){
  cat("Different top products:", difProds, "\n")
}

# Show the top alignment ratio
topProducts <- as.data.frame.matrix(table(submission1$Rank1, submission2$Rank1))
identicalTopRatio <- sum(topProducts*diag(24))/nbProducts
cat("The top predictions are identical for ", round(identicalTopRatio * 100, 2),
    "% of the predictions\n", sep="")

# Optionally drop low priority ids from the product comparison plots
if(dropLowPriorityIds){
  topProducts <- topProducts[-lowPriorityIds, -lowPriorityIds]
}

# Create a matrix with three categories for all products:
# - Both models predict that product
# - The first model predicts that product but the second doesn't
# - The second model predicts that product but the first doesn't
topProducts <- as.matrix(topProducts)
agree <- diag(as.matrix(topProducts))
firstNotSecond <- rowSums(topProducts) - agree
secondNotFirst <- colSums(topProducts) - agree
agreeCompare <- rbind(agree, firstNotSecond, secondNotFirst)
melted_agree <- melt(1 + agreeCompare, na.rm = TRUE)
p1 <- ggplot(melted_agree, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       trans = "sqrt", space = "Lab",
                       name="Top prod count") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() + 
  ylab("") +
  xlab("")
print(p1)
ggplotly(p1)

# Plot the top products alignment 
melted_topProd <- melt(1 + topProducts, na.rm = TRUE)
p2 <- ggplot(melted_topProd, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       trans = "sqrt", space = "Lab",
                       name="Top prod count") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() + 
  ylab("") +
  xlab("")
print(p2)
ggplotly(p2)
