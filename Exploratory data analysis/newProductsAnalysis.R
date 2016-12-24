# Study the evolution of new products over time

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Santander")

# Load the required libraries
library(ggplot2)
library(data.table)

# Target date 
targetDate <- "12-11-2016"

baseProducts <- factor(c("ahor_fin", "aval_fin", "cco_fin", "cder_fin",
                         "cno_fin", "ctju_fin", "ctma_fin", "ctop_fin",
                         "ctpp_fin", "deco_fin", "deme_fin", "dela_fin", 
                         "ecue_fin", "fond_fin", "hip_fin", "plan_fin",
                         "pres_fin", "reca_fin", "tjcr_fin", "valo_fin",
                         "viv_fin", "nomina", "nom_pens", "recibo"
)
)

# Construct the probed leaderboard data
probedJun16 <- data.table(newProdSimple = baseProducts,
                          fecha_dato = as.Date("2016-06-28"),
                          # N = c(0, 0, 8785, 3, 2270, 49, 429, 183, 109, 0, 0, 90, 
                          # 1875, 4, 3, 51, 6, 2946, 3747, 261, 4, 4065, 3827, 7831)
                          # N = c(0, 0, 9762, 9, 2520, 48, 477, 203, 119, 0, 0, 99, 
                          # 2084, 115, 20, 14, 6, 3273, 4161, 292, 0, 4514, 4251, 8702)
                          N = c(0, 0, 9704, 9, 2505, 48, 474, 201, 119, 0, 0, 99, 
                                2072, 114, 20, 14, 6, 3254, 4137, 290, 0, 4487, 4600, 8651)
)



##################################################################

# Load the new products data
productPosFlank <- readRDS(file.path(getwd(), "Feature engineering",
                                     targetDate, "product positive flanks.rds"))

# # Study the distribution of users that only purchased one new product
# a <- productPosFlank[fecha_dato=="2015-06-28" , .N, ncodpers]
# singleProdNcodpers <- a[N==1, ncodpers]
# b <- productPosFlank[ncodpers %in% singleProdNcodpers, .N, newProduct]

# Analyze the new products in the desired month
allMonths <- sort(unique(productPosFlank$fecha_dato))
studiedMonths <- allMonths[c(4, 5, 16)]
newProdsMonth <- productPosFlank[fecha_dato %in% studiedMonths,]
prodCounts <- newProdsMonth[, .N, .(newProdSimple, fecha_dato)]
for(i in 1:length(studiedMonths)){
  zeroProds <- baseProducts[!baseProducts %in%
                              prodCounts[fecha_dato == studiedMonths[i], 
                                         newProdSimple]]
  if(length(zeroProds)>0){
    prodCounts <- rbind(prodCounts, data.table(newProdSimple = zeroProds,
                                               fecha_dato = studiedMonths[i],
                                               N = 0))
  }
}

p <- ggplot(prodCounts, aes(x = newProdSimple, y=N, fill=factor(fecha_dato))) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5))
print(p)

# Compare June 15 with June 16
prodCounts <- rbind(prodCounts, probedJun16)
monthSumCounts <- prodCounts[, .(monthSum = sum(N)), fecha_dato]
prodCounts[, monthSum := monthSumCounts$monthSum[
  match(fecha_dato, monthSumCounts$fecha_dato)]]
prodCounts[, newProdFraction := N/monthSum]
q <- ggplot(prodCounts, aes(x = newProdSimple, y=N, fill=factor(fecha_dato))) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5))
print(q)
