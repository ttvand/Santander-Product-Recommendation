# Hyperparameter description logic
getHyperParDescr <- function(hyperpar){
  list(nrounds = nrounds, etaC = 10, subsample = 1,
       colsample_bytree = 0.5, max.depth = 6,
       min_child_weight = 0, gamma = 0)
  paste0(hyperpar$nrounds, " rounds -",
         " etaC ", hyperpar$etaC,
         " - colsample ", hyperpar$colsample_bytree,
         " - depth ", hyperpar$max.depth,
         " - minWeight ", hyperpar$min_child_weight,
         " - gamma ", hyperpar$gamma
         )
}