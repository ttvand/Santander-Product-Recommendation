# Weights extraction logic
getModelWeights <- function(dateValue, targetVar, dateTargetWeights){
  if(all(is.na(dateValue))){
    targetWeights <- rep(1, length(dateValue))
  } else{
    targetVarWeights <- dateTargetWeights[targetProduct==targetVar,]
    if(nrow(targetVarWeights)>0){
      # Get the values based on the last date or the number of months back
      if(class(dateValue) == "Date"){
        targetWeights <- targetVarWeights[match(dateValue,
                                                targetVarWeights$weightDate),
                                          relativeWeight]
      } else{
        targetWeights <- targetVarWeights[match(dateValue,
                                                targetVarWeights$monthsBack),
                                          relativeWeight]
      }
    } else{
      stop("Product - date value not in dateTargetWeights")
      # targetWeights <- rep(1, length(dateValue))
    }
  }
  targetWeights
}