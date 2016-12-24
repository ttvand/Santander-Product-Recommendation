# Exponential probability normalize logic:
# Use a binary search approach to find the desired exponential
probExponentNormaliser <- function(probs, multiplier, weights = NULL,
                                   nbIt = 50){
  # Normalize the weights
  if(is.null(weights) || length(weights) != length(probs)){
    weights <- rep(1, length(probs))
  }
  normWeights <- weights/mean(weights)
  
  low <- ifelse(multiplier>1, 0, 1)
  high <- ifelse(multiplier>1, 1, 10)
  targetSum <- multiplier * sum(probs*normWeights)
  for(i in 1:nbIt){
    mid <- low + (high-low)/2
    if(sum((probs^mid)*normWeights) < targetSum){
      high <- mid
    } else{
      low <- mid
    }
  }
  out <- probs^mid
  out
}