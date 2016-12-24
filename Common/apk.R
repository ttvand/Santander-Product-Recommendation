# Adapted from: https://github.com/benhamner/Metrics/blob/master/R/R/metrics.r#L181

# Compute the mean average precision at 7

map7 <- function(records, returnScores=FALSE){
  nbRecords <- nrow(records)
  scores <- rep(0, nbRecords)
  
  for(i in 1:nbRecords){
    if(records[i,8]>0){
    score <- 0.0
    cnt <- 0.0
    for(j in 1:7){
      if(records[i,j]){
        cnt <- cnt + 1
        score <- score + cnt/j 
      }
    }
    scores[i] <- score / min(7, records[i,8])
    } else{
      scores[i] <- 0
    }
  }
  
  if(!returnScores){
    out <- mean(scores)
  } else{
    out <- scores
  }
  out
}

#' Compute the average precision at k
#'
#' This function computes the average precision at k
#' between two sequences
#'
#' @param k max length of predicted sequence
#' @param actual ground truth set (vector)
#' @param predicted predicted sequence (vector)
#' @export
apk <- function(k, actual, predicted)
{
  score <- 0.0
  cnt <- 0.0
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(i>1 && predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  score
}

#' Compute the mean average precision at k
#'
#' This function computes the mean average precision at k
#' of two lists of sequences.
#'
#' @param k max length of predicted sequence
#' @param actual list of ground truth sets (vectors)
#' @param predicted list of predicted sequences (vectors)
#' @export
mapk <- function (k, actual, predicted, returnScores=FALSE)
{
  if( length(actual)==0 || length(predicted)==0 ) 
  {
    return(0.0)
  }
  
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    scores[i] <- apk(k, actual[i], predicted[i,])
  }
  
  if(!returnScores){
    out <- mean(scores)
  } else{
    out <- scores
  }
  out
}