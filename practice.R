
#' Turn df to Vector
#'
#' @param df 
#' @author augusta manchester
#' @details it allows one to turn a data frame into a vector of the means
#' of each column. 
#' @return A vector with column means.
#' @export
col_means<-function(df) {
  if(!is.data.frame(df)){
    stop("has to be data frame")
  }
  means<-sapply(df, mean)
  return(means)
}

#' Count NA Values
#'
#' @param vec 
#' @author augusta manchester
#' @details it allows one to count the number of NAs in a vector
#' @return a count of NAs
#' @export
count_na<-function(vec) {
  na<-0
  for (i in vec) {
    if (is.na(i)) {
      na<-na+1
    }
  }
  return(na)
}

