### I believe the next couple of functions are unnecessary (leftover from)
### old coding, but they're not hurting anything and I don't want to delete
### them in case something breaks

#' Zero crossings for ActiGraph data
#'
#' @param data the data to analyze for zero crossings
#'
#' @keywords internal
#'
AG_zero_cross <- function(data){
  n <- c(data[-1],0) #nth observation
  npo <- c(0,data[-length(data)])  #n plus one

  n <- sapply(n, function(x) if(x<=0) 'Negative' else 'Positive')
  npo <- sapply(npo, function(x) if(x<=0) 'Negative' else 'Positive')

  return(length(which(n!=npo)))

}

#' Calculate proportion of zeroes in a data stream
#'
#' @param data the data stream to analyze
#'
#' @keywords internal
#'
AG_zero_prop <- function(data){

  return(length(which(data==0))/length(data))

}

#' Compute lag-one autocorrelation
#'
#' @param x numeric vector for the computation
#'
#' @return a numeric scalar giving the lag-one autocorrelation
#' @keywords internal
#'
acf.lag1 <- function(x) {

  n <- length(x)
  a <- mean((x[-1]-mean(x[-1]))*(x[-n]-mean(x[-n])))
  v <- var(x)

  if ((v==0)|(is.na(v)))
    val <- 0
  if ((v!=0)&(is.na(v)==F))
    val <- a/v

  return(val)
}

#' Alternate lag-1 autocorrelation function
#'
#' This function is used to compute lag-1 autocorrelation for the
#' \href{https://www.ncbi.nlm.nih.gov/pubmed/27015380}{SIP method}
#'
#' @param x numeric vector for the computation
#'
#' @keywords internal
#'
acf.lag1.alt <- function(x) {
  acf(x, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
}

#' Perform mean imputation
#'
#' This function is used to perform mean imputation for the
#' \href{https://www.ncbi.nlm.nih.gov/pubmed/27015380}{SIP method}
#'
#' @param x data on which to perform the imputation
#'
#' @keywords internal
#'
replace.na <- function(x) {
  # replace NA values with column means
  col.means <- colMeans(x, na.rm = TRUE)
  col.names <- rep(names(x), each = dim(x)[1])
  x[is.na(x)] <- col.means[col.names[is.na(x)]]
  return(x)
}
