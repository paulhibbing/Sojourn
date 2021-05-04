#' Prepare ActiGraph data for entry into the Sojourn neural network
#'
#' @param df The ActiGraph data
#' @param variable The variable to prepare
#'
#' @keywords internal
prep_nnet_soj3x_original <- function(
  df,
  variable = c("counts", "counts.2", "counts.3", "vect.mag"),
  suffix = ""
) {

  variable <- match.arg(variable)

  nnetinputs <-
    tapply(
      inactivities[ ,variable],
      inactivities$sojourns,
      quantile,
      probs=c(.1,.25,.5,.75,.9)
    ) %>%
    unlist(.) %>%
    as.vector(.)

  nnetinputs <- matrix(
    nnetinputs,
    length(nnetinputs)/5,
    5,
    byrow=TRUE
  )
  nnetinputs <- as.data.frame(nnetinputs)
  names(nnetinputs) <- c("X10.","X25.","X50.","X75.","X90.")
  nnetinputs$acf <- 0

  g <- 1
  for (soj in unique(inactivities$sojourns)) {

    counts <- inactivities[inactivities$sojourns==soj, variable]

    if (sum(counts)>0) {

      temp <- acf(counts,lag.max=1,plot=F)
      nnetinputs$acf[g] <- as.numeric(unlist(temp[1,1])[1])

    }

    g <- g+1

  }

  nnetinputs$acf[is.na(nnetinputs$acf)] <- mean(
    nnetinputs$acf,
    na.rm=TRUE
  )

  nnetinputs %>%
  stats::setNames(., paste0(names(.), suffix)) %>%
  stats::setNames(., gsub("[.]{2}", ".", names(.)))

}

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
#' \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{SIP method}
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
#' \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{SIP method}
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
