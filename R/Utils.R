### I believe the next couple of functions are unnecessary (leftover from)
### old coding, but they're not hurting anything and I don't want to delete
### them in case something breaks

AG_zero_cross <- function(data){
  n <- c(data[-1],0)  ; npo <- c(0,data[-length(data)])  #n is for nth observation. npo is n plus one
  n <- sapply(n, function(x) if(x<=0) 'Negative' else 'Positive') ; npo <- sapply(npo, function(x) if(x<=0) 'Negative' else 'Positive')
  return(length(which(n!=npo)))
}

AG_zero_prop <- function(data){
  return(length(which(data==0))/length(data))
}
