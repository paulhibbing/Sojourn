youth_network_shape <- function(data, sojourns = T, RAW = F, epoch = 1, verbose = T, expand = T){
  if(verbose) cat(paste('\n***\t***\t***\t***\nCalculating prediction for', data$id[1], '\n'))
  if(RAW) data$Vector.Magnitude <- data$ENMO

  if(!sojourns){
    data$sojourns <- rep(1:ceiling(nrow(data)/15), each = 15)[1:nrow(data)]
    cat('\nGetting 15s features...')
  } else{

    if(!'sojourns'%in%names(data)) {
      data <- cbind(data, get_youth_sojourns(vm = data$Vector.Magnitude, RAW = RAW, epoch = epoch))
      cat('\nGetting Sojourns with default settings...')
    } else{
      if(!verbose) cat('\nGetting Sojourn features...')
      if(length(unique(table(data$sojourns)))==1&unique(table(data$sojourns))[1]==15){
        message('Looks like this is in 15-second blocks. Returning NULL.')
        return(NULL)
      }
    }
  }

  #if(RAW) Time <- data$timestamp else
  Time <- data$Time
  groups <- as.list(c('id','Visit','sojourns')[c('id','Visit','sojourns')%in%names(data)])
  #if(!verbose) cat('... Calculating features...')
  data <- suppressWarnings(data %>% dplyr::group_by_(.dots = groups) %>% dplyr::summarise(VM_counts = sum(Vector.Magnitude),
    VM_Mean = mean(Vector.Magnitude), VM_zcr = AG.zero.cross(Vector.Magnitude),VM_zero_prop=AG.zero.prop(Vector.Magnitude),
    VM_Q10 = quantile(Vector.Magnitude, probs=0.10),
    VM_Q25 = quantile(Vector.Magnitude, probs=0.25),
    VM_Q50 = quantile(Vector.Magnitude, probs=0.50),
    VM_Q75 = quantile(Vector.Magnitude, probs=0.75),
    VM_Q90 = quantile(Vector.Magnitude, probs=0.90),
    VM_CV = cv(Vector.Magnitude), VM_Var = var(Vector.Magnitude),
    VM_lag1 = acf(Vector.Magnitude,type="correlation",plot=F)[[1]][2],
    # VM_Skew = skewness(Vector.Magnitude), VM_Kurt = kurtosis(Vector.Magnitude),VM_CV = cv(Vector.Magnitude),
    # VM_entropy = ifelse(sd(Vector.Magnitude)==0,0,spectral_entropy(Vector.Magnitude)),
    # VM_fftmean = mean(as.numeric(fft(Vector.Magnitude))), VM_fftmin = min(as.numeric(fft(Vector.Magnitude))),
    # VM_fftmax = max(as.numeric(fft(Vector.Magnitude))),
    # VM_harmreg_means = ifelse(VM_Mean==0,0,harmonic.regression(Vector.Magnitude,rep(1:n()))[[1]]),
    # VM_harmreg_amp = ifelse(VM_Mean==0,0,harmonic.regression(Vector.Magnitude,rep(1:n()))$pars$amp),
    # VM_harmreg_phase = ifelse(VM_Mean==0,0,harmonic.regression(Vector.Magnitude,rep(1:n()))$pars$phi),
    # VM_harmreg_normts_min = ifelse(VM_Mean==0,0,min(harmonic.regression(Vector.Magnitude,rep(1:n()))$normts)),
    # VM_harmreg_normts_max = ifelse(VM_Mean==0,0,max(harmonic.regression(Vector.Magnitude,rep(1:n()))$normts)),
    # VM_harmreg_normts_mean = ifelse(VM_Mean==0,0,mean(harmonic.regression(Vector.Magnitude,rep(1:n()))$normts)),
    duration = n()*epoch))

  noZeros <-data.frame(do.call(cbind,
    lapply(data[,setdiff(names(data), unlist(groups))],
      function(x) sapply(x,
        function(y) if(is.na(y)) 0 else y))), stringsAsFactors = F)
  data <- data.frame(data[,unlist(groups)], noZeros, stringsAsFactors = F)

  #if(nrow(data)<25) {message('Insufficient data; returning NULL') ; return(NULL)} ##Protection from errors doing princomp

  # scores <- cbind(data[,seq_along(groups)],data.frame(princomp(data[,-c(seq_along(groups))])["scores"]))
  # data$scores.Comp.1 <- scores$scores.Comp.1

  data$scores.Comp.1 <- NA ##This is my workaround since I'm not using princomp anymore

  cat('... Done!\n')
  if(RAW) names(data) <- gsub('VM_','ENMO_',names(data)) ; names(data) <- gsub('ENMO_counts','ENMO_total',names(data))
  if(!sojourns) names(data)[which(grepl('sojourns',names(data)))] <- 'Block'

  if(expand){
    data <- data[rep(seq_len(nrow(data)), times = data$duration),]

  } else{
    Time <-
      tapply(as.character(Time),
        unlist(
          mapply(function(x,y) rep(x,y),
            x = data[,switch(sojourns+1, 'Block', 'sojourns')], y = data$duration, SIMPLIFY = F)),
        function(x) x[1])
  }
  #if(RAW) data$Time <- get.minofday.rat(Time) else
  data$Time <- Time
  #if('Visit'%in%names(data)) data <- data[with(data, order(id,Visit,Time)),] else data <- data[with(data, order(id,Time)),]
  return(data)
}
