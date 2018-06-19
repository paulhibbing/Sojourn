sojourns.startup <- function(){
  ### THIS JUST LOADS ALL THE NECESSARY COMPONENTS (FUNCTIONS, DATA, NNETS, PACKAGES)
  ### AND STARTS WITH A CLEAN SLATE ####
  rm(list=ls())
  packages <- as.vector(installed.packages()[,1])
  load('Sojourn R Materials/Required Packages.RData')
  needs <- which(unname(sapply(requirements, function(x) !x %in% packages)))
  if(!length(needs)==0){
    message('First time using sojourns on this machine? Installing packages. This may take awhile.')
    sapply(requirements[needs], function(x) install.packages(x))
  }
  
  cat('Loading packages...')
  requirements <- c('HarmonicRegression', 'plyr', 'dplyr', 'reshape2', 'raster', 'moments',
                    'nnet', 'caret', 'svDialogs', 'lubridate', 'data.table')
  ## ^^ These are technically the packages you need (maybe some extras), whereas
  ## earlier the list was way longer. I'm shortening it so you don't have to wait
  ## for over a hundred packages to load.
  suppressMessages(suppressWarnings(lapply(requirements, suppressWarnings(library), character.only = T, quietly = T, warn.conflicts = F, verbose = F)))
  cat('\n\n\nPackages loaded. Warnings/messages intentionally suppressed. Shouldn\'t be a problem, but check versions if problems arise.')
  
  cat('\n\nLoading neural nets...')
  load('Sojourn R Materials/Youth Sojourns NNets.RData', envir = globalenv())
  cat('... Done!')
  
  cat('\n\nLoading grids...')
  load('Sojourn R Materials/Sojourn Grids.RData', envir = globalenv())
  cat('... Done!')
  cat('\n\n\nAll set. Everything\'s loaded.')
}

get.enmo <- function(AG, file, window = 1, verbose = T){
  ## The code I've made for calculating ENMO in the demo is actually
  ## different from what I used originally. The original came from
  ## Vincent van Hees, and I've cleaned it up to make it easier to follow,
  ## and possibly faster
  
  ## A lengthy yet hopefully robust way to identify sampling frequency:  
  topLine <- read.csv(file, nrow = 1, header = F, stringsAsFactors = F)
  topLine$V1 <- gsub(' ', '', topLine$V1)
  topLine <- unlist(strsplit(topLine$V1, 'Hz'))[1]
  topLine <- substring(topLine, 1, regexpr('[0-9][^0-9]*$', topLine))
  samp.freq <- as.numeric(substring(topLine, regexpr('[^0-9][0-9]*$', topLine)+1))
  if(verbose) cat('\nDetected sampling frequency:', samp.freq, 'Hz')
  
  ## Now to calculate ENMO
  ENMO <-
    apply(AG[,grepl("accelerometer", names(AG), ignore.case = T)],
          1, function(x) sqrt(sum(x^2))-1) ## This is a vectorized way to
  ## calculate ENMO
  ENMO <-
    ifelse(ENMO<0, 0, ENMO) ## Round up to 0 for any negatives
  
  blocks <- rep(1:(nrow(AG)/(samp.freq*window)),
                each = (samp.freq*window))[1:nrow(AG)]
  
  ENMO <-
    tapply(ENMO, blocks, function(x){
      if(length(x)==(samp.freq*window)) mean(x) else NA
    })
  
  timestamp <-
    gsub('\\.000', '', AG$Timestamp[grepl('\\.000$', AG$Timestamp)])
  
  ENMO = data.frame(Time = as.POSIXlt(timestamp, format = '%m/%d/%Y %H:%M:%S'),
                    ENMO = as.vector(ENMO*1000), stringsAsFactors = F, row.names = NULL)
  return(ENMO)
}

read.ActiGraph <- function(file){
  cat('\nProcessing', basename(file))
    AG <- data.frame(data.table::fread(file, stringsAsFactors = F),
                     stringsAsFactors = F)
    RAW <- if('Axis1'%in%names(AG)) FALSE else TRUE
    
    names(AG)[grepl('Vector', names(AG))] <- 'Vector.Magnitude'
    message('R will now ask you for some information about the data and participant.')
    message('If a dialog box does not pop up, you may have to minimize R.')
  
  id = svDialogs::dlgInput('Enter the participant ID', default = 'DEMO')$res
  Site = switch(svDialogs::dlgMessage('Is the data from the hip?', 'yesno')$res,
                'yes' = 'Hip', 'no' = 'Wrist')
  Sex = switch(svDialogs::dlgMessage('Is the participant female?', 'yesno')$res,
               'yes' = 'F', 'no' = 'M')
  
  ## Age ####
    Age = svDialogs::dlgInput('Please enter the participant\'s age in years:',
                              default = '10')$res
    repeat{
    ageTest <-
      suppressWarnings(grepl('Error',
                             try(as.integer(Age)))|is.na(try(as.integer(Age))))
    if(!ageTest) break else{message('Age not entered correctly. Try again')
      Age = svDialogs::dlgInput('Please enter the participant\'s age in years:',
                                default = '10')$res}
    }
    Age = as.integer(Age)

  ## BMI ####  
    BMI = svDialogs::dlgInput('Please enter the participant\'s BMI',
                              default = '20')$res
      repeat{
      bmiTest <-
        suppressWarnings(grepl('Error',
                               try(as.numeric(BMI)))|is.na(try(as.numeric(BMI))))
      if(!bmiTest) break else{message('BMI not entered correctly. Try again')
        BMI = svDialogs::dlgInput('Please enter the participant\'s BMI',
                                  default = '20')$res}
      }
    BMI = as.numeric(BMI)
    vm <- 'Vector.Magnitude'   
  ## Process Raw Data ####
    if(RAW){
      AG <- get.enmo(AG, file = file)
      vm <- 'ENMO'
    }
    
  ## Identify which ANN to use ####
    ANN <-
      paste(
        tolower(Site),
        switch(('ENMO'%in%names(AG))+1, 'Counts', 'Raw'),
        sep = '')
    
    intensity.fit <- eval(parse(text = ANN))
    FeatureSet <-
      intensity.fit$coefnames
    FeatureSet <- gsub('Sex[MF]', 'Sex', FeatureSet, ignore.case = T)
    
  ## Now to process
    AG <- data.frame(id = id, Site = Site, Age = Age,
                     Sex = Sex, BMI = BMI, AG, stringsAsFactors = F)
    
    cat('\nIdentifying Sojourns...')
      AG <-
        cbind(AG,
              get.sojourns(AG[,vm],
                           Output = if(RAW) 'Raw' else 'Counts',
                           Site = Site))
    cat('\nDone!\n')
    
    AG$YouthSojourn_Intensity <-
      block.soj.committee(
        AG[,vm],
        predict(intensity.fit,
                cbind(network.shape(AG, F, RAW, verbose = T), Sex, Age, BMI)),
        predict(intensity.fit,
                cbind(network.shape(AG, T, RAW, verbose = F), Sex, Age, BMI))
      )
    return(AG)
  }

### I believe the next couple of functions are unnecessary (leftover from)
### old coding, but they're not hurting anything and I don't want to delete
### them in case something breaks
AG.zero.cross <- function(data){
  n <- c(data[-1],0)  ; npo <- c(0,data[-length(data)])  #n is for nth observation. npo is n plus one
  n <- sapply(n, function(x) if(x<=0) 'Negative' else 'Positive') ; npo <- sapply(npo, function(x) if(x<=0) 'Negative' else 'Positive')
  return(length(which(n!=npo)))
}

AG.zero.prop <- function(data){
  return(length(which(data==0))/length(data))
}

### SOJOURNS FUNCTIONS ####

get.sojourns <- function(vm,short=30, Output=NULL, Site = NULL, epoch = 1, difference = 15, threshold = 100){
  #Preliminary note: The epoch parameter isn't used currently.
  #It's in there in case this needs to be adapted so it can scale to other epoch lengths.
  #For the purposes of initial development I'm not going to implement that.
  
  #The Output argument will eventually be used to select the appropriate thresholds. Same with the
  #Site argument
  
  # if(file.exists('RData/Sojourn Grids.RData')){
  #   load('RData/Sojourn Grids.RData', envir = globalenv())
  # } else{
  #   message('Can\'t find RData/Sojourn Grids.RData. Using the provided settings for difference, threshold, and short.')
  # }
  
  if((!is.null(Site))&(!is.null(Output))){
    theGrid <-
      globalenv()$SojGrids[with(globalenv()$SojGrids, paste(Site, Output))==paste(Site, Output),]
    if(nrow(theGrid)!=1) {
      message('Unable to identify a single matching grid. Returning NULL.')
      return(NULL)
    }
    cat('\nUsing the', paste(theGrid$Site, theGrid$Output, 'grid:\n...Difference =',
                             theGrid$Difference, '\n...Threshold =', theGrid$Threshold,
                             '\n...Short =', theGrid$Short))
    
    difference = theGrid$Difference
    threshold = theGrid$Threshold
    short = theGrid$Short
  } else{
    message('Site and Output arguments not entered. Using the provided settings for difference, threshold, and short.')
  }

    
  #### Set up variables  
    y   <- vm
    inds <- seq_along(y)
    total_observations  <- length(y)
    one <- y[-total_observations]
    two <- y[-1]
  
  #### Find transitions
    trans           <- ((one-two)>difference)&(two<=threshold) 	# where are differences in second-by-second counts >15
                                                            # and coming down to <10, i.e. can't use diff() because I need the sign
  
    trans           <- c(0,trans) # Adding the first frame back in and initializing to zero (converting
                                   # logical values to 0's and 1's as well)
    
    trans.inds      <- which(trans==1)       # Indices of the transitions
    
    if(length(trans.inds)==0){
      message('All data fit within one Sojourn. You should follow up with manual inspection.')
      trans.table <- data.frame(sojourns=1,durations=rep(length(y), length(y)))
      return(trans.table)
    }
    
    trans_count   <- length(trans.inds)    # Total number of transitions
  
  #### List duration of each sojourn
    durations           <- diff(trans.inds)*epoch
    durations_count   <- length(durations)
    
    durations           <- c(durations,
                             (total_observations-trans.inds[trans_count])*epoch)   #Adds the duration of the last sojourn
    durations_count   <- length(durations)
    
    first.trans         <- trans.inds[1]
    durations           <- c(first.trans*epoch,durations)  #Adds the duration of the first sojourn
    durations_count   <- length(durations)
    
  #### Get number of sojourns, initialize sojourn markers, and grab mean counts from each sojourn
        ##NOTE: Some of these are only valuable after the last time through the loops
    sojourns        <- seq_along(durations)
    sojourns_count <- length(sojourns)
  
  #####	combine too short sojourns with neighboring sojourn.
  ###### 	this loop repeats until there are no more too-short sojourns
  
  counter=1
  repeat{
    
    too.short       <- which(durations<short)    # Which ones are too short?
    too.short_count <- length(too.short)                # How many are too short?
    
    if(too.short_count==0) 
      break
    
    if(too.short_count>0){	
      counter.1 <- 1 #Variable just for loop 2
      
  #### Loop 2: Combine all Sojourns that occur before the first full-length Sojourn
      repeat{
        if (too.short[counter.1]==counter.1)
        {
          sojourns[1:counter.1] <- sojourns[counter.1+1]
          
          counter.1 <- counter.1+1
        }
        
        if (too.short[counter.1]!=counter.1 | is.na(too.short[counter.1]))
          
          break}	# end loop 2
      
      counter.2 <- sojourns_count #Variable just for loop 3
      counter.too.short_count <- too.short_count #Variable used beyond loop 3
      
  #### Loop 3: Combine all Sojourns that occur after the last full-length Sojourn
      repeat{  # This loop deals with if last too short sojourn is last sojourn of file 
        # (i.e. it only has a first neighbor to combine it with)
        
        if (too.short[counter.too.short_count]==counter.2)
        {
          sojourns[counter.2:sojourns_count] <- sojourns[counter.2-1]
          
          counter.2 <- counter.2-1
          counter.too.short_count <- counter.too.short_count-1
          if(counter.too.short_count==0) break ##<--- For when the first Sojourn that's too short comes at the end of the file
        }
        
        if (too.short[counter.too.short_count]!=counter.2)
          
          break}	#end loop 3
      
  ##### now deal with all other too short sojourns
      junk.too.short <- too.short ### Need a copy of the variable that tells which ones are too short.
      
      if(counter.too.short_count<too.short_count-1){
        junk.too.short <- too.short[-(counter.too.short_count+1:too.short_count)]}
      ## ^^^^ Takes the last one out if it's already
      ## ^^^^ been manipulated in loop 3
      
      if (counter.1>1){
        junk.too.short <- junk.too.short[-(1:counter.1-1)]}
      ## ^^^^ Takes the first one out if it's already been manipulated in loop 2
      
      j.t.s <- length(junk.too.short)     # How many in the middle are too short?
      
      first.neighbors <- junk.too.short-1 #Preceding sojourns
      second.neighbors <- junk.too.short+1 #Succeeding sojourns

      durations.first.neighbors <- durations[first.neighbors]
      durations.second.neighbors <- durations[second.neighbors]
      
      
      #	Combine too short sojourns with the neighbor that is shorter in duration
      # (e.g. first neighbor = 60 seconds long and second neighbor = 300 seconds long,
      # it gets combined with first neighbor)
      
      revised.sojourns <- sojourns
      
      # I don't think any of these lines are necessary
        #	put in dummy duration for too.short sojourns at beginning and end of file
        # durations.first.neighbors[is.na(durations.first.neighbors)] <- 100000
        # durations.second.neighbors[is.na(durations.second.neighbors)] <- 100000
        # n.neighbors <- length(durations.first.neighbors)
        # n.neighbors.2 <- length(durations.second.neighbors)
      
      inds.first <- which(durations.first.neighbors<=durations.second.neighbors)
      inds.second <- which(durations.first.neighbors>durations.second.neighbors)
      
      too.short.inds.first <- junk.too.short[inds.first] #These are wacky variables. They turn
      too.short.inds.second <- junk.too.short[inds.second] #Values into indices, based on the
                                                            #fact that revised.sojourns has a different
                                                              #length from junk.too.short and subsequent variables

      
      #i.e. junk.too.short gives the Sojourn numbers for which ones are too short.
      #Those numbers correspond to the indices of revised.sojourns
      
      revised.sojourns[too.short.inds.first] <- first.neighbors[inds.first]       # Special attention to these two lines
      revised.sojourns[too.short.inds.second] <- second.neighbors[inds.second]    # These lines clarify what each variable's purpose is in merging too short sojourns with neighbors
      
  ##### Next we deal with instances where we need to combine more than 2 sojourns, since
      # that can cause weird behavior in revised.sojourns. Essentially consecutive too-short
      # Sojourns can cause the running Sojourn tab to skip numbers, then go back, then jump back
      # to where you would have expected it to be. The next few lines essentially smooth that out.
      

      # When combining Sojourns, the too-short Sojourn is labelled with the same number as its
      # preceding or succeeding Sojourn (whichever is shorter). When this works properly,
      # it presents as a repeat-and-skip in the Sojourn numbering, e.g. 4, 6, 7, 7, 9...
      # If there are two consecutive too-short Sojourns, they can each be labeled with the other's
      # number, which causes a skip-then-back in the numbering, e.g. 6, 7, 9, 8, 9...
      # To fix that, just check to see if the difference in consecutive Sojourns is negative, which
      # will indicate that multiple too-short Sojourns need to be combined. You can then accomplish
      # that by changing the number from the negative difference to match the Sojourn before it

      inds.order <- which(diff(revised.sojourns)<0) #Where are there negative differences?
      if (length(inds.order)>0)
        revised.sojourns[inds.order+1] <- revised.sojourns[inds.order] #Apply the previous Sojourn's
                                                                      #number to the Sojourn with the
                                                                     #negative difference
      
  #### Get new durations now that sojourns are combined 
      
      revised.durations      <- as.vector(tapply(durations,revised.sojourns,sum))
      # ^^^^ Bit of an abstract call here
      # basically collapsing the original durations by sum, based on the revised sojourns
      
      revised.sojourns_count <- length(revised.durations)
      revised.sojourns       <- seq_along(revised.durations) # Re-assign Sojourn numbers now that the duration of each is known

      ## Last thing to do is re-initialize the starting variables for the next pass through the loop.
      durations       <- revised.durations
      durations_count <- length(durations)
      sojourns        <- revised.sojourns
      sojourns_count  <- length(sojourns)
      
    }  # End of the big IF statement at the start of loop 1
    
    counter <- counter+1
  }	# end loop 1
  
  #####	After getting the Sojourns combined in the loop,
  ##### it's a piece of cake to format the data frame we want to return
  
  durations.1 <- rep(durations,durations)
  sojourns.1 <- rep(sojourns,durations)
  trans.table <- data.frame(sojourns=sojourns.1,durations=durations.1)
  
  return(trans.table)
}	#	end

network.shape <- function(data, sojourns = T, RAW = F, epoch = 1, verbose = T, expand = T){
  if(verbose) cat(paste('\n***\t***\t***\t***\nCalculating prediction for', data$id[1], '\n'))
  if(RAW) data$Vector.Magnitude <- data$ENMO
  
  if(!sojourns){
    data$sojourns <- rep(1:ceiling(nrow(data)/15), each = 15)[1:nrow(data)]
    cat('\nGetting 15s features...')
  } else{
  
    if(!'sojourns'%in%names(data)) {
      data <- cbind(data, get.sojourns(vm = data$Vector.Magnitude, RAW = RAW, epoch = epoch))
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

block.soj.committee <- function(Vector.Magnitude, Block.Est, Sojourns.Est, Hip = T, RAW = F, epoch = 1){
  
  ##Initialize threshold for counts variables
  threshold <-
    ifelse(Hip,
           floor(100*(epoch/60)), ## Based on 100 cpm standard
           floor(275*(epoch/5))) ## Based on Crouter 2015, 275 counts/5s
  
  if(RAW){
    threshold <- ifelse(Hip, 63.3, 35.6) ## ENMO thresholds are epoch-independent and taken
    ## from Hildebrand 2016
  }
  
  Intensity.Est     <- ''
  
  if(RAW){
    Intensity.Est <- ifelse(Vector.Magnitude<threshold&(Sojourns.Est=='Sedentary'|Block.Est=='Sedentary'), 'Sedentary', Intensity.Est)
    Intensity.Est <- ifelse(Sojourns.Est=='MVPA'&Block.Est=='MVPA', 'MVPA',Intensity.Est)
  } else{Intensity.Est     <- ifelse(Intensity.Est==''&(Sojourns.Est=='Sedentary'|Block.Est=='Sedentary'),'Sedentary',Intensity.Est)
  } # If one says it's sedentary, give sedentary
  
  Counter <- cumsum(Vector.Magnitude>threshold)
  n <- rep(tapply(Counter, Counter, length),tapply(Counter, Counter, length))
  Intensity.Est <- ifelse(Intensity.Est==''&n>=60, 'Sedentary',Intensity.Est) #If 60+ consecutive values <=threshold don't have an intensity, assign sedentary
  
  
  Counter <- cumsum(Sojourns.Est!='MVPA'|Block.Est!='MVPA')
  n <- rep(tapply(Counter, Counter, length),tapply(Counter, Counter, length))
  Intensity.Est <- ifelse(Intensity.Est==''&n>60, 'MVPA',Intensity.Est)
  
  Intensity.Est <- ifelse(Intensity.Est=='','Light',Intensity.Est) # Assign light for undetermined values
  
  Intensity.Est <- factor(Intensity.Est, levels = c('Sedentary','Light','MVPA'))  
  return(Intensity.Est)}