youth_sojourn_tree <- function(Vector.Magnitude, Block.Est, Sojourns.Est, Hip = T, RAW = F, epoch = 1){

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
