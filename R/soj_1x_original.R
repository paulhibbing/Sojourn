#' Invoke the original uni-axial Sojourn method
#'
#' Calls the uni-axial Sojourn method from \href{https://www.ncbi.nlm.nih.gov/pubmed/23860415}{Lyden et al. (2014)}.
#'
#' @param counts numeric vector of vertical axis counts
#' @param perc.cut decision tree threshold 1
#' @param perc.cut.2 decision tree threshold 2
#' @param perc.cut.3 decision tree threshold 3
#' @param too.short minimum length of one Sojourn
#' @param sit.cut cut-point for determining sitting
#' @param long.soj maximum length of one Sojourn
#'
#' @return a data frame of processed data
#' @export
#'
#' @examples
#' data(example_data, package = "Sojourn")
#' results_1x <- soj_1x_original(example_data$axis1)
#' \dontrun{
#' head(results_1x)
#' }
#'
soj_1x_original <- function(counts,perc.cut=0.05,perc.cut.2=0.12,
  perc.cut.3=0.55,too.short=10,sit.cut=90,long.soj=120) {

  y <- counts
  # identify sojourns.
  inds <- 1:length(y)

  mmm <- length(y)
  one <- y[-mmm]
  two <- y[-1]

  # transitions from 0 to >0
  trans.up <- (one==0)&(two>0)
  # transitions from >0 to 0
  trans.down <- (one>0)&(two==0)

  trans <- c(0,trans.up+trans.down)
  trans.inds <- (1:mmm)[trans==1]

  # indices where transitions take place
  trans.inds <- c(1,trans.inds,(mmm+1))

  # how long are the sojourns and the zeros
  durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]

  # identify if interval is zeros or >0s (they alternate)
  type <- rep("zeros",length=length(durations))
  if (y[1]==0)
    type <- rep(c("zeros","act"),length=length(durations))
  if (y[1]>0)
    type <- rep(c("act","zeros"),length=length(durations))

  soj.table <- data.frame(type,durations,trans.inds=trans.inds[-length(trans.inds)])

  soj.table$act.type.1 <- "undetermined"
  soj.table$act.type.1[(soj.table$type=="zeros")&(soj.table$durations>sit.cut)] <- "sedentary"
  soj.table$act.type.1[(soj.table$type=="act")&(soj.table$durations>too.short)] <- "activity"



  # combine neighboring undetermineds
  mmm <- dim(soj.table)[1]
  prev.was.undet.inds <-
    (2:mmm)[(soj.table$act.type.1[2:mmm]=="undetermined")&
        (soj.table$act.type.1[1:(mmm-1)]=="undetermined")]
  if (length(prev.was.undet.inds)>0)
    rev.soj.table <- soj.table[-prev.was.undet.inds,]
  mmm <- dim(rev.soj.table)[1]

  rev.soj.table$durations <-
    c((rev.soj.table$trans.inds[-1]-
        rev.soj.table$trans.inds[-mmm]),
      rev.soj.table$durations[mmm])

  mmm <- dim(rev.soj.table)[1]

  # find too short undetermineds
  too.short.undet.inds <- (1:mmm)[(rev.soj.table$durations<too.short)&(rev.soj.table$act.type.1=="undetermined")]

  if (length(too.short.undet.inds)>0)
  {
    while (too.short.undet.inds[1]==1)
    {
      too.short.undet.inds <- too.short.undet.inds[-1]
      rev.soj.table <- rev.soj.table[-1,]
      rev.soj.table$trans.inds[1] <- 1
      mmm <- dim(rev.soj.table)[1]
      too.short.undet.inds <- too.short.undet.inds-1
    }

    last <- length(too.short.undet.inds)
    while (too.short.undet.inds[last]==mmm)
    {
      too.short.undet.inds <- too.short.undet.inds[-last]
      junk <- rev.soj.table$durations[(mmm-1)]
      rev.soj.table <- rev.soj.table[-mmm,]
      mmm <- dim(rev.soj.table)[1]
      rev.soj.table$durations[mmm] <- junk+rev.soj.table$durations[mmm]
      last <- length(too.short.undet.inds)
    }

    # short undetermineds between two acts of same type
    to.delete.inds <-
      (too.short.undet.inds)[rev.soj.table$act.type.1[too.short.undet.inds-1]==rev.soj.table$act.type.1[too.short.undet.inds+1]]
    done.inds <- (1:length(too.short.undet.inds))[rev.soj.table$act.type.1[too.short.undet.inds-1]==rev.soj.table$act.type.1[too.short.undet.inds+1]]
    too.short.undet.inds <- too.short.undet.inds[-done.inds]

    # between two acts of different types
    junk <- rev.soj.table[too.short.undet.inds,]

    junk$act.type.1 <- "sedentary"
    junk$act.type.1[junk$type=="act"] <- "activity"
    rev.soj.table[too.short.undet.inds,] <- junk

    rev.soj.table <- rev.soj.table[-to.delete.inds,]


  }


  mmm <- dim(rev.soj.table)[1]
  junk <- c(rev.soj.table$act.type.1[2:mmm]==rev.soj.table$act.type.1[1:(mmm-1)])
  same.as.prev.inds <- (2:mmm)[junk]
  if (length(same.as.prev.inds)>0)
  {
    rev.soj.table <- rev.soj.table[-same.as.prev.inds,]
    mmm <- dim(rev.soj.table)[1]
    rev.soj.table$durations <-
      c((rev.soj.table$trans.inds[-1]-
          rev.soj.table$trans.inds[-mmm]),
        rev.soj.table$durations[mmm])
    last.obs <- rev.soj.table$durations[mmm]-1+rev.soj.table$trans.inds[mmm]

    if (last.obs != length(y))
      rev.soj.table$durations[mmm] <- length(y)-rev.soj.table$trans.inds[mmm]+1

  }

  trans.inds <- c(rev.soj.table$trans.inds,length(y)+1)
  durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]

  soj.table <- data.frame(durations)

  sojourns <- rep(1:length(soj.table$durations),soj.table$durations)
  perc.gt.0 <- tapply(y>0,sojourns,mean)

  soj.table$perc.gt.0 <- perc.gt.0

  soj.table$revised.type <- "sit.still"
  soj.table$revised.type[soj.table$perc.gt.0>perc.cut.3] <- "activity"
  soj.table$revised.type[(soj.table$perc.gt.0>perc.cut)&(soj.table$perc.gt.0<=perc.cut.2)&(soj.table$durations>sit.cut)] <- "sit.move"
  soj.table$revised.type[(soj.table$perc.gt.0>perc.cut)&(soj.table$perc.gt.0<=perc.cut.2)&(soj.table$durations<=sit.cut)] <- "stand.still"
  soj.table$revised.type[(soj.table$perc.gt.0>perc.cut.2)&(soj.table$perc.gt.0<=perc.cut.3)] <- "stand.small.move"

  durations <- soj.table$durations
  type <- soj.table$revised.type

  sojourns <- rep(1:length(durations),durations)
  type <- rep(type,durations)
  perc.gt.0 <- rep(perc.gt.0,durations)
  durations <- rep(durations,durations)
  nnn <- length(sojourns)

  longer.acts <- unique(sojourns[(durations>(long.soj-1))])

  f <- function(s)
  {
    dur <- 	unique(durations[sojourns==s])
    sub.sojourns <- rep(1:floor(dur/(long.soj/2)),
      times=c(rep((long.soj/2),floor(dur/(long.soj/2))-1),
        dur-(floor(dur/(long.soj/2))-1)*(long.soj/2)))
    sub.sojourns <- s + sub.sojourns/(max(sub.sojourns)+1)
    return(sub.sojourns)
  }
  new.values <- sapply(longer.acts,f)
  starts <- sapply(match(longer.acts,sojourns),paste,":",sep="")
  ends <- length(sojourns) - match(longer.acts,rev(sojourns)) + 1
  indices <- mapply(paste,starts,ends,MoreArgs=list(sep=""),USE.NAMES=F)
  indices <- unlist(lapply(parse(text = indices), eval))
  sojourns[indices] <- unlist(new.values)

  # apply METs to zeros
  METs <- rep(NA,length(type))
  METs[(type=="sit.still")] <- 1
  METs[(type=="sit.move")] <- 1.2
  METs[(type=="stand.still")] <- 1.5
  METs[(type=="stand.small.move")] <- 1.7


  data <- data.frame(counts=y,sojourns=sojourns,durations=durations,type=type,METs=METs,perc.gt.0=perc.gt.0)

  # prepare to apply nnet to the activity sojourns
  nnn <- dim(data)[1]
  act.inds <- (1:nnn)[(data$type=="activity")]
  act.data <- data[act.inds,]
  act.durations <- table(act.data$sojourns)

  quantiles <- tapply(act.data$counts,act.data$sojourns,quantile,p=c(.1,.25,.5,.75,.9))
  nn.data <- as.data.frame(do.call("rbind",quantiles))
  nn.data$acf <- tapply(act.data$counts,act.data$sojourns,acf.lag1)
  nn.data <- nn.data[,c(1:6)]

  names(nn.data) <- c("X10.","X25.","X50.","X75.","X90.","acf")

  nnetinputs <- scale(nn.data,center=cent,scale=scal)

  # apply nnet and put it back into the dataset
  est.mets.1 <- NA #predict(MA.reg.nn,nnetinputs)
  est.mets.2 <- predict(ALL.reg.nn,nnetinputs)

  #act.mets.1 <- rep(est.mets.1,act.durations)
  act.mets.2 <- rep(est.mets.2,act.durations)

  data$METs <- METs
  data$METs.2 <- METs

  data$METs[act.inds] <- act.mets.2
  data$METs.2[act.inds] <- act.mets.2

  data$level <- "sed"
  data$level[data$METs>=1.5] <- "light"
  data$level[data$METs>=3] <- "mod"
  data$level[data$METs>=6] <- "vig"
  data$level <- factor(data$level,levels=c("sed","light","mod","vig"))

  data$level.2 <- "sed"
  data$level.2[data$METs.2>=1.5] <- "light"
  data$level.2[data$METs.2>=3] <- "mod"
  data$level.2[data$METs.2>=6] <- "vig"
  data$level.2 <- factor(data$level.2,levels=c("sed","light","mod","vig"))
  n <- dim(data)[1]
  inds <- (1:n)[data$METs<1]
  data$METs[inds] <- 1

  data <- data[,c(1,2,3,4,5,6,8)]
  data
}
