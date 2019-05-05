#' Invoke the original tri-axial Sojourn method
#'
#' Calls the tri-axial Sojourn method from \href{https://www.ncbi.nlm.nih.gov/pubmed/23860415}{Lyden et al. (2014)}.
#'
#' @param counts numeric vector of activity counts from the first axis
#' @param counts.2 numeric vector of activity counts from the second axis
#' @param counts.3 numeric vector of activity counts from the third axis
#' @param vect.mag vector magnitude of the activity counts
#' @param short minimum length of one Sojourn
#'
#' @return a data frame of processed data
#' @export
#'
#' @examples
#' data(example_data, package = "Sojourn")
#' if (isTRUE(requireNamespace("Sojourn.Data"))) {
#' results_3x <- soj_3x_original(
#'   example_data$axis1,
#'   example_data$axis2,
#'   example_data$axis3,
#'   example_data$Vector.Magnitude
#' )
#'
#' head(results_3x)
#' }
#'
soj_3x_original <- function(counts, counts.2,
  counts.3, vect.mag, short=30) {

  if (!requireNamespace("Sojourn.Data", quietly = TRUE)) {
    stop(paste(
      "You must install the package `Sojourn.Data`",
      "to use this function."
    ))
  }

  y <- counts
  counts.2 <- counts.2
  counts.3 <- counts.3

  inds <- 1:length(y)

  mmm <- length(y)
  one <- y[-mmm]
  two <- y[-1]

  # find transitions

  trans <- ((one-two)>15)&(two<=10) 	# this is how i find initial transitions

  trans <- c(0,trans)

  trans.inds <- (1:mmm)[trans==1]

  # how long between transistions

  durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]

  #	put first duration in and make last trans go till end of file

  dd <- length(durations)
  tt <- length(trans.inds)
  durations[dd+1] <- mmm-trans.inds[tt]
  dd <- length(durations)
  durations.junk <- trans.inds[1]
  durations <- c(durations.junk,durations)
  dd <- length(durations)

  durations.compare <- durations
  length(durations.compare)

  # get number of sojourns

  sojourns <- rep(1:length(durations),1)
  sojourns.long <- rep(sojourns,durations)
  mean.cnts.soj <- as.vector(tapply(y,sojourns.long,mean))

  # combine too short sojourns.

  #	combine too short sojourns with neighboring sojourn.
  # 	this loop repeats until there are no more too short sojourns

  counter <- 1

  repeat { # loop 1

    too.short <- (1:dd)[durations<short]
    ts <- length(too.short)

    if(length(too.short)==0)
      break

    if(length(too.short)>0) {

      # this loop deals with instances where the first too.short sojourn is
      # first sojourn of file ie. it only has a second neighbor to combine it
      # with

      counter.1 <- 1

      repeat { # loop 2

        if (too.short[counter.1]==counter.1) {

          sojourns[1:counter.1] <- sojourns[counter.1+1]

          counter.1 <- counter.1+1

        }

        if (too.short[counter.1]!=counter.1)
          break

      }	# end loop 2

      s <- length(sojourns)

      # this loop deals with if last too short sojourn is last sojourn of file
      # ie. it only has a first neighbor to combine it with

      counter.2 <- s
      counter.ts <- ts

      repeat {

        if (too.short[counter.ts]==counter.2) {

          sojourns[counter.2:s] <- sojourns[counter.2-1]

          counter.2 <- counter.2-1
          counter.ts <- counter.ts-1

        }

        if (too.short[counter.ts]!=counter.2)
          break

      }	#end loop 3

      s <- length(sojourns)

      # now deal with all other too short sojourns

      junk.too.short <- too.short

      if(counter.ts<ts-1) {
        junk.too.short <- too.short[-(counter.ts+1:ts)]
      }

      if (counter.1>1) {
        junk.too.short <- junk.too.short[-(1:counter.1-1)]
      }

      j.t.s <- length(junk.too.short)

      first.neighbors <- junk.too.short-1
      second.neighbors <- junk.too.short+1

      #	right now i combine too short sojourns with its neighbor that was
      #	shorter in duration (e.g. first neighbor = 60 seconds long and second
      #	neighbor = 300 seconds long, it gets combined with first neighbor)

      revised.sojourns <- sojourns

      durations[junk.too.short]

      durations.first.neighbors <- durations[first.neighbors]
      durations.second.neighbors <- durations[second.neighbors]

      #	put in dummy duration for too.short sojourns at beginning and end of file
      durations.first.neighbors[is.na(durations.first.neighbors)] <- 100000
      durations.second.neighbors[is.na(durations.second.neighbors)] <- 100000

      n.neighbors <- length(durations.first.neighbors)
      n.neighbors.2 <- length(durations.second.neighbors)

      inds.first <- (1:n.neighbors)[
        durations.first.neighbors<=durations.second.neighbors
      ]
      inds.second <- (1:n.neighbors)[
        durations.first.neighbors>durations.second.neighbors
      ]

      too.short.inds.first <- junk.too.short[inds.first]
      too.short.inds.second <- junk.too.short[inds.second]

      revised.sojourns[too.short.inds.first] <- first.neighbors[inds.first]
      revised.sojourns[too.short.inds.second] <- second.neighbors[inds.second]

      # deal with instances where need to combine more than 2 sojourns - i.e.
      # short sojourn became first neighbor, and then sojourn before first
      # neighbor also becomes that sojourn via second neighbor grouping - want
      # all 3 of these sojourns to be combined.

      rs <- length(revised.sojourns)

      one.order <- revised.sojourns[-rs]
      two.order <- revised.sojourns[-1]

      o <- length(one.order)

      inds.order <- (1:o)[one.order>two.order]
      if (length(inds.order>0))
        revised.sojourns[inds.order+1] <- revised.sojourns[inds.order]

      # get new durations now that sojourns are combined

      rs <- length(revised.sojourns)
      revised.durations <- as.vector(tapply(durations,revised.sojourns,sum))

      rd <- length(revised.durations)

      # get new sojourns now that durations are combined

      revised.sojourns <- rep(1:length(revised.durations),1)
      rs <- length(revised.sojourns)

      durations <- revised.durations
      dd <- length(durations)
      sojourns <- revised.sojourns
      s <- length(sojourns)

    }

    #	print(counter)
    counter <- counter+1

  }	# end loop 1

  #	 make table of durations and sojourns etc

  trans.table <- data.frame(
    counts = y,
    counts.2 = counts.2,
    counts.3 = counts.3,
    vect.mag = vect.mag,
    sojourns = 0,
    durations = 0,
    perc.soj = NA,
    soj.type.all = NA,
    soj.mets.all = NA
  )

  tt <- dim(trans.table)[1]
  durations.1 <- rep(durations,durations)
  sojourns.1 <- rep(sojourns,durations)

  trans.table$durations <- durations.1
  trans.table$sojourns <- sojourns.1

  #	get percent non zero in table

  perc.soj <- tapply(y>0,sojourns.1,mean)

  perc.soj <- rep(perc.soj,durations)

  trans.table$perc.soj <- perc.soj


  ### get inds.inactivities so can test nnet only to distinguish between
  ### lifestyle and sedentary

  #	now get inactivity indices

  inds.inacts <- (1:tt)[trans.table$perc.soj<0.7]
  inactivities <- trans.table[inds.inacts,]
  i.a <- dim(inactivities)[1]

  inact.trans.inds <- c(1,(1+(1:i.a)[
    inactivities$sojourns[-1]!=inactivities$sojourns[-i.a]])
  )

  inact.durations <- inactivities$durations[inact.trans.inds]

  #	get nnetinputs for vertical axis

  nnetinputs <- as.vector(
    unlist(
      tapply(
        inactivities$counts,
        inactivities$sojourns,
        quantile,
        probs=c(.1,.25,.5,.75,.9)
      )
    )
  )
  length(nnetinputs)
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

    counts <- inactivities$counts[inactivities$sojourns==soj]

    if (sum(counts)>0) {

      temp <- acf(counts,lag.max=1,plot=F)
      nnetinputs$acf[g] <- as.numeric(unlist(temp[1,1])[1])

    }

    g <- g+1
    #	print(g)

  }

  nnetinputs$acf[is.na(nnetinputs$acf)] <- mean(
    nnetinputs$acf,
    na.rm=TRUE
  )

  ####	get nnetinputs.2 - second axis

  nnetinputs.2 <- as.vector(
    unlist(
      tapply(
        inactivities$counts.2,
        inactivities$sojourns,
        quantile,
        probs=c(.1,.25,.5,.75,.9)
      )
    )
  )
  length(nnetinputs.2)
  nnetinputs.2 <- matrix(
    nnetinputs.2,
    length(nnetinputs.2)/5,5,
    byrow=TRUE
  )
  nnetinputs.2 <- as.data.frame(nnetinputs.2)
  names(nnetinputs.2) <- c("X10.2","X25.2","X50.2","X75.2","X90.2")
  nnetinputs.2$acf.2 <- 0

  g <- 1
  for (soj in unique(inactivities$sojourns)) {

    counts <- inactivities$counts.2[inactivities$sojourns==soj]

    if (sum(counts)>0) {

      temp <- acf(counts,lag.max=1,plot=F)
      nnetinputs.2$acf.2[g] <- as.numeric(unlist(temp[1,1])[1])

    }
    g <- g+1
    #	print(g)
  }

  nnetinputs.2$acf.2[is.na(nnetinputs.2$acf.2)] <- mean(
    nnetinputs.2$acf.2,
    na.rm=TRUE
  )


  ####get nnetinputs.3 - third axis

  nnetinputs.3 <- as.vector(
    unlist(
      tapply(
        inactivities$counts.3,
        inactivities$sojourns,
        quantile,
        probs=c(.1,.25,.5,.75,.9)
      )
    )
  )
  length(nnetinputs.3)
  nnetinputs.3 <- matrix(
    nnetinputs.3,
    length(nnetinputs.3)/5,
    5,
    byrow=TRUE
  )
  nnetinputs.3 <- as.data.frame(nnetinputs.3)
  names(nnetinputs.3) <- c("X10.3","X25.3","X50.3","X75.3","X90.3")
  nnetinputs.3$acf.3 <- 0

  g <- 1
  for (soj in unique(inactivities$sojourns)) {

    counts <- inactivities$counts.3[inactivities$sojourns==soj]

    if (sum(counts)>0) {

      temp <- acf(counts,lag.max=1,plot=F)
      nnetinputs.3$acf.3[g] <- as.numeric(unlist(temp[1,1])[1])

    }
    g <- g+1
    #print(g)
  }

  nnetinputs.3$acf.3[is.na(nnetinputs.3$acf.3)] <- mean(
    nnetinputs.3$acf.3,
    na.rm=TRUE
  )

  ####get nnetinputs.vm - vector magnitude

  nnetinputs.vm <- as.vector(
    unlist(
      tapply(
        inactivities$vect.mag,
        inactivities$sojourns,
        quantile,
        probs=c(.1,.25,.5,.75,.9)
      )
    )
  )
  length(nnetinputs.vm)
  nnetinputs.vm <- matrix(
    nnetinputs.vm,
    length(nnetinputs.vm)/5,
    5,
    byrow=TRUE
  )
  nnetinputs.vm <- as.data.frame(nnetinputs.vm)
  names(nnetinputs.vm) <- c("X10.vm","X25.vm","X50.vm","X75.vm","X90.vm")
  nnetinputs.vm$acf.vm <- 0

  g <- 1
  for (soj in unique(inactivities$sojourns)) {

    counts <- inactivities$vect.mag[inactivities$sojourns==soj]

    if (sum(counts)>0) {

      temp <- acf(counts,lag.max=1,plot=F)
      nnetinputs.vm$acf.vm[g] <- as.numeric(unlist(temp[1,1])[1])

    }
    g <- g+1
    #print(g)
  }

  nnetinputs.vm$acf.vm[is.na(nnetinputs.vm$acf.vm)] <- mean(
    nnetinputs.vm$acf.vm,
    na.rm=TRUE
  )

  #	combine inputs so can center and scale

  inputs <- cbind(nnetinputs,nnetinputs.2)
  inputs <- cbind(inputs,nnetinputs.3)
  inputs <- cbind(inputs,nnetinputs.vm)
  inputs <- cbind(inputs,inact.durations)

  inputs <- scale(
    inputs,
    center = Sojourn.Data::cent.1,
    scale = Sojourn.Data::scal.1
  )
  inputs <- as.data.frame(inputs)

  #	predict type using all axes + vm.  i intially had a lot of prediction nnets
  #	here (ie different axis) but have removed them and only include the one that
  #	looks "the best".  there are definitely others we can use/try

  #	remove NA's

  inputs.1 <- inputs[,-(13)]
  inputs.1 <- inputs.1[,-(1:2)]

  cool.all <- predict(
    Sojourn.Data::class.nnn.6,
    inputs.1
  )

  #	add soj.type to trans table

  junk.cool.all <- as.vector(apply(cool.all,1,which.max))

  cool.all <- rep(junk.cool.all,inact.durations)

  trans.table$soj.type.all[inds.inacts] <- cool.all
  #	assign mets to types.

  trans.table$soj.mets.all[
    (trans.table$soj.type.all==1)&(trans.table$perc.soj<=0.12)
  ] <- 1.5
  trans.table$soj.mets.all[
    (trans.table$soj.type.all==1)&(trans.table$perc.soj>0.12)
  ] <- 1.7
  trans.table$soj.mets.all[
    (trans.table$soj.type.all==3)&(trans.table$perc.soj<=0.05)
  ] <- 1
  trans.table$soj.mets.all[
    (trans.table$soj.type.all==3)&(trans.table$perc.soj>0.05)
  ] <- 1.2

  #	this identifies activities for nnet all - 6 means activity

  trans.table$soj.type.all[trans.table$perc.soj>=0.7] <- 6

  inds.activity.all <- (1:tt)[
    (trans.table$perc.soj>=0.7)|
      (trans.table$soj.type.all==2)|
      (trans.table$soj.type.all==4)
  ]

  act.trans.table.all <- trans.table[inds.activity.all,]
  dim(act.trans.table.all)
  activity.durations.all <- table(act.trans.table.all$sojourns)

  quantiles.all <- tapply(
    act.trans.table.all$counts,
    act.trans.table.all$sojourns,
    quantile,
    p = c(.1,.25,.5,.75,.9)
  )
  nn.trans.table.all <- as.data.frame(
    do.call("rbind",quantiles.all)
  )

  #	i realize i am getting lag1 differently than i do for inactivities...i should change to use function throughout.
  nn.trans.table.all$acf <- tapply(
    act.trans.table.all$counts,
    act.trans.table.all$sojourns,
    acf.lag1
  )
  nn.trans.table.all <- nn.trans.table.all[,c(1:6)]

  names(nn.trans.table.all) <- c("X10.","X25.","X50.","X75.","X90.","acf")

  nnetinputs.acts.all <- scale(
    nn.trans.table.all,
    center = Sojourn.Data::cent,
    scale = Sojourn.Data::scal
  )

  #	predict METs

  act.mets.all <- predict(
    Sojourn.Data::reg.nn,
    nnetinputs.acts.all
  )
  act.mets.all <- rep(act.mets.all,activity.durations.all)

  #	put back in table

  trans.table$soj.mets.all[inds.activity.all] <- act.mets.all

  #	get breaks from sitting

  #	trans.table$do.breaks <- 0
  trans.table$soj.breaks.all <- 0

  soj.posture <- as.vector(trans.table$soj.mets.all)
  s.p <- length(soj.posture)

  soj.one.posture <- soj.posture[-s.p]
  soj.two.posture <- soj.posture[-1]

  soj.trans <- (soj.one.posture<1.5)&(soj.two.posture>=1.5)
  soj.trans <- c(0,soj.trans)
  soj.trans.inds <- (1:s.p)[soj.trans==1]

  trans.table$soj.breaks.all <- soj.trans
  #	sum(trans.table$soj.breaks.all)

  names(trans.table)[8:10] <- c("type","METs","break")

  trans.table <- trans.table[,-c(8,10)]

  return(trans.table)

}
