#' Summarize outcomes from data processed using the Sojourn method
#'
#' A function to summarize predictions made by the original Sojourn method of
#' \href{https://www.ncbi.nlm.nih.gov/pubmed/23860415}{Lyden et al. (2014)}.
#'
#' @param est.mets numeric vector of predicted metabolic equivalents
#' @param units time units associated with each row of data
#'
#' @return a data frame summarizing the predictions made by the Sojourn method.
#' @export
#'
#' @examples
#' data(example_data, package = "Sojourn")
#' example_data <-
#'   soj_3x_original(
#'     example_data$axis1,
#'     example_data$axis2,
#'     example_data$axis3,
#'     example_data$Vector.Magnitude
#'   )
#'
#' compute.bouts.info(example_data$METs)
#'
compute.bouts.info <- function(est.mets, units="secs") {
  # est.mets is a vector of estimated METs
  # units = "secs" or "mins" - the amount of time each entry in est.mets represents
  if(units == "secs") {
    time.units <- 60
  } else {
    time.units <- 1
  }

  mets.length <- length(est.mets)
  inds <- 1:mets.length
  one <- est.mets[-mets.length]
  two <- est.mets[-1]

  # number of transitions from <1.5 to >=1.5
  sed.to.gt.sed.trans <- sum((one<1.5)&(two>=1.5))

  # transitions from <3 to >=3
  trans.up <- (one<3)&(two>=3)

  # transitions from >=3 to <3
  trans.down <- (one>=3)&(two<3)
  trans <- c(0,trans.up+trans.down)
  trans.inds <- (1:mets.length)[trans==1]

  # indices where transitions take place
  trans.inds <- c(1, trans.inds, (mets.length+1))

  # how long are the periods of activity and inactivity
  durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]

  # identify if interval is activity or inactivity (they alternate)
  types <- rep("inactive",length=length(durations))

  if (est.mets[1]<3)
    types <- rep(c("inactive","active"),length=length(durations))
  if (est.mets[1]>=3)
    types <- rep(c("active","inactive"),length=length(durations))

  # Create some empty vectors which will be used to keep track of the
  # start and end points of the bouts in the durations vector.
  bout.starts <- c()
  bout.ends <- c()

  # Bouts can occur in two ways:
  # 1) Multiple periods of >3 MET activity with one or more short periods or low activity in between.
  #    The combined time of low activity is 2 minutes or less and the total time 10 minutes or more.
  # 2) A period of 10 or more uninterrupted minutes of >3 MET activity with large periods of low activity before and after.

  # Search for bouts of the first type:

  # Find all sets of adjacent periods of inactivity with total duration less than 2 minutes.
  indices <- seq_len(length(durations))[types=="inactive"]

  for(i in indices) {
    # amount of inactive time in the current possible-bout
    current.bout.inactive.time <- 0
    # index of the last inactive period that will be included in the current possible-bout
    j <- i

    # add inactive periods to the right of the current starting index of our possible-bout,
    # until adding another would put us over the 2-minute limit
    nextvalue <- durations[i]
    while(current.bout.inactive.time + nextvalue <= 2*time.units) {
      current.bout.inactive.time <- current.bout.inactive.time + nextvalue
      j <- j + 2
      if( j <= length(durations) ) {
        # if we haven't yet reached the end of the durations vector,
        # increment j and get the next value
        nextvalue <- durations[j]
      } else {
        # if we have reached the end of the durations vector,
        # set nextvalue to a large number so we'll exit the loop
        nextvalue <- 2*time.units + 1
      }
    }
    # correct the value of j - we really didn't want to increment it that last time
    # since we ended up not including the corresponding inactive period in our possible-bout.
    j <- j - 2

    # if this possible bout would have already been found by starting from an earlier index, forget about it
    if(i > 2) {
      if(current.bout.inactive.time + durations[i - 2] <= 2*time.units) {
        current.bout.inactive.time <- 0
      }
    }

    # if we found a possible bout, record that information
    if(current.bout.inactive.time > 0) {
      # save the start position of the bout in the durations vector
      # (the bout starts at the period of activity preceeding the period of inactivity located at index i)
      # (unless i = 1, when there is no preceeding period of activity)
      if(i > 1) {
        bout.starts <- c(bout.starts, (i - 1))
      } else {
        bout.starts <- c(bout.starts, 1)
      }

      # save the end position of the bout in the durations vector
      # (the bout ends at the period of activity following the period of inactivity located at index j)
      # (unless j = length(durations), when there is no following period of activity)
      if(j < length(durations)) {
        bout.ends <- c(bout.ends, (j + 1))
      } else {
        bout.ends <- c(bout.ends, j)
      }
    }
  }


  # Out of the possible bouts located above, keep only those with total time of at least 10 minutes.
  keepers <- c()
  for(i in seq_len(length(bout.starts))) {
    if(sum(durations[bout.starts[i]:bout.ends[i]]) >= 10*time.units) {
      keepers <- c(keepers, i)
    }
  }

  bout.starts <- bout.starts[keepers]
  bout.ends <- bout.ends[keepers]


  # Check to see if any of the possible bouts above have overlapping start and end indices.
  # If so, keep the first and eliminate those that overlap with it.
  i <- 1
  while(i < length(bout.starts)) {
    if( bout.starts[i + 1] <= bout.ends[i] ) {
      bout.starts <- bout.starts[-(i + 1)]
      bout.ends <- bout.ends[-(i + 1)]
    } else {
      i <- i + 1
    }
  }



  # Search for bouts of the second type:
  indices <- seq_len(length(durations))[types=="active"]

  for(i in indices) {
    if(durations[i] >= 10*time.units) {
      # Is this a type 2 bout?  it might be..
      is.bout <- TRUE

      # If this period of activity is preceeded by a period of inactivity,
      # check to see how long that period of inactivity was.  If it was short,
      # this is a type 1 bout and will have been located above already
      if(i > 1) {
        if(durations[i - 1] <= 2*time.units) {
          is.bout <- FALSE
        }
      }

      # If this period of activity is followed by a period of inactivity,
      # check to see how long that period of inactivity was.  If it was short,
      # this is a type 1 bout and will have been located above already
      if(i < length(durations)) {
        if(durations[i + 1] <= 2*time.units) {
          is.bout <- FALSE
        }
      }

      # If this turned out to be a type 2 bout, add it to bout.starts and bout.ends
      if(is.bout) {
        bout.starts <- c(bout.starts, i)
        bout.ends <- c(bout.ends, i)
      }
    }
  }

  # Convert the values in bout.starts from indices in the durations vector
  # to the corresponding indices in the est.mets vector, and combine the values
  # into one vector to be used to extract the relevant seconds from est.mets
  indices <- c()

  for(i in seq_len(length(bout.starts))) {
    bout.starts[i] <- sum( durations[seq_len( bout.starts[i] - 1 )] ) + 1
    bout.ends[i] <- sum( durations[seq_len( bout.ends[i] )] )
    indices <- c(indices, bout.starts[i]:bout.ends[i])
  }

  num.bouts <- length(bout.starts)
  bout.hours <- length(indices)/(60*time.units)
  bout.MET.hours <- sum(est.mets[indices])/(60*time.units)
  info <- data.frame(
    num.bouts=num.bouts,
    bout.hours=bout.hours,
    bout.MET.hours=bout.MET.hours,
    sed.to.gt.sed.trans=sed.to.gt.sed.trans
  )

  return(info)
}
