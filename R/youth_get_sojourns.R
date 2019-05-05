#' Label Sojourns in a data stream according to the youth-specific algorithm
#'
#' Identify Sojourns using the algorithm of \href{https://www.ncbi.nlm.nih.gov/pubmed/29135657}{Hibbing et al. (2018)}
#'
#' @param vm vector of triaxial accelerometer values, either the vector
#'   magnitude for activity counts, or the Euclidian Norm Minus One for raw
#'   acceleration
#' @param short numeric scalar. Shortest allowable duration for a Sojourn.
#'   Should be detected automatically from the internally-stored grid search
#'   values for the attachment site (hip or wrist) and data type (counts or raw)
#' @param Output the data type (counts or raw)
#' @param Site the attachment site (hip or wrist)
#' @param epoch the epoch length, in seconds
#' @param difference the difference parameter
#' @param threshold the threshold parameter
#' @param verbose logical. Print processing updates to the console?
#'
#' @return A data frame (with \code{nrow} equal to \code{length(vm)}) that gives
#'   sojourn labels and durations
#' @export
#'
#' @examples
#' data(example_data, package = "Sojourn")
#' if (isTRUE(requireNamespace("Sojourn.Data"))) {
#' get_youth_sojourns(example_data$Vector.Magnitude,
#'   Output = "Counts", Site = "Hip")
#' }
get_youth_sojourns <- function(vm,short=30, Output = c("Counts", "Raw"),
  Site = c("Hip", "Wrist"), epoch = 1, difference = 15,
  threshold = 100, verbose = FALSE) {

  if (!requireNamespace("Sojourn.Data", quietly = TRUE)) {
    stop(paste(
      "You must install the package `Sojourn.Data`",
      "to use this function."
    ))
  }

  Output <- match.arg(Output, c("Counts", "Raw", "Error"))
  Site <- match.arg(Site, c("Hip", "Wrist", "Error"))

  #Preliminary note: The epoch parameter isn't used currently. It's in there in
  #case this needs to be adapted so it can scale to other epoch lengths.

  #For the purposes of initial development I'm not going to implement that.

  #The Output argument will eventually be used to select the appropriate
  #thresholds. Same with the Site argument

  if (all(length(Site) == 1, length(Output) == 1)) {

    theGrid <- Sojourn.Data::youth_grids[
      with(
        Sojourn.Data::youth_grids,
        paste(Site, Output)
      )==paste(Site, Output),
    ]

    if (nrow(theGrid)!=1) {
      message(paste(
        "Unable to identify exactly one matching",
        "grid. Returning NULL."
      ))
      return(NULL)
    }

    if (verbose)  cat(messager(1, theGrid = theGrid))

    difference <- theGrid$Difference
    threshold <- theGrid$Threshold
    short <- theGrid$Short

  } else{

    message(paste(
      "Site and Output arguments not entered",
      "correctly. Using defaults."
    ))

  }


  #### Set up variables
  y   <- vm
  inds <- seq_along(y)
  total_observations  <- length(y)
  one <- y[-total_observations]
  two <- y[-1]

  #### Find transitions
  trans <- ((one-two)>difference)&(two<=threshold)
  # ^^ where are differences in second-by-second counts >15
  # ^^ and coming down to <10, i.e. can't use diff() because I need the sign

  trans <- c(0,trans)
  # ^^ Adding the first frame back in and initializing to zero (converting
  # ^^ logical values to 0's and 1's as well)

  trans.inds <- which(trans==1)
  # ^^ Indices of the transitions

  if (length(trans.inds)==0){

    message(paste(
      "All data fit within one Sojourn.",
      "You should follow up with manual inspection."
    ))

    trans.table <- data.frame(
      sojourns=1,
      durations=rep(length(y), length(y))
    )

    return(trans.table)

  }

  trans_count <- length(trans.inds)
  # ^^ Total number of transitions

  #### List duration of each sojourn

  durations <- diff(trans.inds)*epoch
  durations_count <- length(durations)

  durations <- c(
    durations,
    (total_observations-trans.inds[trans_count])*epoch
  )
  # ^^ Adds the duration of the last sojourn

  durations_count <- length(durations)

  first.trans <- trans.inds[1]
  durations <- c(first.trans*epoch,durations)
  # ^^ Adds the duration of the first sojourn

  durations_count <- length(durations)

  #### Get number of sojourns, initialize sojourn markers, and grab mean counts
  #### from each sojourn
  ## NOTE: Some of these are only valuable after the last time
  ## through the loops
  sojourns <- seq_along(durations)
  sojourns_count <- length(sojourns)

  #####	combine too short sojourns with neighboring sojourn.
  ###### 	this loop repeats until there are no more too-short sojourns

  counter <- 1

  repeat {

    too.short <- which(durations<short)
    too.short_count <- length(too.short)

    if (too.short_count == 0)  break

    if (too.short_count > 0) {

      counter.1 <- 1 #Variable just for loop 2

      #### Loop 2: Combine all Sojourns that occur before the first full-length
      #### Sojourn
      repeat{

        if (too.short[counter.1]==counter.1) {

          sojourns[1:counter.1] <- sojourns[counter.1+1]

          counter.1 <- counter.1+1

        }

        if (too.short[counter.1]!=counter.1 | is.na(too.short[counter.1])) {

          break

        }

      }	# end loop 2

      counter.2 <- sojourns_count #Variable just for loop 3
      counter.too.short_count <- too.short_count #Variable used beyond loop 3

      #### Loop 3: Combine all Sojourns that occur after the last full-length Sojourn
      repeat {

        # This loop deals with if last too short sojourn is last sojourn of file
        # (i.e. it only has a first neighbor to combine it with)

        if (too.short[counter.too.short_count]==counter.2) {

          sojourns[counter.2:sojourns_count] <- sojourns[counter.2 - 1]

          counter.2 <- counter.2 - 1
          counter.too.short_count <- counter.too.short_count-1

          if (counter.too.short_count==0)  break
            ## ^^ For when the first Sojourn that's too short comes at the end
            ## ^^ of the file

        }

        if (too.short[counter.too.short_count] != counter.2) break

      }	#end loop 3

      ##### now deal with all other too short sojourns
      junk.too.short <- too.short
      ### ^^ Need a copy of the variable that tells which ones are too short.

      if (counter.too.short_count<too.short_count-1) {

        junk.too.short <- too.short[
          -(counter.too.short_count+1:too.short_count)
        ]

      }
      ## ^^^^ Takes the last one out if it's already
      ## ^^^^ been manipulated in loop 3

      if (counter.1>1){

        junk.too.short <- junk.too.short[-(1:counter.1-1)]

      }
      ## ^^^^ Takes the first one out if it's already been manipulated in loop 2

      j.t.s <- length(junk.too.short) # How many in the middle are too short?

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

      inds.first <- which(
        durations.first.neighbors<=durations.second.neighbors
      )
      inds.second <- which(
        durations.first.neighbors>durations.second.neighbors
      )

      too.short.inds.first <- junk.too.short[inds.first]
      too.short.inds.second <- junk.too.short[inds.second]
      # ^^ These are wacky variables. They turn Values into indices, based on
      # ^^ the fact that revised.sojourns has a different length from
      # ^^ junk.too.short and subsequent variables


      #i.e. junk.too.short gives the Sojourn numbers for which ones are too short.
      #Those numbers correspond to the indices of revised.sojourns

      revised.sojourns[too.short.inds.first] <- first.neighbors[inds.first]
      # ^^ Special attention to these two lines
      revised.sojourns[too.short.inds.second] <- second.neighbors[inds.second]
      # ^^ These lines clarify what each variable's purpose is in merging too
      # short sojourns with neighbors

      ##### Next we deal with instances where we need to combine more than 2
      #sojourns, since that can cause weird behavior in revised.sojourns.
      #Essentially consecutive too-short Sojourns can cause the running Sojourn
      #tab to skip numbers, then go back, then jump back to where you would have
      #expected it to be. The next few lines essentially smooth that out.


      # When combining Sojourns, the too-short Sojourn is labelled with the same
      # number as its preceding or succeeding Sojourn (whichever is shorter).
      # When this works properly, it presents as a repeat-and-skip in the
      # Sojourn numbering, e.g. 4, 6, 7, 7, 9... If there are two consecutive
      # too-short Sojourns, they can each be labeled with the other's number,
      # which causes a skip-then-back in the numbering, e.g. 6, 7, 9, 8, 9... To
      # fix that, just check to see if the difference in consecutive Sojourns is
      # negative, which will indicate that multiple too-short Sojourns need to
      # be combined. You can then accomplish that by changing the number from
      # the negative difference to match the Sojourn before it

      inds.order <- which(diff(revised.sojourns)<0)
      # ^^ Where are there negative differences?

      if (length(inds.order)>0)
        revised.sojourns[inds.order+1] <- revised.sojourns[inds.order]
      # ^^ Apply the previous Sojourn's number to the Sojourn with the
      # ^^ negative difference

      #### Get new durations now that sojourns are combined

      revised.durations      <- as.vector(tapply(durations,revised.sojourns,sum))
      # ^^ Bit of an abstract call here
      # ^^ basically collapsing the original durations by sum, based on the
      # ^^ revised sojourns

      revised.sojourns_count <- length(revised.durations)
      revised.sojourns       <- seq_along(revised.durations)
      # ^^ Re-assign Sojourn numbers now that the duration of each is known

      ## Last thing to do is re-initialize the starting variables for the next
      ## pass through the loop.
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
  trans.table <- data.frame(
    sojourns=sojourns.1,
    durations=durations.1
  )

  return(trans.table)

}
