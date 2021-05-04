#' Combine short Sojourns
#'
#' Iteratively combine sojourns that are too short, until all Sojourns meet the
#' minimum length requirement
#'
#' @inheritParams soj_3x_original
#' @param durations durations of a set of Sojourns
#' @param sojourns vector of Sojourn assignments
#'
#' @keywords internal
#' @name combine_sojourns
combine_soj3x <- function(durations, short, sojourns) {

  counter <- 1

  repeat { # loop 1

    ## Find Sojourns that are (still) too short

      if (!any(durations < short)) break

      too.short <- which(durations < short)

    ## Combine any short Sojourns at the start of the file

      if (1 %in% too.short) {

        sojourns <-
          diff(too.short) %>%
          {. != 1} %>%
          which(.) %>%
          dplyr::first(.) %>%
          {. + 1} %>%
          pmax(sojourns, .)

      }

    ## Combine any short Sojourns at the end of the file

      if (dplyr::last(too.short) %in% max(sojourns)) {

        sojourns <-
          rev(too.short) %>%
          .[diff(.) != -1] %>%
          dplyr::first(.) %>%
          pmin(sojourns, .)

      }

    ## Deal with all other short Sojourns by
    ## combining them with their shortest neighbor
    ## THIS CURRENTLY COMBINES WITH *LONGEST* NEIGHBOR

      # First match *all* sojourns to their shortest neighbors

        short_matches <-
          seq(sojourns) %>%
          sapply(function(x, durations, l) {
            {x + c(-1, 1)} %>%
            durations[.] %>%
            ifelse(is.na(.), 0, .) %>%
            which.max(.) %>%
            switch(x - 1, x + 1) %>%
            sojourns[.] %>%
            ifelse(x %in% c(1, l), NA, .)
          }, durations, l = length(durations))

      # Then insert those assignments for cases where the Sojourn
      # is actually too short

        revised.sojourns <-
          seq(sojourns) %>%
          {. %in% too.short} %>%
          ifelse(
            short_matches,
            sojourns
          )

      # Fix any Sojourns that were assigned out of sequence

        out_of_sequence <-
          diff(revised.sojourns) %>%
          {. < 0}

        if (any(out_of_sequence)) {
          out_of_sequence %<>% which(.)
          revised.sojourns[out_of_sequence + 1] <-
            revised.sojourns[out_of_sequence]
        }

    ## Update variables now that things are combined

      revised.durations <- durations <-
        tapply(durations, revised.sojourns, sum) %>%
        as.vector(.)

      revised.sojourns <- sojourns <- seq(revised.durations)

      counter %<>% {. + 1}

  }

  list(sojourns = sojourns, durations = durations, counter = counter)

}

#' @keywords internal
#' @rdname combine_sojourns
combine.sojourns <- function(durations, short) {

  # combine too short sojourns.

  # FIXME:
  # I (IJS) think that this and find.transitions() are the weak point of the
  # method. Much improvement could be accomplished by making this smarter.
  # But my efforts to improve it haven't been that effective. If you have
  # lots of free-living training data and want to make SIP/Sojourns better,
  # focus on this!

  # Handle the case where the first or last sojourn is too short
  bool.too.short <- durations<short
  # If all sojourn durations are too short, glom them all.
  if(all(bool.too.short))
    return(sum(durations))
  counter.1 <- which.min(bool.too.short)
  counter.2 <- length(durations)+1-which.min(rev(bool.too.short))
  durations <- c(sum(durations[1:counter.1]),
                 durations[(counter.1+1):(counter.2-1)],
                 sum(durations[counter.2:length(durations)]))

  #   combine too short sojourns with neighboring sojourn.
  #   this loop repeats until there are no more too short sojourns

  repeat {

    sojourns <- 1:length(durations)
    too.short <- sojourns[durations<short]
    ts <- length(too.short)

    if(ts==0)
      break

    # now deal with all other too short sojourns
    #   right now i combine too short sojourns with its neighbor that was shorter in duration (e.g. first neighbor = 60 seconds long and second neighbor = 300 seconds long, it gets combined with first neighbor)

    durations.first.neighbors <- durations[too.short-1]
    durations.second.neighbors <- durations[too.short+1]

    too.short.inds.first <- too.short[
      durations.first.neighbors <=
        durations.second.neighbors
    ]
    too.short.inds.second <- too.short[
      durations.first.neighbors >
        durations.second.neighbors
    ]

    sojourns[too.short.inds.first] <- too.short.inds.first-1
    sojourns[too.short.inds.second] <- too.short.inds.second+1

    # deal with instances where need to combine more than 2 sojourns - i.e.
    # short sojourn became first neighbor, and then sojourn before first
    # neighbor also becomes that sojourn via second neighbor grouping - want all
    # 3 of these sojourns to be combined.

    inds.order <- (1:(length(sojourns)-1))[diff(sojourns)<0]
    sojourns[inds.order+1] <- sojourns[inds.order]

    # get new durations now that sojourns are combined

    durations <- as.vector(tapply(durations,sojourns,sum))

  }

  return(durations)

}
