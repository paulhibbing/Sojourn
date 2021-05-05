#' Invoke the original triaxial Sojourn method
#'
#' Calls the triaxial Sojourn method from \href{https://pubmed.ncbi.nlm.nih.gov/23860415/}{Lyden et al. (2014)}.
#'
#' @param counts numeric vector of activity counts from the first axis
#' @param counts.2 numeric vector of activity counts from the second axis
#' @param counts.3 numeric vector of activity counts from the third axis
#' @param vect.mag vector magnitude of the activity counts
#' @param short minimum length of one Sojourn
#' @param verbose logical. Print updates to console?
#'
#' @return a data frame of processed data
#' @export
#'
#' @examples
#' data(example_data, package = "Sojourn")
#' results_3x <- soj_3x_original(
#'   example_data$axis1,
#'   example_data$axis2,
#'   example_data$axis3,
#'   example_data$Vector.Magnitude
#' )
#'
#' utils::head(results_3x)
#'
soj_3x_original <- function(counts, counts.2,
  counts.3, vect.mag, short=30, verbose = FALSE) {

  if (missing(vect.mag)) vect.mag <- sqrt(
    (counts^2)+(counts.2^2)+(counts.3^2)
  )

  ## Find initial transitions and durations

    if (verbose) cat(
      "\n...Getting initial transition/duration values"
    )

    trans <-
      {diff(counts) * -1} %>%
      {(. > 15) & (counts[-1] <= 10)} %>%
      c(0, .)

    trans.inds <- which(trans==1)

    durations <- c(
      dplyr::first(trans.inds),
      diff(trans.inds),
      length(counts) - dplyr::last(trans.inds)
    )

  ## Get initial sojourn information

    if (verbose) cat(
      "\n...Getting initial Sojourn values"
    )

    sojourns <- seq(durations)

    sojourns.long <- rep(sojourns, durations)

    mean.cnts.soj <-
      tapply(counts, sojourns.long, mean) %>%
      as.vector(.)

  ## Combine Sojourns that are too short

    if (verbose) cat(
      "\n...Combining short Sojourns"
    )

    combined <- combine_soj3x(
      durations, short, sojourns, verbose
    )

  ## Tabulate durations, Sojourns, etc

    trans.table <-
      data.frame(
        counts = counts,
        counts.2 = counts.2,
        counts.3 = counts.3,
        vect.mag = vect.mag,
        sojourns = rep(combined$sojourns, combined$durations),
        durations = rep(combined$durations, combined$durations),
        perc.soj = NA,
        soj.type.all = NA,
        soj.mets.all = NA
      ) %>%
      within({perc.soj = rep(
        tapply(counts > 0, sojourns, mean),
        combined$durations
      )})

  ## Now get inactivity indices

    if (verbose) cat(
      "\n...Finding inactivity periods"
    )

    inds.inacts <- which(trans.table$perc.soj<0.7)
    inactivities <- trans.table[inds.inacts,]

    inact.durations <-
      diff(inactivities$sojourns) %>%
      {which(. != 0) + 1} %>%
      c(1, .) %>%
      inactivities$durations[.]

  ## Get activity type predictions

    if (verbose) cat(
      "\n...Identifying activity types"
    )

    cool.all <-
      cbind(
        prep_nnet_soj3x_original(inactivities, "counts"),
        prep_nnet_soj3x_original(inactivities, "counts.2", ".2"),
        prep_nnet_soj3x_original(inactivities, "counts.3", ".3"),
        prep_nnet_soj3x_original(inactivities, "vect.mag", ".vm"),
        inact.durations
      ) %>%
      scale(cent.1, scal.1) %>%
      as.data.frame(.) %>%
      predict(class.nnn.6, .)

  ## Add soj.type to trans table

    trans.table$soj.type.all[inds.inacts] <-
      max.col(cool.all) %>%
      rep(inact.durations)

  #	Assign METs by types and previous criteria (percent of non-zeros)

    if (verbose) cat(
      "\n...Calculating METs"
    )

    trans.table$soj.mets.all <-
      ifelse(
        trans.table$soj.type.all == 1 & trans.table$perc.soj <= 0.12,
        1.5, NA_real_
      ) %>%
      ifelse(
        trans.table$soj.type.all == 1 & trans.table$perc.soj > 0.12,
        1.7, .
      ) %>%
      ifelse(
        trans.table$soj.type.all == 3 & trans.table$perc.soj <= 0.05,
        1, .
      ) %>%
      ifelse(
        trans.table$soj.type.all == 3 & trans.table$perc.soj > 0.05,
        1.2, .
      ) %>%
      ifelse(
        trans.table$soj.type.all %in% c(2, 4) & trans.table$perc.soj > 0.12,
        1.7, .
      ) %>%
      ifelse(
        trans.table$soj.type.all %in% c(2, 4) & trans.table$perc.soj <= 0.12,
        1.5, .
      )

  ## Identify activities for the EE nnet

    is_activity <- trans.table$perc.soj>=0.7

    if(any(is_activity)) {

      trans.table$soj.type.all %<>% ifelse(is_activity,  6, .)

      trans.table$soj.mets.all[is_activity] <-
        trans.table[is_activity, ] %>%
        split(., .$sojourns) %>%
        lapply(prep_nnet_soj3x_original, "counts") %>%
        do.call(rbind, .) %>%
        within({
          ## Recalculate lag-1 because here it's done via acf.lag1
          ## while in prep_nnet... it's done via acf (very slight
          ## differences)
          acf = tapply(
            trans.table$counts[is_activity],
            trans.table$sojourns[is_activity],
            acf.lag1
          )
        }) %>%
        scale(center = cent, scale = scal) %>%
        predict(reg.nn, .) %>%
        rep(dplyr::count(trans.table[is_activity, ], sojourns)$n)

    } else {

      warning("No Activity in File. Verify the file is valid.")

    }

  ## Get Breaks from sitting (not currently included in output)
  ## Then finish up

    if (verbose) cat(
      "\n...Finishing up"
    )

    trans.table %>%
    {.$soj.mets.all[-nrow(.)] < 1.5} %>%
    {. & trans.table$soj.mets.all[-1] >= 1.5} %>%
    c(0, .) %>%
    {within(trans.table, {
      soj.breaks.all = .
    })} %>%
    .[ ,c(
      "counts", "counts.2", "counts.3", "vect.mag",
      "sojourns", "durations", "perc.soj", "soj.mets.all"
    )] %>%
    stats::setNames(c(
      "axis1","axis2","axis3","VM","sojourns",
      "durations","perc.soj","METs"
    ))

}
