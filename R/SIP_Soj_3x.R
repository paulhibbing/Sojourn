#' Triaxial Sojourn method for the \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{SIP method}
#'
#' @param ag combined ActiGraph and activPAL data on which to identify
#'   transitions and make predictions
#' @param short the minimum duration of a qualifying Sojourn
#'
#' @return A data frame of processed data using the
#'   \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{SIP method}
#' @export
#'
#' @examples
#' data(SIP_ag, package = "Sojourn")
#' data(SIP_ap, package = "Sojourn")
#' data <- Sojourn::enhance_actigraph(SIP_ag, SIP_ap)
#' if (isTRUE(requireNamespace("Sojourn.Data"))) {
#'   utils::head(sojourn_3x_SIP(data))
#' }
sojourn_3x_SIP <- function(ag, short = 30) {

  if (!requireNamespace("Sojourn.Data", quietly = TRUE)) {
    stop(
      "You must install the package `Sojourn.Data`",
      "to use this function.\n  If it is missing on CRAN, use ",
      "devtools::install_github(\"paulhibbing/Sojourn.Data\")"
    )
  }

  y <- ag$counts
  counts.2 <- ag$axis2
  counts.3 <- ag$axis3
  vect.mag <- ag$vm

  used_vars <- c("counts", "axis2", "axis3", "vm")
  ag$Date <- NULL

  durations <- find.transitions(y)
  durations <- combine.sojourns(durations, short)
  sojourns <- rep(1:length(durations), durations)

  if("ActivityBlocks" %in% colnames(ag)) {
    temp <- sojourns + ag$ActivityBlocks
    durations <- as.vector(tapply(temp, temp, length))
    durations <- combine.sojourns(durations, short)
    sojourns <- rep(1:length(durations), durations)
  }

  #    make table of durations and sojourns etc

  trans.table <- data.frame(
    counts = y,
    counts.2 = counts.2,
    counts.3 = counts.3,
    vect.mag = vect.mag,
    sojourns = sojourns,
    durations = rep(durations, durations),
    perc.soj = NA,
    type = NA,
    METs = NA
  )

  soj.table <- data.frame(
    durations = durations,
    perc.soj = tapply(y > 0, sojourns, mean),
    type = 6,
    METs = NA
  )

  #   get percent non zero in table

  ### get inds.inactivities so can test nnet only to distinguish between
  ### lifestyle and sedentary

  inputs <- prep.nnetinputs(
    ag[soj.table$perc.soj[sojourns] < 0.7,],
    sojourns[soj.table$perc.soj[sojourns] < 0.7],
    acf.lag1.alt
  )

  inact.inputs <- as.data.frame(
    scale(
      inputs,
      center = Sojourn.Data::cent.1,
      scale = Sojourn.Data::scal.1
    )
  )

  rownames(inact.inputs) <- NULL

  #   predict type using all axes + vm.  i intially had a lot of prediction
  #   nnets here (ie different axis) but have removed them and only include the
  #   one that looks "the best".  there are definitely others we can use/try

  #   remove NaNs created by scaling by 1/0
  inact.inputs <- inact.inputs[,-c(1, 2, 13)]

  #   add soj.type to trans table

  soj.table$type[soj.table$perc.soj < 0.7] <- apply(
    predict(
      Sojourn.Data::class.nnn.6,
      inact.inputs
    ),
    1,
    which.max
  )

  #   assign mets to types.

  if("ActivityCode" %in% colnames(ag)) {

    # bout marked sedentary by activPAL?
    temp <- aggregate(
      ag$ActivityCode == 0,
      list(sojourns),
      mean
    )$x >= 0.5

    soj.table$type[soj.table$type %in% c(1, 3)] <- ifelse(
      temp[soj.table$type %in% c(1, 3)],
      3,
      1
    )

  }

  soj.table$METs[
    (soj.table$type==1)&(soj.table$perc.soj<=0.12)
  ] <- 1.5
  soj.table$METs[
    (soj.table$type==1)&(soj.table$perc.soj>0.12)
  ] <- 1.7
  soj.table$METs[
    (soj.table$type==3)&(soj.table$perc.soj<=0.05)
  ] <- 1
  soj.table$METs[
    (soj.table$type==3)&(soj.table$perc.soj>0.05)
  ] <- 1.2

  #   this identifies activities for nnet all - 6 means activity i realize i am
  #   getting lag1 differently than i do for inactivities...i should change to
  #   use function throughout.

  inputs <- prep.nnetinputs(
    ag[soj.table$type[sojourns] %in% c(2, 4, 6),],
    sojourns[soj.table$type[sojourns] %in% c(2, 4, 6)],
    acf.lag1
  )
  act.inputs <- inputs[c("X10.","X25.","X50.","X75.","X90.","acf")]
  rownames(act.inputs) <- NULL
  act.inputs <- as.data.frame(
    scale(
      act.inputs,
      center = Sojourn.Data::cent,
      scale = Sojourn.Data::scal
    )
  )

  #   predict METs

  act.mets.all <- predict(Sojourn.Data::reg.nn, act.inputs)
  soj.table$METs[is.na(soj.table$METs)] <- act.mets.all

  #   put back in table

  trans.table$perc.soj <- soj.table$perc.soj[sojourns]
  trans.table$type <- soj.table$type[sojourns]
  trans.table$METs <- soj.table$METs[sojourns]

  trans.table <- trans.table[,-8] # remove $type

  if("ActivityCode" %in% names(ag)) {
    trans.table$ActivityCode <- ag$ActivityCode
    ag$ActivityCode <- NULL
    names(ag) <- gsub(
      "CumulativeStepCount", "AP_Steps",
      gsub(
        "AP.steps", "AP_Steps",
        names(ag), ignore.case = TRUE
      )
    )
    trans.table$AP_Steps <- diff(c(0, ag$AP_Steps))
    ag$ActivityBlocks <- NULL
    ag$AP_Steps <- NULL
  }

  first_names <- c("Time", setdiff(names(ag), "Time"))
  ag <- ag[ ,first_names]

  names(ag) <- gsub("^Time$", "Timestamp", names(ag))
  names(ag) <- gsub("^axis", "counts.", names(ag))
  names(ag) <- gsub("^vm$", "vect.mag", names(ag))

  SIP_names <- c(
    "sojourns", "durations", "perc.soj", "METs",
    "ActivityCode", "AP_Steps"
  )
  SIP_names <- SIP_names[SIP_names %in% names(trans.table)]


  trans.table <- cbind(ag, trans.table[ ,SIP_names])
  row.names(trans.table) <- NULL

  if (is.null(attr(ag, "AG.header"))) {
    attr(trans.table, "AG.header") <- "Processed with sojourns"
  } else {
    header <- attr(trans.table, "AG.header")
    attr(trans.table, "AG.header") <- append(
      "Processed with sojourns", header, length(header)-1
    )
  }

  return(trans.table)

}
