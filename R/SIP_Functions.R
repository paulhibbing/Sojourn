#' Find transitions
#'
#' Examine a data stream for qualifying transitions according to the
#' \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{SIP method}
#'
#' @param y the data stream to examine
#'
#' @keywords internal
#'
find.transitions <- function(y) {

  N <- length(y)

  trans <- (-diff(y) > 15) & (y[-1] <= 10)
  trans.inds <- c(0,2:N,N)[c(TRUE,trans,TRUE)]
  durations <- diff(trans.inds)

  return(durations)

}

#' Check the formatting of a data frame for use in the SIP pipeline
#'
#' @param frame The data frame to check
#' @param expected_var_names character vector. The expected variable names in
#'   \code{frame}
#' @param expected_classes character vector. The expected variable classes in
#'   \code{frame}
#' @inheritParams enhance_actigraph
#'
#' @keywords internal
#'
SIP_frame_test <- function(
  frame, expected_var_names, expected_classes, verbose
) {

  AGread_names <- c(
    "Axis1", "Axis2", "Axis3",
    "Vector.Magnitude", "Timestamp"
  )

  if (all(AGread_names %in% names(frame))) {

    frame$Time <- NULL
    names(frame) <- ifelse(
      names(frame) %in% AGread_names,
      sapply(
        names(frame),
        function(x) {
          switch(
            x, "Axis1" = "counts",
            "Axis2" = "axis2",
            "Axis3" = "axis3",
            "Vector.Magnitude" = "vm",
            "Timestamp" = "Time"
          )
        }
      ),
      names(frame)
    )

    stopifnot(
      all(expected_var_names %in% names(frame))
    )

    other_vars <- setdiff(names(frame), expected_var_names)
    other_vars <- frame[ ,other_vars]
    frame <- frame[ ,expected_var_names]

  }

  actual_var_names <- names(frame)
  actual_classes <- unname(
    unlist(
      sapply(frame, function(x) class(x)[1])
    )
  )
  actual_classes <- ifelse(
    grepl("POSIX", actual_classes), "POSIX...", actual_classes
  )

  err_msg <- paste(
    "Expecting a ", length(expected_var_names),
    "-column data frame with names:\n    c(\"",
    paste(expected_var_names, collapse = "\", \""),
    "\")\n  and immediate classes:\n    c(\"",
    paste(expected_classes, collapse = "\", \""),
    "\")",
    sep = ""
  )

  if (any(
    !identical(expected_var_names, actual_var_names),
    !identical(expected_classes, actual_classes)
  )) {

    if (verbose) print(actual_classes)
    stop(err_msg)

  }

  if (exists("other_vars")) frame <- cbind(frame, other_vars)

  frame

}

#' Combine ActiGraph and activPAL data
#'
#' Merge data streams for separate monitors in the
#' \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{SIP method}
#'
#' @param ag ActiGraph data
#' @param ap activPAL data
#' @param verbose logical. Print information to console?
#'
#' @export
#'
#' @examples
#' data(SIP_ag, package = "Sojourn")
#' data(SIP_ap, package = "Sojourn")
#' combined_data <- enhance_actigraph(SIP_ag, SIP_ap)
#' utils::head(combined_data)
enhance_actigraph <- function(ag,ap, verbose = FALSE) {

  ag <- SIP_frame_test(
    frame = ag,
    expected_var_names = c(
      "counts", "axis2", "axis3", "vm", "Time"
    ),
    expected_classes = c(
      "integer", "integer", "integer", "numeric", "POSIX..."
    ),
    verbose = verbose
  )

  stopifnot(
    lubridate::tz(ag$Time) == lubridate::tz(ap$Time)
  )

  ap$ActivityBlocks <- cumsum(
    c(TRUE, as.logical(diff(ap$ActivityCode)))
  )
  # It would be nice to leave the datasets as zoo objects, but this seems
  # like it could lead to problems by calling unexpected methods.
  # FIXME need to deal with mismatches in the times spanned by these data
  ap_merge_names <- c(
    "ActivityCode", "ActivityBlocks", "CumulativeStepCount"
  )
  temp <- merge(
    zoo::zoo(NULL, ag$Time),
    zoo::zoo(
      ap[ ,ap_merge_names],
      ap$Time - diff(ag$Time)[1]/2
    )
  )
  temp[1,is.na(temp[1,])] <- 0
  ag[ ,ap_merge_names] <- zoo::na.locf(temp)[ag$Time]

  return(ag)

}

#' Shape data for input into the neural networks
#'
#' Prepare data for use in the neural networks of the
#' \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{SIP method}
#'
#' @param ag ActiGraph and activPAL combined data
#' @param sojourns the Sojourns identified on the data
#' @param lag.fun function to use for autocorrelations
#'
#' @keywords internal
#'
prep.nnetinputs <- function(ag, sojourns, lag.fun) {

  inputs <- do.call(
    data.frame,
    aggregate(
      ag[1:4],
      list(sojourns),
      function(x) {

      c(
        X = quantile(x, probs = c(.1, .25, .5, .75, .9)),
        acf = lag.fun(x)
      )

      }
    )[-1]
  )

  # for consistency with the existing data
  names(inputs) <- do.call(
    paste0,
    expand.grid(
      c(
        paste0("X", c(10, 25, 50, 75, 90)),
        "acf"
      ),
      ".",
      c("", 2, 3, "vm")
    )
  )
  names(inputs)[6] <- "acf"
  inputs$inact.durations <- tapply(sojourns, sojourns, length)
  #    # The original code *appears* to replace NAs with column means, but
  #    # *actually* the values that would have been NA are initialized to 0 and
  #    # their computation is skipped.
  inputs[is.na(inputs)] <- 0
  #    inputs[, paste0("acf", c("", ".2", ".3", ".vm"))] <-
  #        replace.na(inputs[, paste0("acf", c("", ".2", ".3", ".vm"))])
  return(inputs)
}

