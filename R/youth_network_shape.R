#' Format data for input into the youth Sojourn method's neural networks
#'
#' @param data a data frame of monitor and demographic data to format
#' @param sojourns logical. Should the data be windowed by Sojourn (TRUE) or
#'   with a static window (15-s; FALSE)?
#' @param RAW logical. Are the data raw acceleration?
#' @param epoch numeric. The epoch length of the data. Values other than one are
#'   not recommended.
#' @param verbose logical. Print processing updates to the console?
#' @param expand logical. Should collapsed data be replicated to match the
#'   length of the original input?
#' @param time_var character scalar giving the variable name in \code{data} that
#'   gives the timestamps
#' @param first_print logical. Print extra information?
#' @param ... further arguments passed to \link{messager}
#'
#' @return A data frame containing data that are ready for making predictions
#'   with the youth Sojourn method's neural networks
#' @keywords internal
#'
youth_network_shape <- function(data, sojourns = TRUE, RAW = FALSE,
  epoch = 1, verbose = TRUE, expand = TRUE, time_var = "Timestamp",
  first_print = TRUE, ...) {

  if (verbose & first_print) {

    cat(messager(4, ...))

  }

  if (RAW) {

    data$Vector.Magnitude <- data$ENMO

  }

  if (!sojourns) {
    data$sojourns <- rep(1:ceiling(nrow(data)/15), each = 15)[1:nrow(data)]
    if (verbose) cat(messager(5))
  } else{

    if (!"sojourns" %in% names(data)) {

      data <- cbind(
        data,
        get_youth_sojourns(
          vm = data$Vector.Magnitude,
          Output = switch(RAW + 1, "Counts", "Raw"),
          epoch = epoch
        )
      )

      if (verbose) cat(messager(6))

    } else{

      if (verbose) cat(messager(7))
      if (length(unique(table(data$sojourns)))==1 &
          unique(table(data$sojourns))[1]==15) {

        message(paste(
          "Looks like this is in 15-second blocks.",
          "Returning NULL."
        ))

        return(NULL)

      }

    }
  }

  #if (RAW) Time <- data$timestamp else
  Time <- data[ ,time_var]
  groups <- c("id", "Visit", "sojourns")[
    c("id", "Visit", "sojourns") %in% names(data)
  ]

  data <- data %>%
    dplyr::group_by_at(.vars = groups) %>%
    dplyr::summarise(
      VM_counts = sum(.data$Vector.Magnitude),
      VM_Mean = mean(.data$Vector.Magnitude),
      VM_zcr = AG_zero_cross(.data$Vector.Magnitude),
      VM_zero_prop = AG_zero_prop(.data$Vector.Magnitude),
      VM_Q10 = quantile(.data$Vector.Magnitude, probs=0.10),
      VM_Q25 = quantile(.data$Vector.Magnitude, probs=0.25),
      VM_Q50 = quantile(.data$Vector.Magnitude, probs=0.50),
      VM_Q75 = quantile(.data$Vector.Magnitude, probs=0.75),
      VM_Q90 = quantile(.data$Vector.Magnitude, probs=0.90),
      VM_CV = sd(.data$Vector.Magnitude) / mean(.data$Vector.Magnitude) * 100,
      VM_Var = var(.data$Vector.Magnitude),
      VM_lag1 = acf(.data$Vector.Magnitude,type="correlation",plot=F)[[1]][2],
      duration = n()*epoch
    )

  noZeros <- data.frame(
    do.call(cbind,
      lapply(data[, setdiff(names(data), unlist(groups))],
        function(x)
          sapply(x,
            function(y)
              if (is.na(y))
                0
            else
              y))),
    stringsAsFactors = FALSE
  )

  data <- data.frame(
    data[, unlist(groups)],
    noZeros,
    stringsAsFactors = FALSE
  )

  data$scores.Comp.1 <- NA
  ## ^^ This is my workaround since I'm not using princomp anymore

  if (verbose) cat(messager(8))

  if (RAW) {

    names(data) <- gsub('VM_','ENMO_',names(data))
    names(data) <- gsub('ENMO_counts','ENMO_total',names(data))

  }

  if (!sojourns) {

    names(data)[which(grepl('sojourns',names(data)))] <- 'Block'

  }

  if (expand) {

    data <- data[rep(seq_len(nrow(data)), times = data$duration), ]

  } else{

    Time <- tapply(
      as.character(Time),
      unlist(
        mapply(function(x,y) rep(x,y),
          x = data[, switch(sojourns+1, "Block", "sojourns")],
          y = data$duration, SIMPLIFY = FALSE)),
      function(x) x[1]
    )

  }

  data[ ,time_var] <- Time

  return(data)

}
