#' Apply the youth Sojourn method
#'
#' Function for using the youth Sojourn method developed by
#' \href{https://www.ncbi.nlm.nih.gov/pubmed/29135657}{Hibbing et al. (2018)}
#'
#' @param AG a data frame of monitor and demographic data
#' @param vm the variable to use for processing, either
#'   \code{"Vector.Magnitude"} (for activity counts) or \code{"ENMO"} (for raw
#'   acceleration)
#' @param Site the wear location of the monitor, either \code{"Hip"} or
#'   \code{"Wrist"}
#' @param demo_interactive logical. Input demographics interactively if missing
#'   variables are identified during format checking?
#' @param verbose logical. Print processing updates to the console?
#' @param ... Further arguments passed to \code{\link{youth_name_test}}
#'
#' @return The original data frame, plus additional predictions made by the
#'   Sojourn method
#'
#' @note The functions \link[AGread]{read_AG_counts} and
#'   \link[AGread]{read_AG_raw} are recommended for assembling the
#'   monitor-specific portion of the \code{AG} data frame.
#' @export
#'
#' @examples
#'   data(example_data, package = "Sojourn")
#'   \donttest{
#'   if (isTRUE(requireNamespace("Sojourn.Data"))) {
#'     results_youth_soj <- apply_youth_sojourn(
#'       AG = example_data,
#'       vm = "Vector.Magnitude",
#'       Site = "Hip"
#'     )
#'     head(results_youth_soj)
#'  }
#'  }
#'
apply_youth_sojourn <- function(AG, vm = c("Vector.Magnitude", "ENMO"),
  Site = c("Hip", "Wrist"), demo_interactive = FALSE, verbose = FALSE, ...) {

  if (!requireNamespace("Sojourn.Data", quietly = TRUE)) {
    stop(paste(
      "You must install the package `Sojourn.Data`",
      "to use this function."
    ))
  }

  ## Test Input

    AG <- youth_name_test(AG, demo_interactive = demo_interactive, ...)
    vm <- match.arg(vm, c("Vector.Magnitude", "ENMO", "Error"))
    Site <- match.arg(Site, c("Hip", "Wrist", "Error"))
    stopifnot(length(vm) == 1, vm %in% names(AG), length(Site) == 1)

  ## Identify which ANN to use

    Output <- switch(
      vm,
      "Vector.Magnitude" = "Counts",
      "ENMO" = "Raw"
    )

    ANN <- paste(
        tolower(Site),
        Output,
        sep = ''
    )
    ANN <- paste("Sojourn.Data::youth", ANN, sep = "_")

    intensity.fit <- eval(parse(text = ANN))
    FeatureSet <- intensity.fit$coefnames
    FeatureSet <- gsub("Sex[MF]", "Sex", FeatureSet, ignore.case = TRUE)

  ## Mark the Sojourns

    if (verbose)  cat(messager(2))

    AG <- cbind(
      AG,
      get_youth_sojourns(
        AG[,vm],
        Output = Output,
        Site = Site,
        verbose = verbose
      )
    )

    if (verbose) cat(messager(3))

  ## Get the predictions

    meta_names <- c("Sex", "SexM", "Age", "BMI")
    meta <- AG[ ,names(AG) %in% meta_names]
    AG <- AG[ ,setdiff(names(AG), meta_names)]

    y_15 <- predict(
      intensity.fit,
      cbind(
        youth_network_shape(
          data = AG,
          sojourns = FALSE,
          RAW = switch(Output, "Counts" = FALSE, "Raw" = TRUE),
          verbose = verbose,
          id = unique(AG$id)
        ),
        meta
      ),
      type = "class"
    )

    y_soj <- predict(
      intensity.fit,
      cbind(
        youth_network_shape(
          data = AG,
          sojourns = TRUE,
          RAW = switch(Output, "Counts" = FALSE, "Raw" = TRUE),
          verbose = verbose,
          first_print = FALSE
        ),
        meta
      ),
      type = "class"
    )

    AG$youth_sojourn_intensity <- youth_sojourn_tree(
      AG[ ,vm], y_15, y_soj
    )

    AG <- cbind(AG, meta)
    names(AG) <- gsub("SexM", "Sex", names(AG))

    if (all(AG$Sex %in% 0:1)) {
      AG$Sex <- ifelse(AG$Sex == 0, "F", "M")
    }

    first_names <- c("id", "Sex", "Age", "BMI")
    AG <- AG[ ,c(first_names, setdiff(names(AG), first_names))]

    return(AG)

}
