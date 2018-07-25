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
#' data(example_data, package = "Sojourn")
#' apply_youth_sojourn(AG = example_data, vm = "Vector.Magnitude", Site = "Hip")
#'
apply_youth_sojourn <- function(AG, vm = c("Vector.Magnitude", "ENMO"),
  Site = c("Hip", "Wrist"), demo_interactive = FALSE, verbose = FALSE) {

  ## Test Input

    AG <- youth_name_test(AG, demo_interactive = demo_interactive)
    vm <- match.arg(vm, c("Vector.Magnitude", "ENMO"), TRUE)
    Site <- match.arg(Site, c("Hip", "Wrist", "Error"), TRUE)
    stopifnot(length(vm) == 1, vm %in% names(AG), length(Site) == 1)


  ## Identify which ANN to use

    Output <-
      switch(vm, "Vector.Magnitude" = "Counts", "ENMO" = "Raw")

    ANN <-
      paste(
        tolower(Site),
        Output,
        sep = '')
    ANN <- paste("youth", ANN, sep = "_")

    intensity.fit <- eval(parse(text = ANN))
    FeatureSet <- intensity.fit$coefnames
    FeatureSet <- gsub('Sex[MF]', 'Sex', FeatureSet, ignore.case = TRUE)

  ## Mark the Sojourns

    if (verbose) {
      cat(messager(2))
    }

      AG <- cbind(AG,
        get_youth_sojourns(AG[,vm],
          Output = Output,
          Site = Site,
          verbose = verbose
        )
      )

    if (verbose) {
      cat(messager(3))
    }

  ## Get the predictions

    meta_names <- c("Sex", "Age", "BMI")
    meta <- AG[ ,meta_names]
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
      )
    )

    y_soj <-
      predict(
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
        )
      )

    AG$youth_sojourn_intensity <-
      youth_sojourn_tree(
        AG[ ,vm],
        y_15,
        y_soj
      )

    return(AG)
}
