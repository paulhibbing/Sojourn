apply_youth_sojourn <- function(AG, verbose = FALSE) {

  ## Test Input

    stopifnot(
      all(c("id", "Site", "Age", "Sex", "BMI") %in% names(AG))
    )
    vm <- if ("Axis1" %in% names(AG)) "Axis1" else "ENMO"

  ## Identify which ANN to use

    ANN <-
      paste(
        tolower(unique(AG$Site)),
        switch(('ENMO'%in%names(AG))+1, 'Counts', 'Raw'),
        sep = '')
    ANN <- paste("youth", ANN, sep = "_")

    intensity.fit <- eval(parse(text = ANN))
    FeatureSet <-
      intensity.fit$coefnames
    FeatureSet <- gsub('Sex[MF]', 'Sex', FeatureSet, ignore.case = T)


  ## Mark the Sojourns

    if (verbose) cat('\nIdentifying Sojourns...')

      AG <-
        cbind(AG,
              get_youth_sojourns(AG[,vm],
                           Output = if(RAW) 'Raw' else 'Counts',
                           Site = Site))

    if (verbose) cat('\nDone!\n')

  ## Get the predictions
    meta_names <- c("Sex", "Age", "BMI")
    meta <- AG[ ,meta_names]
    AG <- AG[ ,setdiff(names(AG), meta_names)]

    AG$youth_sojourn_yntensity <-
      youth_sojourn_tree(
        AG[ ,vm],
        predict(intensity.fit,
                cbind(
                  youth_network_shape(AG, FALSE, RAW, verbose = TRUE),
                  meta
                )
        ),
        predict(intensity.fit,
                cbind(
                  youth_network_shape(AG, TRUE, RAW, verbose = FALSE),
                  meta
                )
        )
      )
    return(AG)
}
