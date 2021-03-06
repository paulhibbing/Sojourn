# Need id, Vector.Magnitude, Site, Sex, as.integer(Age)
input_demographic <- function(){

  id <- svDialogs::dlgInput('Enter the participant ID', default = 'DEMO')$res
  Site <- switch(svDialogs::dlgMessage('Is the data from the hip?', 'yesno')$res,
                'yes' = 'Hip', 'no' = 'Wrist')
  Sex <- switch(svDialogs::dlgMessage('Is the participant female?', 'yesno')$res,
               'yes' = 'F', 'no' = 'M')

  ## Age ####
    Age <- svDialogs::dlgInput('Please enter the participant\'s age in years:',
                              default = '10')$res
    repeat{
    ageTest <- try(as.integer(Age), TRUE)

    if(class(ageTest) == "try-error" | is.na(ageTest)) {

      message('Age not entered correctly. Try again')
      Age <- svDialogs::dlgInput('Please enter the participant\'s age in years:',
        default = '10')$res}

    } else{

      break

    }

    Age = as.integer(Age)

  ## BMI ####
    BMI <- svDialogs::dlgInput('Please enter the participant\'s BMI',
                              default = '20')$res
      repeat{
      bmiTest <- try(as.numeric(BMI), TRUE)

      if(class(bmiTest) == "try-error" | is.na(bmiTest)) {

        message('BMI not entered correctly. Try again')
        BMI <- svDialogs::dlgInput('Please enter the participant\'s BMI',
          default = '20')$res

      } else{

        break

      }
      }

    BMI <- as.numeric(BMI)
    vm <- 'Vector.Magnitude'

}

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
