#' Interactively input demographic information
#'
#'
#' @param ... Further arguments passed to svDialogs functions
#' @return A data frame containing an ID, attachment site of the monitor (hip or
#'   wrist), and the participant's sex, age, and BMI
#' @export
#'
#' @examples
#' if (interactive()) {
#'   input_demographic()
#' }
input_demographic <- function(...){

  id <- svDialogs::dlgInput('Enter the participant ID', default = 'DEMO', ...)$res
  Site <- switch(svDialogs::dlgMessage('Is the data from the hip?', 'yesno', ...)$res,
    'yes' = 'Hip', 'no' = 'Wrist')
  Sex <- switch(svDialogs::dlgMessage('Is the participant female?', 'yesno', ...)$res,
    'yes' = 'F', 'no' = 'M')

  ## Age

  Age <- svDialogs::dlgInput('Please enter the participant\'s age in years:',
    default = '10', ...)$res

  repeat{
    ageTest <- try(as.integer(Age), TRUE)

    if(class(ageTest) == "try-error" | is.na(ageTest)) {

      message('Age not entered correctly. Try again')
      Age <- svDialogs::dlgInput('Please enter the participant\'s age in years:',
        default = '10', ...)$res

    } else{

    break

    }
  }

  Age <- as.integer(Age)

  ## BMI

  BMI <- svDialogs::dlgInput('Please enter the participant\'s BMI',
    default = '20', ...)$res

  repeat{
    bmiTest <- try(as.numeric(BMI), TRUE)

    if(class(bmiTest) == "try-error" | is.na(bmiTest)) {

      message('BMI not entered correctly. Try again')
      BMI <- svDialogs::dlgInput('Please enter the participant\'s BMI',
        default = '20', ...)$res

    } else{

      break

    }
  }

  BMI <- as.numeric(BMI)

  ## Finish up

  return(
    data.frame(id, Site, Age, Sex, BMI, stringsAsFactors = FALSE)
  )

}
