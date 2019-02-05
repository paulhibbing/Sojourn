#' Sample data for exploring original and youth Sojourn methods
#'
#' @format A data frame with 79989 rows and 11 variables:
#' \describe{
#'   \item{id}{An example ID for the data set}
#'   \item{Sex}{An example sex for the data set}
#'   \item{Age}{An example age (in years) for the data set}
#'   \item{BMI}{An example body mass index (in kg/m^2) for the data set}
#'   \item{Timestamp}{POSIX-formatted variable giving the timestamp for each
#'   observation}
#'   \item{axis1}{Activity counts from the first axis}
#'   \item{axis2}{Activity counts from the second axis}
#'   \item{axis3}{Activity counts from the third axis}
#'   \item{Vector.Magnitude}{Vector magnitude of activity counts
#'   (sqrt(sum(axis1^2, axis2^2, axis3^2)))}
#'   \item{steps}{Predicted steps taken}
#'   \item{incline}{Inclinometer status (0 = off, 1 = lying, 2 = sitting, 3 =
#'   standing)}
#' }
"example_data"

#' ActiGraph sample data for exploring Sojourns Including Posture (SIP) method.
#'
#' @format A data frame with 12257 rows and 5 variables:
#' \describe{
#'   \item{counts}{Activity counts from the first axis}
#'   \item{axis2}{Activity counts from the second axis}
#'   \item{axis3}{Activity counts from the third axis}
#'   \item{vm}{Vector magnitude of activity counts}
#'   \item{Time}{POSIX-formatted variable giving the timestamp for each
#'   observation}
#' }
"SIP_ag"

#' activPAL sample data for exploring Sojourns Including Posture (SIP) method
#'
#' @format A data frame with 12257 rows and 5 variables:
#' \describe{
#'   \item{Time}{POSIX-formatted variable giving the timestamp for each
#'   observation}
#'   \item{DataCount}{Integer value giving the index of the sample from
#'   which the row of data is drawn}
#'   \item{Interval}{Duration (in seconds) of the interval from one data point
#'   to the next}
#'   \item{ActivityCode}{Integer giving the posture activity classification:
#'   0 is sedentary, 1 is standing, and 2 is stepping}
#'   \item{CumulativeStepCount}{Integer giving the cumulative step count from
#'   the start of the file to the current data point.}
#'   \item{ActivityScore}{Numeric giving MET-hours}
#' }
"SIP_ap"
