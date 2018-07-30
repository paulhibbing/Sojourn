#' Read activPAL event files
#'
#' @param filename path to an activPAL event file
#'
#' @return A data frame of activPAL data
#' @export
#'
#' @examples
AP.file.reader <- function(filename) {

  # read an activPAL events file.
  opt <- options(stringsAsFactors = FALSE)
  deffile <- sub(" Events\\.csv", ".def", filename)
  header <- read.csv(deffile, header = FALSE, row.names = 1)
  # I refuse to believe that there isn't a better way to do this.
  header <- as.list(as.data.frame(t(header)))

  start.time <- as.POSIXlt(strptime(header$StartTime, "#%Y-%m-%d %H:%M:%S#"))
  samp.freq <- as.numeric(header$SamplingFrequency)

  data <- read.csv(filename)
  # Test whether the timeseries starts at a whole second boundary.
  # This is here because I want to be lazy and see if I need to handle the
  # case where it does not.
  # Update: Never mind, this is useless because the Excel timestamp doesn't
  # always have enough significant figures
  #    if(round(data$Time[1] %% 1 * 24*60*60 * samp.freq) %% samp.freq)
  #        warning("ActivPAL time series is offset by a fraction of a second.")

  n <- dim(data)[1]
  # We use the start time in the header rather than converting the Excel dates
  # in the data set because Excel pretends DST doesn't exist.
  # This wouldn't work if the data started at a fractional second; hence the
  # test above
  data$Time <- start.time + data$DataCount / samp.freq
  # This should be less fragile but I'm lazy.
  names(data) <- c("Time", "DataCount", "Interval", "ActivityCode",
    "CumulativeStepCount", "ActivityScore")

  options(opt)
  return(data)
}
