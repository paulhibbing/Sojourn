#' Read an activPAL events file
#'
#' @param filename character. Path to the file
#' @param tz character. The timezone to use
#'
#' @return Data frame reflecting the data contained in \code{filename}.
#' @note There must be a corresponding \code{.def} file located in the same
#'   directory as \code{filename}
#' @export
#'
#' @examples
#' ap_file <- system.file(
#' "extdata/sampledata Events.csv",
#' package = "Sojourn"
#' )
#' read_AP(ap_file)
read_AP <- function(filename, tz = "UTC") {

  deffile <- sub(" Events\\.csv", ".def", filename)

  if (!file.exists(deffile)) {
    stop(paste0(
      "Cannot find DEF file, i.e., `",
      basename(deffile), "`"
    ))
  }

  header <- read.csv(
    deffile, header = FALSE,
    stringsAsFactors = FALSE, row.names = 1
  )
  header <- as.list(as.data.frame(t(header)))

  start.time <- as.POSIXlt(
    header$StartTime, tz,
    format = "#%Y-%m-%d %H:%M:%S#"
  )
  samp.freq <- as.numeric(header$SamplingFrequency)

  data <- data.frame(
    data.table::fread(filename, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE, row.names = NULL
  )

  n <- dim(data)[1]
  data$Time <- start.time + data$DataCount / samp.freq

  names(data) <- c("Time", "DataCount", "Interval", "ActivityCode",
    "CumulativeStepCount", "ActivityScore")

  return(data)

}
