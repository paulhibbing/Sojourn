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
#' "extdata/sampledata_Events.csv",
#' package = "Sojourn"
#' )
#' if (isTRUE(requireNamespace("data.table"))) {
#'   ap_data <- read_AP(ap_file)
#'   utils::head(ap_data)
#' }
read_AP <- function(filename, tz = "UTC") {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(paste(
      "You must install the `data.table`",
      "package to use this function"
    ))
  }

  deffile <- sub("[ _]Events\\.csv", ".def", filename)

  if (!file.exists(deffile)) {
    stop(paste0(
      "\n  Cannot find DEF file, i.e., `",
      basename(deffile), "`. ",
      "Make sure that file is in the\n  same directory as `",
      basename(filename), "`",
      ", or consider using\n  `",
      "activpalProcessing::activpal.file.reader()`, ",
      "but beware of potential\n  timezone/daylight saving ",
      "issues with the timestamps in the latter function"
    ))
  }

  header <- utils::read.csv(
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
