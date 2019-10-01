#' Predict transitions using RuLSIF
#'
#' @param threshold the change score transition threshold
#' @param change_score vector of change scores
#'
#' @return A vector indicating transition (1) or non-transition (0)
#' @export
#'
#' @examples
#' x <- seq(0, 1, 0.01)
#' RuLSIF_predict(0.75, c(x, rev(x)))
RuLSIF_predict <- function(threshold, change_score) {

  runs <- PAutilities::index_runs(change_score >= threshold)
  result <- numeric(length(change_score))
  transitions <- c(
    runs$start_index[runs$values],
    runs$end_index[runs$values]
  )

  result[transitions] <- 1

  result

}
