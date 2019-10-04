#' Predict transitions using a threshold-based approach
#'
#' @param threshold the change score transition threshold
#' @param change_score vector of change scores
#' @param refractory_period integer. Minimum allowable number of indices between
#'   predictions
#'
#' @return A vector indicating transition (1) or non-transition (0)
#' @export
#'
#' @examples
#' x <- seq(0, 1, 0.01)
#' predict_transitions(0.75, c(x, rev(x)))
predict_transitions <- function(
  threshold, change_score, refractory_period = 0
) {

  runs <- PAutilities::index_runs(change_score >= threshold)
  result <- numeric(length(change_score))
  transitions <- c(
    runs$start_index[runs$values],
    runs$end_index[runs$values]
  )

  result[transitions] <- 1

  if (refractory_period > 0) {
    result <- refractory(result, refractory_period)
  }

  result

}
