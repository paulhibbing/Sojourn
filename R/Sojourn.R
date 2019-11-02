#' Sojourn: Apply Sojourn Methods for Processing ActiGraph Accelerometer Data
#'
#' The Sojourn package provides a convenient way to apply the family of Sojourn
#' methods published in previous works including Lyden K, Keadle S, Staudenmayer
#' J, & Freedson P (2014) <doi:10.1249/MSS.0b013e3182a42a2d>, Ellingson LD,
#' Schwabacher IJ, Kim Y, Welk GJ, & Cook DB (2016)
#' <doi:10.1249/MSS.0000000000000915>, and Hibbing PR, Ellingson LD, Dixon PM, &
#' Welk GJ (2018) <doi:10.1249/MSS.0000000000001486>.
#'
#' It is meant for use with data from ActiGraph monitors and (in the case of
#' Sojourns Including Posture, by Ellingson et al. (2016)) activPAL monitors.
#' File reading is not included in the functionality of the Sojourn package. For
#' help with that preliminary step, users are directed to the packages AGread
#' (for ActiGraph files) and activpalProcessing (for activPAL files).
#'
#'
#' @docType package
#' @name Sojourn
NULL

#' @importFrom magrittr %>%
NULL

#' @importFrom stats acf predict quantile sd var aggregate
NULL

#' @importFrom dplyr n
NULL

#' @importFrom rlang .data
NULL

#' @import nnet
NULL

#' @useDynLib Sojourn
#' @importFrom Rcpp sourceCpp
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
