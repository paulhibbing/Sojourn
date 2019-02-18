context("Sojourn methods return as expected")
library(Sojourn)


test_that("original Sojourn returns as expected", {
  data(example_data, package = "Sojourn")

  results_3x <- soj_3x_original(
    example_data$axis1,
    example_data$axis2,
    example_data$axis3,
    example_data$Vector.Magnitude
  )
  testthat::expect_known_output(
    results_3x, "soj_3x.rds"
  )

  results_1x <- soj_1x_original(example_data$axis1)
  testthat::expect_known_output(
    results_1x, "soj_1x.rds"
  )

})

test_that("SIP returns as expected", {

})

test_that("youth Sojourn returns as expected", {

})
