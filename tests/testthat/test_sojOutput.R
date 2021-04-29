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

  data(SIP_ag, package = "Sojourn")
  data(SIP_ap, package = "Sojourn")

  ag <- enhance_actigraph(SIP_ag, SIP_ap)
  testthat::expect_known_output(
    ag, "sip_ag.rds"
  )

  sip_estimate <- sojourn_3x_SIP(ag)
  testthat::expect_known_output(
    sip_estimate, "sip_estimate.rds"
  )

})

test_that("youth Sojourn returns as expected", {

  data(example_data, package = "Sojourn")
  results_youth_soj <- apply_youth_sojourn(
    AG = example_data,
    vm = "Vector.Magnitude",
    Site = "Hip"
  )

  testthat::expect_known_output(
    results_youth_soj, "soj_youth.rds"
  )

})
