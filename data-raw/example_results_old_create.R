# This file is precarious. Use with caution.
#
# In this file, the example for `Sojourn::apply_youth_sojourn` is run, and the
# results are saved. The catch is, the example must be run with a specific
# version of Sojourn (current as of f03ab831), which also means it needs to be
# run in an independent session without Sojourn.rproj open.
#
# The reason is, the original youth Sojourn models had neural networks stored in
# `train` objects (from caret) that were too large for distribution. So I had to
# pull out the necessary pieces and adjust the way predictions are generated.
# The results from running the original code (using the enormous objects) need
# to be stored so I can compare against the new results.
#
# In retrospect, this is exactly what the `testthat` package is for. But I guess
# this is what I'm going with.

rm(list = ls())
data(example_data, package = "Sojourn")

old_results <- Sojourn::apply_youth_sojourn(
  AG = example_data, vm = "Vector.Magnitude", Site = "Hip"
)

save(
  old_results,
  file = file.path(
    "C:/users/Paul/Desktop/Sojourn",
    "data-raw/example_results_old.RData")
)
