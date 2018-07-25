rm(list = ls())
devtools::load_all()

AG <- AGread::read_AG_counts("data-raw/Lyden_Demo/AG03MN.csv", verbose = TRUE)
AG$Vector.Magnitude <- AGread::get_VM(AG[ ,c("axis1", "axis2", "axis3")])

AG$id <- "TestID"
AG$Age <- 12
AG$Sex <- "F"
AG$BMI <- 22

keep <- c("id", "Sex", "Age", "BMI", "Timestamp",
  "axis1", "axis2", "axis3", "Vector.Magnitude", "steps", "incline")

example_data <- AG[1:10000 ,keep]
devtools::use_data(example_data, overwrite = TRUE)

# Test to be sure it runs, and quickly
# apply_youth_sojourn(AG = AG, vm = "Vector.Magnitude", Site = "Hip")
