rm(list=ls())
devtools::load_all()

data <- AGread::read_AG_counts("data-raw/Lyden_Demo/AG03MN.csv")

# apply the sojourn.3x function (METs column gives estimates in MET-seconds)
soj.3x.estimate <- soj_3x_original(
  data$axis1,
  data$axis2,
  data$axis3,
  AGread::get_VM(data[ , paste("axis", 1:3, sep = "")])
)

# apply the sojourn.1x function (METs column gives estimates in MET-seconds)
soj.1x.estimate <- soj_1x_original(data$axis1)
