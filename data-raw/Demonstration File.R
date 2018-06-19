rm(list = ls())
source("data-raw/z__internal_add.R")

# Use youth grids
  # load("data-raw/Sojourn Grids.RData")
  # youth_grids <- SojGrids
  # internal_add(youth_grids)

# Use youth nnets
  # load("data-raw/Youth Sojourns NNets.RData")
  # sapply(c("hipCounts", "hipRaw", "wristCounts", "wristRaw"),
  #   function(x) {
  #     assign(paste("youth", x, sep = "_"),
  #       get(x, envir = globalenv()),
  #       envir = globalenv())
  #     rm(list = x, envir = globalenv())
  #     })
  # internal_add(youth_hipCounts)
  # internal_add(youth_hipRaw)
  # internal_add(youth_wristCounts)
  # internal_add(youth_wristRaw)

# Leftover demo code
  countsFile <- 'ActiGraph Files/ExampleFile_CountsHip.csv'
  countsData <- read.ActiGraph(countsFile)
  View(countsData)

  rawFile <- 'ActiGraph Files/ExampleFile_RawHip.csv'
  rawData <- read.ActiGraph(rawFile)
