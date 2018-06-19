# clear memory
rm(list=ls())

# load the required neural networks
load("~/sojournRfiles/nnet3ests.RData")
load("~/sojournRfiles/cent.1.RData")
load("~/sojournRfiles/scal.1.RData")
load("~/sojournRfiles/class.nnn.use.this.Rdata")

# load the R functions
source("~/sojournRfiles/sojourn.functions.R")

# load the nnet library
library(nnet)

# load an ActiGraph RT3X file
data <- AG.file.reader("~/sojournRfiles/AG03MN.csv")

# apply the sojourn.3x function (METs column gives estimates in MET-seconds)
soj.3x.estimate <- sojourn.3x(data$counts,data$axis2,data$axis3,data$vm)

# apply the sojourn.1x function (METs column gives estimates in MET-seconds)
soj.1x.estimate <- sojourn.1x(data$counts)
