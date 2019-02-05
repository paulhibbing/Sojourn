rm(list = ls())
devtools::load_all()


# IGNORE THIS SECTION -----------------------------------------------------

# source("data-raw/Lyden_Demo/sojourn.functions.R")
# load("data/example_data.rda")

# read input data
# actigraph <- AG.file.reader(
#   "data-raw/SIP_Demo/sampledata_1secDataTable.csv",
#   skip = 11
# )

# RE-RUN THIS SECTION IF NEEDED -------------------------------------------

# ag <- AGread::read_AG_counts(
#   "data-raw/SIP_Demo/sampledata_1secDataTable.csv",
#   skip = 11
# )
#
# ag <- setNames(
#   with(
#     ag,
#     data.frame(
#       Axis1, Axis2, Axis3, Vector.Magnitude, #as.character(Timestamp),
#       Timestamp,
#       stringsAsFactors = FALSE
#     )
#   ),
#   c("counts", "axis2", "axis3", "vm", "Time")
# )
#
# SIP_ag <- ag
# devtools::use_data(SIP_ag, overwrite = TRUE)
# rm(SIP_ag)

# leaving out these two steps *should* give the same output as plain
# sojourns; if it doesn't, please report it as a bug

load("data/SIP_ag.rda")

# SIP_ap <- AP.file.reader("inst/extdata/sampledata Events.csv")
# devtools::use_data(SIP_ap, overwrite = TRUE)
load("data/SIP_ap.rda")

ag <- enhance.actigraph(SIP_ag, SIP_ap)

# run SIP
sip.estimate <- sojourn_3x_SIP(ag)

# play with sip.estimate here

# save the SIP output to disk
sojourns.file.writer(sip.estimate, "your.output.file.name.here")
