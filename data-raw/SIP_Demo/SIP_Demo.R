rm(list = ls())
devtools::load_all()

# read input data
actigraph <- AG.file.reader("your.input.file.here")

# leaving out these two steps *should* give the same output as plain
# sojourns; if it doesn't, please report it as a bug
activpal <- AP.file.reader("your.activPAL.file.here")
data <- enhance.actigraph(actigraph, activpal)

# run SIP
sip.estimate <- sojourn_3x_SIP(data)

# play with sip.estimate here

# save the SIP output to disk
sojourns.file.writer(sip.estimate, "your.output.file.name.here")
