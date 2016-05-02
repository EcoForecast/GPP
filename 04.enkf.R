library(data.table)
load("model.output.RData")
load("input.data.RData")

# Get gpp from after today
forecast.rows <- data.dt[, which(date > forecast.start.date)]
gpp.inds <- grep("gpp", colnames(model.samples))[forecast.rows]

gpp.forecast.samples <- model.samples[, gpp.inds]
# TODO: Same thing for fPAR and SIF?
# TODO: Update the forecast moving forward in time...

#gpp.prior <-


