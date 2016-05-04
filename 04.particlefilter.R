library(data.table)
library(ggplot2)
load("Rdata/current.forecast.RData")
load("Rdata/input.data.RData")
load("Rdata/aux.model.data.RData")
source("Rdata/particlefilter.functions.R")

today <- as.Date(Sys.Date())
forecast.rows <- data.dt[, which(date >= forecast.start.date)]
today.row <- data.dt[, which(date == today)]

quants <- c(0.025, 0.5, 0.975)

model.keep <- particlefilter(today.row, model.keep)
inds.gpp <- grep("gpp", colnames(model.keep))
gpp.forecast <- model.keep[,inds.gpp]

out.forecast <- as.data.table(t(apply(gpp.forecast,2,quantile,quants)))
colnames(out.forecast) <- c("low.forecast","med.forecast","high.forecast")

png(sprintf("figures/current.forecast.png", today.row))
plot(plot.particlefilter(today.row, ensemble.forecast, out.forecast))
dev.off()

save(model.keep, ensemble.forecast, file="current.forecast.RData")
