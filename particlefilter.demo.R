library(data.table)
library(ggplot2)
load("model.output.RData")
load("input.data.RData")
load("aux.model.data.RData")
source("particlefilter.functions.R")

stop.pf <- as.Date(Sys.Date() - 1)
forecast.rows <- data.dt[, which(date >= forecast.start.date)]
plot.rows <- data.dt[, which(date >= forecast.start.date & date <= stop.pf)]

cols.keep <- unlist(c(sapply(states, function(x) sprintf("%s[%d]", x, forecast.rows)), params))
model.samples <- model.samples[, cols.keep]
model.keep <- model.samples

quants <- c(0.025, 0.5, 0.975)
inds.gpp <- grep("gpp", colnames(model.samples))
gpp.model <- model.samples[,inds.gpp]
out.model <- as.data.table(t(apply(gpp.model,2,quantile,quants)))
colnames(out.model) <- c("low.model","med.model","high.model")
out.model <- cbind(out.model, "x"=1:nrow(out.model))

for(t in plot.rows){
    model.keep <- particlefilter(t, model.keep)

    gpp.forecast <- model.keep[,inds.gpp]

    out.forecast <- as.data.table(t(apply(gpp.forecast,2,quantile,quants)))
    colnames(out.forecast) <- c("low.forecast","med.forecast","high.forecast")

    png(sprintf("figures/time.%d.png", t))
    plot(plot.particlefilter(t, out.model, out.forecast))
    dev.off()
}

ensemble.forecast <- out.model
save(model.keep, ensemble.forecast, file="current.forecast.RData")
