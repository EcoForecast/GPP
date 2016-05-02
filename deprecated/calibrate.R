library(data.table)
load("input.data.RData")

data.sub <- data.dt[gpp.max > 0.3]

# Mean GPP
data.sub[, modis.gpp.mean := mu * par.mean]
data.sub[, plot(modis.gpp.mean, gpp.mean)]
fit.mean <- lm(gpp.mean ~ modis.gpp.mean + 0, data.sub)
abline(fit.mean)

# Mean GPP
data.sub[, modis.gpp.max := mu * par.max]
data.sub[, plot(modis.gpp.max, gpp.max)]
fit.max <- lm(gpp.max ~ modis.gpp.max + 0, data.sub)
abline(fit.max)
