library(data.table)
library(ggplot2)
library(gridExtra)
library(lubridate)

load("flux-download/download.flux.RData")
dat <- data.table(masterflux2)
rm(masterflux2)

# Convert -999 to NA values
to.na <- function(x){
    if(is.numeric(x)) x[x < -990] <- NA
    return(x)
}

dat <- dat[, lapply(.SD, to.na)]

dat[, min := hour*60]
dat[, datetime := as.POSIXct(paste(year, month, day, floor(hour), min %% 60), format = "%Y %m %j %H %M")]
dat[, date := as.Date(datetime)]

flux.data <- dat[, list(par.mean = mean(par, na.rm=T),
                       par.max = max(par, na.rm=T),
                       par.sum = sum(par, na.rm=T),
                       tair.mean = mean(tair, na.rm=T),
                       tair.max = max(tair, na.rm=T),
                       preicp.sum = sum(precipitation, na.rm=T),
                       gpp.mean = mean(gpp, na.rm=T),
                       gpp.max = max(gpp, na.rm=T)),
                by = date]

inf.to.na <- function(x){
    x[is.infinite(x)] <- NA
    return(x)
}
flux.data <- flux.data[, lapply(.SD, inf.to.na)]

save(flux.data, file="flux-download/flux.processed.RData")

