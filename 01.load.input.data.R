suppressMessages({
    library(data.table)
})

# Load OCO data
load("oco-download/oco.data.RData")
wcr.sif[, measurement.date := as.Date(measurement.date)]

# Load MODIS data
load("modis-download/Willow-Creek.Fpar_1km.RData")
wcr.fpar <- data.table(quants)

# Load processed flux tower data
load("flux-download/flux.processed.RData")
#wcr.fpar$par <- 150     # TODO: This is a MADE-UP value in the PAR range. Replace with real data.


# Align data based on dates
start.date <- as.Date("2000-01-01", tz = "UTC")
end.date <- as.Date(Sys.Date())
dates <- seq.Date(start.date, end.date, by="day")
data.dt <- data.table(date = dates)

setkey(data.dt, date)
setkey(flux.data, date)
data.dt <- flux.data[data.dt]

setkey(data.dt, date)
setkey(wcr.fpar, time)
data.dt <- wcr.fpar[data.dt]
setnames(data.dt, "time", "date")

setkey(data.dt, date)
setkey(wcr.sif, measurement.date)
data.dt <- wcr.sif[data.dt]
setnames(data.dt, "measurement.date", "date")

# Convert NaN to NA
nan2na <- function(x) {
    x[is.nan(x)] <- NA
    return(x)
}

data.dt <- data.dt[, lapply(.SD, nan2na)]

# Beta distribution parameters
data.dt[, modis_alpha := Alpha(mu, stdev)]
data.dt[, modis_beta := Beta(mu, stdev)]

save(data.dt, file="input.data.RData")

