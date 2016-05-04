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


# Align data based on dates
start.date <- as.Date("2000-01-01", tz = "UTC")
end.date <- as.Date(Sys.Date() + 180)
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

# Get day of year
data.dt[, doy := as.numeric(strftime(date, "%j"))]

save(data.dt, file="Rdata/input.data.RData")

