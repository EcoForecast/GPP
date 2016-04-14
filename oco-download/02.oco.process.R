# Search bounding box with specified edge size
#' @param lat Measurement latitude
#' @param lon Measurement longitude
#' @param target.coords Coordinate pair (lat,lon vector) for center of bounding box
#' @param distance Edge length of bounding box
box.search <- function(lat, lon, target.coords, distance){
    coords <- cbind(lat, lon)
    cmin <- target.coords - distance
    cmax <- target.coords + distance
    in.box <- apply(coords, 1, function(x) all(x > cmin & x < cmax))
    return(in.box)
}

# Convert fluorescence from ph/s(/m2/sr/um) to mw(/m2/sr/nm)
# 1 mW = 1/1000 J / s
# Energy E (J) of N photons of wavelength L (m) : E = Nhc/L
#' @param N Fluorescence value, reported as ph/s/m2/sr/wavelength
#' @param wl Measurement wavelength (nm), either 757 or 771
ph.s.mw.convert <- function(N, wl){
    hc <- 6.62607004e-34 * 299792458 # Planck constant (h) x speed of light (c)
    wl.m <- wl * 10^-9               # Convert nm to m 
    sif.mw <- N * hc / wl.m
    return(sif.mw)
}

# Perform full processing
#' @param oco.dat Full downloaded data.table
#' @param coords Site coordinate vector (lat,lon)
#' @param distance Box size, in coordinate degrees (default = 0.1 deagres ~ 6 miles)
process <- function(oco.dat, coords, distance=0.1){
    require(data.table)
    # Subset data to site by bounding box
    dat <- oco.dat[box.search(measurement.lat, measurement.lon, coords, distance)]

    # Remove values with low quality flags
    dat <- dat[fluorescence.qual.flag == 0]

    # Remove negative and NA (coded as -99999) fluorescence values
    dat <- dat[!(fluorescence.757 < 0 | fluorescence.771 < 0)]

    # Convert to mw-based units (see ph.s.mw.convert function definition)
    sif.names <- c("sif.757", "sif.757.unc", "sif.771", "sif.771.unc")
    select.cols <- c("fluorescence.757", "fluorescence.757.unc",
                     "fluorescence.771", "fluorescence.771.unc")
    wl <- c(757, 757, 771, 771)
    dat[, (sif.names) := mapply(ph.s.mw.convert, .SD, wl, SIMPLIFY=FALSE),
        .SDcols = select.cols]

    # Process date and time
    dat[, measurement.time := as.POSIXct(measurement.time)]
    dat[, measurement.date := as.POSIXct(measurement.date)]

    # Calculate the spatial average (all space in grid over box)
    dat.avg <- dat[, lapply(.SD, mean, na.rm=TRUE),
                   by=measurement.date, .SDcols=sif.names]

}

# Make sure I'm in the project base directory
if(grepl("-download", getwd())) setwd("..")

# Apply processing to each site
# wcr -- Willow Creek
# lcr -- Lost creek
# syl -- Sylvania
library(data.table)

oco.dat <- fread("oco-download/fluorescence.csv", header=TRUE)

coords.wcr <- c(45.8060, -90.0798)
coords.lcr <- c(46.0827, -89.9792)
coords.syl <- c(46.2420, -89.3476)

distance <- 0.1

wcr.sif <- process(oco.dat, coords.wcr, distance = distance)
lcr.sif <- process(oco.dat, coords.lcr, distance = distance)
syl.sif <- process(oco.dat, coords.syl, distance = distance)

save(wcr.sif, lcr.sif, syl.sif, file="oco-download/oco.data.RData")
