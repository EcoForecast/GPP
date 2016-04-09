library(data.table)
oco.dat <- fread("fluorescence.csv", header=TRUE)

# Search bounding box with specified edge size
box.search <- function(lat, lon, target.coords, distance){
    coords <- cbind(lat, lon)
    cmin <- target.coords - distance
    cmax <- target.coords + distance
    in.box <- apply(coords, 1, function(x) all(x > cmin & x < cmax))
    return(in.box)
}

# Subset fluorescence data by site
coords.wcr <- c(45.8060, -90.0798)
coords.lcr <- c(46.0827, -89.9792)
coords.syl <- c(46.2420, -89.3476)

distance <- 0.1
wcr.sif <- oco.dat[box.search(measurement.lat, measurement.lon, coords.wcr, distance)]
lcr.sif <- oco.dat[box.search(measurement.lat, measurement.lon, coords.lcr, distance)]
syl.sif <- oco.dat[box.search(measurement.lat, measurement.lon, coords.syl, distance)]


# Generate location map
library(maps)
coords <- rbind(coords.wcr, coords.lcr, coords.syl)
oco.sites <- oco.dat[, .N, by=list(measurement.lat, measurement.lon)]
zo <- 0.25
ylims <- oco.dat[, c(min(measurement.lat)-zo, max(measurement.lat)+zo)]
xlims <- oco.dat[, c(min(measurement.lon)-zo, max(measurement.lon)+zo)]
map("state", xlim=xlims, ylim=ylims)
oco.sites[points(measurement.lon, measurement.lat, pch=20, cex=0.5,
                 col=rgb(0, 0, 0, 0.05))]
points(coords, pch=17, cex=2, col=c("red", "blue", "green"))


# Convert times to POSIXct
oco.dat[, c("measurement.time", "measurement.date") := 
        list(as.POSIXct(measurement.time), as.POSIXct(measurement.date))]

# Calculate means and uncertainties by date
summary.func <- function(x){
    m <- mean(x, na.rm=TRUE)
    s <- sd(x, na.rm=TRUE)
    se <- 1.96 * s / sqrt(length(x))
    hi <- m + se
    lo <- m - se
    return(list(mu = m, hi = hi, lo = lo))
}
data.cols <- c("fluorescence.757", "fluorescence.771")
oco.means <- oco.dat[, summary.func(fluorescence.757), by = measurement.date]


ts.plt <- ggplot(oco.means) + 
    aes(x = measurement.date, y = mu, ymin=hi, ymax=lo) + 
    geom_ribbon()
plot(ts.plt)
