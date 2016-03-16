# Download data up to latest
source("oco-download/oco.download.R")
start.date <- as.POSIXlt("2014-09-07", tz = "GMT")
end.date <- as.POSIXlt(Sys.Date())
Date <- end.date
while(difftime(Date, start.date) > 0){
    print(Date)
    dl <- oco.download.date(Date = Date, Return=TRUE, Write=TRUE)
    Date <- as.POSIXlt(as.Date(Date) - 1)
}


fluorescence.dat <- read.csv("oco-download/fluorescence.csv", stringsAsFactors=FALSE)
convert.to.date <- function(x){
    if(!is.numeric(x)){
        x <- as.POSIXlt(x)
    }
    return(x)
}
fluorescence.dat <- as.data.frame(lapply(fluorescence.dat, convert.to.date))

png("figures/oco.current.png", width=5, height=5, units="in", res=150)
par(mfrow=c(2,1), mar=c(5,5,1,1), cex=1)
with(fluorescence.dat, {
	plot(measurement.date, fluorescence.757, type='l')
	plot(measurement.date, fluorescence.771, type='l')
})
dev.off()
