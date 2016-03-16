# Download data up to latest

source("oco-download/oco.download.R")
try(file.remove("oco-download/fluorescence.csv"))
start.date <- as.POSIXlt("2014-09-07", tz = "GMT")
end.date <- as.POSIXlt(Sys.Date())
Date <- end.date
while(difftime(Date, start.date) > 0){
    print(Date)
    dl <- oco.download.date(Date = Date, Return=TRUE, Write=TRUE)
    Date <- as.POSIXlt(as.Date(Date) - 1)
}

fluorescence.dat <- read.csv("oco-download/fluorescence.csv")
png("figures/oco.current.png", width=5, height=5, units="in", res=300)
par(mfrow=c(2,1))
with(fluorescence.dat,
	plot(measurement.time, fluorescence.757, type='l')
	plot(measurement.time, fluorescence.771, type='l')
)
dev.off()
