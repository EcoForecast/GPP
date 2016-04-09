# Make sure I'm in the project base directory
if(grepl("-download", getwd())) setwd("..")

# Download data up to latest
source("oco-download/oco.download.function.R")
start.date <- as.POSIXlt("2014-09-07", tz = "GMT")
end.date <- as.POSIXlt(Sys.Date())
Date <- end.date
while(difftime(Date, start.date) > 0){
    print(Date)
    dl <- oco.download.date(Date = Date, Return=TRUE, Write=TRUE)
    Date <- as.POSIXlt(as.Date(Date) - 1)
}
