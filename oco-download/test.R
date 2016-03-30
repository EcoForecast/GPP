# Change directories if too deep. Then source function definition
wd <- getwd()
if(grepl("oco-download/", wd)) setwd("..")
source("oco-download/oco.download.R")

# Clear out log files
file.remove(file.path("oco-download", c("checked.files", 
                                        "checked.urls",
                                        "fluorescence.csv",
                                        "current.h5")))

# I know there are data for Feb 28, 2016
Date <- as.POSIXlt("2016-02-28")

oco.download.date(Date, Return=TRUE, check.date=FALSE)
