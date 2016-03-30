# Get a bunch of data 
library(XML)
library(RCurl)
library(ncdf4)

# Set latidue bounding box
lat.min <- 45
lat.max <- 47
lon.min <- -91
lon.max <- -89

in.box <- function(lat, lon){
    lat > lat.min & lat < lat.max & lon > lon.min & lon < lon.max
}

ftp.url.base <- "ftp://oco2.gesdisc.eosdis.nasa.gov/data/s4pa/OCO2_DATA/OCO2_L2_Lite_SIF.7r/2016/"

file.list.raw <- getURL(ftp.url.base, verbose=TRUE, ftp.use.epsv=TRUE,
                    dirlistonly=TRUE)
file.list.split <- strsplit(file.list.raw, split="\n")[[1]]
file.list <- file.list.split[!grepl(".*.xml$", file.list.split)]

# Download a files and see how many fit in bounding box
for(i in seq_along(file.list)){
    fname <- file.list[i]
    fpath <- file.path("oco-download", "raw.download", fname)
    full.url <- paste0(ftp.url.base, fname)
    system(paste("wget -P oco-download/raw.download/", full.url))
    oco.file <- nc_open(fpath)
    lat <- ncvar_get(oco.file, "latitude")
    lon <- ncvar_get(oco.file, "longitude")
    indices <- which(in.box(lat, lon))
    nc_close(oco.file)
    if(length(indices) == 0) file.remove(fpath)
}
