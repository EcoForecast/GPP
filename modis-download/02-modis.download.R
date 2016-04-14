# Make sure I'm in the project base directory
if(grepl("-download", getwd())) setwd("..")

library(data.table)     # fread, general data manipulation
library(bit64)          # For reading long integers
#library(curl)          # To grab directory information
#library(stringr)       # To process date in MODIS name

base.url <- "ftp://daac.ornl.gov/data/modis_ascii_subsets/C5_MOD15A2/data/"

# Locate sites of interest
modis.site.file <- "modis-download/modis.sites.RData"
if(file.exists(modis.site.file)){
    load("modis-download/modis.sites.RData")
} else {
    source("modis-download/01-modis.get.site.info.R")
}
site.names <- modis.sites[, Site_ID]
fnames <- sprintf("MOD15A2.%s.txt", site.names)

# For checking if download is needed
# Get raw directory listing
#con <- curl(base.url)
#dat <- readLines(con)
#close(con)

#fileinfo <- dat[grep(fnames, dat)]

# Get the date modified (for checking if need to download)
#month.search <- paste0(month.abb, collapse = "|")
#date.search <- sprintf("(%s) +[[:digit:]]{1,2}", month.search)
#date.string <- paste(str_extract(fileinfo, date.search), "2016")
#date.posix <- as.POSIXlt(date.string, format="%b %d %Y", tz="UTC")

# Download file
# NOTE: It's pretty small, so no real need to check for download every time
modis.list <- list()
for(fname in fnames){
    full.url <- paste0(base.url, fname)
    modis.dat <- fread(full.url, header=TRUE)
    modis.list[[fname]] <- modis.dat
}

save(modis.list, file="modis-download/modis.data.RData")
