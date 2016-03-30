library(data.table)     # fread, general data manipulation
library(bit64)          # For reading long integers
#library(curl)          # To grab directory information
#library(stringr)       # To process date in MODIS name

base.url <- "ftp://daac.ornl.gov/data/modis_ascii_subsets/C5_MOD15A2/data/"

# Locate sites of interest
# TODO: Add other locations -- Sylvania, Lost Creek
fnames <- c("MOD15A2.fn_uswiwill.txt"           # Willow creek
            )

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
    full.path <- file.path("modis-download", fname)
    modis.dl <- download.file(full.url, full.path)
    modis.dat <- fread(full.path, header=TRUE)
    modis.list[[fname]] <- modis.dat
}

save(modis.list, file="modis-download/modis.data.RData")
