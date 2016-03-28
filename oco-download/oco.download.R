oco.download.date <- function(Date, Write=TRUE, Return=FALSE, check.exists=TRUE){
    require(rhdf5)
    require(XML)

    # Check if date is already in fluorescence. If it is, then skip.
    if(check.exists & file.exists("oco-download/fluorescence.csv")){
        Date.simple <- as.character(as.Date(Date))
        current.dat <- read.csv("oco-download/fluorescence.csv", stringsAsFactors = FALSE)
        if(Date.simple %in% current.dat$measurement.date){
            return("Date exists")
        }
    }

    # Get all download URLs
    doy <- Date$yday + 1            # Get day of year (need to offset by 1 for some reason)
    year <- Date$year + 1900        # Get current year (years since 1900)
    h5.url.base <- sprintf("http://oco2.gesdisc.eosdis.nasa.gov/opendap/OCO2_L2_IMAPDOAS.7r/%02d/%03d", year, doy)
    h5.url.contents <- paste0(h5.url.base, "/contents.html")
    h5.raw.table <- try(readHTMLTable(h5.url.contents, stringsAsFactors=FALSE)[[1]])
    if(class(h5.raw.table) == "try-error"){
        warning("Unable to download file list. Check internet connection, or data for today may not be available.")
        return("Unable to download")
    }
    h5.list <- h5.raw.table[grep("oco*", h5.raw.table[,1]),1]

    local.path <- "oco-download/current.h5"

    # Loop over all daily files and save the fluorescence values
    for(h5 in h5.list){
        h5.url <- paste0(h5.url.base, "/", h5)

        # Download file
        system(paste("wget -O", local.path, h5.url))

        # Get latitude and longitude
        lat <- h5read(local.path, "SoundingGeometry/sounding_latitude")
        lon <- h5read(local.path, "SoundingGeometry/sounding_longitude")
        hf.coords <- c("lat" = 42.5378, "lon" = -72.1715)
        index <- which.min((lat - hf.coords["lat"])^2 + (lon - hf.coords["lon"]))

        # Get values of fluorescence and time
        measure.list <- list(
            fluorescence.757 = h5read(local.path, "DOASFluorescence/fluorescence_radiance_757nm_idp")[index],
            fluorescence.757.unc = h5read(local.path, "DOASFluorescence/fluorescence_radiance_757nm_uncert_idp")[index],
            fluorescence.771 = h5read(local.path, "DOASFluorescence/fluorescence_radiance_757nm_idp")[index],
            fluorescence.771.unc = h5read(local.path, "DOASFluorescence/fluorescence_radiance_771nm_uncert_idp")[index],
            fluorescence.qual.flag = h5read(local.path, "DOASFluorescence/fluorescence_qual_flag_idp")[index],
            measurement.time.raw = h5read(local.path, "SoundingGeometry/sounding_time_string")[index]
        )

        measure.list$measurement.time <- strptime(measure.list$measurement.time.raw, "%Y-%m-%dT%H:%M:%S", tz = "GMT")
        measure.list$measurement.date <- strftime(measure.list$measurement.time, "%Y-%m-%d")

        # Append to old fluorescence table
        if(Write){
            csv.path <- "oco-download/fluorescence.csv"
            if(!file.exists(csv.path)){
                cat(c(names(measure.list), "\n"), file = csv.path, sep=",")
            }
            write.table(measure.list, file = csv.path, sep=",",
                        row.names=FALSE, col.names=FALSE, append=TRUE)
        }
        if(Return) return(measure.list)
    }
}
