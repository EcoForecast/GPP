oco.download.date <- function(Date, Write=TRUE, Return=FALSE,
                              check.date=TRUE,
                              check.url=TRUE){
    require(rhdf5)
    require(XML)

    # Set latidue bounding box
    lat.min <- 45
    lat.max <- 47
    lon.min <- -91
    lon.max <- -89

    in.box <- function(lat, lon){
        lat > lat.min & lat < lat.max & lon > lon.min & lon < lon.max
    }

    # Check if date is already in fluorescence. If it is, then skip.
    if(check.date & file.exists("oco-download/fluorescence.csv")){
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

    # Check if URL is already in URL list
    check.url.file <- "oco-download/checked.urls"
    if(check.url & file.exists(check.url.file)){
        url.exists <- any(grepl(h5.url.base, readLines(check.url.file)))
        if(url.exists){
            return("URL exists")
        }
    }

    # Add URL to check file 
    cat(paste0(h5.url.base, "\n"), file=check.url.file, append=TRUE)


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

        # TODO: Add URL to downloads list -- check against it in the future when trying files

        # Download file
        system(paste("wget -O", local.path, h5.url))

        # Get latitude and longitude
        lat <- h5read(local.path, "SoundingGeometry/sounding_latitude")
        lon <- h5read(local.path, "SoundingGeometry/sounding_longitude")
        indices <- which(in.box(lat, lon))

        if(length(indices) == 0){
            print("No coordinates in bounding box. Moving to next file")
            next
        } 

        # Get values of fluorescence and time
        measure.list <- list(
            fluorescence.757 = h5read(local.path, "DOASFluorescence/fluorescence_radiance_757nm_idp")[indices],
            fluorescence.757.unc = h5read(local.path, "DOASFluorescence/fluorescence_radiance_757nm_uncert_idp")[indices],
            fluorescence.771 = h5read(local.path, "DOASFluorescence/fluorescence_radiance_757nm_idp")[indices],
            fluorescence.771.unc = h5read(local.path, "DOASFluorescence/fluorescence_radiance_771nm_uncert_idp")[indices],
            fluorescence.qual.flag = h5read(local.path, "DOASFluorescence/fluorescence_qual_flag_idp")[indices],
            measurement.time.raw = h5read(local.path, "SoundingGeometry/sounding_time_string")[indices]
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
