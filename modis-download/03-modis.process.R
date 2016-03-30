library(data.table)
library(bit64)

load("modis-download/modis.data.RData")

# Scaling factors for integer values
# Source: https://lpdaac.usgs.gov/sites/default/files/public/modis/docs/MODIS-LAI-FPAR-User-Guide.pdf
band.factors <- c("Lai_1km"=0.1, "Fpar_1km"=0.01)

# Plot data for each site
for(i in seq_along(modis.list)){
    modis.name <- names(modis.list)[i]
    for(b in names(band.factors)){
        dat.full <- modis.list[[i]][Band == b]
        dat <- as.matrix(dat.full[, as.character(1:49), with=F]) * band.factors[b]
        # TODO: Parse the dates
        png(paste0("figures/", modis.name, ".", b, ".png"))
        matplot(dat, type='l', main=paste(names(modis.list)[i], b))
        dev.off()
    }
}


# Subset MODIS file

