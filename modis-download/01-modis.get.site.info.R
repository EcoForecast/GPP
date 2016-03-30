# Get site information
library(data.table)

site.info.url <- "ftp://daac.ornl.gov/data/modis_ascii_subsets/5_MODIS_Subset_Sites_Information_Collection5.csv"
modis.site.info <- fread(site.info.url, header=TRUE)

# Get names of sites
sites <- c("Willow Creek", "Lost Creek", "^Sylvania")
modis.sites <- modis.site.info[sapply(sites, grep, Site_Name,
                                      ignore.case=TRUE)]

save(modis.sites, file="modis-download/modis.sites.RData")
