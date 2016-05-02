library(XML)
library(data.table)
flux.url="http://flux.aos.wisc.edu/data/cheas/wcreek/flux/current/"
flux.dat=readHTMLTable(flux.url,stringsAsFactors=FALSE)[[1]]

# Download data and add to master flux list
masterflux <- list()
for(flux.name in flux.dat$Name){
  if(grepl(".csv",flux.name)){
    print(flux.name)
    dat <- fread(paste0(flux.url, flux.name), skip=126, header=TRUE)
    masterflux[[flux.name]]=dat
  }
}
masterflux2=rbindlist(masterflux)

save(masterflux2,file="flux-download/download.flux.RData")

