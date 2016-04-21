library(XML)
flux.url="http://flux.aos.wisc.edu/data/cheas/wcreek/flux/current/"
flux.dat=readHTMLTable(flux.url,stringsAsFactors=FALSE)[[1]]
str(flux.dat)

masterflux=list()
flux.dat$Name
for(flux.name in flux.dat$Name){
  if(grepl(".csv",flux.name)){
    download.file(paste0(flux.url,flux.name),flux.name)
    con=file(flux.name)
    dat=read.csv(con, header=FALSE,skip=126)
    header<-readLines(con)[127]
    ##split the header
    split=strsplit(header,split=",")[[1]] 
    ##add to list
    ##attach the headers to columns
    colnames(dat)=unlist(split)
    masterflux[[flux.name]]=dat
  }
}
masterflux2=do.call(rbind,masterflux)
save(masterflux2,file="download.flux.RData")
