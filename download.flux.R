library(XML)
flux.url="http://flux.aos.wisc.edu/data/wcreek-raw/ascii/"
flux.dat=readHTMLTable(flux.url,stringsAsFactors=FALSE)[[1]]
flux.dat
str(flux.dat)

##create the list, which is a bunch of data tables, then after that squish em together
masterflux=list()

for(flux.name in flux.dat$Name){
if(grepl(".csv",flux.name)){
 download.file(paste0(flux.url,flux.name),flux.name)
 con=gzfile(flux.name)
 readLines(con)[30:40]
 header <- readLines(con)[31]
 dat=read.csv(con, header=FALSE, skip=31)
 #close(con)
 ##lop off columns 10-16
 dat2=dat[,1:9]
 
 ##split the header
 split=strsplit(header,split=",")[[1]] 
 ##add to list
 ##attach the headers to columns 1-9
 colnames(dat2)=unlist(split)

 masterflux[[flux.name]]=dat2
}
}

masterflux2=do.call(rbind,masterflux)
head(masterflux2)
dim(masterflux2)
##command will be masterflux=do.call("Name of list",rbind)
 
save(masterflux2,file="download.flux.RData")


