library(XML)
library(data.table)
flux.url="http://flux.aos.wisc.edu/data/cheas/wcreek/flux/current/"
flux.dat=readHTMLTable(flux.url,stringsAsFactors=FALSE)[[1]]
str(flux.dat)

masterflux=list()
flux.dat$Name
for(flux.name in flux.dat$Name){
  if(grepl(".csv",flux.name)){
    print(flux.name)
    download.file(paste0(flux.url,flux.name),flux.name)
    con=file(flux.name)
    header<-readLines(con)[127]
    dat=read.csv(con, header=FALSE,skip=127)
    
    
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
dailyGPP=tapply(masterflux2$gpp,list(masterflux2$day,masterflux2$year),sum)

#


dir.create("figures")
png("figures/dailyGPPstacked.png")
plot(1:366,dailyGPP[,1],ylim=c(-10,1000),col="black",xlab="DOY",ylab="GPP (mu mol/s/m2)")
for (i in 2:ncol(dailyGPP))
{
  lines(1:366,dailyGPP[,i],col=i,add=T)
}
legend("topleft",legend=c(colnames(dailyGPP)),cex=0.4,fill=1:18)
dev.off()

dir.create("figures")
png("figures/dailyGPPtimeseries.png")
dailyGPPstack=stack(as.data.frame(dailyGPP))
plot(1:nrow(dailyGPPstack),dailyGPPstack[,1],ylim=c(-10,1000),col="black",xlab="Year",ylab="GPP (mu mol/s/m2)",type="l",xaxt="n")
ticks=c(0,rep(NA,ncol(dailyGPP)-1))
for(i in 1:ncol(dailyGPP)-1){
ticks[i]= 366*i
}
axis(side=1,at=ticks,labels=colnames(dailyGPP),las=2)
length(ticks)
dev.off()



