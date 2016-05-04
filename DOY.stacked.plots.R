
xvals <- tapply(data.dt$doy,year(data.dt$date),function(x) return(x))

png("figures/dailyGPPstacked.png", width = 600, height = 600)
yvals <- tapply(data.dt$gpp.mean,year(data.dt$date),function(x) return(x))
plot(1:max(unlist(xvals),na.rm=TRUE),ylim=(c(0,max(unlist(yvals),na.rm=TRUE))),type="n", ylab = "", xlab = "")
mapply(lines,xvals,yvals,col=1:dim(xvals)+6) #added 6 because easier to read
title(main="GPP", xlab="Day of Year", cex.main = 3, cex.lab = 2)
dev.off()

png("figures/dailyFPARstacked.png", width = 600, height = 300)
yvals <- tapply(data.dt$mu,year(data.dt$date),function(x) return(x))
plot(1:max(unlist(xvals),na.rm=TRUE),ylim=(c(0,max(unlist(yvals),na.rm=TRUE))),type="n", xlab = "", ylab = "")
title(main="MODIS 15 fPAR", xlab="Day of Year", cex.main = 3, cex.lab = 2)
mapply(points,xvals,yvals,col=1:dim(xvals)+6, lwd= 2, na.rm =TRUE) #added 6 because easier to read
dev.off()

png("figures/dailyPARstacked.png", width = 600, height = 300)
yvals <- tapply(data.dt$par.mean,year(data.dt$date),function(x) return(x))
plot(1:max(unlist(xvals),na.rm=TRUE),ylim=(c(0,max(unlist(yvals),na.rm=TRUE))),type="n", xlab = "", ylab = "")
title(main="PAR", xlab="Day of Year", cex.main = 3, cex.lab = 2)
mapply(lines,xvals,yvals,col=1:dim(xvals)+6, lwd= 2, na.rm =TRUE) #added 6 because easier to read
dev.off()
