load("out.RData")

# A time series of the outputs, with MODIS fPAR and OCO for comparison
gpp.ind <- grep("gpp", colnames(out))
ci <- t(apply(out[,gpp.ind], 2, quantile, c(0.025, 0.5, 0.975)))
png(filename = "figures/model.output.png")
matplot(ci, type='l', col=c(2,1,2), lty=c(2,1,2))
abline(v=seq(1,length(gpp.ind), by=365))
dev.off()
