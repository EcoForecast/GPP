load("model.output.RData")

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

# Time series of the outputs
plot.ci <- function(v){
    ind <- grep(v, colnames(model.samples))
    ci <- t(apply(model.samples[,ind], 2, quantile, c(0.025, 0.5, 0.975)))
    x <- 1:nrow(ci)
    plot(0, 1, type='n', xlim=range(x), ylim=range(ci),
         xlab="Day", ylab=v, main=paste("Model output:", v))
    ciEnvelope(x, ci[,1], ci[,3], col="lightblue")
    lines(x, ci[,2])
}

arg <- commandArgs(trailingOnly = TRUE)
if(length(arg) > 1) png(filename = "figures/model.output.png")
par(mfrow=c(3,1))
plot.ci("gpp")
plot.ci("PAR")
plot.ci("fpar")
if(length(arg) > 1) dev.off()
