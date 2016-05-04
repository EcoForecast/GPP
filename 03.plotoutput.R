load("model.output.RData")
load("aux.model.data.RData")

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

# Time series of the outputs

plot.ci <- function(v, xlim=NULL, ylim=NULL){
    ind <- grep(v, colnames(model.samples))
# Something weird in last fpar timestep
    ind <- ind[-length(ind)]
    ci <- t(apply(model.samples[,ind], 2, quantile, c(0.025, 0.5, 0.975)))
    x <- 1:nrow(ci)
    if(is.null(xlim)) xlim = range(x)
    if(is.null(ylim)) ylim = range(ci)
    plot(0, 1, type='n', xlim=xlim, ylim=ylim,
         xlab="Day", ylab=v, main=paste("Model output:", v))
    ciEnvelope(x, ci[,1], ci[,3], col="lightblue")
    lines(x, ci[,2])
}

arg <- commandArgs(trailingOnly = TRUE)
if(length(arg) > 1) png(filename = "figures/model.ts.png")
par(mfrow=c(length(states),1), mar=c(2, 2, 1, 1))
for(v in states) plot.ci(v, xlim=c(0, 6000))
if(length(arg) > 1) dev.off()

# Density plots for parameters

if(length(arg) > 1) png(filename = "figures/model.params.png")
par(mfrow=c(4,4))
for(v in params) plot(density(model.samples[,v]), main=v)
if(length(arg) > 1) dev.off()

