load("model.output.RData")

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

# Time series of the outputs
ts.vars <- c("gpp", "PAR", "fpar", "gpp_sif", "sif", "eps")

# Something weird in last fpar timestep
plot.ci <- function(v, xlim=NULL, ylim=NULL){
    ind <- grep(v, colnames(model.samples))
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
par(mfrow=c(length(ts.vars),1), mar=c(2, 2, 1, 1))
for(v in ts.vars) plot.ci(v, xlim=c(0, 6000))
if(length(arg) > 1) dev.off()

# Density plots for parameters
dens.vars <- c("apar", "bpar", "cpar", "tau_par",
               "fpwidth", "fpcenter", "tau_fpar",
               "tau_modis", "tau_flux",
               "lue", "tau_process", "tau_eps",
               "m_sif", "b_sif", "tau_sif", "tau_oco")


if(length(arg) > 1) png(filename = "figures/model.params.png")
par(mfrow=c(4,4))
for(v in dens.vars) plot(density(model.samples[,v]), main=v)
if(length(arg) > 1) dev.off()

