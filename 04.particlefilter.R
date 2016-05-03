particlefilter <- function(t, model.keep){
    print(t)
    # MODIS data
    o.fpar.modis <- data.dt[t,mu]
    # Flux data
    o.par <- data.dt[t,par.mean]
    o.gpp.flux <- data.dt[t,gpp.mean]
    #o.gpp.flux.tau <- data.dt[t,gpp.tau]
    # OCO data
    o.sif <- data.dt[t,sif.757]

    ##############################
    # Model output 

    nmod <- nrow(model.keep)
    output <- data.table(n = 1:nmod)

    for(v in states){
        vec <- model.keep[, grep(sprintf("%s[%d]", v, t), colnames(model.keep), fixed=TRUE)]
        output[,(v) := vec]
    }
    for(v in params){
        output[,(v) := model.keep[, grep(v, colnames(model.keep))]]
    }

    means <- output[, lapply(.SD, mean)]

    # Calculate observation uncertainty
    q95 <- c(-1,1) * qnorm(0.95)
    q99 <- c(-1,1) * qnorm(0.99)

    # Apply particle filter
    if(!is.na(o.fpar.modis)){
        like.fpar <- dnorm(output[,fpar], o.fpar.modis, output[,tau_modis])
    } else {
        like.fpar <- rep(1, nmod)
    }
        #ind.fpar <- sample.int(nmod, nmod, replace=TRUE, prob=like.fpar)
        #o.fpar.modis.unc <- o.fpar.modis * q99 * 1/sqrt(means[,tau_modis])
        #output <- output[fpar >= min(o.fpar.modis.unc)
                         #][fpar <= max(o.fpar.modis.unc)]
    if(!is.na(o.gpp.flux)){
        like.gpp <- dnorm(output[,gpp], o.gpp.flux, output[,tau_flux])
    } else {
        like.gpp <- rep(1, nmod)
    }
        #o.gpp.flux.unc <- o.gpp.flux * q99 * 1/sqrt(means[,tau_flux])
        #output <- output[gpp >= min(o.gpp.flux.unc)
                         #][gpp <= max(o.gpp.flux.unc)]
    index <- sample.int(nmod, nmod, replace=TRUE, prob=like.fpar*like.gpp)
    model.keep <- model.keep[index,]
    # Pseudocode for resampling
    # model.new <- model.keep[index,]
    # gpp.new <- gpp(fpar, par, lue, eps)
    # gpp.function <- function(fpar, par, lue, eps){
    #   gpp <- fpar * par * lue + eps
    ## TODO: SIF

    #n.keep <- output[,n]
    #print(length(n.keep))

    #model.keep <- model.keep[n.keep,]
}

plot.particlefilter <- function(t, out.model, out.forecast){
    obs.point <- 1 + t - forecast.rows[1]
    out_quants <- cbind(out.model, out.forecast)
    p4 <- ggplot(out_quants) + aes(x=x) +
        geom_ribbon(aes(ymin=low.model, ymax=high.model), fill="blue", alpha=0.3) +
        geom_ribbon(aes(ymin=low.forecast, ymax=high.forecast), fill="red", alpha=0.3) +
        geom_line(aes(y=med.model), color="black") +
        geom_line(aes(y=med.forecast), color="darkred") + 
        geom_vline(xintercept=obs.point, col="black", linetype="dashed")
    return(p4)
    
}   # end loop over time

library(data.table)
library(ggplot2)
load("model.output.RData")
load("input.data.RData")

tomorrow <- as.Date(Sys.Date() + 1)
forecast.rows <- data.dt[, which(date >= forecast.start.date)]
plot.rows <- data.dt[, which(date >= forecast.start.date & date <= tomorrow)]
gpp.new <- rep(NA, nrow(data.dt))

cols.keep <- unlist(c(sapply(states, function(x) sprintf("%s[%d]", x, forecast.rows)), params))
model.samples <- model.samples[, cols.keep]
model.keep <- model.samples

quants <- c(0.025, 0.5, 0.975)
inds.gpp <- grep("gpp", colnames(model.samples))
gpp.model <- model.samples[,inds.gpp]
out.model <- as.data.table(t(apply(gpp.model,2,quantile,quants)))
colnames(out.model) <- c("low.model","med.model","high.model")
out.model <- cbind(out.model, "x"=1:nrow(out.model))

##############################
# Observations 
#t <- forecast.rows[1]
  
for(t in plot.rows){
    model.keep <- particlefilter(t, model.keep)

    gpp.forecast <- model.keep[,inds.gpp]

    out.forecast <- as.data.table(t(apply(gpp.forecast,2,quantile,quants)))
    colnames(out.forecast) <- c("low.forecast","med.forecast","high.forecast")

    png(sprintf("figures/time.%d.png", t))
    plot(plot.particlefilter(t, out.model, out.forecast))
    dev.off()
}
