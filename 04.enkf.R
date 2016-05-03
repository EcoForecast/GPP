library(data.table)
load("model.output.RData")
load("input.data.RData")

forecast.rows <- data.dt[, which(date >= forecast.start.date)]
gpp.new <- rep(NA, nrow(data.dt))

#' This is is just updating the forecast with the observed data
#' no forecasting forward

 
for(t in forecast.rows){ 

  ##############################
  # Observations 
  
  # MODIS data
  o.fpar.modis <- data.dt[t,mu]
  # Flux data
  o.par <- data.dt[t,par.mean]
  o.gpp.flux <- data.dt[t,gpp.mean]
  o.gpp.flux.tau <- data.dt[t,gpp.tau]
  # OCO data
  o.sif <- data.dt[t,sif.757]
  
  ##############################
  # Model output 
  model <- data.table(
  gpp = model.samples[, grep("gpp", colnames(model.samples))[t]],
  fpar = model.samples[, grep("fpar", colnames(model.samples))[t]],
  par = model.samples[, grep("PAR", colnames(model.samples))[t]],
  eps = model.samples[, grep("eps", colnames(model.samples))[t]],
  
  lue = model.samples[, grep("lue", colnames(model.samples))],
  m.sif = model.samples[, grep("m_sif", colnames(model.samples))],
  b.sif = model.samples[, grep("b_sif", colnames(model.samples))],
  
  tau.process =  model.samples[, grep("tau_process", colnames(model.samples))],
  tau.sif = model.samples[, grep("tau_sif", colnames(model.samples))],
  tau.modis = model.samples[, grep("tau_modis", colnames(model.samples))],
  tau.flux = model.samples[, grep("tau_flux", colnames(model.samples))]
  )
  
  means <- model[, lapply(.SD, mean)]

  ###############
  f.gpp.sif <- ifelse(!is.na(o.sif), 
                      (means[,m.sif] * o.sif + means[,b.sif]) * means[,tau.sif], NA)
  f.gpp.modis <- ifelse(!is.na(o.fpar.modis),
                        (o.fpar.modis * means[,par] * means[,lue] + means[,eps]) * means[,tau.modis], NA)
  f.gpp.flux <- ifelse(!is.na(o.gpp.flux),
                       o.gpp.flux * o.gpp.flux.tau, NA)
  f.gpp.model <- means[,gpp] * means[,tau.process]
  
  sum_numer <- sum(c(f.gpp.model, f.gpp.sif, f.gpp.modis, f.gpp.flux),na.rm=T)
  sum_denom <- sum(c(means[,tau.process], 
                   ifelse(!is.na(f.gpp.sif),means[,tau.sif],NA),
                   ifelse(!is.na(f.gpp.modis),means[,tau.modis],NA),
                   ifelse(!is.na(f.gpp.flux),means[,tau.flux],NA)),na.rm = T)
  # Weighted sum 
  gpp.new[t] <- sum_numer/sum_denom
  cat("*")
}


# plot 
gpp.model <- colMeans(model.samples[, grep("gpp", colnames(model.samples))])

plot(gpp.model[forecast.rows], type = "l")
lines(gpp.new[forecast.rows], col="red")
