library(data.table)
load("model.output.RData")
load("input.data.RData")

start.date <- as.Date("2016-03-28")

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
  
  vars1 <- c("gpp","fpar","PAR","eps")
  vars2 <- list("lue","m_sif","b_sif","tau_process","tau_sif","tau_modis","tau_flux")
  
  model <- data.table(n = 1:1000)
  
  for(v in vars1){
    model[,(v) := model.samples[, grep(v, colnames(model.samples))[t]]]
  }
  for(v in vars2){
    model[,(v) := model.samples[, grep(v, colnames(model.samples))]]
  }

  means <- model[, lapply(.SD, mean)]
  
  ###############
  f.gpp.sif <- ifelse(!is.na(o.sif), 
                      (means[,m_sif] * o.sif + means[,b._sif]) * means[,tau_sif], NA)
  f.gpp.modis <- ifelse(!is.na(o.fpar.modis),
                        (o.fpar.modis * means[,par] * means[,lue] + means[,eps]) * means[,tau_modis], NA)
  f.gpp.flux <- ifelse(!is.na(o.gpp.flux),
                       o.gpp.flux * o.gpp.flux.tau, NA)
  f.gpp.model <- means[,gpp] * means[,tau_process]
  
  sum_numer <- sum(c(f.gpp.model, f.gpp.sif, f.gpp.modis, f.gpp.flux),na.rm=T)
  sum_denom <- sum(c(means[,tau_process], 
                     ifelse(!is.na(f.gpp.sif),means[,tau_sif],NA),
                     ifelse(!is.na(f.gpp.modis),means[,tau_modis],NA),
                     ifelse(!is.na(f.gpp.flux),means[,tau_flux],NA)),na.rm = T)
  # Weighted sum 
  gpp.new[t] <- sum_numer/sum_denom
  cat("*")
}


# plot 
gpp.model <- colMeans(model.samples[, grep("gpp", colnames(model.samples))])

plot(gpp.model[forecast.rows], type = "l")
lines(gpp.new[forecast.rows], col="red")
