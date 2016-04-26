suppressMessages({
    library(data.table)
    library(rjags)
})

source("stats.helpers.R")

# Load OCO data
load("oco-download/oco.data.RData")
wcr.sif[, time := as.Date(measurement.date)]

# Load MODIS data
load("modis-download/Willow-Creek.Fpar_1km.RData")
wcr.fpar <- data.table(quants)
load("flux-download/par.Rdata")
#wcr.fpar$par <- 150     # TODO: This is a MADE-UP value in the PAR range. Replace with real data.
par.table <- data.table("time" = as.Date(date.daily[which(as.Date(date.daily) %in% quants$time)]),
                         "par" = par.daily.sum[which(as.Date(date.daily) %in% quants$time)])
wcr.fpar <- merge(wcr.fpar, par.table, by = "time")


  
# Align data based on dates
start.date <- as.Date("2000-01-01", tz = "UTC")
end.date <- as.Date(Sys.Date())
dates <- seq.Date(start.date, end.date, by="day")
data.dt <- data.table(date = dates)

setkey(data.dt, date)
setkey(wcr.fpar, time)
data.dt <- wcr.fpar[data.dt]

setkey(data.dt, time)
setkey(wcr.sif, time)
data.dt <- wcr.sif[data.dt]
setnames(data.dt, "measurement.date", "time")

RandomWalk <- "
model{

#### Process Model
for(i in 2:ntime){
    gpp[i] ~ dnorm(gpp[i-1], tau_process)
}

#### Data Model
for(j in 1:n_has_modis){
    gpp_fpar[has_modis[j]] ~ dnorm(gpp[has_modis[j]], tau_fpar)
    fpar[has_modis[j]] <- gpp[has_modis[j]] / (PAR[has_modis[j]] * lue)
    fpar_modis[has_modis[j]] ~ dbeta(modis_alpha[has_modis[j]], modis_beta[has_modis[j]])
}

for(j in 1:n_has_sif){
    gpp_sif[has_sif[j]] ~ dnorm(gpp[has_sif[j]], tau_sif)
    sif[has_sif[j]] <- (gpp[has_sif[j]] - b_sif) / m_sif
    sif_oco[has_sif[j]] ~ dnorm(sif[has_sif[j]], tau_oco[has_sif[j]])
}


#### Priors
gpp[1] ~ dnorm(mu_ic,tau_ic)
lue ~ dlnorm(mu_lue, tau_lue)
tau_fpar ~ dgamma(a_fpar, r_fpar)
m_sif ~ dnorm(mu_m_sif, tau_m_sif)
b_sif ~ dnorm(mu_b_sif, tau_b_sif)
tau_sif ~ dgamma(a_sif, r_sif)
tau_process ~ dgamma(a_process, r_process)

}
"

# Beta distribution parameters
data.dt[, modis_alpha := Alpha(mu, stdev)]
data.dt[, modis_beta := Beta(mu, stdev)]

has_modis <- data.dt[, which(!is.na(mu) & stdev > 0)]
has_sif <- data.dt[, which(!is.na(sif.757))]


data <- list(ntime = nrow(data.dt),
             has_modis = has_modis,
             n_has_modis = length(has_modis),
             has_sif = has_sif,
             n_has_sif = length(has_sif),
             modis_alpha = data.dt$modis_alpha,
             modis_beta = data.dt$modis_beta,
             #fpar_modis = data.dt$mu,
             #tau_modis = 1/data.dt$stdev^2,
             PAR = data.dt$par,
             mu_lue = logmu(0.6, 0.2), tau_lue = logtau(0.6, 0.2), # From BETY
             a_fpar = 0.1, r_fpar = 0.1,        # Uninformative priors
             sif_oco = data.dt$sif.757,
             tau_oco = 1/data.dt$sif.757.unc^2,
             mu_m_sif = 11.82, tau_m_sif = 1/3^2,    # Means from Yang et al. 2015 GRL 
             mu_b_sif = 1.19, tau_b_sif = 1/0.5^2,   # SD's are guesses for loosely-informative priors
             a_sif = 0.1, r_sif = 0.1,            # Uninformative priors
             a_process = 0.1, r_process = 0.1,            # Uninformative priors
             mu_ic = 0, tau_ic = 1/5)


nchain <- 3
n.iter <- 10000
burnin <- 50000
#init <- list()
#for(i in 1:nchain){
    #y.samp = sample(y,length(y),replace=TRUE)
    #init[[i]] <- list(tau_add=1/var(diff(y.samp)),
                        #tau_obs=5/var(y.samp))
#}

j.model   <- jags.model(file = textConnection(RandomWalk),
                        data = data,
                        n.chains = 3)

update(j.model, n.iter = burnin)
vars <- c("gpp", "lue", "m_sif", "b_sif")
jags.out   <- coda.samples(model = j.model,
                            variable.names = vars,
                            n.iter = n.iter,
                            thin = 50)
  
out <- as.matrix(jags.out)
save(out, file="out.RData")

# A time series of the outputs, with MODIS fPAR and OCO for comparison
gpp.ind <- grep("gpp", colnames(out))
ci <- t(apply(out[,gpp.ind], 2, quantile, c(0.025, 0.5, 0.975)))

png(filename = "figures/model.output.png", width = 1000, height = 1100)
par(mfrow=c(3,1), mar=c(3,3,1,1))
xtime <- as.POSIXct(data.dt[[6]])
matplot(x=xtime, y=ci, type='l', col=c("red", "black", "red"), lty=c(2,1,2), xaxt = 'n')
axis.POSIXct(1, xtime, at = seq(xtime[1], xtime[length(xtime)], by="year"))
plot(data.dt[,mu])
plot(data.dt[,sif.757])
dev.off()

