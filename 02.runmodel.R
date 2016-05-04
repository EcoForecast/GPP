suppressMessages({
    library(rjags)
    library(data.table)
})

load("Rdata/input.data.RData")
source("stats.helpers.R")

zero <- 1e-5
data.dt[stdev <= 0, mu := NA]
data.dt[mu < zero, mu := zero]

# Remove data to give us something to forecast
data.cols <- c("mu", "par.mean", "gpp.mean", "sif.757")

# If there's a command line argument

forecast.start.date <- as.Date("2016-03-27")
data.dt[date > forecast.start.date, (data.cols) := NA]

data <- list(ntime = nrow(data.dt),
             doy = data.dt[,doy],
             # MODIS data
             fpar_modis = data.dt$mu,
             # Flux data
             PAR = data.dt$par.mean,
             gpp_flux = data.dt$gpp.mean,
             # OCO data
             sif_oco = data.dt$sif.757,
             # Priors
             mu_lue = logmu(0.02, 0.01), tau_lue = logtau(0.02, 0.01), # FROM DATA -- double-dipped
             mu_eps_ic = 0, tau_eps_ic = 0.1,
             a_eps = 0.1, r_eps = 0.1,          # Uninformative priors
             a_fpar = 0.1, r_fpar = 0.1,        # Uninformative priors
             a_sif = 0.1, r_sif = 0.1,          # Uninformative priors
             a_flux = 0.1, r_flux = 0.1,
             a_modis = 0.1, r_modis = 0.1,
             mu_m_sif = 11.82, tau_m_sif = 1/3^2,    # Means from Yang et al. 2015 GRL
             mu_b_sif = 1.19, tau_b_sif = 1/0.5^2,   # SD's are guesses for loosely-informative priors
             a_oco = 0.1, r_oco=0.1)

# MCMC configuration 
nchain <- 10
n.iter <- 15000
burnin <- 2000

init <- list()
gpp.comp <- data$gpp_flux[!is.na(data$gpp_flux)]
fpar.comp <- data$fpar_modis[!is.na(data$fpar_modis)]
par.comp <- data$PAR[!is.na(data$PAR)]
nt <- data$ntime
for(i in 1:nchain){
    init[[i]] <- list(gpp = sample(gpp.comp, nt, replace=TRUE),
                      fpar = sample(fpar.comp, nt, replace=TRUE))
}

print("Compiling JAGS model...")
j.model <- jags.model(file = "gpp.lue.simple.bug",
                      data = data,
                      inits = init,
                      n.chains = nchain)

print("Updating JAGS model (burnin)...")
n.update <- 20
for(i in 1:n.update){
    print(sprintf("[%d%%]", i*100/n.update))
    update(j.model, n.iter = round(burnin/n.update))
}

states <- c("gpp","fpar")#,"PAR","sif", "eps")
params <- c("lue","m_sif","b_sif","tau_process","tau_sif","tau_modis","tau_flux")
vars <- c(states, params)
print("Sampling JAGS model...")
jags.out <- coda.samples(model = j.model,
                         variable.names = vars,
                         n.iter = n.iter,
                         thin = 5)

print("Done! Saving output...")
model.samples <- as.matrix(jags.out)
input.data <- data.dt

fname <- "Rdata/aux.model.data.RData"
save(forecast.start.date, states, params, input.data,file = fname)
sprintf("File %s successfully saved = %s",fname, as.character(basename(fname) %in% dir("Rdata")))

fname <- "Rdata/model.output.RData"
save(model.samples, file=fname)
sprintf("File %s successfully saved = %s",fname, as.character(basename(fname) %in% dir("Rdata")))
print("Done!")

