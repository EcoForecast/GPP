suppressMessages({
    library(rjags)
    library(data.table)
})

load("input.data.RData")
source("stats.helpers.R")

data.dt[stdev <= 0, mu := NA]
data.dt[mu < 1e-3, mu := 1e-3]
has_modis <- data.dt[, which(!is.na(mu))]
has_sif <- data.dt[, which(!is.na(sif.757))]
has_flux <- data.dt[, which(!is.na(gpp.mean))]
no_modis <- data.dt[, which(is.na(mu))][-1]

data <- list(ntime = nrow(data.dt),
             has_modis = has_modis,
             n_has_modis = length(has_modis),
             no_modis = no_modis,
             n_no_modis = length(no_modis),
             has_sif = has_sif,
             n_has_sif = length(has_sif),
             has_flux = has_flux,
             n_has_flux = length(has_flux),
             # MODIS data
             #modis_alpha = data.dt$modis_alpha,
             #modis_beta = data.dt$modis_beta,
             modis_mu = data.dt$mu,
             modis_sd = data.dt$stdev,
             # Flux data
             PAR = data.dt$par.mean,
             gpp_flux = data.dt$gpp.mean,
             # OCO data
             sif_oco = data.dt$sif.757,
             tau_oco = 1/data.dt$sif.757.unc^2,
             # Priors
             mu_lue = logmu(0.02, 0.01), tau_lue = logtau(0.02, 0.01), # FROM DATA -- double-dipped
             mu_eps_ic = 0, tau_eps_ic = 0.1,
             a_eps = 0.1, r_eps = 0.1,            # Uninformative priors
             a_fpar_ic = 0.5, b_fpar_ic = 0.5,  # Uninformative priors
             a_fpar = 0.1, r_fpar = 0.1,        # Uninformative priors
             mu_m_sif = 11.82, tau_m_sif = 1/3^2,    # Means from Yang et al. 2015 GRL
             mu_b_sif = 1.19, tau_b_sif = 1/0.5^2,   # SD's are guesses for loosely-informative priors
             a_sif = 0.1, r_sif = 0.1,            # Uninformative priors
             a_flux = 0.1, r_flux = 0.1,
             a_process = 0.1, r_process=0.1)

nchain <- 3
n.iter <- 10000
burnin <- 50000

init <- list()
gpp.comp <- data$gpp[!is.na(data$gpp)]
for(i in 1:nchain){
    init[[i]] <- list(gpp = sample(gpp.comp,data$ntime,replace=TRUE))
}

j.model <- jags.model(file = "gpp.lue.bug",
                      data = data,
                      inits = init,
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
matplot(ci, type='l', col=c(2,1,2), lty=c(2,1,2))
abline(v=seq(1,length(gpp.ind), by=365))

#png(filename = "figures/model.output.png", width = 1000, height = 1100)
#par(mfrow=c(3,1), mar=c(3,3,1,1))
#xtime <- as.POSIXct(data.dt[[6]])
#matplot(x=xtime, y=ci, type='l', col=c("red", "black", "red"), lty=c(2,1,2), xaxt = 'n')
#axis.POSIXct(1, xtime, at = seq(xtime[1], xtime[length(xtime)], by="year"))
#plot(data.dt[,mu])
#plot(data.dt[,sif.757])
#dev.off()

