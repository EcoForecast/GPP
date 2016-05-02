suppressMessages({
    library(rjags)
    library(data.table)
})

load("input.data.RData")
source("stats.helpers.R")

zero <- 1e-5
data.dt[stdev <= 0, mu := NA]
data.dt[mu < zero, mu := zero]

data <- list(ntime = nrow(data.dt),
             # MODIS data
             fpar_modis = data.dt$mu,
             # Flux data
             PAR = data.dt$par.mean,
             gpp_flux = data.dt$gpp.mean,
             # OCO data
             sif_oco = data.dt$sif.757,
             # Priors
             a_process = 0.1, r_process = 0.1,
             mu_lue = logmu(0.02, 0.01), tau_lue = logtau(0.02, 0.01), # FROM DATA -- double-dipped
             mu_eps_ic = 0, tau_eps_ic = 0.1,
             a_eps = 0.1, r_eps = 0.1,            # Uninformative priors
             a_fpar_ic = 0.5, b_fpar_ic = 0.5,  # Uninformative priors
             a_fpar = 0.1, r_fpar = 0.1,        # Uninformative priors
             a_sif = 0.1, r_sif = 0.1,            # Uninformative priors
             a_flux = 0.1, r_flux = 0.1,
             a_modis = 0.1, r_modis = 0.1,
             mu_m_sif = 11.82, tau_m_sif = 1/3^2,    # Means from Yang et al. 2015 GRL
             mu_b_sif = 1.19, tau_b_sif = 1/0.5^2,   # SD's are guesses for loosely-informative priors
             a_oco = 0.1, r_oco=0.1)

nchain <- 5
n.iter <- 10000
burnin <- 500000

init <- list()
gpp.comp <- data$gpp[!is.na(data$gpp)]
for(i in 1:nchain){
    init[[i]] <- list(gpp = sample(gpp.comp,data$ntime,replace=TRUE))
}

j.model <- jags.model(file = "gpp.lue.simple.bug",
                      data = data,
                      inits = init,
                      n.chains = nchain)

update(j.model, n.iter = burnin)
vars <- c("gpp", "lue", "m_sif", "b_sif")
jags.out   <- coda.samples(model = j.model,
                            variable.names = vars,
                            n.iter = n.iter,
                            thin = 50)

out <- as.matrix(jags.out)
save(out, file="out.RData")

