library(data.table)
library(rjags)

load("input.data.RData")
source("stats.helpers.R")

zero <- 1e-5
data.dt <- data.dt[1:365]
data.dt[mu < zero, mu := zero]
fpar.mu <- data.dt[, mu]
fpar.sd <- data.dt[, stdev]

likemodel <- "model{
# Process model
for(i in 2:ny){
    # Random walk, but moment-matched to mean and variance of Beta distribution
    secterm[i] <- (fpar[i-1] * (1 - fpar[i-1]) * tau_fpar - 1)
    a[i] <- fpar[i-1] * secterm[i]
    b[i] <- (1 - fpar[i-1]) * secterm[i]
    fpar[i] ~ dbeta(a[i], b[i])
    fpar.modis[i] ~ dnorm(fpar[i], tau_modis)

    #fpar[i] ~ dnorm(fpar[i-1], tau_fpar)
}

# Prior
fpar[1] ~ dbeta(1,1)
tau_fpar ~ dgamma(0.1, 0.1)
tau_modis ~ dgamma(0.1, 0.1)
}"

data <- list(fpar.modis = fpar.mu,
             ny = length(fpar.mu))
n.chain <- 1

init <- list()
for(i in n.chain){
    init[[i]] <- list(fpar = sample(fpar.mu, data$ny, replace=TRUE))
}

jm <- jags.model(file=textConnection(likemodel), data=data,
                 inits = init, n.chains = n.chain)

update(jm, 10000)

out <- coda.samples(jm, n.iter = 1000,
                    variable.names = c("fpar", "tau_fpar", "tau_modis"))
out.mat <- as.matrix(out)

fpar.ind <- grep("fpar", colnames(out.mat))
ci <- t(apply(out.mat[,fpar.ind], 2, quantile, c(0.025, 0.5, 0.975)))
matplot(ci, type='l', col=c(2,1,2), lty=c(2,1,2), ylim=c(-0.5,1.5))

