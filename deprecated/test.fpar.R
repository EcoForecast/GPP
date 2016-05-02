library(data.table)
library(rjags)

load("input.data.RData")
source("stats.helpers.R")

y <- data.dt[, mu]
y[y < 1e-3] <- 1e-3
sd_modis <- data.dt[, stdev]
sd_modis[is.na(sd_modis) | sd_modis <= 0] <- -999

#yc <- y
#yc[yc < 1e5] <- NA
#y0 <- y
#y0[!(y0 < 1e5)] <- NA

#yc.ind <- which(y == 0)
#y0.ind <- which()

#index <- is.na(y)

model <- "model{
# Process model
for(i in 2:ny){
    fpar[i] ~ dnorm(fpar[i-1], tau_process) T(0,1)
    tau[i] <- ifelse(sd_modis[i] < 0, 1, 1/sd_modis^2)
    y[i] ~ dnorm(fpar[i], tau[i]) T(0,1)
}

# Prior
y[1] ~ dbeta(1,1)
fpar[1] ~ dbeta(1,1)
a ~ dlnorm(amu, atau)
b ~ dlnorm(bmu, btau)
tau_process ~ dgamma(0.1, 0.1)
}"

data <- list(y = y,
             ny = length(y),
             sd_modis = data.dt[,stdev],
             amu = logmu(2, 1),
             atau = logtau(2,1),
             bmu = logmu(2,1),
             btau = logtau(2,1))

jm <- jags.model(file = textConnection(model),
                 data = data,
                 n.chains = 2)

out.raw <- coda.samples(jm, variable.names = c("a", "b", "y"),
                        n.iter = 2000)

