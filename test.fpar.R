library(data.table)
library(rjags)

load("input.data.RData")
source("stats.helpers.R")

y <- data.dt[, mu]

#yc <- y
#yc[yc < 1e5] <- NA
#y0 <- y
#y0[!(y0 < 1e5)] <- NA

#yc.ind <- which(y == 0)
#y0.ind <- which()

#index <- is.na(y)

model <- "model{
# Process model
for(i in 1:ny){
    y[i] ~ dbeta(a,b) * dbern(p)
}
# Prior
a ~ dlnorm(amu, atau)
b ~ dlnorm(bmu, btau)
p ~ dbeta(1,1)
}"

data <- list(y = y,
             ny = length(y),
             amu = logmu(2, 1),
             atau = logtau(2,1),
             bmu = logmu(2,1),
             btau = logtau(2,1))

#data <- list(yc = yc,
             #nyc = length(yc),
             #y0 = y0,
             #ny0 = length(y0),
             #amu = logmu(2, 1),
             #atau = logtau(2,1),
             #bmu = logmu(2,1),
             #btau = logtau(2,1))

jm <- jags.model(file = textConnection(model),
                 data = data,
                 n.chains = 2)

out.raw <- coda.samples(jm, variable.names = c("a", "b", "y0","yc"),
                        n.iter = 2000)

yinds <- grep("y[", names(out.raw))

out <- as.matrix(out.raw)

ci <-

