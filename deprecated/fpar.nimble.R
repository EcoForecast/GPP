library(nimble)
library(data.table)

# Load and prepare data
load("input.data.RData")
source("stats.helpers.R")
consts <- list(
    y = data.dt[, mu],
    ny = nrow(data.dt),
    amu = logmu(2,1), atau = logtau(2,1),
    bmu = logmu(2,1), btau = logtau(2,1))

fparmodel.code <- nimbleCode({
    # Prior
    p ~ dbeta(1,1)
    a ~ dlnorm(amu, atau)
    b ~ dlnorm(bmu, btau)
    # Process
    for(i in 1:ny){
        y[i] ~ dBetaBern(a, b, p)
    }
})

# Define density function
dBetaBern <- nimbleFunction(
    run = function(x = double(), a = double(), b = double(), p = double(),
                   log = logical(0, default = 0)){
        returnType(double())
        # Non-zero data
        if(x != 0){
            if(log) return(dbeta(x, a, b,log=TRUE) + log(1-p)) 
            else return((1-p) * dbeta(x, a, b, log=FALSE))
        } else {
        # Zero data
            if(log) return(log(p))
            else return(p)
        }
    })

# Define random number function
rBetaBern <- nimbleFunction(
    run = function(n = integer(), a = double(), b = double(), p = double()){
        returnType(double())
        isZero <- rbinom(1, p, size=1)
        if(isZero) return(0)
        else return(rbeta(1, a, b))
    })

# Register density function with NIMBLE

registerDistributions(list(
    dBetaBern = list(BUGSdist = "dBetaBern(a, b, p)",
                     discrete = FALSE,
                     range = c(0, 1),
                     types = c('value = double()', 'a = double()',
                               'b = double()', 'p = double()')
    )))

fparmodel <- nimbleModel(fparmodel.code, constants = consts, check=FALSE,
                         name = "fparmodel")

fparMCMC.conf <- configureMCMC(fparmodel)
fparMCMC <- buildMCMC(fparMCMC.conf)
cfparMCMC <- compileNimble(fparMCMC)
cfparMCMC$run(100)


