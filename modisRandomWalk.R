modisRandomWalk <- function(y, n.iter=1000, diag_plot = FALSE){

  library(rjags)
  source("modis.subsets.R")
  
  RandomWalk = "
  model{
  
  #### Data Model
  for(i in 1:n){
  y[i] ~ dnorm(x[i],tau_obs)
  }
  
  #### Process Model
  for(i in 2:n){
  x[i]~dnorm(x[i-1],tau_add)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
  }
  "
  
  data <- list(y=y,n=length(y),
               x_ic=log(1000),tau_ic=100,
               a_obs=1,r_obs=1,a_add=1,r_add=1)
  
  nchain = 3
  init <- list()
  for(i in 1:nchain){
    y.samp = sample(y,length(y),replace=TRUE)
    init[[i]] <- list(tau_add=1/var(diff(y.samp)),
                      tau_obs=5/var(y.samp))
  }
  
  j.model   <- jags.model(file = textConnection(RandomWalk),
                          data = data,
                          inits = init,
                          n.chains = 3)
  
  par(mfrow=c(1,1))
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("x","tau_add","tau_obs"),
                              n.iter = n.iter)
  if(diag_plot) plot(jags.out)
  
  out <- as.matrix(jags.out)
  return(out)
}
