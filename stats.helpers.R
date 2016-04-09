# Functions to facilitate statistics

# Calculate lognormal parameters from sample statistics
logmu <- function(mu, s) log(mu^2/(sqrt(mu^2 + s^2)))
logtau <- function(mu, s) 1/log(1 + s^2/mu^2)

# Calculate beta parameters from sample statistics
Alpha <- function(mu, s) mu * (mu * (1 - mu) / s^2 - 1)
Beta <- function(mu, s) (1 - mu) * (mu * (1 - mu) / s^2 - 1)




