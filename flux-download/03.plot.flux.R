library(data.table)
library(ggplot2)
load("flux-download/flux.processed.RData")

gpp.plot <- ggplot(flux.data) + aes(x=date, y=gpp.mean) + geom_line() +
    labs(x = "Time", y = "GPP (mu mol/s/m2)", title="Flux tower GPP")
par.plot <- ggplot(flux.data) + aes(x=date, y=par.mean) + geom_line() + 
    labs(x = "Time", y = "PAR (W/m2)", title="Flux tower PAR")

png("figures/gpp.png")
plot(gpp.plot)
dev.off()

png("figures/par.png")
plot(par.plot)
dev.off()
