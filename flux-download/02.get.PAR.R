library(ggplot2)
library(gridExtra)
library(lubridate)


load("flux-download/download.flux.RData")
dat <- data.table(masterflux2)
rm(masterflux2)

# Convert -999 to NA values
to.na <- function(x){
    if(is.numeric(x)) x[x < -990] <- NA
    return(x)
}

dat <- dat[, lapply(.SD, to.na)]

dat[, min := hour*60]
dat[, datetime := as.POSIXct(paste(year, month, day, floor(hour), min %% 60), format = "%Y %m %j %H %M")]
dat[, date := as.Date(datetime)]

flux.data <- dat[, list(par.mean = mean(par, na.rm=T),
                       par.max = max(par, na.rm=T),
                       par.sum = sum(par, na.rm=T),
                       tair.mean = mean(tair, na.rm=T),
                       tair.max = max(tair, na.rm=T),
                       preicp.sum = sum(precipitation, na.rm=T),
                       gpp.mean = mean(gpp, na.rm=T),
                       gpp.max = max(gpp, na.rm=T)),
                by = date]

save(flux.data, file="flux-download/flux.processed.RData")

plot_dat <- as.data.frame(cbind(date, par, date.daily, 
                                c(par.daily.mean, rep(NA, length(par)-length(unique(date_grp)))),
                                c(par.daily.max, rep(NA, length(par)-length(unique(date_grp)))), 
                                c(par.daily.sum, rep(NA, length(par)-length(unique(date_grp))))
                                  ))
colnames(plot_dat) <- c("date", "par", "date.daily","par.daily.mean","par.daily.max","par.daily.sum" )

col <- c("#619CFF","#00BA38","#F8766D")

png(filename = "figures/par.png", width = 1600, height = 1200)

p1 <- ggplot(data = plot_dat) + geom_line(aes(x=date, y=par))
p2 <- p1 + geom_line(aes(x=date.daily, y=par.daily.mean), colour = col[1])
p3 <- p1 + geom_line(aes(x=date.daily, y=par.daily.max), colour = col[2])
p4 <- p1 + geom_line(aes(x=date.daily, y=par.daily.sum), colour = col[3])

grid.arrange(p1, p2, p3, p4, ncol = 2)
dev.off()

save(date, date.daily, par.daily.mean, par.daily.max, par.daily.sum, file = "flux-download/par.RData")
