require(ggplot2)
require(gridExtra)
require(lubridate)


load("flux-download/download.flux.RData")
dat <- masterflux2


min <- dat$hour*60
dat$date <- as.POSIXct(paste(dat$year, dat$month, dat$day, floor(dat$hour), min %% 60  ), format = "%Y %m %j %H %M")
par <- dat$par

date_grp <- sprintf("%04d%02d%02d", 
                        year(dat$date),
                        month(dat$date),
                        day(dat$date))
date.daily <- c(as.POSIXct(unique(date_grp), format = "%Y%m%j"),
                rep(NA, length(par)-length(unique(date_grp))))
par.daily.mean <- tapply(X = dat$par, INDEX = date_grp, 
                            FUN = function(x) mean(x, na.rm=T))
par.daily.max <- tapply(X = dat$par, INDEX = date_grp, 
                          FUN = function(x) max(x, na.rm=T))
par.daily.sum <- tapply(X = dat$par, INDEX = date_grp, 
                          FUN = function(x) sum(x, na.rm=T))


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

save(date, date.daily, par.daily.mean, par.daily.max, par.daily.sum, file = "flux-download/par.Rdata")
