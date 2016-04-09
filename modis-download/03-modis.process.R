# Make sure I'm in the project base directory
if(grepl("-download", getwd())) setwd("..")

suppressMessages({
    library(data.table)
    library(bit64)
})

load("modis-download/modis.data.RData")

# Scaling factors for integer values
# Source: https://lpdaac.usgs.gov/sites/default/files/public/modis/docs/MODIS-LAI-FPAR-User-Guide.pdf
band.factors <- c("Lai_1km"=0.1, "Fpar_1km"=0.01)

sitenames <- function(name){
  if(name == "MOD15A2.fn_uswiwill.txt"){
    sitename <- "Willow-Creek"
  }else if(name == "MOD15A2.fn_uslcreek.txt") {
    sitename <- "Lost-Creek"
  }else if(name == "MOD15A2.fn_ussylvan.txt"){
    sitename <- "Sylvania"
  }
}


# Subset the data to the 3x3 grid around the center point
M <- matrix(1:49, nrow=7, byrow = T); c = 4
n = 3; s= floor(sqrt(n))
subset <- as.character(array(M[(c-s):(c+s),(c-s):(c+s)]))

dir.create("figures")

for(i in seq_along(modis.list)){
  modis.name <- sitenames(names(modis.list)[i])
  for(b in names(band.factors)){
    dat.full <- modis.list[[i]][Band == b]
    dat <- as.data.frame(dat.full[, subset, with=F]) * band.factors[b]
    dat$time <- dat.full$Date
    dat_melt <- melt(dat, id.vars = "time", measure.vars = subset, variable.factor=FALSE)
    dat_melt$time = as.POSIXlt(substr(dat_melt$time,2,8),format="%Y%j")
    stats <- function(x){
        quants <- quantile(x, c(0.025, 0.5, 0.975))
        mu <- mean(x)
        stdev <- sd(x)
        return(c("mu"=mu, "stdev"=stdev, quants))
    }
    quants <- data.frame(t((apply(dat[,(subset)],1, stats))))
    colnames(quants)[3:5] <- c("low","mean","high")
    quants$time <- as.Date( as.POSIXlt(substr(dat$time,2,8),format="%Y%j"))
    save(dat, dat_melt, quants, file = sprintf("modis-download/%s.%s.RData", modis.name, b))
  }
}


# Example plots 
library(ggplot2)
library(gridExtra)

b = "Fpar_1km"
for(i in seq_along(modis.list)){
  
  modis.name <- sitenames(names(modis.list)[i])
  load(sprintf("modis-download/%s.%s.RData", modis.name, b))
  
  p1 <- ggplot(data=dat_melt) + 
    geom_line(aes(x=as.Date(time), y=value, colour=variable), alpha = .7) +
    labs(title = modis.name, y = b, x = "") +
    scale_x_date(date_breaks = "1 years", date_minor_breaks = "2 months", date_labels = "%Y") + 
    theme_bw() + theme(text = element_text(size=18)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  p2 <-  ggplot(quants) + 
    geom_line(aes(y=mean, x=time, colour = "mean")) +
    geom_ribbon(aes(ymin=low, ymax=high, x=time, fill = "95%"), alpha = 0.3)+
    scale_colour_manual("",values="darkblue")+
    scale_fill_manual("",values="blue") +
    labs(y = b, x = "Time") +
    scale_x_date(date_breaks = "1 years", date_minor_breaks = "2 months", date_labels = "%Y") +
    theme_bw() + theme(text = element_text(size=18)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(p1, p2, ncol=1)
  
}
