# Fit a random walk
library(ggplot2)
library(gridExtra)

load("modis-download/modis.data.RData")
sitenames <- function(name){
  if(name == "MOD15A2.fn_uswiwill.txt"){
    sitename <- "Willow-Creek"
  }else if(name == "MOD15A2.fn_uslcreek.txt") {
    sitename <- "Lost-Creek"
  }else if(name == "MOD15A2.fn_ussylvan.txt"){
    sitename <- "Sylvania"
  }
}
source("modisRandomWalk.R")

b = "Fpar_1km"

for(i in seq_along(modis.list)){
  modis.name <- sitenames(names(modis.list)[i])
  load(sprintf("%s.%s.Rdata", modis.name, b))
  
  out <- modisRandomWalk(quants$time,quants$mean, n.iter=10000, diag_plot=F)
  out <- as.data.frame(out)
  
  p <- ggplot(out)
  p1 <- p + geom_histogram(aes(x = tau_add, y = ..density..), bins=30)
  p2 <- p + geom_histogram(aes(x = tau_obs, y = ..density..), bins=30)
  p3 <- p + geom_point(aes(x = tau_add,y = tau_obs))
  grid.arrange(p1, p2, p3, layout_matrix = matrix(c(1,2,3,3),2,2,byrow=TRUE), top = modis.name)
  
  
  out_quants <- as.data.frame(t(apply(out[,3:ncol(out)],2,quantile,c(0.025,0.5,0.975))))
  colnames(out_quants) <- c("low","mean","high")
  out_quants$time <- as.Date( as.POSIXlt(substr(dat$time,2,8),format="%Y%j"))
  
  p4 <- ggplot(out_quants) +
    geom_line(aes(y=mean, x=time, colour = "mean obvs")) +  
    geom_line(aes(y=quants$mean, x=quants$time, colour = "mean pred"), alpha =.3)+
    geom_ribbon(aes(ymin=low, ymax=high, x=time, fill = "95%"), alpha = 0.3)+
    scale_colour_manual("",values=c("darkred","darkblue"))+
    scale_fill_manual("",values="red") +
    labs(title = modis.name, y = b, x = "Time") +
    scale_x_date(date_breaks = "1 years", date_minor_breaks = "2 months", date_labels = "%Y") +
    scale_y_continuous(limits = c(-.5,1.5)) +
    theme_bw() + theme(text = element_text(size=18)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot(p4)
  
}
