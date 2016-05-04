load("Rdata/model.output.RData")
load("Rdata/aux.model.data.RData")

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

# Time series of the outputs

plot.ci <- function(v, model.samples,data.dt){
  
  quants <- c(0.025, 0.5, 0.975)
  ind <- grep(v, colnames(model.samples))
  # Something weird in last fpar timestep
  ind <- ind[-length(ind)]
  model <- model.samples[,ind]
  out.model <- as.data.table(t(apply(model,2,quantile,quants)))
  colnames(out.model) <- c("low.model","med.model","high.model")
  x <- 1:nrow(out.model)
  date <- data.dt[,date]
  out.model <- cbind(out.model, x , "date"= date[1:length(ind)])
  
  p <- ggplot(out.model) + aes(x=date) +
    geom_ribbon(aes(ymin=low.model, ymax=high.model), fill="blue", alpha=0.3) +
    geom_line(aes(y=med.model), size=1, color="darkblue") +
    labs(y = v, x = "Time")+
    scale_x_date(date_breaks = "1 years", date_minor_breaks = "1 months", date_labels = "%Y") +
    theme_bw() + theme(text = element_text(size=18)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}

arg <- commandArgs(trailingOnly = TRUE)

for(v in states){
  p <- plot.ci(v, model.samples,data.dt)
  if(length(arg) > 1) png(filename = sprintf("figures/model.ts.%s.png",v), width = 900,200)
  plot(p)
  if(length(arg) > 1) dev.off()
}


# Density plots for parameters

if(length(arg) > 1) png(filename = "figures/model.params.png", width = 900,height = 500)
par(mfrow=c(2,4))
for(v in params) plot(density(model.samples[,v]), main=v)
if(length(arg) > 1) dev.off()

