load("Rdata/model.output.RData")
load("Rdata/aux.model.data.RData")
require(ggplot2)

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

for(v in states){
  p <- plot.ci(v, model.samples,data.dt)
  png(filename = sprintf("figures/model.ts.%s.png",v), width = 900,200)
  plot(p)
  dev.off()
}


# Density plots for parameters

png(filename = "figures/model.params.png", width = 900,height = 500)
par(mfrow=c(2,4))
for(v in params) plot(density(model.samples[,v]), main=v)
dev.off()

