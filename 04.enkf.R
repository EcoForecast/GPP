load("out.RData")
load("input.data.RData")

# Get gpp from after today
today <- as.Date(Sys.Date())
gpp.ind <- grep("gpp", colnames(out))


