library(maps)

hf.coords <- c("lat" = 42.5378, "lon" = -72.1715)
wc.coords <- c("lat" = 45.8060, "lon" = -90.0798)
lc.coords <- c("lat" = 46.0827, "lon" = -89.9792)
sy.coords <- c("lat" = 46.2420, "lon" = -89.3476)

latlon <- cbind(as.numeric(lat), as.numeric(lon))

map("world")
#points(hf.coords[2], hf.coords[1], col="red")
points(wc.coords[2], wc.coords[1], col="red", pch=16)
points(latlon[,2], latlon[,1], pch=".")

hf.dist <- (latlon[,1] - hf.coords)^2 + (latlon[,2] - hf.coords)^2

hf.dist <- min((lat - hf.coords["lat"])^2 + (lon - hf.coords["lon"])^2)
wc.dist <- min((lat - wc.coords["lat"])^2 + (lon - wc.coords["lon"])^2)
lc.dist <- min((lat - lc.coords["lat"])^2 + (lon - lc.coords["lon"])^2)
sy.dist <- min((lat - sy.coords["lat"])^2 + (lon - sy.coords["lon"])^2)

print(sqrt(hf.dist))
print(sqrt(wc.dist))
print(sqrt(lc.dist))
print(sqrt(sy.dist))

