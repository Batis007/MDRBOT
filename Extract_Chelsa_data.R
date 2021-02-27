#### Extract climate data with chelsa ####
library(raster)
#To download Chelsa climate data please check https://chelsa-climate.org/

path = "Enter the path where your Chelsa maps were downloaded"
lst <- list.files(path=path, pattern='tif$',full.names = T) 
#lst 
worldclim <- stack(lst) # making a raster object
points <- data.frame(lon = BOT$lon, lat = BOT$lat)
coordinates(points) <- ~lon + lat

# get the worldclim data----
rp_wc <- raster::extract(worldclim, points)
rp_wc1 <- cbind(BOT, rp_wc)
BOT <- rp_wc1 
