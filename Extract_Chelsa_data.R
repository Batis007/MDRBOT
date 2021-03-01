#### Extract climate data with chelsa ####
library(raster)
library(elsa)
library(spThin)
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

#Load data
data <- read.csv('data_Dothiorella_forthin.csv')
data <- data[,2:4]

# get the worldclim or chelsa data----
worldclim <- raster::getData('worldclim', var='bio', res= 10)

#Leave only one point per grid
data<- elimCellDups(data,worldclim[[1]], c('lon', 'lat'))
#data <- data.frame(lon = data$lon, lat = data$lat)
data <- data.frame(lon = data$lon, lat = data$lat, Occurrence = data$Occurrence)
coordinates(data) <- ~lon + lat

#Using ELSA
#For cellpoints bio layer can be random
cellpoints <- cellFromXY(worldclim[[4]],data)

#Here you can test the ELSA for diffente bioclim variables
Elsavalue <- elsa(worldclim[[1]], d = 1, categorical = FALSE, cells = cellpoints)
#hist(Elsavalue)
mean(Elsavalue, na.rm =TRUE)
ev <- c()
for (i in seq(0.9,2,0.1)) {
  Elsavalue <- elsa(worldclim[[1]], d = i, categorical = FALSE, cells = cellpoints)
  ev <- c(ev,mean(Elsavalue, na.rm =TRUE))
  
}

data <-
  thin( loc.data = data, 
        lat.col = "lat", long.col = "lon", 
        spec.col = "Occurrence", 
        thin.par = 200, reps = 10,
        locs.thinned.list.return = TRUE, 
        write.files = TRUE,
        out.base = "file name",
        out.dir = datasets,
        write.log.file = FALSE)