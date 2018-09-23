# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : which_neighborhood.R
# ...
# ...   find neighborhood in which a geo-coordinate is located
# ...
# ...   ref : https://gis.stackexchange.com/questions/133625/
# ...       checking-if-points-fall-within-polygon-shapefile
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   23-sep-2018
# ...
# ...   patrick.mcdevitt@smu.edu
# ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

library(rgeos)
library(sp)
library(rgdal)
library(ggrepel)
library(maptools)
library(dplyr)

home_dir <- "/home/mcdevitt/_ds/_smu/_src/CapstoneProject/"
zillow_dir <- "./data/ZillowNeighborhoods-OH"
data_dir <- "./data/"

setwd(home_dir)
setwd(zillow_dir)

# ...   read in shapefiles from Zillow definitions

oh_shapefile <- readOGR("ZillowNeighborhoods-OH.shp", layer="ZillowNeighborhoods-OH")
cvg_shapefile <- oh_shapefile[oh_shapefile$City == "Cincinnati", ]

plot(cvg_shapefile)

# ...   set longitudes as the first column and latitudes as the second

dat <- data.frame(Longitude = c(-84.7133, -84.5, -84.3),
                  Latitude =c(39.2, 39.15, 39.1),
                  names = c("Safeco Field", "Key Arena", "Century Link"))
points(dat, col = "red")


# Assignment modified according

coordinates(dat) <- ~ Longitude + Latitude

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile

proj4string(dat) <- proj4string(cvg_shapefile)
over(dat, cvg_shapefile)
over(cvg_shapefile, dat)


points(dat, col = "red")
proj4string(dat) <- proj4string(LLcoor)
over(dat, LLcoor)
over(LLcoor, dat)
