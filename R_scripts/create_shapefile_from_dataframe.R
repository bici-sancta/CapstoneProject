
# ...https://gis.stackexchange.com/questions/214062/create-a-shapefile-from-dataframe-in-r-keeping-attribute-table


library(maptools)
library(rgdal)
library(sp)

#data
site <- c("a","b","c","d")
prop_c <- c(0.88,0.48,0.15,0.47)
prop_b <- c(0.17,0.18,0.09,0.08)
minus_c <- 1-prop_c
minus_b <- 1-prop_b
lat <- c(39.22, 39.0, 39.0, 39.15)
long <- c(-84.7, -84.6, -84.5, -84.3)
MyData <- cbind.data.frame(site, prop_c, prop_b, minus_c, minus_b, lat, long)

#convert data to shapefile    

WGScoor<-  MyData
coordinates(WGScoor)=~long+lat
proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")

LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))

raster::shapefile(LLcoor, "MyShapefile.shp", overwrite = TRUE)

plot(LLcoor)
