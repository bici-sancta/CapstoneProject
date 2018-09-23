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

rm(list=ls())

library(rgeos)
library(sp)
library(rgdal)
library(ggrepel)
library(maptools)
library(dplyr)

home_dir <- "/home/mcdevitt/_ds/_smu/_src/CapstoneProject/"
zillow_dir <- "./data/ZillowNeighborhoods-OH"
data_dir <- "./data/"
plot_dir <- "./plots/"

setwd(home_dir)
setwd(zillow_dir)

# ...   read in shapefiles from Zillow definitions

oh_shapefile <- readOGR("ZillowNeighborhoods-OH.shp", layer="ZillowNeighborhoods-OH")
cvg_shapefile <- oh_shapefile[oh_shapefile$City == "Cincinnati", ]

# ...   drop 2 neighborhoods which are not in Cincinnati

cvg_shapefile <- cvg_shapefile[cvg_shapefile$Name != "Fruit Hill", ]
cvg_shapefile <- cvg_shapefile[cvg_shapefile$Name != "Forestville", ]

plot(cvg_shapefile)

# ...   read in points to find overlap with neighborhoods
# ...   set longitudes as the first column and latitudes as the second

setwd(home_dir)
setwd(data_dir)

infile <- "grid_centroids_250m"
cell_centroids <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

cells <- data.frame(Longitude = cell_centroids$long,
                  Latitude = cell_centroids$lat,
                  id = cell_centroids$cell_id)

points(cells, col = "dodgerblue3", cex = 0.1)

# Assignment modified according

coordinates(cells) <- ~ Longitude + Latitude

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile

proj4string(cells) <- proj4string(cvg_shapefile)

assignment <- over(cells, cvg_shapefile)

# ...   bind result to data frame 

cells_w_neighborhood <- cbind(cell_centroids, assignment)

# ...   subset to only grids that are in a neighborhood

cells_w_neighborhood <- cells_w_neighborhood[!is.na(cells_w_neighborhood$Name), ]

# ...   make a plot to view result

setwd(home_dir)
setwd(plot_dir)

png(filename = "grid_cells_mapped_2_neighborhood.png", 
    units = "in", 
    width = 24,
    height = 18, 
    pointsize = 12, 
    res = 72)

plot(cvg_shapefile)
points(cells, col = "darkgrey", cex = 0.4)
points(cells_w_neighborhood$lat ~ cells_w_neighborhood$long, col = "blue", cex = 0.8)

dev.off()

# ...   write results to .csv file

setwd(home_dir)
setwd(data_dir)

write.table(cells_w_neighborhood, file = "grid_points_250m_w_neighborhood.csv", sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
