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

infile <- "pedestrian_survey_final_20180618_geocoded"
ped_srvy <- read.table(paste0('./', infile, '.txt'), sep = "|", stringsAsFactors = FALSE, header = TRUE)
ped_pts <- data.frame(Longitude = ped_srvy$long,
                  Latitude = ped_srvy$lat,
                  strsegid = ped_srvy$strsegid)

# ...   walk scores

infile <- "./grid_mapped/WalkScoreMasterFileByStreet_mapped_to_grid_cells.csv"
walk_score <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)
walk_pts <- data.frame(Longitude = walk_score$long,
                  Latitude = walk_score$lat,
                  walk_score_id = walk_score$id)

# ...   property transfers

infile <- "./grid_mapped/hamilton_county_property_xfer_2008t2018_mapped_to_grid_cells.csv"
property <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)
prop_pts <- data.frame(Longitude = property$long,
                  Latitude = property$lat,
                  property_number = property$property_number)

# ...   assignment to longitude and latitude

coordinates(cells) <- ~ Longitude + Latitude
coordinates(ped_pts) <- ~ Longitude + Latitude
coordinates(walk_pts) <- ~ Longitude + Latitude
coordinates(prop_pts) <- ~ Longitude + Latitude


# ... project selected data set using the projection of the shapefile

# ...   property transfers
# ...       bind result to data frame 
# ...       subset to only grids that are in a neighborhood

proj4string(prop_pts) <- proj4string(cvg_shapefile)
assignment <- over(prop_pts, cvg_shapefile)
prop_pts_w_neighborhood <- cbind(property, assignment)
prop_pts_w_neighborhood <- prop_pts_w_neighborhood[!is.na(prop_pts_w_neighborhood$Name), ]

# ...   walk score
# ...       bind result to data frame 
# ...       subset to only grids that are in a neighborhood
proj4string(walk_pts) <- proj4string(cvg_shapefile)
assignment <- over(walk_pts, cvg_shapefile)
walk_pts_w_neighborhood <- cbind(walk_score, assignment)
walk_pts_w_neighborhood <- walk_pts_w_neighborhood[!is.na(walk_pts_w_neighborhood$Name), ]

# ...   grid cells
# ...       bind result to data frame 
# ...       subset to only grids that are in a neighborhood
proj4string(cells) <- proj4string(cvg_shapefile)
assignment <- over(cells, cvg_shapefile)
cells_w_neighborhood <- cbind(cell_centroids, assignment)
cells_w_neighborhood <- cells_w_neighborhood[!is.na(cells_w_neighborhood$Name), ]

# ...   pedestrian survey
# ...       bind result to data frame 
# ...       subset to only grids that are in a neighborhood
proj4string(ped_pts) <- proj4string(cvg_shapefile)
assignment <- over(ped_pts, cvg_shapefile)
ped_pts_w_neighborhood <- cbind(ped_srvy, assignment)
ped_pts_w_neighborhood <- ped_pts_w_neighborhood[!is.na(ped_pts_w_neighborhood$Name), ]

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

png(filename = "pedestrian_survey_mapped_2_neighborhood.png", 
    units = "in", 
    width = 24,
    height = 18, 
    pointsize = 12, 
    res = 72)
plot(cvg_shapefile)
points(ped_pts, col = "darkgrey", cex = 0.4)
points(ped_pts_w_neighborhood$lat ~ ped_pts_w_neighborhood$long, col = "forestgreen", cex = 0.8)
dev.off()

png(filename = "walk_score_mapped_2_neighborhood.png", 
    units = "in", 
    width = 24,
    height = 18, 
    pointsize = 12, 
    res = 72)
plot(cvg_shapefile)
points(walk_pts, col = "darkgrey", cex = 0.2)
points(walk_pts_w_neighborhood$lat ~ walk_pts_w_neighborhood$long, col = "forestgreen", cex = 0.5)
dev.off()

png(filename = "property_xfer_mapped_2_neighborhood.png", 
    units = "in", 
    width = 24,
    height = 18, 
    pointsize = 12, 
    res = 72)
plot(cvg_shapefile)
points(prop_pts, col = "darkgrey", cex = 0.2)
points(prop_pts_w_neighborhood$lat ~ prop_pts_w_neighborhood$long, col = "dodgerblue4", cex = 0.5)
dev.off()

# ...   write results to .csv file

setwd(home_dir)
setwd(data_dir)

write.table(cells_w_neighborhood, file = "grid_points_250m_w_neighborhood.csv", sep = ",",
            row.names = FALSE,
            col.names = TRUE)

write.table(ped_pts_w_neighborhood, file = "pedestrian_survey_w_neighborhood.csv", sep = ",",
            row.names = FALSE,
            col.names = TRUE)

write.table(walk_pts_w_neighborhood, file = "./grid_mapped/WalkScoreMasterFileByStreet_in_cincinnati_mapped_to_grid_cells.csv", sep = ",",
            row.names = FALSE,
            col.names = TRUE)

write.table(prop_pts_w_neighborhood, file = "./grid_mapped/hamilton_county_property_xfer_2008t2018_in_cincinnati_mapped_to_grid_cells.csv", sep = ",",
            row.names = FALSE,
            col.names = TRUE)
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
