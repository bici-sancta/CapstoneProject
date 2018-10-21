
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : grid_cell_assignment.R
# ...
# ...   organizer to prep data sets for submit to which_grdi_cell_function()
# ...
# ...   ref : https://stackoverflow.com/questions/21977720/
# ...           r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   21-sep-2018
# ...
# ...   patrick.mcdevitt@smu.edu
# ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

rm(list=ls())

library(sp)
library(rgeos)
library(geosphere)
library(dplyr)
library(tictoc)
library(ggplot2)

library(viridis)
library(wesanderson)

library(ggrepel)
library(rgdal)
library(rgeos)
library(maptools)

library(stringi)

printf <- function(...) invisible(cat(sprintf(...)))

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   define some directory locations
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")
grid_mapped_dir <- ("./data/grid_mapped")
plot_dir <- ("./plots/")
src_dir <- ("./R_scripts")
zillow_dir <- ("./data/ZillowNeighborhoods-OH")

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   define some utility functions
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(src_dir)

source("./which_grid_cell_function.R")
source("./which_grid_cell_function_big.R")
source("./cincy_zip_codes.R")
source("./clean_fire_incident.R")

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

grid_file <- "grid_points_250m_w_neighborhood"
grid_centroid <- read.csv(paste0('./', grid_file, '.csv'), stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "SORTA_Bus_Stops"
bus_stop <- read.csv(paste0('./', infile, '.csv'),
                       stringsAsFactors = FALSE, header = TRUE)

names(bus_stop) <- tolower(names(bus_stop))

# [1] "x"          "y"          "linedirid"  "sequence"   "stopnum"    "stopid"     "nodeid"     "stopabbr"   "stopname"   "nodeabbr"  
#[11] "lon"        "lat"        "farezoneid" "inservice"  "nodename"   "lineid"     "lineabbr"   "linename"   "usershort1" "dirnum"    
#[21] "tpfield00"  "objectid"   "globalid"  

colnames(bus_stop)[colnames(bus_stop) == 'lon'] <- 'long'
cols_2_keep <- c("stopid", "objectid", "long", "lat", "lineid")
bus_stop <- bus_stop[cols_2_keep]

# ...   insert decimal place into lat/lon strings

lon <- as.character(bus_stop$long)
lat <- as.character(bus_stop$lat)

stri_sub(lon, 4, 3) <- '.' # ... insert . at 4th character ... after -84nnnnn
stri_sub(lat, 3, 2) <- '.' # ... insert . at 3rd character ... after 39nnn

bus_stop$long <- as.numeric(lon)
bus_stop$lat <- as.numeric(lat)

cincy_min_latitude <- 39.0
cincy_max_latitude <- 39.3
cincy_max_longitude <- -84.3
cincy_min_longitude <- -84.72

bus_stop <- bus_stop[bus_stop$lat > cincy_min_latitude & bus_stop$lat < cincy_max_latitude,]
bus_stop <- bus_stop[bus_stop$long > cincy_min_longitude & bus_stop$long < cincy_max_longitude,]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in shapefile of neighborhoods for plotting
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(zillow_dir)

oh_shapefile <- readOGR("ZillowNeighborhoods-OH.shp", layer="ZillowNeighborhoods-OH")
cvg_shapefile <- oh_shapefile[oh_shapefile$City == "Cincinnati", ]

# ...   drop 2 neighborhoods which are not in Cincinnati

cvg_shapefile <- cvg_shapefile[cvg_shapefile$Name != "Fruit Hill", ]
cvg_shapefile <- cvg_shapefile[cvg_shapefile$Name != "Forestville", ]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   eliminate points that are outside city boundaries
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_overlap <- bus_stop
df_overlap$Longitude <- df_overlap$long
df_overlap$Latitude <- df_overlap$lat

coordinates(df_overlap) <- ~Longitude + Latitude

proj4string(df_overlap) <- proj4string(cvg_shapefile)

df_in_city <- over(df_overlap, cvg_shapefile)
df_stops_city <- cbind(bus_stop, df_in_city)
df_stops_city <- df_stops_city[!is.na(df_stops_city$RegionID),]

bus_stop <- df_stops_city

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   aggregate on stopid, count number of objectids
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

bus_stop_unique <- bus_stop %>%
                    group_by(stopid, lat, long) %>%
                    summarize(n_object = n())

bus_stop_unique <- as.data.frame(bus_stop_unique)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

sp.df_to_map <- bus_stop_unique
coordinates(sp.df_to_map) <- ~long+lat

sp.grid_centroid <- grid_centroid
coordinates(sp.grid_centroid) <- ~long+lat

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   calculate distance pairs between all point pairs
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

dist_pair <- gDistance(sp.df_to_map, sp.grid_centroid, byid = TRUE)
    
# ...   mid distance is closest point

min_dist <- apply(dist_pair, 1, function(x) order(x, decreasing = FALSE)[1])
 
df_dist <- NULL

for (ir in 1:length(min_dist))
{
    dist <- dist_pair[ir, min_dist[ir]]
    df_dist <- rbind(df_dist, data.frame(dist))
}

# ...   define corresponding columns from grid data frame

stop_id <- bus_stop_unique[min_dist, 1]
lat_stop <- bus_stop_unique[min_dist, 2]
long_stop <- bus_stop_unique[min_dist, 3]
n_object <- bus_stop_unique[min_dist, 4]

# ...   new data frame ... all prior columns + grid centroid identifiers and coords

df_mapped_to_grid <- cbind(grid_centroid, stop_id, lat_stop, long_stop, n_object, df_dist)


# [1] "ix"          "iy"          "cell_id"     "lat"         "long"        "State"       "County"      "City"        "Name"       
#[10] "RegionID"    "dist"        "stop_id"     "lat_stop"    "long_stop"   "farezone_id" "n_object"   
 
cols_2_keep <- c("cell_id", "lat", "long", "Name", "dist", "stop_id", "lat_stop", "long_stop", "n_object")

grid_cells_w_bus_stop_dist <- df_mapped_to_grid[, cols_2_keep]
grid_cells_w_bus_stop_dist <- as.data.frame(grid_cells_w_bus_stop_dist)

colnames(grid_cells_w_bus_stop_dist)[colnames(grid_cells_w_bus_stop_dist) == 'Name'] <- 'neighborhood'


# ...   make a plot to visualize result

setwd(home_dir)
setwd(plot_dir)

hoods <- ggplot() +  geom_point(data=cvg_shapefile, aes(x=long, y=lat, group=group), size = 0.1, alpha = 0.8)

# ...   Basic map of event severity

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ...   change : data = xxx ; color = yyy for each new data set / variable to map
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ...   plot 1

png(filename = paste0(infile, "_distances.png"), 
    units = "in", 
    width = 14,
    height = 9,
    pointsize = 12, 
    res = 72)

hoods +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    geom_point(data = grid_cells_w_bus_stop_dist, aes(x = long, y = lat, color = dist), shape = 19, size = 3, alpha = 0.9) +
    geom_point(data = bus_stop_unique, aes(x = long, y = lat), color = "firebrick2", shape = 15, size = 1, alpha = 0.3) + 
    ggtitle(infile) +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
    coord_cartesian(xlim = c(-84.35, -84.72), ylim = c(39.04, 39.23)) +
    theme(legend.position = c(0.1, 0.9), 
       legend.justification = c(0, 1))

dev.off()

# ...   plot 2

png(filename = paste0(infile, "_n_object_ids.png"), 
    units = "in", 
    width = 14,
    height = 9,
    pointsize = 12, 
    res = 72)

hoods +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    geom_point(data = grid_cells_w_bus_stop_dist, aes(x = long, y = lat, color = n_object), shape = 19, size = 3, alpha = 0.9) +
    geom_point(data = bus_stop_unique, aes(x = long, y = lat), color = "firebrick2", shape = 15, size = 1, alpha = 0.3) + 
    ggtitle(infile) +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
    coord_cartesian(xlim = c(-84.35, -84.72), ylim = c(39.04, 39.23)) +
    theme(legend.position = c(0.1, 0.9), 
       legend.justification = c(0, 1))

dev.off()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   save fle to csv
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(grid_mapped_dir)

file_name <- "grid_cells_w_bus_stop_distances.csv"

write.table(grid_cells_w_bus_stop_dist, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
