
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : quelle_cellule.R
# ...
# ...   find closest grid point (cell centroid) to data value location 
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

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")

setwd(home_dir)
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "pedestrian_near_miss_incidents_geocodes"
near_miss <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

infile <- "pedestrian_survey_final_20180618_geocoded"
ped_srvy <- read.table(paste0('./', infile, '.txt'), sep = "|", stringsAsFactors = FALSE, header = TRUE)

infile <- "grid_centroids_250m"
grid_centroid <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   transform to spatial objects
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

sp.near_miss <- near_miss
coordinates(sp.near_miss) <- ~long+lat

sp.ped_srvy <- ped_srvy
coordinates(sp.ped_srvy) <- ~long+lat

sp.grid_centroid <- grid_centroid
coordinates(sp.grid_centroid) <- ~long+lat

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   calculate distance pairs between all point pairs
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

d <- gDistance(sp.grid_centroid, sp.ped_srvy, byid = TRUE)

# ...   mid distance is closest point

min.d <- apply(d, 1, function(x) order(x, decreasing = FALSE)[1])

# ...   define corresponding columns from grid data frame

dist <- d[min.d]
ident <- grid_centroid[min.d, 3]
lats <- grid_centroid[min.d, 4]
lons <- grid_centroid[min.d, 5]

# ...   new data frame ... all prior columns + grid centroid identifiers and coords

newdata <- cbind(ped_srvy, dist, ident, lats, lons)

# ...   make a plot to visualize result

base_pts <- ggplot(newdata, aes(long, lat)) +
  geom_point(aes(fill = "dodgerblue4", size = 3, alpha = 0.9))
base_pts

base_pts +
    geom_point(data = newdata, aes(x = lons, y = lats, color = "red"), size = 3) +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2) +
    xlim(-84.6, -84.4) +
    ylim(39.1, 39.20)
    
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    
    
    
