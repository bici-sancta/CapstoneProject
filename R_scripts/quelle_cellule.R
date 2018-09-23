
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
plot_dir <- ("./plots/")

setwd(home_dir)
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "pedestrian_near_miss_incidents_geocodes"
near_miss <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

infile <- "pedestrian_survey_w_neighborhood"
ped_srvy <- read.table(paste0('./', infile, '.csv'), sep = ",", stringsAsFactors = FALSE, header = TRUE)

infile <- "grid_points_250m_w_neighborhood"
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
hood <- grid_centroid[min.d, 9]
hood_id <- grid_centroid[min.d, 10]

# ...   new data frame ... all prior columns + grid centroid identifiers and coords

newdata <- cbind(ped_srvy, dist, ident, lats, lons, hood, hood_id)

# ...   make a plot to visualize result

base_pts <- ggplot(newdata, aes(x = long, y = lat), size = 0.2) +
  geom_point(color = "dodgerblue3", shape = 5, alpha = 0.9)
base_pts

setwd(home_dir)
setwd(plot_dir)

png(filename = "pedestrian_survey_mapped_2_grid_cell.png", 
    units = "in", 
    width = 24,
    height = 18, 
    pointsize = 12, 
    res = 72)
base_pts +
    geom_point(data = newdata, aes(x = lons, y = lats), color = "darkorchid3", shape = 15, size = 5) +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2)
dev.off()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-