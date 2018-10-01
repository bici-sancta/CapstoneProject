
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : which_grid_cell.R
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
library(dplyr)
library(tictoc)

printf <- function(...) invisible(cat(sprintf(...)))

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")
plot_dir <- ("./plots/")

setwd(home_dir)
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


cincy_zip_code = c("45202", "45203", "45204", "45205", "45206", "45207", "45208",
                   "45209", "45211", "45212", "45213", "45214", "45215", "45216",
                   "45217", "45219", "45220", "45223", "45224", "45225", "45226",
                   "45227", "45229", "45230", "45231", "45232", "45233", "45237",
                   "45238", "45239", "45243", "45248")


infile <- "pedestrian_near_miss_incidents_geocodes"
near_miss <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

infile <- "pedestrian_survey_w_neighborhood"
ped_srvy <- read.table(paste0('./', infile, '.csv'), sep = ",", stringsAsFactors = FALSE, header = TRUE)

infile <- "grid_points_250m_w_neighborhood"
grid_centroid <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)


infile <- "traffic_crash_reports_20180918"
traffic_crash <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(traffic_crash) <- tolower(names(traffic_crash))
names(traffic_crash)[names(traffic_crash) == 'latitude_x'] <- 'lat'
names(traffic_crash)[names(traffic_crash) == 'longitude_x'] <- 'long'

# ... traffic crash coordinates are not always reliable ... filter on reasonable values

traffic_crash <- mutate(traffic_crash, long = ifelse(long > 84, long * -1, long))

traffic_crash <- traffic_crash[traffic_crash$lat > 39 & traffic_crash$lat < 39.3,]
traffic_crash <- traffic_crash[traffic_crash$long > -84.72 & traffic_crash$long < -84.3,]

traffic_crash <- traffic_crash[!is.na(traffic_crash$long),]
traffic_crash <- traffic_crash[!is.na(traffic_crash$lat),]

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

# ... split data set into smaller units ... cannot allocate enough memory otherwise

tc_rebuild <- data.frame()

for (izip in cincy_zip_code)
{
    tic("subset of traffic crash distancing ...")
    
    tc_subset <- traffic_crash[traffic_crash$zip == izip,]
    
    printf("izip | tc_subset size = %5s | %5d\n", izip, dim(tc_subset)[1])
           
    sp.traffic_crash <- tc_subset
    
    coordinates(sp.traffic_crash) <- ~long+lat
    
    d2 <- gDistance(sp.grid_centroid, sp.traffic_crash, byid = TRUE)
    min.d2 <- apply(d2, 1, function(x) order(x, decreasing = FALSE)[1])
    dist2 <- d[min.d2]
    
    ident <- grid_centroid[min.d2, 3]
    lats <- grid_centroid[min.d2, 4]
    lons <- grid_centroid[min.d2, 5]
    hood <- grid_centroid[min.d2, 9]
    hood_id <- grid_centroid[min.d2, 10]

    newdata2 <- cbind(tc_subset, dist2, ident, lats, lons, hood, hood_id)
    printf("newdata size =  %d\n", dim(newdata2)[1])
    
    tc_rebuild <- rbind(tc_rebuild, newdata2)
    printf("tc_rebuild size =  %d\n", dim(tc_rebuild)[1])

    toc()
}

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

base_pts <- ggplot(ped_srvy, aes(x = long, y = lat), size = 0.2) +
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

base_pts <- ggplot(traffic_crash, aes(x = long, y = lat), size = 0.2) +
  geom_point(color = "dodgerblue3", shape = 5, alpha = 0.9)
base_pts

png(filename = "traffic_crash_mapped_2_grid_cell.png", 
    units = "in", 
    width = 24,
    height = 18, 
    pointsize = 12, 
    res = 72)
base_pts +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2) +
    geom_point(data = tc_rebuild, aes(x = lons, y = lats), color = "darkorchid2", shape = 15, size = 4, alpha = 0.4)
dev.off()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-