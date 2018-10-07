
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

printf <- function(...) invisible(cat(sprintf(...)))

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   define some directory locations
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")
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

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

infile <- "pedestrian_near_miss_incidents_geocodes"
near_miss <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "pedestrian_survey_w_neighborhood"
ped_srvy <- read.table(paste0('./', infile, '.csv'), sep = ",", stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

grid_file <- "grid_points_250m_w_neighborhood"
grid_centroid <- read.csv(paste0('./', grid_file, '.csv'), stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "WalkScoreMasterFileByStreet"
walk_score <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(walk_score) <- tolower(names(walk_score))

cols_2_keep <- c("id", "street", "zipcode", "lat",
                 "long", "walkscore", "walkscoredesc")
walk_score <- subset(walk_score, select = cols_2_keep)

cincy_min_latitude <- 39.0
cincy_max_latitude <- 39.3
cincy_max_longitude <- -84.3
cincy_min_longitude <- -84.72

walk_score <- walk_score[walk_score$lat > cincy_min_latitude & walk_score$lat < cincy_max_latitude,]
walk_score <- walk_score[walk_score$long > cincy_min_longitude & walk_score$long < cincy_max_longitude,]


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "street_centerlines_w_pci_rating"
streets <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(streets) <- tolower(names(streets))
names(streets)[names(streets) == 'latitude'] <- 'lat'
names(streets)[names(streets) == 'longitude'] <- 'long'

cols_2_keep <- c("strsegid", "category", "rank", "surface",
                 "area", "length", "width", "cagclass",
                 "lane_cnt", "lat", "long")
streets <- subset(streets, select = cols_2_keep)

#streets = streets[!duplicated(streets$strsegid), ]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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

ped_crash <- traffic_crash[traffic_crash$typeofperson == "P - PEDESTRIAN",]

# ...   Death           $10,082,000
# ...   Disabling       $1,103,000
# ...   Evident         $304,000
# ...   Possible injury $141,000
# ...   No injuryobserved $46,600

ped_crash$injury_type_num <- sapply(ped_crash$injuries, substr, 1, 1)
ped_crash$injury_type_num <- as.integer(ped_crash$injury_type_num)
ped_crash$cost <- sapply(ped_crash$injury_type_num, switch, 
                  '5' = 10.082, 
                  '4' = 1.103, 
                  '3' = 0.304, 
                  '2' = 0.141,
                  '1' = 0.046)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "2017_NFIRS_Cincinnati_Fire_Department_Incident_Data"
fire_incident <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

names(fire_incident) <- tolower(names(fire_incident))

names(fire_incident)[names(fire_incident) == 'latitude'] <- 'lat'
names(fire_incident)[names(fire_incident) == 'longitude'] <- 'long'

fire_agg <- fire_incident %>%
                group_by(incident_type_desc, incident_type_id) %>%
                summarize(num_incdnt = n())

# ...   fire incidents related to vehicle accidents

incident_2_keep <- c(322, 323, 324, 352, 463)

red1 <- fire_incident[fire_incident$incident_type_id == 322,]
red2 <- fire_incident[fire_incident$incident_type_id == 323,]
red3 <- fire_incident[fire_incident$incident_type_id == 324,]
red4 <- fire_incident[fire_incident$incident_type_id == 352,]
red5 <- fire_incident[fire_incident$incident_type_id == 463,]

fire_incident_red <- rbind(red1, red2, red3, red4, red5)

# ...   not all have lat& long in original data file ... remove for now, consider to look up geocoords
# ...   1/2 of motor vehicle accidents DO NOT HAVE LAT/LONG COORDS

fire_incident_red <- fire_incident_red[!is.na(fire_incident_red$lat),]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "Cincinnati_Fire_Incidents_CAD_including_EMS_ALS_BLS"
fire_incident <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

names(fire_incident) <- tolower(names(fire_incident))

names(fire_incident)[names(fire_incident) == 'latitude_x'] <- 'lat'
names(fire_incident)[names(fire_incident) == 'longitude_y'] <- 'long'

# ... 23,500 of 314,009 events do not have lat/long
# ...   some lat/long are (apparently) reversed


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
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_mapped <- which_grid_cell_big(grid_centroid, streets)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

cell_cost <- df_mapped %>%
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(sum_cost = sum(cost), event_count = n(), sum_num = sum(injury_type_num))
    
street_agg <- df_mapped %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(sum_lane_cnt = sum(lane_cnt), sum_width = sum(width), sum_area = sum(area), num_streets = n())

walk_score_agg <- df_mapped %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(mean_walk_score = mean(walkscore),
                          min_walk_score = min(walkscore),
                          max_walk_score = max(walkscore),
                          num_walk_scores = n())

fire_incd_agg <- df_mapped %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(num_fire_incd = n())

# ...   make a plot to visualize result

setwd(home_dir)
setwd(plot_dir)

hoods <- ggplot() +  geom_point(data=cvg_shapefile, aes(x=long, y=lat, group=group), size = 0.1, alpha = 0.4)

# ...   Basic map of event severity

png(filename = paste0(infile, "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ...   change : data = xxx ; color = yyy for each new data set / variable to map
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

hoods +
    geom_point(data = ped_survey_agg, aes(x = long_cell, y = lat_cell, color = num_reports), shape = 15, size = 2.5, alpha = 0.8) + 
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    geom_point(data = df_mapped, aes(x = long, y = lat), color = "black", shape = 5, size = 0.2, alpha = 0.4) +
    ggtitle(infile) +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
    coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   save fle to csv
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

file_name <- paste0(infile, "_mapped_to_grid_cells.csv")

write.table(df_mapped, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
