
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : 005.1_gca_traffic_crash.R
# ...
# ...   THIS IS STARTING ATTEMPT TO DISTRIBUTE PEDESTRIAN ACCIDENTS IN KERNEL
# ...   FUNCTION
# ..    DO NOT USE UNTIL FURTHER EVALAUTION IS COMPLETE
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   21-oct-2018
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

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

grid_file <- "grid_points_250m_w_neighborhood"
grid_centroid <- read.csv(paste0('./', grid_file, '.csv'), stringsAsFactors = FALSE, header = TRUE)

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

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   test purposes to reduce data set
#ped_crash <- traffic_crash[traffic_crash$crashseverityid == 2,]
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   test purposes to reduce data set


ped_crash$cdate <- strptime(ped_crash$crashdate, "%m/%d/%Y %H:%M:%S")
ped_crash$cyear <- format(as.Date(ped_crash$cdate, format="%d/%m/%Y"),"%Y")
ped_crash$cyear <- as.integer(ped_crash$cyear)
ped_crash <- subset(ped_crash, select = -c(cdate))

ped_bar_plot <- ped_crash %>% 
                group_by(crashseverity, cyear) %>%
                summarize(num_events = n())

ped_bar_plot <- transform(ped_bar_plot, num_events = ifelse(crashseverity == "2 - INJURY", num_events/10, num_events))
ped_bar_plot$crashseverity[ped_bar_plot$crashseverity == "2 - INJURY"] <- "2 - INJURY (Num Events / 10)"


png(filename = "number_pedestrian_crash_events.png",
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)

p <- ggplot(data = ped_bar_plot, aes(x = cyear, y = num_events, fill = crashseverity)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        xlab("Year") +
        ylab("Number of Events") +
        ggtitle("Cincinnati - Pedestrian - Vehicle Crashes")

p + scale_fill_brewer(palette="Set1") +
#        theme_minimal() +
        theme(legend.position = c(0.2, 0.8), text = element_text(size = 20))

dev.off()

# ...   Death           $10,082,000
# ...   Disabling       $1,103,000
# ...   Evident         $304,000
# ...   Possible injury $141,000
# ...   No injuryobserved $46,600

ped_crash$injury_type_num <- sapply(ped_crash$injuries, substr, 1, 1)
ped_crash$injury_type_num <- as.integer(ped_crash$injury_type_num)

ped_crash$cost <- sapply(ped_crash$injury_type_num, switch, 
                  "1" = 0.046,
                  "2" = 0.141,
                  "3" = 0.304, 
                  "4" = 1.103, 
                  "5" = 10.082) 

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

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   test purposes to reduce data set
#cvg_shapefile <- cvg_shapefile[cvg_shapefile$Name == "CUF", ]
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   test purposes to reduce data set

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   eliminate points that are outside city boundaries
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_overlap <- ped_crash
df_overlap$Longitude <- df_overlap$long
df_overlap$Latitude <- df_overlap$lat

coordinates(df_overlap) <- ~Longitude + Latitude

proj4string(df_overlap) <- proj4string(cvg_shapefile)

df_in_city <- over(df_overlap, cvg_shapefile)
df_stops_city <- cbind(ped_crash, df_in_city)
df_stops_city <- df_stops_city[!is.na(df_stops_city$RegionID),]

ped_crash <- df_stops_city

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   temporary reduction to evaluate kernel function
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ped_crash <- ped_crash[ped_crash$crashseverityid == 1,]

#ped_crash <- ped_crash[ped_crash$instanceid == "A9279468-2855-4412-8274-03B240A2D523"
#                                            |
#                       ped_crash$instanceid == "F2692C22-1165-4518-B15F-3241E4F7007E"
#                                            |
#                       ped_crash$instanceid == "8524CD34-5625-4EB7-ABCE-FC6E487B29B3", ]


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

sp.df_to_map <- ped_crash
coordinates(sp.df_to_map) <- ~long+lat

sp.grid_centroid <- grid_centroid
coordinates(sp.grid_centroid) <- ~long+lat

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   kernel radius search distance

r_kernel = 0.005

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   calculate distance pairs between all point pairs
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

dist_pair <- gDistance(sp.df_to_map, sp.grid_centroid, byid = TRUE)

dist_pair[dist_pair > r_kernel] <- NA

dist_pair_sf <- (1 - (dist_pair / r_kernel) ^ 2) ^ 2

sf_sf <- 1 / colSums(dist_pair_sf, na.rm = TRUE)

dp_sf_t <- t(dist_pair_sf)

dp_kcc <- data.frame(cbind(dp_sf_t, sf_sf, ped_crash$cost, 1))

dp_kcc_scaled <- subset(dp_kcc, select = -c(sf_sf, V4198, V4199))
dp_kcc_event_cnt <- subset(dp_kcc, select = -c(sf_sf, V4198, V4199))

for (icol in 1 : 4196)
{
    dp_kcc_scaled[,icol] <- dp_kcc[,icol] * dp_kcc[,4197] * dp_kcc[,4198]
    dp_kcc_event_cnt[,icol] <- dp_kcc[,icol] * dp_kcc[,4197] * dp_kcc[,4199]
}

dp_kcc_t <- data.frame(t(dp_kcc_scaled))
df_kcc <- data.frame(rowSums(dp_kcc_t, na.rm = TRUE))
names(df_kcc) <- c("krnl_cost")

dp_kcc_cnt <- data.frame(t(dp_kcc_event_cnt))
df_kcc_cnt <- data.frame(rowSums(dp_kcc_cnt, na.rm = TRUE))
names(df_kcc_cnt) <- c("num_events")

log_floor <- min(df_kcc[df_kcc$krnl_cost > 0,])/exp(1)
log_floor_events <- min(df_kcc[df_kcc_cnt$num_events > 0,])/exp(1)

hist(log(df_kcc$krnl_cost + log_floor), col = "dodgerblue4")
hist(log(df_kcc_cnt$num_events + log_floor_events), col = "dodgerblue3")

# ...   define corresponding columns from grid data frame

#event_id <- ped_crash[min_dist, 1]
#lat_stop <- ped_crash[min_dist, 2]
#long_stop <- ped_crash[min_dist, 3]
#n_object <- ped_crash[min_dist, 4]

# ...   new data frame ... all prior columns + grid centroid identifiers and coords

df_mapped_to_grid <- cbind(grid_centroid, df_kcc, df_kcc_cnt)

cols_2_drop <- c("ix", "iy", "State", "County", "City", "Name", "RegionID")

df_mapped_to_grid <- df_mapped_to_grid[, !names(df_mapped_to_grid) %in% cols_2_drop]


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#df_mapped <- which_grid_cell_big(grid_centroid, ped_crash)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

ped_crash_agg <- df_mapped_to_grid %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(sum_cost = sum(cost),
                          num_events = n())



# Get the stacked barplot
barplot(ped_crash, col=colors()[c(23,89,12)] , border="white", space=0.04, font.axis=2, xlab="year")

# Grouped barplot
barplot(ped_crash, col = colors()[c(23,89,12)] , border="white", font.axis=2, beside=T, legend=rownames(data), xlab="group", font.lab=2)

# ...   make a plot to visualize result

setwd(home_dir)
setwd(plot_dir)

hoods <- ggplot() +  geom_polygon(data=cvg_shapefile, aes(x=long, y=lat, group=group), size = 0.1, alpha = 0.1) + 
                        geom_point(data=cvg_shapefile, aes(x=long, y=lat, group=group), size = 0.1, alpha = 0.4)

# ...   Basic map of event severity

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ...   change : data = xxx ; color = yyy for each new data set / variable to map
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ...   plot 1

png(filename = paste0(infile, "_sum_cost", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)

hoods +
    geom_point(data = df_mapped_to_grid, aes(x = long, y = lat, color = log10(krnl_cost)),
               shape = 19,
               size = 2.75,
               alpha = 0.8) + 
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    ggtitle("Pedestrian - Cost (kernel distributed, r = 0.005)") +
    xlab("Longitude") + ylab("Latitude") +
    scale_color_gradientn(colors = rev(rainbow(9))[2:9])
    coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   plot 1

png(filename = paste0(infile, "_num_events", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)

hoods +
    geom_point(data = df_mapped_to_grid, aes(x = long, y = lat, color = log2(num_events)),
               shape = 19,
               size = 2.75,
               alpha = 0.8) + 
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    ggtitle("Pedestrian - Num Events (kernel distributed, r = 0.005)") +
    xlab("Longitude") + ylab("Latitude") +
    scale_color_gradientn(colors = rev(rainbow(9))[2:9])
    coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   save fle to csv
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(grid_mapped_dir)

file_name <- paste0(infile, "_mapped_to_grid_cells.csv")
write.table(df_mapped, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

file_name <- "ped_crash_cost_kernel_distribution.csv"
write.table(df_mapped_to_grid, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
