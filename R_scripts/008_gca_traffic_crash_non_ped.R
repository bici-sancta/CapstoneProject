
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : 008_gca_traffic_crash_non_ped.R
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

non_ped_crash <- traffic_crash[!(traffic_crash$typeofperson == "P - PEDESTRIAN"),]

non_ped_crash$cdate <- strptime(non_ped_crash$crashdate, "%m/%d/%Y %H:%M:%S")
non_ped_crash$cyear <- format(as.Date(non_ped_crash$cdate, format="%d/%m/%Y"),"%Y")
non_ped_crash$cyear <- as.integer(non_ped_crash$cyear)
non_ped_crash <- subset(non_ped_crash, select = -c(cdate))

non_ped_bar_plot <- non_ped_crash %>% 
                group_by(crashseverity, cyear) %>%
                summarize(num_events = n())

non_ped_bar_plot <- transform(non_ped_bar_plot, num_events = ifelse(crashseverity == "2 - INJURY", num_events/100, num_events))
non_ped_bar_plot$crashseverity[non_ped_bar_plot$crashseverity == "2 - INJURY"] <- "2 - INJURY (Num Events / 100)"

non_ped_bar_plot <- transform(non_ped_bar_plot, num_events = ifelse(crashseverity == "3 - PROPERTY DAMAGE ONLY (PDO)", num_events/1000, num_events))
non_ped_bar_plot$crashseverity[non_ped_bar_plot$crashseverity == "3 - PROPERTY DAMAGE ONLY (PDO)"] <- "3 - PROPERTY DAMAGE (Num Events / 1000)"

png(filename = "number_non_pedestrian_crash_events.png",
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)

p <- ggplot(data = non_ped_bar_plot, aes(x = cyear, y = num_events, fill = crashseverity)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        xlab("Year") +
        ylab("Number of Events") +
        ggtitle("Cincinnati - Non-Pedestrian - Vehicle Crashes")

p + scale_fill_brewer(palette="Set1") +
#        theme_minimal() +
        theme(legend.position = c(0.01, 0.95), text = element_text(size = 10), legend.justification = c(0, 1))

dev.off()

# ...   Death           $10,082,000
# ...   Disabling       $1,103,000
# ...   Evident         $304,000
# ...   Possible injury $141,000
# ...   No injuryobserved $46,600

non_ped_crash$injury_type_num <- sapply(non_ped_crash$injuries, substr, 1, 1)
non_ped_crash$injury_type_num <- as.integer(non_ped_crash$injury_type_num)

non_ped_crash$injury_type_num[is.na(non_ped_crash$injury_type_num)] <- 1


df_cost <- as.data.frame(sapply(non_ped_crash$injury_type_num, switch, 
                                      '5' = 10.082, 
                                      '4' = 1.103, 
                                      '3' = 0.304, 
                                      '2' = 0.141,
                                      '1' = 0.046))
colnames(df_cost) <- "cost"

non_ped_crash <- cbind(non_ped_crash, df_cost)

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

df_overlap <- non_ped_crash

df_overlap$Longitude <- df_overlap$long
df_overlap$Latitude <- df_overlap$lat

coordinates(df_overlap) <- ~Longitude + Latitude

proj4string(df_overlap) <- proj4string(cvg_shapefile)

df_in_city <- over(df_overlap, cvg_shapefile)
df_crash_city <- cbind(non_ped_crash, df_in_city)
df_crash_city <- df_crash_city[!is.na(df_crash_city$RegionID),]

non_ped_crash <- df_crash_city

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_mapped <- which_grid_cell_big(grid_centroid, non_ped_crash)

cols_2_drop <- c("State", "County", "City", "Name", "RegionID")
df_mapped[, cols_2_drop] <- list(NULL)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

non_ped_crash_agg <- df_mapped %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(sum_cost = sum(cost),
                          num_events = n())

non_ped_crash_agg <- as.data.frame(non_ped_crash_agg)

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

png(filename = paste0(infile, "_non_pedestrian_sum_cost", "_mapped_2_grid.png"), 
    units = "in", 
    width = 15,
    height = 9,
    pointsize = 12, 
    res = 72)

hoods +
    geom_point(data = non_ped_crash_agg, aes(x = long_cell, y = lat_cell, color = log10(sum_cost+0.1)), shape = 19, size = 2.5, alpha = 0.8) + 
#    geom_point(data = df_mapped, aes(x = long, y = lat), color = "firebrick2", shape = 5, size = 0.1, alpha = 0.1) +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    ggtitle(infile) +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9], name = "Sum Cost (log)") +
    coord_cartesian(xlim = c(-84.35, -84.72), ylim = c(39.04, 39.23)) +
    theme(legend.position = c(0.01, 0.95), 
       legend.justification = c(0, 1))

dev.off()

# ...   plot 2

png(filename = paste0(infile, "_num_events", "_mapped_2_grid.png"), 
    units = "in", 
    width = 15,
    height = 9,
    pointsize = 12, 
    res = 72)

hoods +
    geom_point(data = non_ped_crash_agg, aes(x = long_cell, y = lat_cell, color = log10(num_events+1)), shape = 19, size = 2.5, alpha = 0.8) + 
#    geom_point(data = df_mapped, aes(x = long, y = lat), color = "firebrick2", shape = 5, size = 0.1, alpha = 0.1) +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    ggtitle(infile) +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9], name = "Number Events (log)") +
    coord_cartesian(xlim = c(-84.35, -84.72), ylim = c(39.04, 39.23)) +
    theme(legend.position = c(0.01, 0.95), 
       legend.justification = c(0, 1))

dev.off()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   save fle to csv
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(grid_mapped_dir)

file_name <- "non_ped_crash_mapped_to_grid_cells.csv"

write.table(df_mapped, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

file_name <- "non_ped_crash_aggregated_in_grid_cells.csv"

write.table(non_ped_crash_agg, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
