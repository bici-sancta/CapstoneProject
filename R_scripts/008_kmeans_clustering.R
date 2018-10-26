
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

home_dir <- ("C:/Users/Preeti/Github/CapstoneProject/")
data_dir <- ("./data/")
grid_mapped_dir <- ("./data/grid_mapped")
plot_dir <- ("./plots/clustering/")
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

infile <- "df_model_with_kmeans"
kmeans <- read.csv(paste0('./', infile, '.csv'),
                     stringsAsFactors = FALSE, header = TRUE)

cincy_min_latitude <- 39.0
cincy_max_latitude <- 39.3
cincy_max_longitude <- -84.3
cincy_min_longitude <- -84.72

kmeans <- kmeans[kmeans$lat > cincy_min_latitude & kmeans$lat < cincy_max_latitude,]
kmeans <- kmeans[kmeans$long > cincy_min_longitude & kmeans$long < cincy_max_longitude,]

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
names(kmeans)[names(kmeans) == "cell_id"] <- "k_cell_id"
kmeans

df_mapped <- which_grid_cell_big(grid_centroid, kmeans)

df_mapped

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_5 = mean(kmeans_5))

# ...   make a plot to visualize result

setwd(home_dir)
setwd(plot_dir)

hoods <- ggplot() +  geom_point(data=cvg_shapefile, aes(x=long, y=lat, group=group), size = 0.1, alpha = 0.4)

# ...   Basic map of event severity

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ...   change : data = xxx ; color = yyy for each new data set / variable to map
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_5 = mean(kmeans_5))


# ...   plot 1

png(filename = paste0(infile, "kmeans_5", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
  geom_point(data = kmeans_agg, aes(x = long_cell, y = lat_cell, color = kmeans_5), shape = 15, size = 2.5, alpha = 0.8) + 
  geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
  #    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
  ggtitle(infile) +
  xlab("Longitude") + ylab("Latitude") +
  #    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
  coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   kmeans_6

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_6 = mean(kmeans_6))


png(filename = paste0(infile, "kmeans_6", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
  geom_point(data = kmeans_agg, aes(x = long_cell, y = lat_cell, color = kmeans_6), shape = 15, size = 2.5, alpha = 0.8) + 
  geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
  #    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
  ggtitle(infile) +
  xlab("Longitude") + ylab("Latitude") +
  #    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
  coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()


# ...   kmeans 7

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_7 = mean(kmeans_7))


png(filename = paste0(infile, "kmeans_7", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
  geom_point(data = kmeans_agg, aes(x = long_cell, y = lat_cell, color = kmeans_7), shape = 15, size = 2.5, alpha = 0.8) + 
  geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
  #    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
  ggtitle(infile) +
  xlab("Longitude") + ylab("Latitude") +
  #    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
  coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   plot 3

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_8 = mean(kmeans_8))

png(filename = paste0(infile, "kmeans_8", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
  geom_point(data = kmeans_agg, aes(x = long_cell, y = lat_cell, color = kmeans_8), shape = 15, size = 2.5, alpha = 0.8) + 
  geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
  #    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
  ggtitle(infile) +
  xlab("Longitude") + ylab("Latitude") +
  #    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
  coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   9

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_9 = mean(kmeans_9))

png(filename = paste0(infile, "kmeans_9", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
  geom_point(data = kmeans_agg, aes(x = long_cell, y = lat_cell, color = kmeans_9), shape = 15, size = 2.5, alpha = 0.8) + 
  geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
  #    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
  ggtitle(infile) +
  xlab("Longitude") + ylab("Latitude") +
  #    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
  coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   kmeans 3

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_3 = mean(kmeans_3))

png(filename = paste0(infile, "kmeans_3", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
  geom_point(data = kmeans_agg, aes(x = long_cell, y = lat_cell, color = kmeans_3), shape = 15, size = 2.5, alpha = 0.8) + 
  geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
  #    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
  ggtitle(infile) +
  xlab("Longitude") + ylab("Latitude") +
  #    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
  coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   kmeans 2

kmeans_agg <- df_mapped %>% 
  group_by(cell_id, lat_cell, long_cell) %>%
  summarize(kmeans_2 = mean(kmeans_2))

png(filename = paste0(infile, "kmeans_2", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
  geom_point(data = kmeans_agg, aes(x = long_cell, y = lat_cell, color = kmeans_2), shape = 15, size = 2.5, alpha = 0.8) + 
  geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
  #    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
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
setwd(grid_mapped_dir)

file_name <- paste0(infile, "_mapped_to_grid_cells.csv")

write.table(df_mapped, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
