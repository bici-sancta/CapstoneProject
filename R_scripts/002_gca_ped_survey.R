
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

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")
plot_dir <- ("./plots/")
src_dir <- ("./R_scripts")
zillow_dir <- ("./data/ZillowNeighborhoods-OH")

setwd(home_dir)
setwd(src_dir)

source("./which_grid_cell_function.R")
source("./which_grid_cell_function_big.R")
source("./cincy_zip_codes.R")

setwd(home_dir)
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "pedestrian_survey_w_neighborhood"
ped_survey <- read.table(paste0('./', infile, '.csv'), sep = ",", stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

grid_file <- "grid_points_250m_w_neighborhood"
grid_centroid <- read.csv(paste0('./', grid_file, '.csv'), stringsAsFactors = FALSE, header = TRUE)

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

df_mapped <- which_grid_cell_big(grid_centroid, ped_survey)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

ped_survey_agg <- df_mapped %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(num_reports = n())

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
 