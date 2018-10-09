
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

infile <- "hamilton_county_property_xfer_2008t2018_geocoded"
property_id_geocode <- read.table(paste0('./', infile, '.txt'),
                       sep = "|",
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "hamilton_county_property_xfer_2008t2018"
property <- read.csv(paste0('./', infile, '.csv'),
                       stringsAsFactors = FALSE, header = TRUE)

#> names(property)
# [1] "book"                "plat"                "parcel"              "parcel_id"           "tax_district"       
# [6] "owner_name_1"        "owner_name_2"        "land_value"          "building_value"      "property_class"     
#[11] "house_number"        "street_name"         "street_suffix"       "zip_code"            "month_of_sale"      
#[16] "day_of_sale"         "year_of_sale"        "number_parcels_sold" "sale_price"          "valid_sale"         
#[21] "conveyance_number"   "deed_type"           "appreaisal_area"     "prior_owner"         "property_number"    

property$zip_five <- substr(property$zip_code, 1, 5)

property <- property[property$zip_five %in% cincy_zip_code, ]

cols_2_keep <- c("property_class", "zip_five", "month_of_sale", "day_of_sale", "year_of_sale",
                "number_parcels_sold", "sale_price", "valid_sale", "deed_type",
                "property_number")

property <- property[cols_2_keep]

property <- merge(property, property_id_geocode)

cincy_min_latitude <- 39.0
cincy_max_latitude <- 39.3
cincy_max_longitude <- -84.3
cincy_min_longitude <- -84.72

property <- property[property$lat > cincy_min_latitude & property$lat < cincy_max_latitude,]
property <- property[property$long > cincy_min_longitude & property$long < cincy_max_longitude,]

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

df_mapped <- which_grid_cell_big(grid_centroid, property)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

property_agg <- df_mapped %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(median_sale_price = median(sale_price),
                          max_sale_price = max(sale_price),
                          num_prop_sales = n())

# ...   make a plot to visualize result

setwd(home_dir)
setwd(plot_dir)

hoods <- ggplot() +  geom_point(data=cvg_shapefile, aes(x=long, y=lat, group=group), size = 0.1, alpha = 0.4)

# ...   Basic map of event severity

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ...   change : data = xxx ; color = yyy for each new data set / variable to map
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ...   plot 1

png(filename = paste0(infile, "median_sale_price", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
    geom_point(data = property_agg, aes(x = long_cell, y = lat_cell, color = log10(median_sale_price)), shape = 15, size = 2.5, alpha = 0.8) + 
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
#    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
    ggtitle(infile) +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
    coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   plot 1

png(filename = paste0(infile, "max_sale_price", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
    geom_point(data = property_agg, aes(x = long_cell, y = lat_cell, color = log10(max_sale_price)), shape = 15, size = 2.5, alpha = 0.8) + 
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
#    geom_point(data = df_mapped, aes(x = long, y = lat), color = "lightgrey", shape = 5, size = 0.2, alpha = 0.2) +
    ggtitle(infile) +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(9))[3:9]) +
    coord_cartesian(xlim = c(-84.25, -84.75), ylim = c(39., 39.25))

dev.off()

# ...   plot 3

png(filename = paste0(infile, "num_property_sales", "_mapped_2_grid.png"), 
    units = "in", 
    width = 18,
    height = 9,
    pointsize = 12, 
    res = 72)


hoods +
    geom_point(data = property_agg, aes(x = long_cell, y = lat_cell, color = log10(num_prop_sales)), shape = 15, size = 2.5, alpha = 0.8) + 
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
