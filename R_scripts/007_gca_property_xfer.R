
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
# ...   eliminate points that are outside city boundaries
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_overlap <- property
df_overlap$Longitude <- df_overlap$long
df_overlap$Latitude <- df_overlap$lat

coordinates(df_overlap) <- ~Longitude + Latitude

proj4string(df_overlap) <- proj4string(cvg_shapefile)

df_in_city <- over(df_overlap, cvg_shapefile)
df_prop_city <- cbind(property, df_in_city)
df_prop_city <- df_prop_city[!is.na(df_prop_city$RegionID),]

property <- df_prop_city

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   do a little present value adjustment
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

infile <- "zillow_market_appreciation"
value_adj <- read.csv(paste0('./', infile, '.csv'),
                       stringsAsFactors = FALSE, header = TRUE)

names(property)[names(property) == "year_of_sale"] <- "year"
names(property)[names(property) == "Name"] <- "neighborhood"

property <- (merge(value_adj, property, by = 'year'))

property$sale_price_adj <- property$sale_price * property$appreciation

cols_2_drop <- c("median", "appreciation", "month_of_sale", "day_of_sale", "sale_price",
                 "geo_accuracy", "State", "County", "City", "RegionID")

property[, cols_2_drop] <- list(NULL)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   add property category description
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(grid_mapped_dir)

infile <- "../dictionaries/hamilton_county_land_use_codes.csv"
property_codes <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

property <- merge(property, property_codes, by = "property_class", all.x = TRUE)
property$category <- tolower(property$category)

# ...   reduce category types to 4

property$category[property$category == "agricultural"] <- "other"
property$category[property$category == "public utilities"] <- "other"
property$category[is.na(property$category)] <- "other"
property$valid_sale[property$valid_sale == "U"] <- "Y"

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   neighborhood characteristics
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

property_hoods <- as.data.frame(property %>%
                        group_by(neighborhood, category, valid_sale) %>%
                        summarize(median_sale_price = median(sale_price_adj),
                                  max_sale_price = max(sale_price_adj),
                                  num_prop_sales = n()))

property_hood_valid <- property_hoods[property_hoods$valid_sale == "Y", ]
property_hood_valid_resid <- property_hood_valid[property_hood_valid$category == "residential", ]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_mapped <- which_grid_cell_big(grid_centroid, property)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

property_agg <- as.data.frame(df_mapped %>% 
                group_by(cell_id, lat_cell, long_cell, category, valid_sale) %>%
                summarize(median_sale_price = median(sale_price_adj),
                          max_sale_price = max(sale_price_adj),
                          num_prop_sales = n()))


# ...   make a plot to visualize result

setwd(home_dir)
setwd(plot_dir)

hoods <- ggplot() +  geom_point(data=cvg_shapefile, aes(x=long, y=lat, group=group), size = 0.1, alpha = 0.4)

# ...   Basic map of event severity

# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ...   change : data = xxx ; color = yyy for each new data set / variable to map
# ...   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ...   plot 1

png(filename = "property_xfers_median_sale_price.png", 
    units = "in", 
    width = 15,
    height = 9,
    pointsize = 12, 
    res = 72)

prop_sub <- property_agg[property_agg$median_sale_price > 50000,]

hoods +
    geom_point(data = prop_sub, aes(x = long_cell, y = lat_cell, color = log10(median_sale_price+1)), shape = 19, size = 2.5, alpha = 0.8) + 
#    geom_point(data = df_mapped, aes(x = long, y = lat), color = "firebrick2", shape = 5, size = 0.1, alpha = 0.1) +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    ggtitle("Cincinnati - Property Transfers (2008 - 2018)") +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(8))[3:9], name = "Median Sale Price (log)") +
    coord_cartesian(xlim = c(-84.35, -84.72), ylim = c(39.04, 39.23)) +
    theme(legend.position = c(0.01, 0.95), 
       legend.justification = c(0, 1))

dev.off()

# ...   plot 2

png(filename = "property_xfers_num_xfers_price.png", 
    units = "in", 
    width = 15,
    height = 9,
    pointsize = 12, 
    res = 72)

prop_sub2 <- property_agg[property_agg$num_prop_sales < 1000,]

hoods +
    geom_point(data = prop_sub2, aes(x = long_cell, y = lat_cell, color = log10(num_prop_sales)), shape = 19, size = 2.5, alpha = 0.8) + 
#    geom_point(data = df_mapped, aes(x = long, y = lat), color = "firebrick2", shape = 5, size = 0.1, alpha = 0.1) +
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +
    ggtitle("Cincinnati - Property Transfers (2008 - 2018)") +
    xlab("Longitude") + ylab("Latitude") +
#    theme_void() +
  scale_color_gradientn(colors = rev(rainbow(8))[3:9], name = "Number Xfers (log)") +
    coord_cartesian(xlim = c(-84.35, -84.72), ylim = c(39.04, 39.23)) +
    theme(legend.position = c(0.01, 0.95), 
       legend.justification = c(0, 1))

dev.off()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   save fle to csv
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(grid_mapped_dir)

base_name <- "cvg_property_xfers"
file_name <- paste0(base_name, "_mapped_to_grid_cells.csv")
write.table(df_mapped, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

base_name <- "cvg_property_xfers"
file_name <- paste0(base_name, "_aggregated_to_cell.csv")
write.table(property_agg, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

base_name <- "cvg_property_xfers"
file_name <- paste0(base_name, "_neighborhood_medians.csv")
write.table(property_hood_valid_resid, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
