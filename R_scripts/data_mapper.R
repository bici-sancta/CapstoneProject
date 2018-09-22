
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : data_mapper_.R
# ...
# ...   code for generating an equal area geo grid pattern 
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   21-sep-2018
# ...
# ...   patrick.mcdevitt@smu.edu
# ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

rm(list=ls())

library(ggplot2)

data_dir <- "./../data/"
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

property_geocode = read.csv("hamilton_county_property_xfer_2008t2018_geocoded.txt",
                            header = TRUE, sep = "|", stringsAsFactors = FALSE)

infile <- "hamilton_county_property_xfer_2008t2018"
hc_xfer <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

infile <- "pedestrian_near_miss_incidents_geocodes"
near_miss <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

infile <- "pedestrian_survey_final_20180618_geocoded"
ped_srvy <- read.table(paste0('./', infile, '.txt'), sep = "|", stringsAsFactors = FALSE, header = TRUE)

infile <- "street_centerlines_w_pci_rating"
street_cl <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(street_cl) <- tolower(names(street_cl))
names(street_cl)[names(street_cl) == 'latitude'] <- 'lat'
names(street_cl)[names(street_cl) == 'longitude'] <- 'long'

infile <- "grid_points_250m"
grid_pts <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
grid_pts <- grid_pts[grid_pts$lat <= 39.221,]

infile <- "pat_addresses_geocoded"
pat_addr <- read.table(paste0('./', infile, '.txt'), sep = "|", stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   organize and subset
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

hc_xfer$zip_five <- substr(hc_xfer$zip_code, 1, 5)

hc_xfer$hca_address <- paste(
    paste(
        gsub("\\s", "", hc_xfer$house_number),
        gsub("\\s", "",hc_xfer$street_name),
        gsub("\\s", "",hc_xfer$street_suffix)),
    "Cincinnati",
    gsub("\\s", "", hc_xfer$zip_five),
    "OH, USA",
    sep = ", ")

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   subset to fewer columns
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

cols_2_keep <- c("hca_address", "property_number", "month_of_sale", "day_of_sale", "year_of_sale", "sale_price", "zip_five")
hc_xfer <- hc_xfer[, cols_2_keep]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   merge geocoordiantes into property transfer info
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

property_sale_geocoded <- merge(hc_xfer, property_geocode, by = "property_number")


cincy_zip_code = c("45202", "45203", "45204", "45205", "45206", "45207", "45208",
                   "45209", "45211", "45212", "45213", "45214", "45215", "45216",
                   "45217", "45219", "45220", "45223", "45224", "45225", "45226",
                   "45227", "45229", "45230", "45231", "45232", "45233", "45237",
                   "45238", "45239", "45243", "45248")


# ...   subset property data outlier values

df_subset <- property_sale_geocoded[property_sale_geocoded$zip_five %in% cincy_zip_code, ]

df_subset <- df_subset[df_subset$long > -85,]
df_subset <- df_subset[df_subset$long < -84,]
df_subset <- df_subset[df_subset$lat > 39.,]
df_subset <- df_subset[df_subset$lat < 39.5,]
df_subset <- df_subset[df_subset$sale_price > 20000,]
df_subset <- df_subset[df_subset$sale_price < 5000000,]
#df_subset <- df_subset[df_subset$year_of_sale > 2008,]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   make some plots
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

base_plot <- ggplot(df_subset, aes(x = long, y = lat, color = log10(sale_price))) +
    geom_point(size = 0.8, alpha = 0.4)

street_plot <- ggplot(street_cl, aes(x = long, y = lat, color = (pci_2015))) +
    geom_point(size = 0.4)
street_plot

base_plot + 
    geom_point(data = street_cl, color = "darkgrey", size = 0.1) +
    geom_point(data = ped_srvy, color = "goldenrod", size = 1.0) +
    geom_point(data = near_miss, color = "red", size = 2.0) +
    geom_point(data = grid_pts, color = "black", size = 0.1, alpha = 0.1) +
    geom_point(data = pat_addr, color = "forestgreen", size = 6.0, shape = 18, alpha = 0.8) +
    xlim(-84.72, -84.37) +
    ylim(39.05, 39.25)

base_plot + 
    geom_point(data = street_cl, color = factor(street_cl$category+2), size = 0.1, alpha = 0.5) +
    geom_point(data = ped_srvy, color = "goldenrod", size = 2.0) +
    geom_point(data = near_miss, color = "red", size = 3.0) +
    geom_point(data = grid_pts, color = "white", size = 0.1, alpha = 0.3) +
    geom_point(data = pat_addr, color = "forestgreen", size = 6.0, shape = 18, alpha = 0.8) +
    xlim(-84.55, -84.4) +
    ylim(39.125, 39.175) +
    theme_dark()
