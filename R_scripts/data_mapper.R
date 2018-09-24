
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

home_dir <- "/home/mcdevitt/_ds/_smu/_src/CapstoneProject/"
data_dir <- "./data/"

setwd(home_dir)
setwd(data_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

property_geocode = read.csv("hamilton_county_property_xfer_2008t2018_geocoded.txt",
                            header = TRUE, sep = "|", stringsAsFactors = FALSE)

infile <- "/Under Review/hamilton_county_property_xfer_2008t2018"
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

infile <- "2017_NFIRS_Cincinnati_Fire_Department_Incident_Data"
fire_event <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(fire_event) <- tolower(names(fire_event))
names(fire_event)[names(fire_event) == 'latitude'] <- 'lat'
names(fire_event)[names(fire_event) == 'longitude'] <- 'long'

infile <- "traffic_crash_reports_20180918"
traffic_crash <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(traffic_crash) <- tolower(names(traffic_crash))
names(traffic_crash)[names(traffic_crash) == 'latitude'] <- 'lat'
names(traffic_crash)[names(traffic_crash) == 'longitude'] <- 'long'

infile <- "cincinnati_311_non_emergency_service_requests"
non_emergency <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(non_emergency) <- tolower(names(non_emergency))
names(non_emergency)[names(non_emergency) == 'latitude'] <- 'lat'
names(non_emergency)[names(non_emergency) == 'longitude'] <- 'long'

infile <- "roadkill"
road_kill <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)
names(road_kill) <- tolower(names(road_kill))
names(road_kill)[names(road_kill) == 'latitude'] <- 'lat'
names(road_kill)[names(road_kill) == 'longitude'] <- 'long'

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
    geom_point(size = 0.8, alpha = 0.4) +
    scale_color_gradientn(colours = rev(rainbow(7)),
#                         breaks = c(2, 4, 10, 100, 1000, 10000),
                         trans = "log10")

base_plot

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
    geom_point(data = street_cl, color = "darkgrey", size = 0.1, alpha = 0.2) +
        theme_dark()

#    geom_point(data = road_kill, color = "forestgreen", size = 0.2, alpha = 0.1) +
    geom_point(data = street_cl, color = "black", size = 0.1, alpha = 0.2) +
    geom_point(data = ped_srvy, color = "goldenrod", size = 1.0) +
    geom_point(data = near_miss, color = "red", size = 2.0) +
    geom_point(data = grid_pts, color = "white", size = 0.1, alpha = 0.3) +
#    geom_point(data = fire_event, color = "darkorchid4", size = 1.0, shape = 23, alpha = 0.8) +
    xlim(-84.72, -84.37) +
    ylim(39.05, 39.25) +
#    xlim(-84.55, -84.4) +
#    ylim(39.125, 39.175) +
    theme_dark()

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   add in google map raster image base map
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

library(raster)
library(ggplot2)
library(ggmap)

r <- raster(system.file("external/test.grd", package="raster"))
# just to make it reproducible with ggmap we have to transform to wgs84
r <- projectRaster(r, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

rtp <- rasterToPolygons(r)
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

bm <- ggmap(get_map(location = "Cincinnati", maptype = "roadmap", zoom = 11))

bm + geom_polygon(data = rtpFortMer, 
                  aes(x = long, y = lat, group = group), 
                  alpha = 0.5, 
                  size = 0) +  ## size = 0 to remove the polygon outlines
     scale_fill_gradientn(colours = topo.colors(255))

bm +
#    geom_point(data = street_cl, aes(x = long, y = lat, color = factor(category+2)), size = 0.1) + 
    geom_point(data = ped_srvy, aes(x = long, y = lat), color = "darkorchid3", size = 1.0) +
    geom_point(data = near_miss, aes(x = long, y = lat), color = "red", size = 2.0) +
    geom_point(data = grid_pts, aes(x = long, y = lat), color = "black", size = 0.1, alpha = 0.1) +
    geom_point(data = pat_addr, aes(x = long, y = lat), color = "forestgreen", size = 6.0, shape = 18, alpha = 0.8) +
    xlim(-84.72, -84.37) +
    ylim(39.05, 39.25)

