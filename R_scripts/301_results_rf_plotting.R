# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : assemble_features.R
# ...
# ...   organizer to assemble features from multiple input files
# ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   08-oct-2018
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

library(caret)
library(car)

printf <- function(...) invisible(cat(sprintf(...)))

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   define some directory locations
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
#home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
grid_mapped_dir <- ("./data/grid_mapped")
plot_dir <- ("./plots/")
src_dir <- ("./R_scripts")
zillow_dir <- ("./data/ZillowNeighborhoods-OH")
ppt_plot_dir <- ("./ppt/plots/")

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   define some utility functions
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(src_dir)

source("./cincy_zip_codes.R")
source("./clean_fire_incident.R")

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in some data sets
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(grid_mapped_dir)

# ...   data files  ...
# ...       2017_NFIRS_Cincinnati_Fire_Department_Incident_Data_mapped_to_grid_cells.csv
# ...       hamilton_county_property_xfer_2008t2018_mapped_to_grid_cells.csv
# ...       pedestrian_near_miss_incidents_geocodes_mapped_to_grid_cells.csv
# ...       pedestrian_survey_w_neighborhood_mapped_to_grid_cells.csv
# ...       street_centerlines_w_pci_rating_mapped_to_grid_cells.csv
# ...       traffic_crash_reports_20180918_mapped_to_grid_cells.csv
# ...       WalkScoreMasterFileByStreet_mapped_to_grid_cells.csv
# ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   data sets
# ...   001 - near miss

infile <- "pedestrian_near_miss_incidents_geocodes_mapped_to_grid_cells.csv"
near_miss <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   002 - pedestrian survey

infile <- "ped_survey_aggregate_n_onehot_encoded.csv"
ped_srvy <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   003 - walk scores

infile <- "WalkScoreMasterFileByStreet_in_cincinnati_mapped_to_grid_cells.csv"
walk_score <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   004 - streets

infile <- "street_centerlines_w_pci_rating_mapped_to_grid_cells.csv"
street <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   005 - traffic crash - pedestrian

infile <- "traffic_crash_reports_20180918_mapped_to_grid_cells.csv"
traffic_crash <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   006 - fire incidents

infile <- "2017_NFIRS_Cincinnati_Fire_Department_Incident_Data_mapped_to_grid_cells.csv"
fire_incdnt <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   007 - proprty xfers

infile <- "cvg_property_xfers_aggregated_to_cell.csv"
property <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "cvg_property_xfers_neighborhood_medians.csv"
prop_hood_median <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   008 - traffic crash - non-pedestrian

infile <- "non_ped_crash_aggregated_in_grid_cells.csv"
crash_non_ped <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   009 - bus stops

infile <- "grid_cells_w_bus_stop_distances.csv"
bus_stop <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   010 - 311 non emergency

infile <- "non_emergency_aggregated_in_grid_cells.csv"
non_urgence <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

grid_file <- "grid_points_250m_w_neighborhood"
grid_centroid <- read.csv(paste0('./', grid_file, '.csv'), stringsAsFactors = FALSE, header = TRUE)

# ...   read in shapefiles from Zillow definitions

zillow_dir <- "./data/ZillowNeighborhoods-OH"
data_dir <- "./data/"
plot_dir <- "./plots/"

setwd(home_dir)
setwd(zillow_dir)
oh_shapefile <- readOGR("ZillowNeighborhoods-OH.shp", layer="ZillowNeighborhoods-OH")
cvg_shapefile <- oh_shapefile[oh_shapefile$City == "Cincinnati", ]

# ...   drop 2 neighborhoods which are not in Cincinnati

cvg_shapefile <- cvg_shapefile[cvg_shapefile$Name != "Fruit Hill", ]
cvg_shapefile <- cvg_shapefile[cvg_shapefile$Name != "Forestville", ]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   aggregate within each data set to consolidate features to unique grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

fire_incd_agg <- fire_incdnt %>% 
                group_by(cell_id) %>%
                summarize(num_fire_incd = n())

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

cols_2_keep <- c("cell_id", "category", "valid_sale", "median_sale_price")
property <- property[, cols_2_keep]

cols_2_keep <- c("cell_id", "median_sale_price")

prop_res_y <- property[property$category == "residential" & property$valid == "Y", cols_2_keep]
prop_res_n <- property[property$category == "residential" & property$valid == "N", cols_2_keep]
prop_com_y <- property[property$category == "commercial"  & property$valid == "Y", cols_2_keep]
prop_com_n <- property[property$category == "commercial"  & property$valid == "N", cols_2_keep]
prop_ind_y <- property[property$category == "industrial"  & property$valid == "Y", cols_2_keep]
prop_ind_n <- property[property$category == "industrial"  & property$valid == "N", cols_2_keep]
prop_pbo_y <- property[property$category == "publicly owned"  & property$valid == "Y", cols_2_keep]
prop_pbo_n <- property[property$category == "publicly owned"  & property$valid == "N", cols_2_keep]
prop_pbo_y <- property[property$category == "other"  & property$valid == "Y", cols_2_keep]
prop_pbo_n <- property[property$category == "other"  & property$valid == "N", cols_2_keep]

names(prop_res_y)[names(prop_res_y) == "median_sale_price"] <- "med_sale_res_y"
names(prop_res_n)[names(prop_res_n) == "median_sale_price"] <- "med_sale_res_n"

names(prop_com_y)[names(prop_com_y) == "median_sale_price"] <- "med_sale_com_y"
names(prop_com_n)[names(prop_com_n) == "median_sale_price"] <- "med_sale_com_n"

names(prop_ind_y)[names(prop_ind_y) == "median_sale_price"] <- "med_sale_ind_y"
names(prop_ind_n)[names(prop_ind_n) == "median_sale_price"] <- "med_sale_ind_n"

names(prop_pbo_y)[names(prop_pbo_y) == "median_sale_price"] <- "med_sale_pbo_y"
names(prop_pbo_n)[names(prop_pbo_n) == "median_sale_price"] <- "med_sale_pbo_n"

df_cell_id <- as.data.frame(grid_centroid$cell_id)
names(df_cell_id) <- "cell_id"

property_agg <- merge(df_cell_id, prop_res_y, by = "cell_id", all.x = TRUE)
property_agg <- merge(property_agg, prop_res_n, by = "cell_id", all.x = TRUE)
property_agg <- merge(property_agg, prop_com_y, by = "cell_id", all.x = TRUE)
property_agg <- merge(property_agg, prop_com_n, by = "cell_id", all.x = TRUE)
property_agg <- merge(property_agg, prop_ind_y, by = "cell_id", all.x = TRUE)
property_agg <- merge(property_agg, prop_ind_n, by = "cell_id", all.x = TRUE)
property_agg <- merge(property_agg, prop_pbo_y, by = "cell_id", all.x = TRUE)
property_agg <- merge(property_agg, prop_pbo_n, by = "cell_id", all.x = TRUE)

# ...   impute where mising

property_agg$med_sale_pbo_n[is.na(property_agg$med_sale_pbo_n)] <- 0
property_agg$med_sale_pbo_y[is.na(property_agg$med_sale_pbo_y)] <- 0

property_agg$med_sale_ind_n[is.na(property_agg$med_sale_ind_n)] <- 0
property_agg$med_sale_ind_y[is.na(property_agg$med_sale_ind_y)] <- 0

property_agg$med_sale_com_n[is.na(property_agg$med_sale_com_n)] <- 0
property_agg$med_sale_com_y[is.na(property_agg$med_sale_com_y)] <- 0

names(grid_centroid)[names(grid_centroid) == "Name"] <- "neighborhood"
hood_tmp <- merge(grid_centroid, prop_hood_median, by = "neighborhood", all.x = TRUE)
cols_2_keep <- c("cell_id", "median_sale_price")
hood_tmp <- hood_tmp[, cols_2_keep]

property_agg <- merge(property_agg, hood_tmp, by = "cell_id", all.x = TRUE)

property_agg$med_sale_res_y <- ifelse(is.na(property_agg$med_sale_res_y),
                                      property_agg$median_sale_price,
                                      property_agg$med_sale_res_y)

property_agg$med_sale_res_n <- ifelse(is.na(property_agg$med_sale_res_n),
                                      property_agg$median_sale_price * 0.8,
                                      property_agg$med_sale_res_n)

property_agg$median_sale_price <- NULL

rm(hood_tmp)
rm(prop_res_y, prop_res_n, prop_com_y, prop_com_n, prop_ind_y, prop_ind_n, prop_pbo_y, prop_pbo_n)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

ped_crash_agg <- traffic_crash %>% 
                group_by(cell_id) %>%
                summarize(sum_cost = sum(cost),
                          num_events = n())

street_agg <- street %>% 
                group_by(cell_id) %>%
                summarize(sum_lane_cnt = sum(lane_cnt),
                          sum_width = sum(width),
                          sum_area = sum(area),
                          num_streets = n())

walk_score_agg <- walk_score %>% 
                group_by(cell_id) %>%
                summarize(mean_walk_score = mean(walkscore),
                          min_walk_score = min(walkscore),
                          max_walk_score = max(walkscore),
                          num_walk_scores = n())

near_miss_agg <- near_miss %>% 
                group_by(cell_id) %>%
                summarize(num_near_misses = n())

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   merge data frame into assembled data table
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   001 - near miss
# ...   002 - pedestrian survey
# ...   003 - walk scores
# ...   004 - streets
# ...   005 - traffic crash - pedestrian
# ...   006 - fire incidents
# ...   007 - proprty xfers
# ...   008 - traffic crash - non-pedestrian
# ...   009 - bus stops
# ...   010 - 311 non emergency

df <- merge(grid_centroid, near_miss_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, ped_srvy, by = "cell_id", all.x = TRUE)
df <- merge(df, walk_score_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, street_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, ped_crash_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, fire_incd_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, property_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, crash_non_ped, by = "cell_id", all.x = TRUE)
df <- merge(df, bus_stop, by = "cell_id", all.x = TRUE)
df <- merge(df, non_urgence, by = "cell_id", all.x = TRUE)


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   impute (baseline ... add some better methods here)
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df$num_near_misses[is.na(df$num_near_misses)] <- 0
df$num_streets[is.na(df$num_streets)] <- 0
df$num_fire_incd[is.na(df$num_fire_incd)] <- 0

df$num_events.x[is.na(df$num_events.x)] <- 0
df$sum_cost.x[is.na(df$sum_cost.x)] <- 0
df$num_events.y[is.na(df$num_events.y)] <- 0
df$sum_cost.y[is.na(df$sum_cost.y)] <- 0

df$n_rqst[is.na(df$n_rqst)] <- 0
df$n_request[is.na(df$n_request)] <- 0

df$num_walk_scores[is.na(df$num_walk_scores)] <- 0
df$mean_walk_score <- ifelse(is.na(df$mean_walk_score), median(df$mean_walk_score, na.rm = TRUE), df$mean_walk_score)
df$min_walk_score <- ifelse(is.na(df$min_walk_score), median(df$min_walk_score, na.rm = TRUE), df$min_walk_score)
df$max_walk_score <- ifelse(is.na(df$max_walk_score), median(df$max_walk_score, na.rm = TRUE), df$max_walk_score)

df$sum_lane_cnt <- ifelse(is.na(df$sum_lane_cnt), median(df$sum_lane_cnt, na.rm = TRUE), df$sum_lane_cnt)
df$sum_width <- ifelse(is.na(df$sum_width), median(df$sum_width, na.rm = TRUE), df$sum_width)
df$sum_area <- ifelse(is.na(df$sum_area), median(df$sum_area, na.rm = TRUE), df$sum_area)

df_plot <- df

df[is.na(df)] <- 0


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   make some plots to visualize result
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(ppt_plot_dir)

x_plot_limits <- c(-84.35, -84.72)
y_plot_limits <- c(39.05, 39.225)

plot_width = 15
plot_height = 9
plot_res = 320
plot_pointsize = 12

hoods <- ggplot() +  geom_point(data = cvg_shapefile, aes(x = long, y = lat, group = group),
                                size = 0.1,
                                alpha = 0.9,
                                color = "#666666FF")

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   Random Forest results file ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile = "random_forest_binary_predictor.csv"
df_rf_results <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

df_rf_model <- merge(df_plot, df_rf_results, by = "cell_id")
df_rf_model$binary_observed <- ifelse(df_rf_model$sum_cost.x > 0, 1, 0)

df_rf_model$result <- ifelse((df_rf_model$binary_observed == 1 & df_rf_model$binary_predictor == 0), "FN", 0)
df_rf_model$result <- ifelse((df_rf_model$binary_observed == 0 & df_rf_model$binary_predictor == 1), "FP", df_rf_model$result)
df_rf_model$result <- ifelse((df_rf_model$binary_observed == 0 & df_rf_model$binary_predictor == 0), "TN", df_rf_model$result)
df_rf_model$result <- ifelse((df_rf_model$binary_observed == 1 & df_rf_model$binary_predictor == 1), "TP", df_rf_model$result)

png(filename = paste0("_ppt", "_rf_binary_results", "_map.png"), 
    units = "in", 
    width = plot_width,
    height = plot_height,
    pointsize = plot_pointsize, 
    res = plot_res)

title <- "Random Forest - PSI Regions"

hoods +
    geom_point(data = df_rf_model, aes(x = long.x, y = lat.x, color = result), shape = 19, size = 3, alpha = 0.7) + 
    geom_point(data = grid_centroid, aes(x = long, y = lat), color = "forestgreen", size = 0.2, alpha = 0.2) +

    ggtitle(title) +
    xlab("Longitude") + ylab("Latitude") +
    theme(text = element_text(size = 25)) +
    theme(legend.position = c(0.05, 0.9)) +
    theme(legend.title = element_text(colour = "#666666FF", size = 15, face = "bold")) + 
    theme(legend.text = element_text(colour = "#666666FF", size = 15, face = "bold")) + 
    theme(legend.justification = c(0, 1)) +

#    scale_color_gradientn(colors = rev(rainbow(9)[2:9])) +
#    scale_color_gradient2(midpoint = 1.5) +
#    scale_color_distiller(palette = "Spectral") +
    scale_color_manual(values = c("#CC2222FF", "#AAAAAA33", "#CCCCCC66", "#44CC44FF")) +
    coord_cartesian(xlim = x_plot_limits, ylim = y_plot_limits)

dev.off()

