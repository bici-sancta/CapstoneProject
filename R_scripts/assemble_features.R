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

#home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
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

infile <- "2017_NFIRS_Cincinnati_Fire_Department_Incident_Data_mapped_to_grid_cells.csv"
fire_incdnt <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "pedestrian_near_miss_incidents_geocodes_mapped_to_grid_cells.csv"
near_miss <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "pedestrian_survey_w_neighborhood_mapped_to_grid_cells.csv"
ped_srvy <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "street_centerlines_w_pci_rating_mapped_to_grid_cells.csv"
street <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "traffic_crash_reports_20180918_mapped_to_grid_cells.csv"
traffic_crash <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "WalkScoreMasterFileByStreet_in_cincinnati_mapped_to_grid_cells.csv"
walk_score <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

infile <- "hamilton_county_property_xfer_2008t2018_in_cincinnati_mapped_to_grid_cells.csv"
property <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

infile <- "../dictionaries/hamilton_county_land_use_codes.csv"
property_codes <- read.csv(infile,
                       stringsAsFactors = FALSE, header = TRUE)

property <- merge(property, property_codes, by = "property_class", all.x = TRUE)
property$category <- tolower(property$category)

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

plot(cvg_shapefile, col = "dodgerblue2")


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   aggregate within each data set to consolidate features to unique grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

fire_incd_agg <- fire_incdnt %>% 
                group_by(cell_id) %>%
                summarize(num_fire_incd = n())

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

property_agg <- property %>% 
                group_by(cell_id, category) %>%
                summarize(num_prop_sales = n())

prop_agg_sorted <- property_agg[order(property_agg$cell_id, -property_agg$num_prop_sales),]
prop_category <- prop_agg_sorted[!duplicated(prop_agg_sorted$cell_id),]
prop_category$num_prop_sales <- NULL

table(property_agg$category)

property_value_agg <- property %>% 
                group_by(cell_id) %>%
                summarize(max_sale_price = max(sale_price),
                          median_sale_price = median(sale_price),
                          num_prop_sales = n())

property_agg <- merge(property_value_agg, prop_category, by = "cell_id", all.x = TRUE)

dmy <- dummyVars(" ~ .", data = property_agg)
trsf <- data.frame(predict(dmy, newdata = property_agg))

plot(cvg_shapefile)
points(property$lat ~ property$long, col = "dodgerblue4", cex = 0.5)
points(property$lat ~ property$long, col = factor(property$category), cex = 0.5)

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

ped_survey_agg <- ped_srvy %>% 
                group_by(cell_id) %>%
                summarize(num_reports = n())

near_miss_agg <- near_miss %>% 
                group_by(cell_id) %>%
                summarize(num_near_misses = n())

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   merge data frame into assembled data table
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df <- merge(grid_centroid, fire_incd_agg, by = "cell_id", all.x = TRUE)

df <- merge(df, trsf, by = "cell_id", all.x = TRUE)
df <- merge(df, ped_crash_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, street_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, walk_score_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, ped_survey_agg, by = "cell_id", all.x = TRUE)
df <- merge(df, near_miss_agg, by = "cell_id", all.x = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   impute (baseline ... add some better methods here)
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df$num_near_misses[is.na(df$num_near_misses)] <- 0
df$num_reports[is.na(df$num_reports)] <- 0
df$num_streets[is.na(df$num_streets)] <- 0
df$num_events[is.na(df$num_events)] <- 0
df$num_fire_incd[is.na(df$num_fire_incd)] <- 0
df$sum_cost[is.na(df$sum_cost)] <- 0

df$num_prop_sales[is.na(df$num_prop_sales)] <- 0
df$median_sale_price.imputed <- ifelse(is.na(df$median_sale_price), median(df$median_sale_price, na.rm = TRUE), df$median_sale_price)
df$max_sale_price.imputed <- ifelse(is.na(df$max_sale_price), median(df$max_sale_price, na.rm = TRUE), df$max_sale_price)

df$num_walk_scores[is.na(df$num_walk_scores)] <- 0
df$mean_walk_score.imputed <- ifelse(is.na(df$mean_walk_score), median(df$mean_walk_score, na.rm = TRUE), df$mean_walk_score)
df$min_walk_score.imputed <- ifelse(is.na(df$min_walk_score), median(df$min_walk_score, na.rm = TRUE), df$min_walk_score)
df$max_walk_score.imputed <- ifelse(is.na(df$max_walk_score), median(df$max_walk_score, na.rm = TRUE), df$max_walk_score)

df$sum_lane_cnt.imputed <- ifelse(is.na(df$sum_lane_cnt), median(df$sum_lane_cnt, na.rm = TRUE), df$sum_lane_cnt)

df$sum_width.imputed <- ifelse(is.na(df$sum_width), median(df$sum_width, na.rm = TRUE), df$sum_width)
df$sum_area.imputed <- ifelse(is.na(df$sum_area), median(df$sum_area, na.rm = TRUE), df$sum_area)


#> names(df)
# [1] "cell_id"                   "ix"                        "iy"                        "lat"                       "long"                      "State"                    
# [7] "County"                    "City"                      "Name"                      "RegionID"                  "num_fire_incd"             "median_sale_price"        
#[13] "max_sale_price"            "num_prop_sales"            "sum_cost"                  "num_events"                "sum_lane_cnt"              "sum_width"                
#[19] "sum_area"                  "num_streets"               "mean_walk_score"           "min_walk_score"            "max_walk_score"            "num_walk_scores"          
#[25] "num_reports"               "num_near_misses"           "median_sale_price.imputed" "max_sale_price.imputed"    "mean_walk_score.imputed"   "min_walk_score.imputed"   
#[31] "max_walk_score.imputed"    "sum_lane_cnt.imputed"      "sum_width.imputed"         "sum_area.imputed"         

# "categoryagricultural"     
# "categorycommercial"        "categoryindustrial"        "categorypublicly.owned"    "categorypublic.utilities"  "categoryresidential"

cols_2_keep <- c("cell_id", "lat", "long", "Name", "sum_cost", "num_fire_incd", "num_prop_sales", "num_streets", "num_walk_scores",
                 "num_reports", "num_near_misses", "median_sale_price.imputed", "max_sale_price.imputed",
                 "mean_walk_score.imputed", "min_walk_score.imputed", "max_walk_score.imputed", "sum_lane_cnt.imputed", "sum_width.imputed", "sum_area.imputed",
                 "categoryagricultural", "categorycommercial", "categoryindustrial", "categorypublicly.owned",
                 "categorypublic.utilities","categoryresidential")

df_model_w_ids <- df[, cols_2_keep]

cols_2_keep <- c("sum_cost", "num_fire_incd", "num_prop_sales", "num_streets", "num_walk_scores",
                 "num_reports", "num_near_misses", "median_sale_price.imputed", "max_sale_price.imputed",
                 "mean_walk_score.imputed", "min_walk_score.imputed", "max_walk_score.imputed", "sum_lane_cnt.imputed", "sum_width.imputed", "sum_area.imputed",
                 "categoryagricultural", "categorycommercial", "categoryindustrial", "categorypublicly.owned",
                 "categorypublic.utilities","categoryresidential")

df_model <- df[, cols_2_keep]

df_model$PSI <- (df_model$predict*df_model$sum_cost)-df_model$predict

fit1 <- lm(sum_cost ~ (.)^2, df_model)
summary(fit1)

df_model$predict <- predict(fit1, df_model)

df_model <- df_model %>% mutate(cost_gt_pred = ifelse(sum_cost > (1.10 * predict), sum_cost, NA))

par(bg = "lightgrey")
plot(sum_cost ~ predict, data = df_model,
     col = "darkgrey", 
     xlim = c(0, 30),
     ylim = c(0, 30))
points(cost_gt_pred ~ predict, data = df_model, col = "dodgerblue4")
abline(a = 0, b = 1, col = "dodgerblue3")





# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ... write some .csv files 
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

write.csv(df_model_w_ids, "df_model_w_lat_long.csv", row.names = FALSE)
write.csv(df_model, "df_model.csv", row.names = FALSE)
