
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
library(caret)

printf <- function(...) invisible(cat(sprintf(...)))

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")
grid_mapped_dir <- ("./data/grid_mapped")
plot_dir <- ("./plots/")
src_dir <- ("./R_scripts")
zillow_dir <- ("./data/ZillowNeighborhoods-OH")
ppt_plot_dir <- ("./ppt/plots/")

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

infile <- "PedSafety_061818_Final_from_Mel_geocoded"
ped_survey <- read.table(paste0('./', infile, '.txt'), sep = "|", stringsAsFactors = FALSE, header = TRUE)

infile <- "ped_survey_request_types"
ped_rqst_type <- read.table(paste0('./', infile, '.csv'), sep = ",", stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

grid_file <- "grid_points_250m_w_neighborhood"
grid_centroid <- read.csv(paste0('./', grid_file, '.csv'), stringsAsFactors = FALSE, header = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   read in shapefile of neighborhoods for plotting & overlay determination
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

df_overlap <- ped_survey
df_overlap$Longitude <- df_overlap$long
df_overlap$Latitude <- df_overlap$lat

coordinates(df_overlap) <- ~Longitude + Latitude

proj4string(df_overlap) <- proj4string(cvg_shapefile)

df_in_city <- over(df_overlap, cvg_shapefile)
df_srvy_city <- cbind(ped_survey, df_in_city)
df_srvy_city <- df_srvy_city[!is.na(df_srvy_city$RegionID),]

ped_survey <- df_srvy_city

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_mapped <- which_grid_cell_big(grid_centroid, ped_survey)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   one-hot encode categorical column
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

survey <- df_mapped

# [1] "requestid"           "requesttyp"          "requestdat"          "comments"            "usertype"           
# [6] "near_inter"          "near_str"            "strsegid"            "additional_comments" "sna_name"           
#[11] "srvy_address"        "geo_address"         "lat"                 "long"                "geo_address_type"   
#[16] "geo_accuracy"        "State"               "County"              "City"                "Name"               
#[21] "RegionID"            "dist"                "cell_id"             "lat_cell"            "long_cell"          
#[26] "hood"                "hood_id"            

cols_2_keep <- c("cell_id", "lat_cell", "long_cell", "requestid", "requesttyp", "usertype")
survey <- survey[, cols_2_keep]
survey <- merge(survey, ped_rqst_type, by = "requesttyp", all.x = TRUE)
survey$requesttyp <- NULL
survey$usertype[survey$usertype == "travels (other)"] <- "other"
survey$usertype[survey$usertype == "uses an assistive device"] <- "assist"

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   aggregate features to each cell id
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

survey_agg <- survey %>% 
                group_by(cell_id, lat_cell, long_cell, requestid, rqst_factor, usertype) %>%
                summarize(n_rqst = n())

survey_agg <- as.data.frame(survey_agg)

# ...dummyVars will transform all characters and factors columns
# ...   (the function never transforms numeric columns)
# ...       and return the entire data set

dummy_variable <- dummyVars(" ~ .", data = survey_agg)
survey_encoded <- data.frame(predict(dummy_variable, newdata = survey_agg))

# ...   aggregage to cell_id .. have to sum each factor ...

survey_encoded_agg <- survey_encoded %>% 
                group_by(cell_id, lat_cell, long_cell) %>%
                summarize(
                    ACCESS = sum(rqst_factorACCESS),
                    DBLPRK= sum(rqst_factorDBLPRK),
                    DNOTYLD = sum(rqst_factorDNOTYLD),
                    JYWALK = sum(rqst_factorJYWALK),
                    LVISIB = sum(rqst_factorLVISIB),
                    LWFWS= sum(rqst_factorLWFWS),
                    NOBIKEF = sum(rqst_factorNOBIKEF),
                    NOSWLK = sum(rqst_factorNOSWLK),
                    OTHER = sum(rqst_factorOTHER),
                    PRKINT = sum(rqst_factorPRKINT),
                    PRKSWLK = sum(rqst_factorPRKSWLK),
                    SPEED = sum(rqst_factorSPEED),
                    VRRLSS = sum(rqst_factorVRRLSS),
                    WLKSIG = sum(rqst_factorWLKSIG),
                    XWALK = sum(rqst_factorXWALK),
                    assist = sum(usertypeassist),
                    bikes = sum(usertypebikes),
                    drives = sum(usertypedrives),
                    other = sum(usertypeother),
                    walks = sum(usertypewalks),
                    n_rqst = sum(n_rqst))

names(survey_encoded_agg) <- tolower(names(survey_encoded_agg))

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   make a plot to visualize result
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(plot_dir)

hoods <- ggplot() +
        geom_polygon(data=cvg_shapefile, aes(x=long, y=lat, group=group), color = "lightgrey",
                     size = 0.1,
                     alpha = 0.2)

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
    geom_point(data = survey_encoded, aes(x = long_cell, y = lat_cell, color = n_rqst), shape = 15, size = 2.5, alpha = 0.8) + 
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
setwd(grid_mapped_dir)


file_name <- paste0(infile, "_mapped_to_grid_cells.csv")

write.table(df_mapped, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

file_name <- "ped_survey_aggregate_n_onehot_encoded.csv"
write.table(survey_encoded_agg, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 