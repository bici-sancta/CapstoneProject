
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : 008_gca_non_urgence_non_ped.R
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
library(data.table)

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

infile <- "311_non_emergency_data_reduced"
non_urgence <- read.csv(paste0('./', infile, '.csv'), stringsAsFactors = FALSE, header = TRUE)

names(non_urgence) <- tolower(names(non_urgence))

names(non_urgence)[names(non_urgence) == "latitude"] <- "lat"
names(non_urgence)[names(non_urgence) == "longitude"] <- "long"

non_urgence <- non_urgence[non_urgence$lat > 39 & non_urgence$lat < 39.3,]
non_urgence <- non_urgence[non_urgence$long > -84.72 & non_urgence$long < -84.3,]

non_urgence <- non_urgence[!is.na(non_urgence$long), ]
non_urgence <- non_urgence[!is.na(non_urgence$lat), ]


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

df_overlap <- non_urgence

df_overlap$Longitude <- df_overlap$long
df_overlap$Latitude <- df_overlap$lat

coordinates(df_overlap) <- ~Longitude + Latitude

proj4string(df_overlap) <- proj4string(cvg_shapefile)

df_in_city <- over(df_overlap, cvg_shapefile)
df_emerg_city <- cbind(non_urgence, df_in_city)
df_emerg_city <- df_emerg_city[!is.na(df_emerg_city$RegionID),]

non_urgence <- df_emerg_city

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   assign data values to grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

df_mapped <- which_grid_cell_big(grid_centroid, non_urgence)

cols_2_drop <- c("State", "County", "City", "Name", "RegionID")
df_mapped[, cols_2_drop] <- list(NULL)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   accumulate sum of costs in each grid cell
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

non_urgence_agg <- as.data.frame(dcast(setDT(df_mapped), cell_id+lat_cell+long_cell ~ issue.category, length))

names(non_urgence_agg) <- tolower(names(non_urgence_agg))
names(non_urgence_agg) <- gsub(x = names(non_urgence_agg), pattern = "/", replacement = "_")

sum_lst <- names(non_urgence_agg)
sum_lst <- sum_lst[4:length(sum_lst)]

non_urgence_agg$n_request <- rowSums(non_urgence_agg[, sum_lst])

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   save fle to csv
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(grid_mapped_dir)

file_name <- "non_emergency_mapped_to_grid_cells.csv"
write.table(df_mapped, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

file_name <- "non_emergency_aggregated_in_grid_cells.csv"
write.table(non_urgence_agg, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
