
library(ggrepel)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)

setwd("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/data/ZillowNeighborhoods-OH")

oh_shapefile <- readOGR("ZillowNeighborhoods-OH.shp", layer="ZillowNeighborhoods-OH")

cvg_shapefile <- oh_shapefile[oh_shapefile$City == "Cincinnati", ]

cvg_shapefile@data$id <- as.character(cvg_shapefile@data$Name)

cvg.points <- fortify(gBuffer(cvg_shapefile, byid = TRUE, width = 0), region = "Name")
cvg.df <- inner_join(cvg.points, cvg_shapefile@data, by = "id")

cvg.cent <- cvg.df %>%
  group_by(id) %>%
  summarize(long = median(long), lat = median(lat))


neighborhoods <- ggplot(cvg.df, aes(long, lat)) + 
  geom_polygon(aes(fill = id, alpha = 0.1)) +
#  geom_path(color = "white") +
#  coord_equal() +
  theme(legend.position = "none")
#  geom_text_repel(aes(label = id), data = cvg.cent, size = 2)
neighborhoods

neighborhoods +
    geom_point(data = street_cl, color = "lightgrey", size = 0.1, alpha = 0.2) +
    geom_point(data = ped_srvy, color = "dodgerblue3", size = 1.0) +
    geom_point(data = near_miss, color = "red", size = 2.0) +
    geom_point(data = grid_pts, color = "darkgrey", size = 0.1, alpha = 0.3) +
#    geom_point(data = fire_event, color = "darkorchid4", size = 1.0, shape = 23, alpha = 0.8) +
    xlim(-84.72, -84.37) +
    ylim(39.05, 39.25)



