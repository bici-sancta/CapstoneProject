
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : geo_gridder.R
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

printf <- function(...) invisible(cat(sprintf(...)))

home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")

setwd(home_dir)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   function to generate intermediate point (lat, lon) between 2 given
# ...   geo-coordinates and a fractional distance between them
# ...   source : # ... https://www.movable-type.co.uk/scripts/latlong.html
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

geo_intermediate_points <- function(lat1, lon1, lat2, lon2, f)
{   

    
#    Haversine formula:
#    a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
#    c = 2 ⋅ atan2( √a, √(1−a) )
#    d = R ⋅ c
#    where 	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km);
#    note that angles need to be in radians to pass to trig functions!   
#
#    use lat/lon for latitude/longitude in degrees,
#           φ/λ for latitude/longitude in radians
#    mixing degrees & radians is often the easiest route to head-scratching bugs...
    
# ... Intermediate point
# ... An intermediate point at any fraction along the great circle path between two points
# ...   can also be calculated.1
# ...Formula: 
#    a = sin((1−f)⋅δ) / sin δ
#    b = sin(f⋅δ) / sin δ
#    x = a ⋅ cos φ1 ⋅ cos λ1 + b ⋅ cos φ2 ⋅ cos λ2
#    y = a ⋅ cos φ1 ⋅ sin λ1 + b ⋅ cos φ2 ⋅ sin λ2
#    z = a ⋅ sin φ1 + b ⋅ sin φ2
#    φi = atan2(z, √x² + y²)
#    λi = atan2(y, x)
#
# ... where 
# ...   f is fraction along great circle route (f=0 is point 1, f=1 is point 2),
# ...   δ is the angular distance d/R between the two points.
#
    
    R <- 6371 # (km)
    
# ...   convert lat/lon in degrees to radians

    phi1 <- deg_2_rad(lat1)
    lmb1 <- deg_2_rad(lon1)
    phi2 <- deg_2_rad(lat2)
    lmb2 <- deg_2_rad(lon2)
    
# ...   haversine distance ...
    
    del_phi <- phi2 - phi1
    del_lmb <- lmb2 - lmb1
    a <- sin(del_phi/2) * sin(del_phi/2) + cos(phi1) * cos(phi2) * sin(del_lmb/2) * sin(del_lmb/2)
    c <- 2 *  atan2(sqrt(a), sqrt(1-a))
    d <- R * c
    
# ...   intermediate point
    
    del <- d / R
    a <- sin((1-f)*del) / sin(del)
    b <- sin(f * del) / sin(del)
    x <- a * cos(phi1) * cos(lmb1) + b * cos(phi2) * cos(lmb2)
    y <- a * cos(phi1) * sin(lmb1) + b * cos(phi2) * sin(lmb2)
    z <- a * sin(phi1) + b * sin(phi2)
    phi_i <- atan2(z, sqrt(x*x + y*y))
    lmb_i <- atan2(y,x)
    
    lat_i <- rad_2_deg(phi_i)
    lon_i <- rad_2_deg(lmb_i)
    
    p <- list(lat_i, lon_i)
    
    return(p)
}

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   utility functions to convert between degrees and radians
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

deg_2_rad <- function(x)
{   
    return(x * pi / 180)
}

rad_2_deg <- function(theta)
{   
    return(theta * 180 / pi)
}

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   generate the points
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   approximate bounding points for Cincinnati
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

lat_s <- 39.05
lat_n <- 39.221
lon_e <- -84.3687
lon_w <- -84.7121

# ...   gridding distance (meters)

grid_dist <- 0.25

# ...   number of points along latitude and longitude lines

d_lat <- lat_n - lat_s
dy <- d_lat * 111
n_y_pts <- as.integer(dy / grid_dist + 0.5)

d_lon <- lon_e - lon_w
dx <- d_lon * 111
n_x_pts <- as.integer(dx / grid_dist + 0.5)

# ...   data frame for result grid

grid_pts <- data.frame(ix = integer(),
                    iy = integer(),
                    lat = double(),
                    long = double())

lon1 <- lon_e
lon2 <- lon_w

# ...   create grid

for (iy in seq(1 : (n_y_pts+1)))
{
    f <- (iy-1) / n_y_pts
    lat1 <- geo_intermediate_points(lat_s, lon_e, lat_n, lon_e, f)[[1]]
    lat2 <- lat1

    for (ix in seq(1 : (n_x_pts+1)))
    {
        f <- (ix-1) / n_x_pts
        p3 <- geo_intermediate_points(lat1, lon1, lat2, lon2, f)
        
        lat3 <- p3[[1]]
        lon3 <- p3[[2]]
        
        grid_pts <- rbind(grid_pts, list("ix"= ix, "iy" = iy,
                                         "lat"  = lat3, "long" = lon3))
    }
}

# ...   give each cell a centroid and an identity

grid_center <- data.frame(ix = integer(),
                    iy = integer(),
                    cell_id = character(),
                    lat = double(),
                    long = double())

for (iy in seq(1 : n_y_pts))
{
    lat <- grid_pts[grid_pts$ix == ix && grid_pts$iy == iy, 3]
    
    for (ix in seq(1 : n_x_pts))
    {
        lat1 <- grid_pts[grid_pts$iy == iy, 3][ix]
        lat2 <- grid_pts[grid_pts$iy == iy+1, 3][ix+1]
        lon1 <- grid_pts[grid_pts$ix == ix, 4][iy]
        lon2 <- grid_pts[grid_pts$ix == ix+1, 4][iy+1]
        f <- 0.5
        
        centroid <- geo_intermediate_points(lat1, lon1, lat2, lon2, f)
        
        lat3 <- centroid[[1]]
        lon3 <- centroid[[2]]
        
        id_string <- paste0(sprintf("%04d", ix), sprintf("%04d", iy))
        
        nxt <- as.data.frame(list("ix"= ix, "iy" = iy, "cell_id" = id_string, "lat"  = lat3, "long" = lon3))
        
        grid_center <- rbind(grid_center, nxt)
    }
}

# ...   make a plot to view the result

grid_plot <- ggplot(grid_pts, aes(x = long, y = lat)) +
    geom_point(size = 0.1, color = "dodgerblue4", alpha = 0.8)

grid_plot +
    geom_point(data = grid_center, color = "salmon", size = 0.2, shape = 19, alpha = 0.6)

# ...   write grid data frame to csv file

setwd(data_dir)

write.table(grid_pts, file = "grid_points_250m.csv", sep = ",",
            row.names = FALSE,
            col.names = TRUE)

write.table(grid_center, file = "grid_centroids_250m.csv", sep = ",",
            row.names = FALSE,
            col.names = TRUE)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

