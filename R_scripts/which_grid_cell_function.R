
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : which_grid_cell_function.R
# ...
# ...   find closest grid point (cell centroid) to data value location 
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

library(sp)
library(rgeos)
library(geosphere)
library(dplyr)
library(tictoc)

printf <- function(...) invisible(cat(sprintf(...)))

which_grid_cell <- function(grid_centroid, df_to_map)
{

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   transform to spatial objects
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

sp.df_to_map <- df_to_map
coordinates(sp.df_to_map) <- ~long+lat

sp.grid_centroid <- grid_centroid
coordinates(sp.grid_centroid) <- ~long+lat

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   calculate distance pairs between all point pairs
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    dist_pair <- gDistance(sp.grid_centroid, sp.df_to_map, byid = TRUE)

# ...   mid distance is closest point

    min_dist <- apply(dist_pair, 1, function(x) order(x, decreasing = FALSE)[1])

# ...   define corresponding columns from grid data frame

    dist <- dist_pair[min_dist]
    cell_id <- grid_centroid[min_dist, 3]
    lat_cell <- grid_centroid[min_dist, 4]
    long_cell <- grid_centroid[min_dist, 5]
    hood <- grid_centroid[min_dist, 9]
    hood_id <- grid_centroid[min_dist, 10]

# ...   new data frame ... all prior columns + grid centroid identifiers and coords

    df_mapped_to_grid <- cbind(df_to_map, dist, cell_id, lat_cell, long_cell, hood, hood_id)
    
    return (df_mapped_to_grid)
}

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-