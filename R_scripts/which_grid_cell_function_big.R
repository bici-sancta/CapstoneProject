
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

which_grid_cell_big <- function(grid_centroid, df_to_map)
{
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   transform to spatial objects
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    sp.grid_centroid <- grid_centroid
    coordinates(sp.grid_centroid) <- ~long+lat
    
    df_mapped_to_grid <- data.frame()
    
    n_rows <- dim(df_to_map)[1]
    
    irow <- 1
    subset_size <- 10000
    
    while (irow < n_rows)
    {
        tic()
        indx_max <- min(irow + subset_size - 1, n_rows)
    
        df_subset <- df_to_map[seq(irow, indx_max),]
        
        sp.subset <- df_subset
        
        coordinates(sp.subset) <- ~long+lat

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   calculate distance pairs between all point pairs
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        dist_pair <- gDistance(sp.grid_centroid, sp.subset, byid = TRUE)

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

        df_subset_mapped <- cbind(df_subset, dist, cell_id, lat_cell, long_cell, hood, hood_id)

# ...   add new rows to global collection
        
        df_mapped_to_grid <- rbind(df_mapped_to_grid, df_subset_mapped)
        
        irow <- irow + subset_size
        
        toc(log = TRUE, quiet = TRUE)
        log.txt <- tic.log(format = TRUE)
        tic.clearlog()

        printf("\t...which_grid_cell_big() : row %6d of %7d rows; %s\n", irow-1, n_rows, unlist(log.txt))
    }
    
    return (df_mapped_to_grid)

}

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-