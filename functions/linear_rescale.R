#### linear_rescale.R
#### Rudolf Cesaretti, 6/1/2023

#### "linear_rescale" 
####  converts terra SpatRast into dataframe with coordinates,
####  raster values, and whether or not the value is NA
#### 
#### 

###############################################################
######################  linear_rescale  #######################
###############################################################

linear_rescale <- function(data, # vector to be rescaled
                           range_min = 0, # rescaled minimum value
                           range_max = 1# rescaled maximum value
) {
 
  out <- (data - min(data)) / (max(data) - min(data)) * (range_max - range_min) + range_min
  return(out)
}



