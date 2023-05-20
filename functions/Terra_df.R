#### Terra_df.R
#### Rudolf Cesaretti, 5/19/2023

#### "Terra_df" 
####  converts terra SpatRast into dataframe with coordinates,
####  raster values, and whether or not the value is NA
#### 
#### 

pak <- c("rgdal", "raster", "terra")
# Install packages not yet installed
ip <- pak %in% rownames(installed.packages())
if (any(ip == FALSE)) {
  install.packages(pak[!ip])
}
# load packages
invisible(lapply(pak, library, character.only = TRUE))
rm(pak,ip)

###############################################################
#########################  Terra_df  ##########################
###############################################################

Terra_df <- function(r, # Terra SpatRast raster object
                     colname # character vector name of column name for raster values
                     ) {
  df = crds(r, df=T, na.rm=F)
  df$r = as.numeric(values(r))
  df$r[is.nan(df$r)] <- NA
  df$na_cells <- is.na(df$r)
  colnames(df) <- c("x", "y", colname, "na_cells")
  return(df)
}


