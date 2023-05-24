#### RF_ImputeRast.R
#### Rudolf Cesaretti, 5/23/2023

#### "RF_ImputeRast" uses the x and y coordinates and a number of supplied covariate rasters 
#### to predict missing values in a terra SpatRast raster object. The raster to be imputed and the covariates 
#### (as well as the mask, if supplied) need to have the same extents, resolution and coordinate reference systems. 
#### A mask (in raster form) can also be supplied, which restricts the focal area under consideration for teh training dataset. 
#### The training dataset is first reduced by removing areas beyond the mask, and then further reduced by 
#### randomly selecting a number of cells according to the sampling fraction parameter set by the user (default is 0.3)

#### The output of the function is a list that includes:
#### 1) the raster with the NA values replaced by the predicted values of the fandom forest model
#### 2) the training ranger random forest model R^2 
#### 3) the training ranger random forest model prediction error
#### 4) the number of trees in the ranger random forest model

#### The RF_ImputeRast function uses the Terra_df function, and depends on the terra, tidyverse and ranger packages

pak <- c("rgdal", "raster", "terra", "tidyverse", "ranger")
# Install packages not yet installed
ip <- pak %in% rownames(installed.packages())
if (any(ip == FALSE)) {
  install.packages(pak[!ip])
}
# load packages
invisible(lapply(pak, library, character.only = TRUE))
rm(pak,ip)

###############################################################
#######################  RF_ImputeRast  #######################
###############################################################


RF_ImputeRast <- function(r, # Terra SpatRast raster object
                          rnam = names(r), # name for df variable for raster values from focal SpatRast object
                          covs = NULL, # Terra SpatRast raster object with 1 or more stacked layers 
                          Mask = NULL, # mask of focal area under consideration. Used in reducing the sample for the training dataset; training dataset first excludes areas outside of the mask before applying the sampling fraction
                          SampFrac = 0.3, # sampling fraction for training dataset; applied after mask exclusions, if present (see Mask, above)
                          save_mem = FALSE # whether the save.memory parameter should be used for the ranger random forest function
) {
  
  if (!is.null(Mask)){
    m_df = Terra_df(Mask, "mask")
    names(m_df) <- c("x", "y", "mask", "outside")
    r_df = Terra_df(r, rnam)
    r_df = merge(r_df, m_df, all = F, sort = F)
  } else {
    r_df = Terra_df(r, rnam)
  }
  
  if (!is.null(covs)){
    out_list <- list()  # or result_vector <- c()
    for (i in 1:nlyr(covs)) {
      out_list[[i]] = Terra_df(covs[[i]], names(covs[[i]]))
      out_list[[i]] = out_list[[i]] %>% select(-na_cells)
    }
    covs_df <- Reduce(function(x, y) merge(x, y, all = F, sort = F), out_list)
    r_df = merge(r_df, covs_df, all = F, sort = F)
  }
  
  if (!is.null(Mask)){
    
    train = r_df %>% filter(outside == F) %>% filter(complete.cases(.)) %>% 
      select(-na_cells, -outside, -mask) %>% sample_frac(SampFrac)
    
  } else {
    
    train = r_df %>% filter(complete.cases(.)) %>% select(-na_cells) %>% 
      sample_frac(SampFrac)
  }
  
  f <- names(train)
  f <- f[f != names(r)]
  form <- paste(names(r), "~", paste(f, collapse = " + "))
  form <- as.formula(form)
  
  model <- ranger(
    formula = form,
    data = train,
    save.memory = save_mem 
  )
  
  if (!is.null(Mask)){
    
    pred = r_df %>% select(-na_cells, -outside, -mask) %>% select(-!!sym(rnam)) %>% filter(complete.cases(.))
    
  } else {
    
    pred = r_df %>% select(-na_cells) %>% select(-!!sym(rnam)) %>% filter(complete.cases(.))
    
  }
  
  predicted_vals <- predict(model, pred)
  pred$pred <- predicted_vals$predictions
  
  x = r_df %>% left_join(pred, by = f)
  
  r2 = r
  raster_values <- values(r2)
  missing_values <- is.na(raster_values)
  df_values <- x$pred[missing_values]
  r2[missing_values] <- df_values
  names(r2) <- rnam
  
  out <- list()
  out[[1]] <- r2
  out[[2]] <- model$r.squared
  out[[3]] <- model$prediction.error
  out[[4]] <- model$num.trees
  
  return(out)
}



