#### CMex_AG_Map.R
#### Rudolf Cesaretti, 6/6/2023

#### "CMex_AG_Map" 
####  
####  
#### 
#### 

pak <- c("tidyverse", "rgdal", "rgeos", "GISTools", "sf", "stars", "rlang", 
         "ggplot2", "ggsn", "ggnewscale")
# Install packages not yet installed
ip <- pak %in% rownames(installed.packages())
if (any(ip == FALSE)) {
  install.packages(pak[!ip])
}
# load packages
invisible(lapply(pak, library, character.only = TRUE))
rm(pak,ip)

#stars_hillshade = Hillshade
#sf1 = MunicipioYields_Z
#sf1_geom = "geom"
#sf1_aes_string = T
#sf1_fill = "Y_1991_Total"#var[5]
#sf1_alpha = 0.55
#sf1_fill_palette = myPalette
#sf1_fill_palette_n = 11
#sf1_fill_legend_title = myLegendTitle
#sf1_fill_legend_limits = myLimits
#sf1_fill_legend_breaks = myBreaks
#sf2 = Estados
#sf2_geom = geom
#sf2_fill = "transparent"
#sf2_color = "white"
#sf2_linewidth = 1.5
#sf2_label = TRUE
#sf2_label_column = "NOM_ENT"
#sf2_label_size = 9
#sf2_label_color = "white"
#sf2_label_face = "bold"
#sf2_label_nudge_x = Estados2$my_nudge_x
#sf2_label_nudge_y = Estados2$my_nudge_y
#mytitle = paste0(tit[5])
#mysubtitle = paste0(subtit[5])
#north_arrow = TRUE
#north_symbol = 12
#north_scale = 0.15
#north_location = "topleft"
#scale_bar = TRUE
#scale_dist_km = 50
#scale_box_lims = c(357134,464000,2042755,2053000)


###############################################################
########################  CMex_AG_Map  ########################
###############################################################
                        
CMex_AG_Map <- function(stars_hillshade,
                        sf1,
                        sf1_geom,
                        sf1_aes_string = FALSE,
                        sf1_fill,
                        sf1_alpha = 0.55,
                        sf1_fill_palette,
                        sf1_fill_palette_n,
                        sf1_fill_legend_title,
                        sf1_fill_legend_limits,
                        sf1_fill_legend_breaks,
                        sf2,
                        sf2_geom,
                        sf2_fill = "transparent",
                        sf2_color = "white",
                        sf2_linewidth = 1.5,
                        sf2_label = TRUE,
                        sf2_label_column = NULL,#********
                        sf2_label_size = 9,
                        sf2_label_color = "white",
                        sf2_label_face = "bold",
                        sf2_label_nudge_x = NULL,
                        sf2_label_nudge_y = NULL,
                        mytitle = NULL,
                        mysubtitle = NULL,
                        north_arrow = TRUE,
                        north_symbol = 12,
                        north_scale = 0.15,
                        north_location = "topleft",
                        scale_bar = TRUE,
                        scale_dist_km = 50,
                        scale_box_lims = c(357134,464000,2042755,2053000)
                        ) {
  #if (sf1_aes_string == TRUE){
  #  colnames(sf1) <- gsub(sf1_geom, "sf1_geom", colnames(sf1))
  #  colnames(sf1) <- gsub(sf1_fill, "sf1_fill", colnames(sf1))
  #  st_geometry(sf1) <- "sf1_geom"
  #}
  #quo_name

  Out <- ggplot2::ggplot() + 
    geom_stars(data = stars_hillshade) +
    scale_fill_gradient(low = "black", high = "white", guide="none") +
    ggnewscale::new_scale_fill() +
    {if (sf1_aes_string == FALSE) geom_sf(data = sf1, aes(geometry = {{ sf1_geom }}, fill = {{ sf1_fill }}),   
                                           color = "black", size = 1, alpha = sf1_alpha)} +
    #{if (sf1_aes_string == TRUE) geom_sf(data = sf1, aes_string(geometry = !!sf1_geom, fill = !!sf1_fill),   
    #                                       color = "black", size = 1, alpha = sf1_alpha)} +
    {if (sf1_aes_string == TRUE) geom_sf(data = sf1, aes(geometry = !!sym(sf1_geom), fill = !!sym(sf1_fill)), #  
                                         color = "black", size = 1, alpha = sf1_alpha)} +
    #{if (sf1_aes_string == TRUE) geom_sf(data = sf1, aes(geometry = sf1_geom, fill = sf1_fill),   
    #                                     color = "black", size = 1, alpha = sf1_alpha)} +
    #geom_sf(data = sf1, aes(geometry = {{ sf1_geom }}, fill = {{ sf1_fill }}), 
    #        color = "black", size = 1, alpha = sf1_alpha) +
    scale_fill_gradientn(name = sf1_fill_legend_title, colors = sf1_fill_palette(sf1_fill_palette_n), 
                         limits=sf1_fill_legend_limits, breaks= sf1_fill_legend_breaks) +
    geom_sf(data = sf2, aes(geometry = geom), fill = sf2_fill, color = sf2_color, 
            linewidth = sf2_linewidth, inherit.aes = F) +
    {if (sf2_label == TRUE) geom_sf_text(data = sf2, aes(label = {{ sf2_label_column }}), 
                                         size = 9, color = "white", face="bold", 
                                         nudge_x = sf2_label_nudge_x, 
                                         nudge_y = sf2_label_nudge_y)} +
    #{if (sf2_label == TRUE) geom_sf_text(data = sf2, aes(label = !!sym(sf2_label_column)), 
    #                                     size = 9, color = "white", face="bold", 
    #                                     nudge_x = sf2_label_nudge_x, 
    #                                     nudge_y = sf2_label_nudge_y)} +
    labs(title = mytitle, subtitle = mysubtitle) +
    coord_sf() + 
    theme_void() +
    {if (north_arrow == TRUE) north(data = sf2, symbol = north_symbol, 
                                  scale = north_scale, location = north_location)} +
    
    {if (scale_bar == TRUE) geom_rect(aes(xmin = scale_box_lims[1], xmax = scale_box_lims[2], 
                                          ymin = scale_box_lims[3], ymax = scale_box_lims[4]), 
                                      color = "white", fill = "white")} +
    {if (scale_bar == TRUE) scalebar(data = Estados, dist = scale_dist_km, dist_unit = "km", 
                                     transform = F, model = "WGS84", st.color = "black", 
                                     height = 0.03, st.dist = 0.03, st.size = 5, 
                                     st.bottom = T, location = "bottomleft")} +
    theme(legend.justification=c(0,1), 
          legend.position=c(0.85,0.95), 
          legend.title=element_text(color="black", face="bold", size = 14),
          legend.text = element_text(colour="black", size = 8, face="bold"),
          legend.background = element_rect(fill="white",size=0.5, linetype="solid", color ="black"),
          legend.box.background = element_rect(colour = "black"),
          legend.margin=ggplot2::margin(t= 2, r= 2, b= 5, l= 2),
          plot.title = element_text(hjust = 0.5, face="bold", size=16),
          plot.subtitle = element_text(hjust = 0.5, face="bold", size=12),
          plot.background = element_rect(fill = "white", colour = NA))
  
  return(Out)
}

