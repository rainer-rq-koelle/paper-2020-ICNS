# ----------------------- OSM HACK----------------------------------------------
library(tidyverse)
library(osmdata)
library(sf)
library(ggspatial)

osm_apt <- function(.bb_lonlat, .title = NULL, .add_north = TRUE){
  
  q <- opq(bbox = .bb_lonlat) %>% 
    add_osm_feature(
      key = "aeroway"
      ,value =c("aerodrome", "apron", "control_tower", "gate", "hangar"
                ,"helipad", "runway", "taxiway", "terminal") ) %>% 
    osmdata_sf() %>%
    unique_osmdata()
  
  gg<-ggplot() +
    geom_sf(data = q$osm_polygons
            ,inherit.aes = FALSE
            ,color = "lightblue"
            #,fill  = "lightblue"
    ) +
    geom_sf(data = q$osm_lines %>% filter(aeroway != "runway")
            , color = "grey"
    ) +
    geom_sf(data = q$osm_lines %>% filter(aeroway == "runway"),
            inherit.aes = FALSE,
            color = "black",
            size = 2 #.4
            ,alpha = .8) +
    theme_void()
  
  if(!is.null(.title)){
    gg <- gg + labs(title = .title)
  }
  # add north arrow with ggspatial
  if(!is.null(.add_north)){
    gg <- gg +
      ggspatial::annotation_north_arrow(height = unit(0.7, "cm"), width = unit(0.7, "cm"))
  }
  # return aerodrome layout map
  return(gg)  
}

save_osm_apt <- function(.gg, .apt_icao, .format = ".png", .dir="./data-ad-charts/"){
  #filename
  fn <- paste(apt_icao, ".png", sep = "")
  # save plot
  ggsave(filename = fn, path = .dir)
}

db_apt_chart <- function(.apt_icao, .bb_lonlat, .add_north, show = TRUE, ...){
  apt <- osm_apt(.bb_lonlat, .add_north)
  save_osm_apt(apt, .apt_icao)
  if(show){print(apt)}
}
# -------------------------------------------------------------------------

