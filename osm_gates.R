# bounding box for Frankfurt
bbox_eddf_apt <- c(xmin=8.483780 ,ymin=49.991437 ,xmax=8.607684 ,ymax=50.054539)

# retrieve data from osm  
osm_apt <- function(.bb_lonlat){

    q <- opq(bbox = .bb_lonlat) %>% 
           add_osm_feature(
             key = "aeroway"
             ,value =c("aerodrome", "apron", "control_tower", "gate", "hangar"
                          +                 ,"helipad", "runway", "taxiway", "terminal") ) %>% 
           osmdata_sf() # %>%
      #     unique_osmdata()
  
    
    
    q <- opq(bbox = bbox_eddf_apt) %>% 
      add_osm_feature(
        key = "aeroway"
        ,value =c( "aerodrome", "apron", "control_tower", "gate", "hangar"
                  ,"helipad", "runway", "taxiway", "terminal") 
        ) 
