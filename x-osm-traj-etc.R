library(osn)

# from home skip host, etc
session <- osn_connect("espin", host = "localhost", port = 6666)
sv <- state_vector(
  session,
  icao24 = NULL,
  wef = "2019-04-22 09:00:00",
  til = "2019-04-22 10:00:00",
  bbox = c(xmin = 7.536746, xmax = 9.604390, ymin = 49.36732, ymax = 50.69920)
)


library(ggplot2)

sv_gnd <- sv %>% filter(onground == TRUE)

sv_gnd %>% filter(callsign != is.na(callsign)) %>% 
  sample_n(30) %>%
  ggplot(mapping = aes(x = lon, y = lat, colour = callsign)) + geom_point()

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

is_stationary <- function(trj){
  trj <- trj %>% 
    mutate( lat_d  = lat - lag(lat), lon_d  = lon - lag(lon)
           ,lat_dd = lat_d > 1     , lon_dd = lon_d > 1)
  }




eddf_ad <- osm_apt(.bb_lonlat = c(8.479385,49.989753,8.616028,50.055596))
eddf_ad

dlh511 <- sv %>% filter(callsign == "DLH511")
ane14tj<- sv %>% filter(callsign == "ANE14TJ")


vis <- eddf_ad + 
  geom_point( data    = sv_gnd %>% sample_n(20)
             # data     = ane14tj
             ,mapping = aes(x = lon, y = lat, colour = callsign)
             )
vis

# remotes::install_github('thomasp85/gganimate')
library(gganimate)

vis + transition_manual(time) 
  # ggtitle('Now showing', # {closest_state}',
# subtitle = 'Frame {frame} of {nframes}')