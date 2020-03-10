####### Initial stab at extracting processing functions into a R script #######
####### functions taken from paper-2020-ICNS-eidw.Rmd #########################
library(tidyverse)
library(lubridate)
library(tidyr)
library(purrr)

######## Loading OSN state vector data #######################################
# column specification for readr
cols_osn_sv <- cols(
  time = col_double(),
  icao24 = col_character(),
  lat = col_double(),
  lon = col_double(),
  velocity = col_double(),
  heading = col_double(),
  vertrate = col_double(),
  callsign = col_character(),
  onground = col_logical(),
  alert = col_logical(),
  spi = col_logical(),
  squawk = col_character(),
  baroaltitude = col_double(),
  geoaltitude = col_double(),
  lastposupdate = col_double(),
  lastcontact = col_double(),
  hour = col_double()
)

# extract filenames from data source folder
list_data <- function(.apt_icao, .date, .path="./data-src/"){
  files <- list.files(path = .path, pattern = paste0("adsb_", .apt_icao, "_", .date))
  #files <- files[grepl(pattern = .date, files)]
  return(files)
}

# load data files listed in files
load_data <- function(.files, .path="./data-src/", .cols=cols_osn_sv){
  df <- .files %>%
    purrr::map(   function(x){  readr::read_csv(paste0(.path, x), col_types = .cols)  }   )
  return(df)
}

# convert timestamps
coerce_data <- function(.df){
  df <- .df %>%  purrr::map_df(dplyr::bind_rows)
  df <- df %>% 
    mutate( time = lubridate::as_datetime(time)
            ,hour = lubridate::as_datetime(hour))
}

# wrapper for loading functions
load_osn_adsb <- function(.apt_icao, .date, .path="./data-src/", .cols=cols_osn_sv){
  files <- list_data(.apt_icao, .date, .path)
  df    <- load_data(files, .path, .cols) %>% coerce_data
}



##################### TRAJECTORY SELECTION ##################################
# flights within airport vicinity do land or take-off
# overflights have no low altitude


# pick daily flights by comparing with APDF

