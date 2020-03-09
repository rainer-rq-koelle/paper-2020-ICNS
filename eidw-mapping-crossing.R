######### MERGE WITH APDF ######################

cols_apdf <- cols_only(
   AP_C_FLTID = col_character()
  ,AP_C_REG   = col_character()
  ,ADEP_ICAO  = col_character()
  ,ADES_ICAO  = col_character()
  ,ARCTYP     = col_character()
  ,AC_CLASS   = col_character()
  ,SRC_PHASE  = col_character()
  ,AP_C_STND  = col_character()
  ,AP_C_RWY   = col_character()
  ,MVT_TIME_UTC  = col_datetime(format = "%d%.%m%.%Y %T")
  ,BLOCK_TIME_UTC= col_datetime(format = "%d%.%m%.%Y %T")
  ,SCHED_TIME_UTC= col_datetime(format = "%d%.%m%.%Y %T")
  ,C40_CROSS_TIME= col_datetime(format = "%d%.%m%.%Y %T")
  ,C40_CROSS_LAT = col_double(), C40_CROSS_LON = col_double(),C40_CROSS_FL = col_double()
)

#apdf_src <- read_csv2("../__DATA/EIDW/EIDW_2019_FACT.csv", col_types = cols_apdf)
apdf_src <- read_csv2("../__DATA/EDDF/EDDF_2019_FACT.csv", col_types = cols_apdf)
apdf     <- apdf_src %>% 
  filter( BLOCK_TIME_UTC >= as.POSIXct("2019-04-22 00:00:00", tz="UTC")
         ,BLOCK_TIME_UTC <= as.POSIXct("2019-05-20 00:00:00", tz="UTC")
         )
apdf_stats<- apdf %>% mutate(DOF = as.Date(BLOCK_TIME_UTC)) %>%
  group_by(DOF, SRC_PHASE) %>% summarise(N=n())
# write_csv(apdf_stats, "./data/EIDW-study-dof.csv")
# write_csv(apdf_stats, "./data/EDDF-study-dof.csv")

acdb <- read_csv("../__DATA/aircraft/aircraft_db.csv") %>%
  mutate( REG = gsub(pattern="-", replacement="", regid)
          ,REG = toupper(REG)
          ,mdl = toupper(mdl))

osn_acdb_cols = cols(
  .default = col_character(),
  registered = col_date(format = ""),
  reguntil = col_date(format = ""),
  status = col_logical(),
  built = col_date(format = ""),
  firstflightdate = col_date(format = ""),
  seatconfiguration = col_logical(),
  modes = col_logical(),
  adsb = col_logical(),
  acars = col_logical(),
  notes = col_character()
)

acdb2<- read_csv("../__DATA/aircraft/aircraftDatabase.csv", col_types = osn_acdb_cols)
acdb2<- acdb2[2:nrow(acdb2),]
acdb2<- acdb2 %>% 
  mutate(REG = gsub(pattern="-", replacement="", registration)
         ) %>%
  select(icao24, REG, model, typecode) #, operator, operatoricao, operatoriata, operatorcallsign)


################## LINK ADSB with APDF

flts <- apdf %>% mutate(DOF = as.Date(BLOCK_TIME_UTC)) %>%
  select(DOF, SRC_PHASE, AP_C_FLTID, REG = AP_C_REG, ARCTYP, AC_CLASS, AP_C_RWY, AP_C_STND)

flts <- flts %>% left_join(acdb2, by="REG")
flts_ok <- flts %>% filter(!is.na(icao24))
flts_nok<- flts %>% filter( is.na(icao24))

no_icao <- flts_nok %>%
  select(REG, ARCTYP) %>% unique() %>%
  left_join(acdb, by="REG")

fix_icao<- no_icao %>% filter(!is.na(icao)) %>%
  rename(icao24 = icao) %>%
  select(REG, ARCTYP, icao24)   

flts_nok  <- flts_nok %>% left_join(fix_icao, by=c("REG","ARCTYP")) %>%
  mutate(icao24.x = ifelse(is.na(icao24.x), icao24.y, icao24.x))
flts_nok$icao24.y <- NULL
flts_nok  <- flts_nok %>% rename(icao24 = icao24.x)

flts_ok2  <- flts_nok %>% filter(!is.na(icao24))
flts_nok2 <- flts_nok %>% filter( is.na(icao24))

flts_ok   <- flts_ok %>% bind_rows(flts_ok2)
flts      <- flts_ok %>% bind_rows(flts_nok2)

#write_csv(flts, "./data/EIDW-study-flts.csv")                                   
#write_csv(flts, "./data/EDDF-study-flts.csv")                                   


