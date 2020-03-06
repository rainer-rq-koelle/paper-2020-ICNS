library("ROracle")
library("dplyr")

Airport="EDDF"
Radius=50
R=6371/1.852
r=Radius/R
ang=seq(0,2*pi,pi/2)
drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
APT_data <- dbGetQuery(con, "SELECT * FROM SP_AIRPORT_INFO")
dbDisconnect(con)

ARPX=as.numeric(select(filter(APT_data, ICAO_CODE==Airport), ARP_LON))
ARPY=as.numeric(select(filter(APT_data, ICAO_CODE==Airport), ARP_LAT))
lat=asin(sin(ARPY/180*pi)*cos(r)+cos(ARPY/180*pi)*sin(r)*cos(ang));
lon=atan2(sin(ang)*sin(r)*cos(ARPY/180*pi),cos(r)-sin(ARPY/180*pi)*sin(lat));
lat=lat*180/pi
lon=lon*180/pi
Radius_coord=as.data.frame(cbind(LON=lon+ARPX, LAT=lat))
bbox = c(xmin = min(Radius_coord$LON), xmax = max(Radius_coord$LON), ymin = min(Radius_coord$LAT), ymax = max(Radius_coord$LAT))
