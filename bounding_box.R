# boundingbox function
# from http://rstudio-pubs-static.s3.amazonaws.com/19324_dd865f50a2304595b45d86f3022f4681.html
# based on http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates

#' Calculate Bounding Box
#' 
#' Caclulate a bounding box for a center point given a set of coordinates.
#' 
#' @param lat The latitude of the center point.
#' @param lon The longitude of the center point.
#' @param dist The distance from the center point.  
#' @param in.miles logical.  If \code{TRUE} uses miles as the units of 
#' \code{dist}.  If \code{FALSE} uses kilometers.
#' @return Returns a matrix with max/min latitude/longitude values.
#' @references \url{http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates}
#' @keywords bounding_box, coordinates
#' @export
#' @examples
#' bounding_box(38.8977, 77.0366, 1)
bounding_box <- function(lat, lon, dist, unit = "NM") {
  
  ## Helper functions
  if (unit=="mile") {  ang_rad <- function(miles) miles/3958.756 }
  if (unit=="km")   {  ang_rad <- function(miles) miles/1000     }
  if (unit=="NM")   {  ang_rad <- function(miles) miles/3440.0649115}
  
  `%+/-%` <- function(x, margin){x + c(-1, +1)*margin}
  deg2rad <- function(x) x/(180/pi)
  rad2deg <- function(x) x*(180/pi)
  lat_range <- function(latr, r) rad2deg(latr %+/-% r)
  lon_range <- function(lonr, dlon) rad2deg(lonr %+/-% dlon)
  
  r <- ang_rad(dist)
  latr <- deg2rad(lat)
  lonr <- deg2rad(lon)
  dlon <- asin(sin(r)/cos(latr))
  
  m <- matrix(c(lon_range(lonr = lonr, dlon = dlon), 
                lat_range(latr=latr, r=r)), nrow=2, byrow = TRUE)
  
  dimnames(m) <- list(c("lng", "lat"), c("min", "max"))
  m
}