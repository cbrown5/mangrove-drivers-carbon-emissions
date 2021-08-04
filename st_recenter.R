# helper function via https://gist.github.com/mdsumner/2be160dcd66f35893e4e7c926f264c22

st_recenter <- st_recentre <- function(x, clon = NULL, ..., tryfix = TRUE) {
  if (is.null(clon)) return(x)
  if (!st_is_longlat(x)) stop("recentring not appropriate for non longlat data")
  ## save the crs while we do our munging
  crs <- st_crs(x)
  x <- st_set_crs(x, NA)
  
  
  ## try to fix problematic geometry
  if (tryfix) {
    if (all(grepl("POLYGON", st_geometry_type(x)))) x <- suppressWarnings(st_buffer(sf::st_as_sf(x), 0))
    x <- st_wrap_dateline(x, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  }
  wbox <- st_bbox(c(xmin = -180, ymin = -90, xmax = (clon)%%360 - 180, ymax = 90))
  west <- suppressWarnings(st_crop(x, wbox))
  west <- st_set_geometry(west, st_geometry(west) + c(360, 0))
  east <- suppressWarnings(st_crop(x, st_bbox(c(xmin = (clon)%%360 - 180, xmax = 180, ymin = -90, ymax = 90))))
  xx <- rbind(
    west, east
  ) 
  ## ensure geometries are of consistent type
  xx <- sf::st_cast(xx)
  bb <- st_bbox(xx)
  ## hmmm
  # if (bb["xmax"] > 180 && !grepl("\\+over", crs) && !grepl("init", crs)) {
  #   crs <- sprintf("%s +over", crs)
  # }
  st_set_crs(xx, crs)
}
