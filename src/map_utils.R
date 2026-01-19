extractBoundsCoords <- function(map, map_str, map_bounds) {
  north_lat <- map$north
  south_lat <- map$south
  leafletProxy(map_str) %>%
    clearMarkers() %>%
    clearShapes()

  df <- data.frame(c(map_bounds$west, map_bounds$east), c(map_bounds$north, map_bounds$south))
  colnames(df) <- c("X", "Y")

  data_sf_orig <- st_as_sf(
    df,
    coords = c("X", "Y"),
    crs = 4326
  )

  data_sf_3035 <- st_transform(data_sf_orig, 3035)

  coords_3035 <- st_coordinates(data_sf_3035)
  x_min <- min(coords_3035[, 1])
  x_max <- max(coords_3035[, 1])
  y_min <- min(coords_3035[, 2])
  y_max <- max(coords_3035[, 2])
  return(c(x_min, x_max, y_min, y_max))
}