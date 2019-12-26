#' coordToMetersDouble
#'
#' Transforma coordenadas geográficas a coordenadas cartesianas
#' Los valores de está función fueron comprobados en
#' http://www.csgnetwork.com/degreelenllavcalc.html
#'
#' @param vector_lat Latitud
#' @param vector_lon Longitud
#'
#' @return dataframe con las dos columnas transformadas
#' @export
#'
#' @examples
#' d <- data.frame(lon = -70.57299, lat = -33.39064)
#' coordToMetersDouble(vector_lon = d$lon, vector_lat = d$lat)
coordToMetersDouble <- function(vector_lat, vector_lon){
  if(class(vector_lat) != "numeric" | class(vector_lon) != "numeric"){
    vector_lat <- as.numeric(stringr::str_replace_all(stringr::str_replace_all(vector_lat, ",", "."), " ", ""))
    vector_lon <- as.numeric(stringr::str_replace_all(stringr::str_replace_all(vector_lon, ",", "."), " ", ""))
  }
  lat_r <- vector_lat * (pi/180)
  c_lon <- 111412.84 * cos(lat_r) - 93.4 * cos(3 * lat_r) + 0.118 * cos(5 * lat_r)
  c_lat <- 111132.954 - 559.822 * cos(2 * lat_r) + 1.175 * cos(4 * lat_r) - 0.0023 * cos(6 * lat_r)
  vector_lat_m <- vector_lat * c_lat
  vector_lon_m <- vector_lon * c_lon
  vector <- data.frame("lat_m" = vector_lat_m, "lon_m" = vector_lon_m)
  returnValue(vector)
}
