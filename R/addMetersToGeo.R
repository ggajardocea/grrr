#' addMetersToGeo
#'
#' Aplica la función coordToMetersDouble a un dataframe y le pega dos columnas con las correspondientes coor
#' coordenadas cartesianas
#'
#' @param d dataframe que tiene latitud y longitud
#' @param complete Si es que se dejan solo los registros completos
#'
#' @return dataframe con columnas adicionales en coordenadas cartesianas
#' @export
#'
#' @examples
#' d <- data.frame(lon = -70.57299, lat = -33.39064)
#' addMetersToGeo(d)
addMetersToGeo <- function(d, complete = TRUE){
  if(complete){
    d <- d[stats::complete.cases(d),]
  }
  cord_m <- coordToMetersDouble(vector_lat = d$lat, vector_lon = d$lon)
  d <- cbind(d, cord_m)
  returnValue(d)
}
