#' Get distance to water
#' 
#' \code{get_distance2water} gets distance to water from SpatialPointsDataFrame
#' water_layer
#' 
#' @param df a dataframe with at least coordinates  \code{df$x}, \code{df$y} and
#'   date and time in \code{df$dateTime} in POSIXct class.
#' @param water_layer a SpatialPointsDataFrame of waterhole with column 
#'   \code{$name} in data slot.
#' @examples
#' get_distance2water(df,water_layer,projstring)
#' @export


get_distance2water <- function(df,water_layer,projstring){
  filllist <- which(!is.na(df$x))
  dist2water <- rep(NA,nrow(df))
  waterhole <- rep(NA,nrow(df))
  tmp <- dplyr::filter(df,!is.na(x))
  
  sp::coordinates(tmp) <- c("x","y")
  sp::proj4string(tmp) <- projstring
  
  if(proj4string(water_layer) != projstring){
    cat("proj4string differs")
    return(NULL)
  }
  
  for(i in 1:nrow(tmp)){
    DIST <- rgeos::gDistance(tmp[i,],water_layer,byid=T)
    dist2water[filllist[i]]<- min(DIST)
    waterhole[filllist[i]]<- as.character(water_layer@data$name[which.min(DIST)])
  }
  return(list("dist2water"=dist2water,"waterhole"=waterhole))
}
