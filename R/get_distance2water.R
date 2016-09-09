#' Get distance to water
#' 
#' \code{get_distance2water} gets distance to water from SpatialPointsDataFrame
#' water_layer
#' 
#' @param df a dataframe with at least coordinates  \code{df$x}, \code{df$y} and
#'   date and time in \code{df$dateTime} in POSIXct class.
#' @param water_layer a SpatialPointsDataFrame of waterhole with column 
#'   \code{$name} in data slot.
#' @param voronoi polygons of water_layer
#' @return a list with element dist2water and waterhole
#' @examples
#' get_distance2water(df,water_layer,projstring)
#' @importFrom sp %over%
#' @export


get_distance2water <- function(df,water_layer=NULL,voronoi=NULL,projstring){

  filllist <- which(!is.na(df$x))
  dist2water <- rep(NA,nrow(df))
  waterhole <- rep(NA,nrow(df))
  tmp <- dplyr::filter(df,!is.na(x))
  
  sp::coordinates(tmp) <- c("x","y")
  sp::proj4string(tmp) <- projstring
  

  if(sp::proj4string(water_layer) != projstring){
    cat("proj4string differs")
    return(NULL)
  }
  
  if(is.null(voronoi)){
    ext_tmp <- raster::extent(tmp)
    ext_water <- raster::extent(water_layer)
    EXTENT <- MergeExtent(ext_tmp,ext_water)
    voronoi = voronoipolygons(water_layer,EXTENT)
  }
  
  over_voronoi <- tmp %over% voronoi
  dist2water[filllist] <- CalcDist(over_voronoi$x,over_voronoi$y,tmp@coords[,1],tmp@coords[,2])
  waterhole[filllist] <- over_voronoi$name

  return(list("dist2water"=dist2water,"waterhole"=waterhole))
}
