#' Create Voronoi Polygons from a SpatialPoints
#'
#' \code{voronoi} creates voronoi polygons. The functions use the R package
#' deldir and is modified from Carson Farmer :
#' http://carsonfarmer.com/2009/09/voronoi-polygons-with-r/
#' @param layer is a SpatialPoints object
#' @param extent is an extent object from raster packages to set the extent of calculation of polygons
#' @return a SpatialPolygonsDataFrame with the Voronoi polygons
#' @examples
#' voronoipolygons(layer,extent)
#' @export
#

voronoipolygons = function(layer,extent) {
  crds = layer@coords
  z = deldir::deldir(crds[,1], crds[,2],rw=c(extent@xmin,extent@xmax,extent@ymin,extent@ymax))
  w = deldir::tile.list(z)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = sp::Polygons(list(sp::Polygon(pcrds)), ID=as.character(i))
  }
  SP = sp::SpatialPolygons(polys)
  layer@data$x <- NULL
  layer@data$y <- NULL
  voronoi = SpatialPolygonsDataFrame(SP, data=cbind(data.frame(x=crds[,1],
                                                               y=crds[,2],
                                                               row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID')),layer@data)))
  proj4string(voronoi) <- proj4string(layer)
  return(voronoi)
}
