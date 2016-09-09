#' Euclidean Distance
#' 
#' CalcDist Calculate euclidean distance
#' 
#' @examples
#' CalcDist(x1,y1,x2,y2)
#' @export
# 
CalcDist <- Vectorize(function(x1,y1,x2,y2)
{
  return(sqrt((x1-x2)^2 + (y1-y2)^2))
})

#' Merge extents
#' 
#' Merge two different extents
#' 
#' @examples
#' MergeExtent(ext1,ext2)
#' @export
# 
MergeExtent <- function(ext1,ext2){
  xmin <- min(ext1@xmin,ext2@xmin)
  xmax <- max(ext1@xmax,ext2@xmax)
  ymin <- min(ext1@ymin,ext2@ymin)
  ymax <- max(ext1@ymax,ext2@ymax)
  ext1@xmin <- xmin
  ext1@xmax <- xmax
  ext1@ymin <- ymin
  ext1@ymax <- ymax
  return(ext1)
}
