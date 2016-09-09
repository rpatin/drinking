#' Get drinking events from movement data
#' 
#' \code{get_drinking_event} assumes that when animals cross a certain
#' \code{threshold} of distance to water they go drinking. It identifies unique
#' drinking events given a certain \code{return} time above which it is
#' considered as a new visit. Distance to water might be provided as a
#' SpatialPointsDataFrame or as two columns (named \code{dist2water} & \code{waterholes}) in the
#' dataframe \code{df}.
#' 
#' @param df a dataframe with at least coordinates  \code{df$x}, \code{df$y} and
#'   date and time in \code{df$dateTime} in POSIXct class.
#' @param water_layer a SpatialPointsDataFrame of waterhole with column 
#'   \code{$name} in data slot. If NULL, function assumes that column
#'   \code{df$dist2water} gives distance to water
#' @param threshold distance below which animals are considering going to drink
#' @param returnTime return time (in hours) above which an animals can be considering
#'   returning drinking to the former waterhole
#' @param projstring CRS projection of data
#' @param id if TRUE calculate distance for individuals identified by column \code{df$id}
#' @examples
#' get_drinking_event(df,water_layer=NULL,threshold=500,return=6,origin_julday,projstring)
#' @export
# 
# library(dplyr)
# library(rgdal)
# library(sp)
# TZ <- "Africa/Harare"
# UTMstring <- "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# projstring <- UTMstring
# water <- rgdal::readOGR(dsn="../../These/Hwange/Data - Waterholes/data/Formatted/pump.shp",layer="pump")
# zebra <- read.csv("../../These/Hwange/Zebra - Data/data/Formatted/zeball_2016_08_31.csv.gz")
# water_layer <- pump
# ID <- first(zebra$id)
# df <- dplyr::filter(zebra,id==ID)
# df$dateTime <- as.POSIXct(strptime(df$expectTime, "%Y-%m-%d %H:%M:%S"),tz=TZ)
# threshold=500
# returnTime=6

# test <- get_drinking_event(df,water_layer,projstring=projstring,id=F)

get_drinking_event <- function(df,water_layer=NULL,threshold=500,returnTime=6,projstring,id=F){
  
  if(is.null(water_layer)){
    if (is.null(df$dist2water)) {
      cat("No columns dist2water found")
      return(NULL)
    }
  } else {
    waterlist <- get_distance2water(df,water_layer,projstring)
    df$dist2water <- waterlist[["dist2water"]]
    df$waterhole <- waterlist[["waterhole"]]
  }
  if(!id){
    df$id <- "ArbitraryID"
  }
  # test
  zebraevent <- data.frame("id"=as.character(NA),"dateTime"=as.POSIXct(NA),"waterhole"=as.character(NA))
  zebraevent <- dplyr::mutate(zebraevent,id=as.character(id),waterhole=as.character(waterhole),dateTime=as.POSIXct(dateTime))
  
  j<-0
  for(indiv in unique(df$id))
  {
    burst <- filter(df,id==indiv)
    # print(first(burst$burst))
    WH <- NA
    for(i in which(!is.na(burst$waterhole)))
    {
      if(is.na(WH))
      {
        if(burst$dist2water[i] < threshold)
        {
          j<-j+1
          WH <- burst$waterhole[i]
          zebraevent[j,"id"]<- as.character(indiv)
          zebraevent[j,"waterhole"]<- as.character(burst$waterhole[i])
          zebraevent[j,"dateTime"]<- burst$dateTime[i]
        }
      }else{
        if(burst$dist2water[i] < threshold)
        {
          if( (WH == burst$waterhole[i]) & (as.numeric(difftime(burst$dateTime[i],zebraevent[j,"dateTime"],units='hour')) >= returnTime)){
            j<-j+1
            WH <- burst$waterhole[i]
            zebraevent[j,"id"]<- as.character(indiv)
            zebraevent[j,"waterhole"]<- as.character(burst$waterhole[i])
            zebraevent[j,"dateTime"]<- burst$dateTime[i]
          } else if(WH != burst$waterhole[i]){
            j<- j+1
            WH <- burst$waterhole[i]
            zebraevent[j,"id"]<- as.character(indiv)
            zebraevent[j,"waterhole"]<- as.character(burst$waterhole[i])
            zebraevent[j,"dateTime"]<- burst$dateTime[i]
          }
        }
      }
    }
  }
  return(list("data"=df,"events"=zebraevent))
}