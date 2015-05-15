#helper functions for ratlantis

#' "Not in" function
#'
#' This function allows you to check if an element is not in a larger group
#' @param x
#' @param table
#' @export
#define not in function
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0

#' Points to Polygon function
#'
#' This function allows you to change points to polygons
#' @param df
#' @param data
#' @export
#define not in function
points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
    Polygon(coordinates(df[df$id==ID & df$group==group,]))
  }
  get.spPoly  <- function(ID,df) {
    Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
  }
  spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
  SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
}
