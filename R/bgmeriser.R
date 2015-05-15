#' bgmerizer function
#'
#' This function creates bgm file format needed for Atlantis
#' @param map_location location of gis layer stored in wgs84 format
#' @param map_name name of map to be used for bgm creation. Must incude "box_id"
#' column. Other columns may be included (horizmix, vertmix,boundary_code)
#' @param boundary_boxes a list of boxes that boundary boxes in the model
#' @param get_bathymetry Should function automatically add depths (T(rue) or
#' F(alse)
#' @param bathymetry_layer_location location of gis layer stored in wgs84 format.
#' @param bathymetry_layer_name name of bathmetry shp layer. Must be in wgs84
#' format
#' @param bathymetry_cutoff What quantile should be assigned to polygon. Defaults
#' to 90th quantile (to avoid small number of canyons or slope biasing entire
#' polygon)
#' @param bathymetry_levels Depth layers for final Atlantis polygons
#' @param bgmerizer_location location of bgmerizer.jar file (location also must
#' include java.exe file and may not have spaces (e.g. not C:/Desktop/John Doe/bgm))
#' @keywords bgm
#' @details This function creates bgm file format needed for Atlantis by calling
#' java program. It also adds in depth data (if desired) using provided
#' bathymetry layer.  This functions as a wrapper for bgmerizer.jar, available
#' from CSIRO, and adds in required information.
#' @export

rbgmerizer <- function( map_location, map_name, boundary_boxes = NULL,
                        get_bathymetry = TRUE,
                        bathymetry_layer_location, bathymetry_layer_name,
                        bathymetry_cutoff = .9, bathymetry_levels = c(-10, -20,
                                                                      -50, -200,
                                                                      -1000, -2000,
                                                                      -4000),
                        bgmerizer_location){
  #read in the map and check for wgs84 format
  map_for_bgm <- rgdal::readOGR(map_location, layer=map_name)
  if (map_for_bgm@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
      stop(message = "Map must be in wgs84 format")
      }

  if ("box_id" %!in% colnames(map_for_bgm@data)){
      stop(message = "Map data must incluce box_id column")
  }

  #get depth stats for each polygon by overlaying bathymetry layer on map layer

  if (get_bathymetry == T){
    bathymetry <- rgdal::readOGR(bathymetry_layer_location, layer = bathymetry_layer_name)
    if (bathymetry@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
      stop(message = "Bathymetry layer map must be in wgs84 format")
    }
    if (bathymetry_cutoff >= 1 |  bathymetry_cutoff <= 0){
      stop(message = "Bathymetry cutoff must be between 0 and 1")
    }

    if ("Contour" %!in% colnames(bathymetry@data)){
      stop(message = "Bathymetry data must incluce Contour column")
    }
    if (max(bathymetry_levels) > 0){
      stop(message = "Bathymetry levels must all be negative")
    }

    #since these are depths (negative numbers), we don't count the lowest
    #measurements as set by bathymety_cutoff
    bathymetry_cutoff_quant <- function(x){quantile(x, c(1 - bathymetry_cutoff))}
    depth <- sp::over(map_for_bgm, bathymetry, fn=bathymetry_cutoff_quant)
    map_for_bgm@data$Depth <- depth$Contour

    #cut to noted levels
    for (i in 1:nrow(map_for_bgm@data)){
         x <- map_for_bgm@data$Depth[i] - bathymetry_levels
         x <- which(x>0)
         map_for_bgm@data$Depth[i] <- max(bathymetry_levels[x])
    }


   }

  map_for_bgm@data$botz <- map_for_bgm@data$Depth

  #check to make sure all boxes are sequentially labeled
  map_for_bgm@data == map_for_bgm@data[order(map_for_bgm@data$box_id),]
  for ( i in 1:nrow(map_for_bgm@data)){
  if (map_for_bgm@data$box_id[i] != seq (0, nrow(map_for_bgm@data)-1)[i]){
    stop(message = "Box_id must be sequential from 0 to maximum number")
    }
  }

  #set boundary boxes if boundary not included
  if ("boundary" %!in% names (map_for_bgm@data)){
    #default to 0
    map_for_bgm@data$boundary <- 0
    if(length(boundary_boxes)<2 | is.null(boundary_boxes)){
      stop(message = "boundary_boxes must be named or boundary included in map data")
      }
    for (i in 1:length(boundary_boxes)){
      map_for_bgm@data$boundary[map_for_bgm@data$box_id == boundary_boxes[i]] <- 1
    }
  }

  if ("vertmix"  %!in% names (map_for_bgm@data)){
    map_for_bgm@data$vertmix <- .00001
  }

  if ("horizmix"  %!in% names (map_for_bgm@data)){
    map_for_bgm@data$horizmix <- 1
  }

  #write map for future viewing and use by java applet
  rgdal::writeOGR(map_for_bgm, dsn=bgmerizer_location, layer="map_for_bgmeriser"
                  , driver="ESRI Shapefile", overwrite_layer=T)

  #run bgmeriser
  shell(paste("java -jar ", bgmerizer_location,"/bgmeriser.jar -as 4326 ",
              bgmerizer_location, "/map_for_bgmeriser.shp ",bgmerizer_location,
              "/map_for_bgmeriser.bgm", sep=""), intern=T)

  }


