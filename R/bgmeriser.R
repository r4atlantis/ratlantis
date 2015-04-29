#' bgmerizer function
#'
#' This function creates bgm file format needed for Atlantis
#' @param map_location location of gis layer stored in wgs84 format.  Defaults
#'  to working directory
#' @param map_name name of map to be used for bgm creation. Must incude "box_id"
#' column. Other columns may be included
#' @param boundary_boxes a list of boxes that boundary boxes in the model
#' @param get_bathmetry Should function automatically add depths (T(rue) or
#' F(alse)
#' @param bathymetry_layer_location location of gis layer stored in wgs84 format.
#' Defaults to working directory
#' @param bathymetry_layer_name name of bathmetry shp layer. Must be in wgs84
#' format and include a data column named "Contour"
#' @param bathymetry_cutoff What quantile should be assigned to polygon. Defaults
#' to 90th quantile (to avoid small number of canyons or slope biasing entire
#' polygon)
#' @param bathmetry_levels Depth layers for final Atlantis polygons
#' @param bgmerizer_location location of bgmerizer.jar file, assumed to be in
#' working directory along with java.exe file
#' @keywords bgm
#' @details This function creates bgm file format needed for Atlantis by calling
#' java program. It also adds in depth data (if desired) using provided
#' bathymetry layer.
#' layer.
#' @export

rbgmerizer <- function( map_location = getwd(), map_name, boundary_boxes,
                        get_bathymetry = TRUE,
                        bathymetry_layer_location = getwd(), bathymetry_layer,
                        bathymetry_cutoff = .9, bathymetry_levels = c(-10, -20,
                                                                      -50, -200,
                                                                      -1000, -2000,
                                                                      -4000),
                        bgmerizer_location = getwd()){
  #read in the map and check for wgs84 format
  map_for_bgm <- readOGR(map_location, layer=map_name)
  if (map_for_bgm@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
      stop(message = "Map must be in wgs84 format")
      }
  if ("box_id" %!in% colnames(map_for_bgm@data)){
      stop(message = "Map data must incluce box_id column")
  }

  #get depth stats for each polygon by overlaying bathymetry layer on map layer

  if (get_bathymetry == T){
    bathymetry <- readOGR(bathymetry_layer_location, layer = bathymetry_layer)
    if (bathymetry@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
      stop(message = "Bathymetry layer map must be in wgs84 format")
    }
    if (bathymetry_cutoff >= 1 |  bathymetry_cutoff <= 0){
      stop(message = "Bathymetry cutoff must be between 0 and 1")
    }

    if ("Contour" %!in% colnames(bathymetry@data)){
      stop(message = "Bathymetry data must incluce Contour column")
    }
    if (bathymetry_levels < 0){
      stop(message = "Bathymetry levels must all be negative")
    }

    #since these are depths (negative numbers), we don't count the lowest
    #measurements as set by bathymety_cutoff
    bathymetry_cutoff_quant <- function(x){quantile(x, c(1 - bathymetry_cutoff))}
    depth <- over(map_for_bgm, bathymetry, fn=bathymetry_cutoff_quant)
    depth$Depth <- depth$Contour
    map_for_bgm@data$Depth <- depth$Depth
    map_for_bgm@data$Depth2 <- depth$Depth

    #cut to noted levels
    for (i in 1:nrow(map_for_bgm@data)){
         x <- map_for_bgm@data@Depth[i] - bathymetry_levels
         x <- which(x>0)
         map_for_bgm@Depth[i] <- max(bathymetry_levels[x])
    }


   }


  }







# mymapdata=mymapdata[order(mymapdata$box_id),]
# mymapdata$box_id==seq(0,nrow(mymapdata)-1)# should go 0 to (max) with no breaks
# #make sure boundary boxes are set to boundary=1, others to 0
# #FOR THIS MAP ONLY, TWO BOUNDARY BOX
# #1 is for most boxes (dynamic)
# mymap@data$boundary=0
# #0 and 52 are boundaries (set to 0)
# mymap@data[mymap$box_id==0,"boundary"]=1
# mymap@data[mymap$box_id==52,"boundary"]=1
# #change botz to new dcdepthcolumn
# mymap@data$botz=mymap@data$Depth
# #just make all vertmix .00001
# mymap@data$vertmix=.00001
# #just make all horizmix 1
# mymap@data$horizmix=1
# #rewrite as new shapefile
# unlink("mapforbgmeriser*")
# writeOGR(mymap, ".", "mapforbgmeriser", driver="ESRI Shapefile")
#
# #code to run bgmerizer
# shell(paste("cd", getwd()), intern=T)
# shell("java -jar bgmeriser.jar", intern=T)
# shell("java -jar bgmeriser.jar -as 4326 mapforbgmeriser.shp", intern=T)
#
