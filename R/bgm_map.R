#' bgm_map function
#'
#' This function uses the shape file created for the bgm to produce initial maps
#' for presentations
#' @param map_location location of gis layer stored in wgs84 format.  Defaults
#'  to working directory
#' @param map_name name of map to be used; defaults to "map_for_bgmerizer)
#' @param geography_layer_location location of gis layer stored in wgs84 format.
#' Defaults to working directory
#' @param geography_layer_name name of geography shp layer. Must be in wgs84
#' format.
#' @param bathymetry_layer_location location of gis layer stored in wgs84 format.
#' Defaults to working directory
#' @param bathymetry_layer_name name of bathmetry shp layer. Must be in wgs84
#' format and include a data column named "Contour"
#' @param bathymetry_cutoff What quantile should be assigned to polygon. Defaults
#' to 90th quantile (to avoid small number of canyons or slope biasing entire
#' polygon)
#' @param map_box_id Should R place box_id numbers in polygons? Defaults to "T"rue.
#' Requires map to include box_id column
#' @keywords maps
#' @details This function produces charts and maps for Atlantis model area
#' @export

bgm_mapr <- function( map_location = getwd(), map_name = map_for_bgmerizer,
                        bathymetry_layer_location = getwd(), bathymetry_layer_name,
                        bathymetry_cutoff = .9, bathymetry_levels = c(-10, -20,
                                                                      -50, -200,
                                                                      -1000, -2000,
                                                                      -4000),
                        map_box_id = T
                        ){
  #read in the map and check for wgs84 format
  map_for_bgmerizer <- readOGR(map_location, layer=map_name)
  if (map_for_bgm@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
      stop(message = "Map must be in wgs84 format")
      }
  if ( map_box_id == T){
    if ("box_id" %!in% colnames(map_for_bgm@data)){
      stop(message = "Map data must incluce box_id column if map_box_id = T")
    }
  }

  worldmap <- readOGR(geography_layer_location, layer = geography_layer_name)
  worldmapfortified=fortify(worldmap)

  #Creat a base plot
  #coord_fixed keeps scale right (1:1)
  #coord_map uses map projections
  p <- ggplot() + theme_bw()

  # read the .shp file - layer is the same name but without the .shp
  #ASSUMES ATLANTIS MAP IS IN WGS84 (epsg 4326
  map_for_bgmerizer_fortified=fortify(map_for_bgmerizer)
  #have to add back in other data
  map_for_bgmerizer@data$box_id <- row.names(map_for_bgmerizer)
  map_for_bgmerizer=merge(map_for_bgmerizer_fortified, map_for_bgmerizer@data)






  buffer=1
  deepcbox=deepc@bbox
  deepcbox[,"min"]=deepcbox[,"min"]-buffer
  deepcbox[,"max"]=deepcbox[,"max"]+buffer

  #Add map to base plot
  california <- p+coord_fixed()+geom_polygon(data=worldmapfortified,
                                             aes(x=long,
                                                 y=lat, group=group), colour="black", fill="grey", size=.5)
  california1=california+coord_fixed(xlim=c(deepcbox[1,1], deepcbox[1,2]), ylim = c(deepcbox[2,1], deepcbox[2,2]))+theme_bw()

  california2=california1 +
    ylab("Latitude")+xlab("Longitude")+
    theme(axis.title.x = element_text(face="bold", size=20),
          axis.text.x  = element_text(size=16),axis.title.y = element_text(face="bold", size=20),
          axis.text.y  = element_text(size=16) )
  california2
  ggsave("Gulfmap, deepc focus.jpg")

  #add in bathymetry lines

  atlwd=paste("C:/Users", username, "Dropbox/Experiments/FSU Projects/Atlantis Model/Deep C Atlantis Model", sep="/")
  bathymetry<- readOGR(paste(atlwd,"/New Shape files I downloaded/GCOOS/w98e78n31s18_isobath_selected_5-4000m", sep="") , layer="w98e78n31s18_isobath_selected_5-4000m")
  bathymetry=spTransform(bathymetry, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  bathymetryfortified=fortify(bathymetry)
  bathymetry@data$id=rownames(bathymetry@data)

  bathymetrydepth=merge(bathymetryfortified, bathymetry@data)
  #pull out layers you want
  bathymetrydepthselect=bathymetrydepth[bathymetrydepth$CONTOUR %in% c("-10", "-20", "-50", "-200", "-500","-1000", "-2000","-4000"),]
  bathymetrydepthselect$CONTOUR=factor(bathymetrydepthselect$CONTOUR)

  contourspots=bathymetrydepthselect[	abs(bathymetrydepthselect$long)>90 & abs(bathymetrydepthselect$long)<93 &
                                        bathymetrydepthselect$lat>24,]
  contourspots=contourspots[!duplicated(contourspots$CONTOUR),]

  california3=california2+geom_path(data=bathymetrydepthselect,
                                    aes(x=long,
                                        y=lat, group=group), colour="#CCCCCC", fill="NA")+
    geom_text(data=contourspots,
              aes(x=long,
                  y=lat, label=CONTOUR), size=3)+
    labs(linetype="Depth")+scale_linetype_discrete(breaks = rev(levels(bathymetrydepthselect$CONTOUR)))


  #also add in some sites
  #eventually need to merge fortified map with list of other attributes
  GOMsites=read.csv("GOM sites.csv")
  california4=california3 + geom_polygon(data = deepcfortified[deepcfortified$boundary!=1, ], aes(x = long, y = lat, group=group), color="purple", fill=NA, size=1.5,alpha=.5)+
    geom_polygon(data = deepcfortified[deepcfortified$boundary==1, ], aes(x = long, y = lat, group=group), color="orange", fill=NA, size=1.5, alpha=.5)+
    #  geom_point(data = GOMsites, aes(x = Longitude, y = Latitude), size=2, colour="white", fill="white")+
    #  geom_text(data = GOMsites, aes(x = Longitude, y = Latitude-.1, label=Name),size=4, colour="black", fontface="bold")+
    ylab("Latitude")+xlab("Longitude")+
    theme(axis.title.x = element_text(face="bold", size=20),
          axis.text.x  = element_text(size=20),axis.title.y = element_text(face="bold", size=20),
          axis.text.y  = element_text(size=20) )

  dev.off()
  california4
  Sys.sleep(15)
  ggsave("DeepCAtlantismap.jpg", width=10, height=5, units="in"))








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
         map_for_bgm@data$Depth[i] <- max(bathymetry_levels[x])
    }


   }

  map_for_bgm@data@boz <- map_for_bgm@data@Depth

  #check to make sure all boxes are sequentially labeled
  map_for_bgm@data == map_for_bgm@data[order(map_for_bgm@data@box_id,]
  if (map_for_bgm@data@box_id != seq (0, nrow(map_for_bgm@data)-1)){
    stop(message = "Box_id must be sequential from 0 to maximum number")
    }

#set boundary boxes if boundary_code not included
if ("boundary_code" %!in% names (map_for_bgm@data)){
  #default to 1
  map_for_bgm@data@boundary_code <-
  if(length(boundary_boxes)<2 | is.null(boundary_boxes)){
    stop(message = "boundary_boxes must be named or boundary_code included in map data")
    }
  for (i in 1:length(boundary_boxes)){
    map_for_bgm@data@boundary_code[boundary_boxes[i] <- 0
  }
}

if ("vertmix"  %!in% names (map_for_bgm@data)){
  map_for_bgm@data@vertmix <- .00001
}

if ("horizmix"  %!in% names (map_for_bgm@data)){
  map_for_bgm@data@horizmix <- 1
}

#write map for future viewing
writeOGR(map_for_bgm, ".", "map_for_bgmeriser", driver="ESRI Shapefile")

#run bgmeriser
shell(paste("cd", bgmerizer_location, intern=T)
shell("java -jar bgmeriser.jar -as 4326 map_for_bgmeriser.shp", intern=T)
}


