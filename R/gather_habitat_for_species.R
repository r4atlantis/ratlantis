#' gather_habitat_for_species function
#'
#' This function aids in gathering data on species occurence that may be useful
#' in constructing functional groups and diet matrices
#' @param species_list_location where is species list located
#' @param species_list_csv name of csv file with the following column headers:
#' Genus, species,common_name.  functional_group may also be included.
#' @param map_location location of shape file used to create bgm
#' @param map_name name of map used for bgm creation; can be produced by rbgmeriser
#' function or created manually
#' @param habitat_list list of potential habitat types to be included in the
#' model
#' @keywords biology prm, functional groups,
#' @details This function aids in gathering data on species occurrence that may
#' be useful in constructing functional groups and diet matrices.  User submits
#' a list of potential species to be included in the model (may include potential
#' functional groupings). The code gathers data on species from rgbif and rfishbase.
#' @export

gather_habitat_for_species <- function(species_list_location, species_list_csv,
                                            map_location = "empty",map_name = "empty",
                                            habitat_list=
                                            c("estuaries", "mangrove", "intertidal",
                                           "soft", "rocky", "marine", "oceanic",
                                           "neritic", "coral reefs", "soft bottom",
                                           "hard bottom", "seagrass beds", "reef",
                                           "sand", "benthic", "slope"))
{
  #read in the species_list
  species <- read.csv(paste(species_list_location, "/", species_list_csv, sep=""),
                                 header=T)
  if ("Genus" %!in% names(species) |
        "species" %!in% names(species) |
        "common_name" %!in% names(species)){
      stop(message = "Species list needs all of the following columns: Genus,  species, common_name")
      }
  #add scientific name for matching
  species$scientific_name <- paste (species$Genus, species$species,
                                         sep = " ")
  species$scientific_name_underscore <- paste (species$Genus, species$species,
                                    sep = "_")

  #habitat information from fishbase
  #find species
  data(fishbase)
  myfish <- rfishbase::findSpecies(species$scientific_name)

  for (i in 1:length(habitat_list)){
    in_habitat <- as.data.frame(rfishbase::which_fish( habitat_list[i], "habitat", fish.data[myfish]))
    names(in_habitat)[1] <- habitat_list[i]
    species <- cbind(species, in_habitat)
  }

  depth_list <- c("bathydemersal", "bathypelagic", "benthopelagic", "reef-associated",
                  "demersal", "pelagic")

  for (i in 1:length(depth_list)){
    in_habitat <- as.data.frame(rfishbase::which_fish( depth_list[i], "habitat", fish.data[myfish]))
    names(in_habitat)[1] <- depth_list[i]
    species <- cbind(species, in_habitat)
  }

  back <- species

  #get occurrence data using spocc

  map_area <- rgdal::readOGR(map_location, layer = map_name)
  max_polygons <- max(map_area@data$box_id)
  column_box_names <- c("scientific_name", paste("box", 0:max_polygons, sep = "_"))
  box_occurrence <- as.data.frame(matrix (nrow = 0, ncol = length(column_box_names)))
  names(box_occurrence) <- column_box_names
  order_box_occurrence <-box_occurrence

  for (i in 1:length(species$scientific_name)){
  #for (i in 1:3){

    df <- spocc::occ(query = species$scientific_name[i], limit = 1000)
    df <- spocc::occ2df(df)
    df <- na.omit(df)
    sp::coordinates(df) <- ~longitude+latitude
    crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +
                   towgs84=0,0,0")  # geographical, datum WGS84
    proj4string(df) <- crs.geo  # define projection system of our data
    summary(df)

    #put over each polygon and count occurences
    #http://gis.stackexchange.com/questions/110117/counts-the-number-of-points-in-a-polygon-in-r
    #http://r-sig-geo.2731867.n2.nabble.com/Counting-points-within-polygons-td6904827.html

    x <- table(over(df, map_area)$box_id)
    if (length(x) > 0){
      z <- as.data.frame(t(as.matrix(x)))
      colnames(z)=paste("box", colnames(z), sep="_")
      z$scientific_name <- species$scientific_name[i]
      box_occurrence <- merge(box_occurrence,z, all.x=T, all.y=T)
    }
  }

  box_occurrence <- box_occurrence[,names(order_box_occurrence)]

  order_species <- species

  species <- merge(species, box_occurrence)

  return (species)

}





