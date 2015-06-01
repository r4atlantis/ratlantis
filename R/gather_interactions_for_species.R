#' gather_interactions_for_species function
#'
#' This function aids in gathering data on species interactions that may be useful
#' in constructing functional groups and diet matrices
#' @param species_list_location where is species list located
#' @param species_list_csv name of csv file with the following column headers:
#' Genus, species,common_name.  functional_group may also be included.
#' @param map_location location of shape file used to create bgm
#' @param map_name name of map used for bgm creation; can be produced by rbgmeriser
#' function or created manually
#' @param bbox_coordinates bounding box (in EPSG:4326 decimal degrees, defining
#' "left, bottom, right, top" of bounding box) in which you wish for information
#' on interactions
#' @keywords biology prm, functional groups,
#' @details This function aids in gathering data on species interactions that may
#' be useful in constructing functional groups and diet matrices.  User submits
#' a list of potential species to be included in the model (may include potential
#' functional groupings). The code gathers data on species from rglobi.
#' @export

gather_interactions_for_species <- function(species_list_location, species_list_csv,
                                            map_location = "empty",map_name = "empty",
                                            bbox_coordinates = "empty"){
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

  #species interactions using rglobi
  if ((bbox_coordinates == "empty") & (map_name == "empty")){
    print ("Interactions not returned; map coordinates or shape file must be
           supplied")
  } else if (bbox_coordinates == "empty"){
    map_area <- rgdal::readOGR(map_location, layer=map_name)
    bbox_coordinates <- c(map_area@bbox[1,1], map_area@bbox[2,1], map_area@bbox[1,2],
                          map_area@bbox[2,2])
  }
  speciesinteractions <- rglobi::get_interactions_in_area( bbox=bbox_coordinates)

  return (speciesinteractions)

}



